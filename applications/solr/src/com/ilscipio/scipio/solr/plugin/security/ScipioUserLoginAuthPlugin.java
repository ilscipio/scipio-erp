package com.ilscipio.scipio.solr.plugin.security;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.FilterChain;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import org.apache.solr.api.ApiBag;
import org.apache.solr.common.util.ValidatingJsonMap;
import org.apache.solr.security.BasicAuthPlugin;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceContainer;

/**
 * EXPERIMENTAL auth plugin for Solr.
 * <p>
 * TODO: 2018-05-10: several aspects missing to work correctly:
 * * userTenantId (must be hardcoded into username)
 * * externalLoginKey (if possible)
 * * hardcoded dispatcher and settings
 * * perm checks
 */
public class ScipioUserLoginAuthPlugin extends BasicAuthPlugin {

    private ThreadLocal<ServletRequest> request = new ThreadLocal<>(); // hack around the BasicAuthPlugin interface limitations
    private ThreadLocal<String> externalLoginKey = new ThreadLocal<>(); // hack around the BasicAuthPlugin interface limitations

    private static final String webappOnlyJarResource = "org/apache/solr/security/BasicAuthPlugin.class";

    @Override
    public boolean doAuthenticate(ServletRequest request, ServletResponse response, FilterChain chain) throws Exception {
        this.request.set(request);
        this.externalLoginKey.set(request.getParameter("externalLoginKey"));
        return super.doAuthenticate(request, response, chain);
    }

    @Override
    protected AuthenticationProvider getAuthenticationProvider(Map<String, Object> pluginConfig) {
        //return super.getAuthenticationProvider(pluginConfig);
        AuthenticationProvider provider = new ScipioUserLoginAuthenticationProvider();
        provider.init(pluginConfig);
        return provider;
    }

    /**
     * ScipioUserLoginAuthenticationProvider, derived from {@link org.apache.solr.security.Sha256AuthenticationProvider}.
     */
    public class ScipioUserLoginAuthenticationProvider implements AuthenticationProvider {
        private String realm;
        private Map<String, String> promptHeader;

        private final Map<String, Boolean> successLogins = new ConcurrentHashMap<>();
        private long lastLoginClearTime;

        private long maxLoginCacheTime = 10000;

        @Override
        public void init(Map<String, Object> pluginConfig) {
            if (pluginConfig.get("realm") != null) this.realm = (String) pluginConfig.get("realm");
            else this.realm = "solr";

            promptHeader = Collections.unmodifiableMap(Collections.singletonMap("WWW-Authenticate", "Basic realm=\"" + realm + "\""));

            lastLoginClearTime = System.currentTimeMillis();
        }

        @Override
        public ValidatingJsonMap getSpec() {
            return ApiBag.getSpec("cluster.security.BasicAuth.Commands").getSpec();
        }

        @Override
        public Map<String, String> getPromptHeaders() {
            return promptHeader;
        }

        @Override
        public boolean authenticate(String username, String password) {
            checkLoginExpiry();

            final String cacheKey = username + "::" + password;
            if (successLogins.containsKey(cacheKey)) return true;

            ServletRequest request = ScipioUserLoginAuthPlugin.this.request.get();
            if (request == null || !(request instanceof HttpServletRequest)) {
                return false;
            }

            GenericValue userLogin = authenticateUserLoginSafe((HttpServletRequest) request, username, password);

            // WARN: do NOT call any GenericValue methods here! unsafe!
            if (userLogin != null) {
                successLogins.put(cacheKey, Boolean.TRUE);
                return true;
            }
            return false;
        }

        protected void checkLoginExpiry() {
            long currTime = System.currentTimeMillis();
            if ((currTime - lastLoginClearTime) > maxLoginCacheTime) {
                lastLoginClearTime = currTime;
                successLogins.clear();
            }
        }
    }

    public static GenericValue authenticateUserLoginSafe(HttpServletRequest request, String username, String password) {
        ClassLoader solrClassLoader = Thread.currentThread().getContextClassLoader();
        ClassLoader ofbizClassLoader = findParentNonWebappClassLoader(solrClassLoader);
        Thread.currentThread().setContextClassLoader(ofbizClassLoader);
        try {
            return authenticateUserLoginCore(request, username, password);
        } finally {
            Thread.currentThread().setContextClassLoader(solrClassLoader);
        }
    }

    protected static GenericValue authenticateUserLoginCore(HttpServletRequest request, String username, String password) {
        // FIXME: hardcoded names and missing userTenantId handling

        if (username == null || username.isEmpty() || password == null || password.isEmpty()) {
            return null;
        }

        String tenantId = null;
        int tenantSepIdx = username.lastIndexOf(':');
        if (tenantSepIdx > 0 && tenantSepIdx < (username.length() - 1)) {
            tenantId = username.substring(tenantSepIdx + 1);
            username = username.substring(0, tenantSepIdx);
        }

        Delegator delegator = DelegatorFactory.getDelegator("default");
        LocalDispatcher dispatcher = ServiceContainer.getLocalDispatcher("solr", delegator);
        if (tenantId != null) {
            // TODO: delegator for tenantId
            delegator = DelegatorFactory.getDelegator("default");
            dispatcher = ServiceContainer.getLocalDispatcher("solr", delegator);
        } else {
            delegator = DelegatorFactory.getDelegator("default");
            dispatcher = ServiceContainer.getLocalDispatcher("solr", delegator);
        }

        // TODO: try to handle externalLoginKey if present...

        Map<String, Object> result;
        try {
            result = dispatcher.runSync("userLogin", UtilMisc.toMap("login.username", username, "login.password", password));
        } catch (GenericServiceException e) {
            return null;
        }
        if (ModelService.RESPOND_SUCCESS.equals(result.get(ModelService.RESPONSE_MESSAGE))) {
            return (GenericValue) result.get("userLogin");
        }
        return null;
    }

    /**
     * Finds the first classloader in the given classloader's hierarchy that does not contain
     * any Solr webapp classes.
     */
    protected static ClassLoader findParentNonWebappClassLoader(ClassLoader classLoader) {
        return findFirstClassLoaderAncestorWithoutResource(classLoader, webappOnlyJarResource);
    }

    protected static ClassLoader findFirstClassLoaderAncestorWithoutResource(ClassLoader cl, String resourceName) {
        ClassLoader ancestorCl = cl;
        while(ancestorCl != null) {
            if (ancestorCl.getResource(resourceName) == null) {
                return ancestorCl;
            }
            ancestorCl = ancestorCl.getParent();
        }
        return null;
    }

}
