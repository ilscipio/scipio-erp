package com.ilscipio.scipio.solr.plugin.security;

import java.io.Serializable;
import java.lang.invoke.MethodHandles;
import java.security.Principal;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.FilterChain;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import javax.servlet.http.HttpSession;

import org.apache.http.auth.BasicUserPrincipal;
import org.apache.solr.api.ApiBag;
import org.apache.solr.common.util.ValidatingJsonMap;
import org.apache.solr.security.BasicAuthPlugin;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.security.Security;
import org.ofbiz.security.SecurityConfigurationException;
import org.ofbiz.security.SecurityFactory;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceContainer;
import org.ofbiz.webapp.control.LoginWorker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * EXPERIMENTAL auth plugin for Solr.
 * <p>
 * This implements a MINIMAL version of {@link org.ofbiz.webapp.control.LoginWorker}.
 * It does not implement all aspects of LoginWorker such as visit handling.
 * <p>
 * IMPORTANT: This "filter" must not call the method ServletRequest.getParameter.
 * See {@link #getRequestParameterSafe} for details.
 * <p>
 * TODO: 2018-05-10: several aspects missing to work correctly:
 * * userTenantId (must be hardcoded into username)
 * * perm checks
 */
public class ScipioUserLoginAuthPlugin extends BasicAuthPlugin {
    private static final Logger log = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

    // WARN: Debug.log must only be used from component-level classloader
    private static final String module = ScipioUserLoginAuthPlugin.class.getName();

    private ThreadLocal<ServletRequest> request = new ThreadLocal<>(); // hack around the BasicAuthPlugin interface limitations

    private static final String webappOnlyJarResource = "org/apache/solr/security/BasicAuthPlugin.class";

    public static final String LOGIN_SESSION_ATTR = "scpLoginInfo";

    public static final char USERID_TENANTID_SEP = '#';

    public static final String ALL_TENANTS = "all";
    public static final String ALL_TENANTS_SUFFIX = USERID_TENANTID_SEP + ALL_TENANTS;

    protected String entityDelegatorName = "default";
    protected String localDispatcherName = "solr";
    protected long cachedLoginExpiry = 30000;

    /**
     * Logins allowed to be cached globally, sessionless.
     * <p>
     * In security.json this is:
     * <code>"cacheLogins":["solrquery","solrupdate","tenantuser#tenant1"],</code>
     */
    protected Set<String> cacheLogins = Collections.emptySet();
    /**
     * Logins allowed to be cached globally, sessionless, for all tenants.
     * <p>
     * In security.json this is:
     * <code>"cacheLogins":["solrquery#all","solrupdate#all"],</code>
     */
    protected Set<String> cacheLoginsAllTenants = Collections.emptySet();

    private boolean multitenant = EntityUtil.isMultiTenantEnabled();

    @SuppressWarnings("serial")
    public static class UserLoginInfo implements Serializable {
        protected final String userLoginId;
        protected final String tenantId;
        protected final String solrUsername;
        protected final long loginTime;

        public UserLoginInfo(String userLoginId, String tenantId) {
            this.userLoginId = userLoginId;
            this.tenantId = tenantId;
            if (tenantId != null) {
                this.solrUsername = userLoginId + USERID_TENANTID_SEP + tenantId;
            } else {
                this.solrUsername = userLoginId;
            }
            this.loginTime = System.currentTimeMillis();
        }

        public String getUserLoginId() {
            return userLoginId;
        }

        public String getTenantId() {
            return tenantId;
        }

        /**
         * Returns userLoginId + sep + tenantId.
         */
        public String getSolrUsername() {
            return solrUsername;
        }

        public long getLoginTime() {
            return loginTime;
        }

        public boolean isLoginTimeExpired(long cachedLoginExpiry) {
            return (System.currentTimeMillis() - this.loginTime) >= cachedLoginExpiry;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null || !(obj instanceof UserLoginInfo)) return false;
            UserLoginInfo other = (UserLoginInfo) obj;
            return matches(other.userLoginId, other.tenantId);
        }

        public boolean matches(String userLoginId, String tenantId) {
            if (!this.userLoginId.equals(userLoginId)) return false;
            if (this.tenantId == null) {
                if (tenantId != null) return false;
            } else {
                if (!this.tenantId.equals(tenantId)) return false;
            }
            return true;
        }

        public boolean matches(String solrUsername) {
            return this.solrUsername.equals(solrUsername);
        }
    }

    @Override
    public void init(Map<String, Object> pluginConfig) {
        super.init(pluginConfig);

        String entityDelegatorName = (String) pluginConfig.get("entityDelegatorName");
        if (entityDelegatorName != null && !entityDelegatorName.isEmpty()) {
            this.entityDelegatorName = entityDelegatorName;
        }
        String localDispatcherName = (String) pluginConfig.get("localDispatcherName");
        if (localDispatcherName != null && !localDispatcherName.isEmpty()) {
            this.localDispatcherName = localDispatcherName;
        }
        Object multitenant = pluginConfig.get("multitenant");
        if (multitenant instanceof Boolean) {
            this.multitenant = (Boolean) multitenant;
        }

        @SuppressWarnings("unchecked")
        Collection<String> cacheLoginsList = (Collection<String>) pluginConfig.get("cacheLogins");
        Set<String> cacheLogins = new HashSet<>();
        Set<String> cacheLoginsAllTenants = new HashSet<>();
        if (cacheLoginsList != null) {
            for(String entry : cacheLoginsList) {
                if (entry.endsWith(ALL_TENANTS_SUFFIX)) {
                    cacheLoginsAllTenants.add(entry.substring(0, entry.length() - ALL_TENANTS_SUFFIX.length()));
                } else {
                    cacheLogins.add(entry);
                }
            }
        }
        this.cacheLogins = cacheLogins;
        this.cacheLoginsAllTenants = cacheLoginsAllTenants;

        Object cachedLoginExpiryObj = pluginConfig.get("cachedLoginExpiry");
        if (cachedLoginExpiryObj instanceof Long) {
            this.cachedLoginExpiry = (Long) cachedLoginExpiryObj;
        } else if (cachedLoginExpiryObj instanceof Integer) {
            this.cachedLoginExpiry = ((Integer) cachedLoginExpiryObj).longValue();
        } else if (cachedLoginExpiryObj instanceof String) {
            try {
                this.cachedLoginExpiry = Long.parseLong((String) cachedLoginExpiryObj);
            } catch(Exception e) {
                ; // NOTE: cannot use Debug class here!
            }
        }
    }

    @Override
    public boolean doAuthenticate(ServletRequest req, ServletResponse resp, FilterChain filterChain) throws Exception {
        HttpServletRequest request = (HttpServletRequest) req;

        // check for extLoginKey first
        String extLoginKey = getRawRequestParameter(request, "externalLoginKey");
        if (extLoginKey != null && !extLoginKey.isEmpty() && request instanceof HttpServletRequest) {
            HttpSession session = request.getSession(false);
            UserLoginInfo prevUserLoginInfo = (session != null) ? (UserLoginInfo) session.getAttribute(LOGIN_SESSION_ATTR) : null;
            UserLoginInfo userLoginInfo = authenticateExtLoginKeySafe(request, extLoginKey, prevUserLoginInfo);
            if (userLoginInfo != null) {
                if (userLoginInfo != prevUserLoginInfo) {
                    // NOTE: here we must create session if none
                    request.getSession(true).setAttribute(LOGIN_SESSION_ATTR, userLoginInfo);
                }
                filterChain.doFilter(makeAuthRequestWrapper(request, userLoginInfo), resp);
                return true;
            } else {
                // better not; see justification further below
                //session.removeAttribute(LOGIN_SESSION_ATTR);
            }
        }

        // if there's no Authorization header, check if we have a session login, which
        // would have come from ?externalLoginKey above
        if (request.getHeader("Authorization") == null) {
            HttpSession session = request.getSession(false);
            if (session != null) {
                UserLoginInfo userLoginInfo = (UserLoginInfo) session.getAttribute(LOGIN_SESSION_ATTR);
                if (userLoginInfo != null) {
                    filterChain.doFilter(makeAuthRequestWrapper(request, userLoginInfo), resp);
                    return true;
                }
            }
        }

        this.request.set(request); // hack around the BasicAuthPlugin interface limitations
        return super.doAuthenticate(req, resp, filterChain);
    }

    /**
     * Custom request param extraction code that extracts UTF-8-ENCODED parameter values
     * from the query string manually, avoiding ServletRequest.getParameter.
     * <p>
     * For externalLoginKey there's no need to unencode, but other parameters need
     * UTF-8 decoder.
     * <p>
     * IMPORTANT: This "filter" must not call the method ServletRequest.getParameter.
     * It causes a state change which breaks SolrDispatchFilter. Therefore we must parse
     * the query string manually.
     */
    protected static String getRawRequestParameter(HttpServletRequest request, String name) {
        String queryString = request.getQueryString();
        if (queryString == null) return null;
        int paramValIdx;
        if (queryString.startsWith(name + "=")) {
            paramValIdx = name.length() + 1;
        } else {
            int idx = queryString.indexOf("&" + name + "=");
            if (idx < 0) return null;
            paramValIdx = idx + name.length() + 2;
        }
        // TODO: optimize this
        int termIdx = queryString.indexOf('&', paramValIdx);
        if (termIdx < 0) {
            queryString.indexOf(USERID_TENANTID_SEP, paramValIdx);
        }
        if (termIdx >= 0) {
            return queryString.substring(paramValIdx, termIdx);
        } else {
            return queryString.substring(paramValIdx);
        }
    }

    private HttpServletRequest makeAuthRequestWrapper(HttpServletRequest request, UserLoginInfo userLoginInfo) {
        // see BasicAuthPlugin implementation for reference of why this is here
        return new HttpServletRequestWrapper(request) {
            @Override
            public Principal getUserPrincipal() {
                return new BasicUserPrincipal(userLoginInfo.getSolrUsername());
            }
        };
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

        private final Map<String, UserLoginInfo> successLogins = new ConcurrentHashMap<>();

        @Override
        public void init(Map<String, Object> pluginConfig) {
            if (pluginConfig.get("realm") != null) this.realm = (String) pluginConfig.get("realm");
            else this.realm = "solr";

            promptHeader = Collections.unmodifiableMap(Collections.singletonMap("WWW-Authenticate", "Basic realm=\"" + realm + "\""));
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
            HttpServletRequest request = getRequest();
            if (request == null) return false;

            if (username.isEmpty() || password.isEmpty()) {
                return false;
            }

            // NOTE: username is userLoginId + sep + tenantId

            String userLoginId = username;
            String tenantId = null; // TODO: REVIEW: is there any case this could be needed? getRequestParameterSafe(request, "userTenantId")
            if (multitenant) {
                int tenantSepIdx = username.lastIndexOf(USERID_TENANTID_SEP);
                if (tenantSepIdx >= 0) {
                    tenantId = username.substring(tenantSepIdx + 1);
                    if (tenantId.isEmpty()) tenantId = null;
                    userLoginId = username.substring(0, tenantSepIdx);
                    if (userLoginId.isEmpty()) return false;
                }
            }

            UserLoginInfo userLoginInfo;
            HttpSession session = request.getSession(false);

            if (session != null) {
                // HOW THIS WORKS: for basic auth, the browser resends an "Authorization: Basic xxxxx"
                // HTTP header with every single request. We use an explicit session var check to prevent
                // re-invoking the login at every single request.
                userLoginInfo = (UserLoginInfo) session.getAttribute(LOGIN_SESSION_ATTR);
                if (userLoginInfo != null && userLoginInfo.matches(username)) {
                    // don't try to login again
                    return true;
                }
            }

            //if (cacheLogins.contains(username)) { // redundant and incomplete - see below
            final String cacheKey = username + "::" + password; // includes tenantId
            userLoginInfo = successLogins.get(cacheKey);
            if (userLoginInfo != null) {
                if (userLoginInfo.isLoginTimeExpired(cachedLoginExpiry)) {
                    successLogins.remove(cacheKey);
                } else {
                    // ONLY do this if it does not force us to create a session
                    // - the cache is mainly intended for sessionless requests
                    if (session != null) {
                        session.setAttribute(LOGIN_SESSION_ATTR, userLoginInfo);
                    }
                    return true;
                }
            }
            //}

            userLoginInfo = authenticateUserLoginSafe(request, userLoginId, password, tenantId);

            if (userLoginInfo != null) {
                // NOTE: here we must create a new session (even for otherwise sessionless requests),
                // otherwise the UI requests cannot work
                request.getSession(true).setAttribute(LOGIN_SESSION_ATTR, userLoginInfo);
                if (cacheLogins.contains(userLoginInfo.getSolrUsername())
                        || cacheLoginsAllTenants.contains(userLoginInfo.getUserLoginId())) {
                    successLogins.put(cacheKey, userLoginInfo);
                }
                return true;
            } else {
                if (session != null) {
                    session.removeAttribute(LOGIN_SESSION_ATTR);
                }
            }
            return false;
        }

        protected HttpServletRequest getRequest() {
            ServletRequest req = ScipioUserLoginAuthPlugin.this.request.get();
            if (req == null || !(req instanceof HttpServletRequest)) {
                return null;
            }
            return (HttpServletRequest) req;
        }
    }

    protected UserLoginInfo authenticateUserLoginSafe(HttpServletRequest request, String userLoginId, String password, String tenantId) {
        ClassLoader solrClassLoader = Thread.currentThread().getContextClassLoader();
        ClassLoader ofbizClassLoader = findParentNonWebappClassLoader(solrClassLoader);
        Thread.currentThread().setContextClassLoader(ofbizClassLoader);
        try {
            return authenticateUserLoginCore(request, userLoginId, password, tenantId);
        } finally {
            Thread.currentThread().setContextClassLoader(solrClassLoader);
        }
    }

    protected UserLoginInfo authenticateUserLoginCore(HttpServletRequest request, String userLoginId, String password, String tenantId) {
        Delegator delegator;
        LocalDispatcher dispatcher;

        if (multitenant && tenantId != null) {
            delegator = DelegatorFactory.getDelegator(entityDelegatorName + "#" + tenantId);
            if (delegator == null) {
                Debug.logError("Solr: auth: could not get tenant delegator '" + entityDelegatorName + "#" + tenantId
                        + "'; auth failed", module);
                return null;
            }
        } else {
            delegator = DelegatorFactory.getDelegator(entityDelegatorName);
            if (delegator == null) {
                Debug.logError("Solr: auth: could not get delegator '" + entityDelegatorName
                        + "'; auth failed", module);
                return null;
            }
        }

        dispatcher = ServiceContainer.getLocalDispatcher(localDispatcherName, delegator);

        Map<String, Object> result;
        try {
            result = dispatcher.runSync("userLogin", UtilMisc.toMap("login.username", userLoginId, "login.password", password));
        } catch (GenericServiceException e) {
            Debug.logError(e, "Solr: auth: Error logging in user '" + userLoginId + "'"
                    + (tenantId != null ? " (tenant: " + tenantId + ")" : "") + " through userLogin service: " + e.getMessage(), module);
            return null;
        }
        if (ModelService.RESPOND_SUCCESS.equals(result.get(ModelService.RESPONSE_MESSAGE))) {
            GenericValue userLogin = (GenericValue) result.get("userLogin");

            if (!hasSolrPermsCore(request, delegator, userLogin)) {
                Debug.logInfo("Solr: auth: User login failed from user/pass (no base permissions): "
                        + userLoginId + (tenantId != null ? " (tenant: " + tenantId + ")" : ""), module);
                return null;
            }

            Debug.logInfo("Solr: auth: Logged in user from user/pass: "
                    + userLoginId + (tenantId != null ? " (tenant: " + tenantId + ")" : ""), module);

            return new UserLoginInfo(userLogin.getString("userLoginId"), delegator.getDelegatorTenantId());
        } else {
            Debug.logInfo("Solr: auth: User login failed from user/pass: "
                    + userLoginId + (tenantId != null ? " (tenant: " + tenantId + ")" : ""), module);
        }
        return null;
    }

    protected UserLoginInfo authenticateExtLoginKeySafe(HttpServletRequest request, String extLoginKey, UserLoginInfo prevUserLoginInfo) {
        ClassLoader solrClassLoader = Thread.currentThread().getContextClassLoader();
        ClassLoader ofbizClassLoader = findParentNonWebappClassLoader(solrClassLoader);
        Thread.currentThread().setContextClassLoader(ofbizClassLoader);
        try {
            return authenticateExtLoginKeyCore(request, extLoginKey, prevUserLoginInfo);
        } finally {
            Thread.currentThread().setContextClassLoader(solrClassLoader);
        }
    }

    protected UserLoginInfo authenticateExtLoginKeyCore(HttpServletRequest request, String extLoginKey, UserLoginInfo prevUserLoginInfo) {
        GenericValue userLogin = LoginWorker.checkExternalLoginKeyUserLogin(extLoginKey);
        if (userLogin == null) {
            // NOTE: here we return the old userLoginId. This is not perfect security,
            // but often what happens is a user may have a link to solr with an old expired
            // externalLoginKey while he was already logged in. In that case it is
            // only annoying to deny him access. The externalLoginKey is not perfect.
            return prevUserLoginInfo;
        }

        String newUserLoginId = userLogin.getString("userLoginId");
        String newTenantId = userLogin.getDelegator().getDelegatorTenantId();
        if (prevUserLoginInfo != null && prevUserLoginInfo.matches(newUserLoginId, newTenantId)) {
            return prevUserLoginInfo;
        }

        if (!hasSolrPermsCore(request, userLogin.getDelegator(), userLogin)) {
            return null;
        }

        Debug.logInfo("Solr: auth: Logged in user from externalLoginKey: " + newUserLoginId, module);

        return new UserLoginInfo(newUserLoginId, newTenantId);
    }

    protected boolean hasSolrPermsCore(HttpServletRequest request, Delegator delegator, GenericValue userLogin) {
        Security security = null;
        try { // TODO?: try to store the Security instance?
            security = SecurityFactory.getInstance(delegator);
        } catch (SecurityConfigurationException e) {
            Debug.logError(e, "Solr: auth: Error getting ofbiz Security instance: " + e.getMessage(), module);
            return false;
        }
        String serverId = (String) request.getServletContext().getAttribute("_serverId"); // set by CatalinaContainer
        return LoginWorker.hasBasePermission(userLogin, request, security, serverId);
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
