package org.ofbiz.webapp.control;

import com.ilscipio.scipio.ce.webapp.control.util.WebappConfig;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.util.EntityUtilProperties;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.lang.reflect.Field;
import java.util.Map;

/**
 * Packages and caches {@link LoginWorker} settings to avoid overhead/errors.
 *
 * <p>NOTE: This can use either {@link HttpServletRequest#getContextPath()} or {@link ServletContext#getContextPath()}.</p>
 *
 * <p>TODO: annotation for client overrides</p>
 * <p>TODO: migrate more settings here</p>
 *
 * <p>SCIPIO: 3.0.0: Added.</p>
 */
public class LoginConfig extends WebappConfig {

    private static final UtilCache<String, LoginConfig> CACHE = UtilCache.createUtilCache("config.login.main");

    // TODO?: Annotations support (not necessarily better than web.xml due to needless class redefinitions/extensions in several webapps)
    //@Retention(RetentionPolicy.RUNTIME)
    //public @interface Def {
    //}

    protected final boolean autoUserLoginEnabled;
    protected final int autoUserLoginAuthTokenLength;
    protected final int autoUserLoginAuthTokenExpireTime;
    protected final String autoUserLoginCookieNameExpr;
    protected final String autoUserLoginCookieName;
    protected final boolean autoUserLoginCookieAllowPlainValue;
    protected final int autoUserLoginCookieMaxAge;

    // TODO: migrate more of

    protected LoginConfig(Delegator delegator, ServletContext servletContext, HttpSession session, HttpServletRequest request) {
        Boolean aulEnabled = UtilMisc.booleanValue(servletContext.getAttribute("autoUserLoginOn")); // legacy support
        autoUserLoginEnabled = (aulEnabled != null) ? aulEnabled : UtilProperties.asBoolean(EntityUtilProperties.getWebappPropertyValue("security", "autoUserLogin.enabled", delegator, servletContext), true);
        autoUserLoginAuthTokenLength = UtilProperties.asInteger(EntityUtilProperties.getWebappPropertyValue("security", "autoUserLogin.authToken.length", delegator, servletContext), 32);
        autoUserLoginAuthTokenExpireTime = UtilProperties.asInteger(EntityUtilProperties.getWebappPropertyValue("security", "autoUserLogin.authToken.expireTime", delegator, servletContext), 60*60*24*7);
        autoUserLoginCookieNameExpr = EntityUtilProperties.getWebappPropertyValue("security", "autoUserLogin.cookie.name", "${appName}.autoUserLoginId", delegator, servletContext);
        autoUserLoginCookieName = (request != null) ? LoginWorker.expandCookieName(request, autoUserLoginCookieNameExpr) : null;
        autoUserLoginCookieAllowPlainValue = UtilProperties.asBoolean(EntityUtilProperties.getWebappPropertyValue("security", "autoUserLogin.cookie.allowPlainValue", delegator, servletContext), true);
        autoUserLoginCookieMaxAge = UtilProperties.asInteger(EntityUtilProperties.getWebappPropertyValue("security", "autoUserLogin.cookie.maxAge", delegator, servletContext), 60*60*24*30);
    }

    public static class Factory extends WebappConfig.Factory<LoginConfig> {
        public LoginConfig make(Delegator delegator, ServletContext servletContext, HttpSession session, HttpServletRequest request) {
            return new LoginConfig(delegator, servletContext, session, request);
        }
    }

    public static LoginConfig from(HttpServletRequest request) {
        return from(Delegator.from(request), request);
    }

    /**
     * Returns new config from HttpServletRequest, HttpSession or ServletContext
     *
     * <p>NOTE: Always pass request when available; when not, delegator needs to be from request or session to support multitenant.</p>
     */
    public static LoginConfig from(Delegator delegator, Object servletObject) {
        String cacheKey = delegator.getDelegatorName() + "::" + UtilHttp.getWebappContextCacheKey(servletObject);
        LoginConfig config = CACHE.get(cacheKey);
        if (config != null) {
            return config;
        }
        config = WebappConfig.make("security", "login.config.class", Factory.class, delegator, servletObject);
        CACHE.put(cacheKey, config);
        return config;
    }

    public boolean isAutoUserLoginEnabled() {
        return autoUserLoginEnabled;
    }

    public int getAutoUserLoginAuthTokenLength() {
        return autoUserLoginAuthTokenLength;
    }

    public int getAutoUserLoginAuthTokenExpireTime() {
        return autoUserLoginAuthTokenExpireTime;
    }

    public boolean isAutoUserLoginCookieAllowPlainValue() {
        return autoUserLoginCookieAllowPlainValue;
    }

    public int getAutoUserLoginCookieMaxAge() {
        return autoUserLoginCookieMaxAge;
    }

    public String getAutoUserLoginCookieNameExpr() {
        return autoUserLoginCookieNameExpr;
    }

    /**
     * Returns cookie name, or null if not constructed using {@link HttpServletRequest}.
     */
    public String getAutoUserLoginCookieName() {
        return autoUserLoginCookieName;
    }

    @Override
    public String toString() {
        return "{" +
                "autoUserLoginEnabled=" + autoUserLoginEnabled +
                ", autoUserLoginAuthTokenLength=" + autoUserLoginAuthTokenLength +
                ", autoUserLoginAuthTokenExpireTime=" + autoUserLoginAuthTokenExpireTime +
                ", autoUserLoginCookieNameExpr='" + autoUserLoginCookieNameExpr + '\'' +
                ", autoUserLoginCookieName='" + autoUserLoginCookieName + '\'' +
                ", autoUserLoginCookieAllowPlainValue=" + autoUserLoginCookieAllowPlainValue +
                ", autoUserLoginCookieMaxAge=" + autoUserLoginCookieMaxAge +
                '}';
    }

}
