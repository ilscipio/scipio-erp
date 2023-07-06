package com.ilscipio.scipio.base.util;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Session attribute handler/resolver for determining locale, timeZone and currencyUom from regular webapps and store webapps.
 *
 * <p>This is meant to be overridden (see product accessOpResolver) and defined in web.xml as "attrHandler"
 * init-parameter in shops and elsewhere.</p>
 *
 * <p>To skip {@link #from} factory/cache methods, you can simply go straight to {@link #resolver} static factory methods
 * which is easier for non-readonly operations.</p>
 *
 * <p>SCIPIO: 3.0.0: Added.</p>
 */
public class AttrHandler {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String CLASS_PROPERTY = "attrHandler.class";
    public static final String DEFAULT_ATTR_ACCESS_OP_PROPERTY = "attrHandler.defaultAttrAccessOp";
    public static final String BASICLOGIN_EVENT_PROPERTY = "attrHandler.events.basicLogin";

    private static final Map<String, AttrHandler> CACHE = new ConcurrentHashMap<>();
    private static final String DEFAULT_CLASS_NAME = UtilProperties.getPropertyValue("general", CLASS_PROPERTY, Factory.class.getName());

    /**
     * Whether to automatically cache (set) the session variables in getLocale/getTimeZone/getCurrencyUom calls -
     * currently defaults to no.
     *
     * <p>NOTE: False default reflects previous UtilHttp.getLocale() behavior and is because context filter was often too late
     * in the filter chain and various other attributes may be missing early in the filter chain, and also because the stores
     * depended of first-visit and after-login events to set session variables, which ran later.</p>
     */
    private static final AttrAccessOp DEFAULT_ATTR_ACCESS_OP = AttrAccessOp.valueOf(UtilProperties.getPropertyValue("general", DEFAULT_ATTR_ACCESS_OP_PROPERTY, AttrAccessOp.GET.toString()));
    private static final boolean USE_BASICLOGIN_EVENT = UtilProperties.getPropertyAsBoolean("general", BASICLOGIN_EVENT_PROPERTY, true);

    private static final Class<?> DEFAULT_CLASS = resolveClass(DEFAULT_CLASS_NAME);
    private static final AttrHandler DEFAULT = make(DEFAULT_CLASS, null);

    protected final AttrAccessOp defaultAttrAccessOp;
    protected final boolean useBasicLoginEvent;

    public AttrHandler(ServletContext servletContext) { // servletContext may be null
        this.defaultAttrAccessOp = AttrAccessOp.valueOf(UtilValidate.altIfEmpty(servletContext != null ? servletContext.getInitParameter(DEFAULT_ATTR_ACCESS_OP_PROPERTY) : null, DEFAULT_ATTR_ACCESS_OP.toString()));
        this.useBasicLoginEvent = UtilMisc.booleanValue(servletContext != null ? servletContext.getInitParameter(BASICLOGIN_EVENT_PROPERTY) : null, USE_BASICLOGIN_EVENT);
    }

    public static class Factory {
        public AttrHandler make(ServletContext servletContext) {
            return new AttrHandler(servletContext);
        }
    }

    // TODO?: Annotations support (not necessarily better than web.xml due to needless class redefinitions/extensions in several webapps)
    //@Retention(RetentionPolicy.RUNTIME)
    //public @interface Def {
    //}

    /**
     * Returns the default attribute resolver for backend and generic webapps (without store-handling code).
     */
    public static <A extends AttrHandler> A getDefault() {
        return UtilGenerics.cast(DEFAULT);
    }

    /**
     * Returns the attribute resolver for the webapp.
     */
    public static <A extends AttrHandler> A from(Object servletObject) {
        ServletContext servletContext = UtilHttp.getServletContext(servletObject);
        String cacheKey = servletContext.getAttribute("_serverId") + "::" + servletContext.getContextPath();
        AttrHandler resolver = CACHE.get(cacheKey);
        if (resolver == null) {
            String resolverClsName = servletContext.getInitParameter(CLASS_PROPERTY);
            if (UtilValidate.isNotEmpty(resolverClsName)) {
                resolver = make(resolveClass(resolverClsName), servletContext);
            }
            if (resolver == null) {
                resolver = make(DEFAULT_CLASS, servletContext);
                //resolver = getDefault();
            }
            CACHE.put(cacheKey, resolver); // Always populate the cache for speed + prevents duplicate errors
        }
        return UtilGenerics.cast(resolver);
    }

    /**
     * Returns the attribute resolver for the webapp, either cached if matches the given target type or creating
     * a new instance if not cached/cacheable.
     */
    public static <A extends AttrHandler> A from(Object servletObject, Class<?> targetType) {
        AttrHandler attrHandler = from(servletObject);
        if (targetType.isAssignableFrom(attrHandler.getClass())) {
            return UtilGenerics.cast(attrHandler);
        } else {
            return make(targetType, UtilHttp.getServletContext(servletObject));
        }
    }

    /**
     * Returns a resolver for non-cached operations.
     */
    public static <R extends Resolver> R resolver(HttpServletRequest request) {
        return from(request).resolver(request, (HttpSession) null);
    }

    /**
     * Returns a resolver for non-cached operations.
     */
    public static <R extends Resolver> R resolver(HttpServletRequest request, Class<?> targetHandlerType) {
        return from(request, targetHandlerType).resolver(request, (HttpSession) null);
    }

    /**
     * Returns a resolver for non-cached operations.
     */
    public static <R extends Resolver> R resolver(HttpSession session) {
        return from(session).resolver(null, session);
    }

    /**
     * Returns a resolver for non-cached operations.
     */
    public static <R extends Resolver> R resolver(HttpSession session, Class<?> targetHandlerType) {
        return from(session, targetHandlerType).resolver(null, session);
    }

    protected static Class<?> resolveClass(String resolverClsName) {
        try {
            return Class.forName(resolverClsName);
        } catch (ReflectiveOperationException e) {
            Debug.logError(e, "Could not resolve webapp attribute handler class [" + resolverClsName + "]", module);
            return null;
        }
    }

    protected static <A extends AttrHandler> A make(Class<?> resolverCls, ServletContext servletContext) {
        if (resolverCls == null) {
            return null;
        }
        try {
            if (Factory.class.isAssignableFrom(resolverCls)) {
                return UtilGenerics.cast(((Factory) resolverCls.getConstructor().newInstance()).make(servletContext));
            } else {
                try {
                    return UtilGenerics.cast(resolverCls.getConstructor(ServletContext.class).newInstance(servletContext));
                } catch (NoSuchMethodException e) {
                    return UtilGenerics.cast(resolverCls.getConstructor().newInstance());
                }
            }
        } catch (ReflectiveOperationException e) {
            Debug.logError(e, "Could not instantiate webapp attribute handler [" + resolverCls.getName() + "]" +
                    (servletContext != null ? " for webapp " + servletContext.getContextPath() : ""), module);
            return null;
        }
    }

    public AttrAccessOp getDefaultAttrAccessOp(HttpSession session) {
        return defaultAttrAccessOp;
    }

    public boolean isUseBasicLoginEvent() {
        return useBasicLoginEvent;
    }

    public AttrAccessOp getAttrAccessOp(AttrAccessOp accessOp, HttpSession session) {
        return (accessOp != null) ? accessOp : getDefaultAttrAccessOp(session);
    }

    protected HttpSession getSession(HttpServletRequest request, HttpSession session) {
        return (session != null) ? session : request.getSession(false);
    }

    /**
     * Returns (cached) session locale, or resolves one if not set.
     */
    public Locale getSessionLocale(HttpServletRequest request, AttrAccessOp accessOp) {
        return getSessionLocale(request, null, accessOp, null);
    }

    /**
     * Returns (cached) session locale, or resolves one if not set.
     */
    public Locale getSessionLocale(HttpSession session, AttrAccessOp accessOp) {
        return getSessionLocale(null, session, accessOp, null);
    }

    /**
     * Returns (cached) session locale, or resolves one if not set.
     */
    public Locale getSessionLocale(HttpServletRequest request, HttpSession session, AttrAccessOp accessOp, Resolver resolver) {
        session = getSession(request, session);
        accessOp = getAttrAccessOp(accessOp, session);
        Locale locale = null;
        if (accessOp.doGetCache()) {
            locale = (session != null) ? (Locale) session.getAttribute("locale") : null;
            if (accessOp.doClearCacheOnly()) {
                UtilHttp.setLocale(session, (Locale) null);
                return locale;
            } else if (locale != null) {
                return locale;
            }
        }
        if (accessOp.doResolve()) {
            locale = (resolver != null ? resolver : resolver(request, session)).resolveLocale();
        }
        if (accessOp.doSetCache() && (accessOp.doClearCache() || locale != null)) {
            UtilHttp.setLocale(session, locale);
        }
        return locale;
    }

    /**
     * Returns (cached) session timeZone, or resolves one if not set.
     */
    public TimeZone getSessionTimeZone(HttpServletRequest request, AttrAccessOp accessOp) {
        return getSessionTimeZone(request, null, accessOp, null);
    }

    /**
     * Returns (cached) session timeZone, or resolves one if not set.
     */
    public TimeZone getSessionTimeZone(HttpSession session, AttrAccessOp accessOp) {
        return getSessionTimeZone(null, session, accessOp, null);
    }

    /**
     * Returns (cached) session timeZone, or resolves one if not set.
     */
    public TimeZone getSessionTimeZone(HttpServletRequest request, HttpSession session, AttrAccessOp accessOp, Resolver resolver) {
        session = getSession(request, session);
        accessOp = getAttrAccessOp(accessOp, session);
        TimeZone timeZone = null;
        if (accessOp.doGetCache()) {
            timeZone = (session != null) ? (TimeZone) session.getAttribute("timeZone") : null;
            if (accessOp.doClearCacheOnly()) {
                UtilHttp.setTimeZone(session, (TimeZone) null);
                return timeZone;
            } else if (timeZone != null) {
                return timeZone;
            }
        }
        if (accessOp.doResolve()) {
            timeZone = (resolver != null ? resolver : resolver(request, session)).resolveTimeZone();
        }
        if (accessOp.doSetCache() && (accessOp.doClearCache() || timeZone != null)) {
            UtilHttp.setTimeZone(session, timeZone);
        }
        return timeZone;
    }

    /**
     * Returns (cached) session currencyUom, or resolves one if not set.
     */
    public String getSessionCurrencyUom(HttpServletRequest request, AttrAccessOp accessOp) {
        return getSessionCurrencyUom(request, null, accessOp, null);
    }

    /**
     * Returns (cached) session currencyUom, or resolves one if not set.
     */
    public String getSessionCurrencyUom(HttpSession session, AttrAccessOp accessOp) {
        return getSessionCurrencyUom(null, session, accessOp, null);
    }

    /**
     * Returns (cached) session currencyUom, or resolves one if not set.
     */
    public String getSessionCurrencyUom(HttpServletRequest request, HttpSession session, AttrAccessOp accessOp, Resolver resolver) {
        session = getSession(request, session);
        accessOp = getAttrAccessOp(accessOp, session);
        String currencyUom = null;
        if (accessOp.doGetCache()) {
            currencyUom = (session != null) ? (String) session.getAttribute("currencyUom") : null;
            if (accessOp.doClearCacheOnly()) {
                UtilHttp.setCurrencyUom(session, (String) null);
                return currencyUom;
            } else if (currencyUom != null) {
                return currencyUom;
            }
        }
        if (accessOp.doResolve()) {
            currencyUom = (resolver != null ? resolver : resolver(request, session)).resolveCurrencyUom();
        }
        if (accessOp.doSetCache() && (accessOp.doClearCache() || currencyUom != null)) {
            UtilHttp.setCurrencyUom(session, currencyUom);
        }
        return currencyUom;
    }

    /**
     * Returns the userLogin or autoUserLogin from the session, typically as used in resolve calls.
     *
     * <p>May be wanted by client code to prevent multiple session accesses.</p>
     */
    public <E extends Map<String, Object>> E getUserLogin(HttpSession session) {
        if (session == null) {
            return null;
        }
        E userLogin = UtilGenerics.cast(session.getAttribute("userLogin"));
        if (userLogin == null) {
            userLogin = UtilGenerics.cast(session.getAttribute("autoUserLogin"));
        }
        return userLogin;
    }

    /**
     * Returns a resolver for non-cached operations.
     */
    public <R extends Resolver> R resolver(HttpServletRequest request, HttpSession session) {
        return UtilGenerics.cast(resolver().init(request, session));
    }

    protected Resolver resolver() {
        return UtilGenerics.cast(new Resolver());
    }

    /**
     * Resolver class for resolution options and holds (any needed) context of resolve operations.
     *
     * <p>NOTE: This class has a local variable scope only - do not keep an instance.</p>
     */
    public class Resolver {
        protected HttpServletRequest request; // WARN: May be null due to
        protected HttpSession session;

        protected Map<String, Object> userLogin;
        protected boolean checkUserLogin = true;
        protected boolean cacheUserLogin = true;
        protected boolean checkClientRequest = true;

        protected Resolver init(HttpServletRequest request, HttpSession session) { // No constructors in sub-classes
            this.request = request;
            this.session = getSession(request, session);
            return this;
        }

        public AttrHandler handler() {
            return AttrHandler.this;
        }

        // Options (for callers)

        /**
         * Sets explicit user login to use, sometimes useful for consistency across the calls.
         *
         * <p>NOTE: If this is set explicitly, {@link #checkUserLogin(boolean)} is implicitly called.</p>
         */
        public Resolver userLogin(Map<String, Object> userLogin) {
            this.userLogin = userLogin;
            checkUserLogin(userLogin != null);
            return this;
        }

        public Resolver checkUserLogin(boolean checkUserLogin) {
            this.checkUserLogin = checkUserLogin;
            return this;
        }

        public Resolver cacheUserLogin(boolean cacheUserLogin) {
            this.cacheUserLogin = cacheUserLogin;
            return this;
        }

        public Resolver checkClientRequest(boolean checkClientRequest) {
            this.checkClientRequest = checkClientRequest;
            return this;
        }

        // Operations

        public Locale getSessionLocale(AttrAccessOp accessOp) {
            return AttrHandler.this.getSessionLocale(request, session, accessOp, this);
        }

        /**
         * Resolves locale using override, user, default/fallback logic - never returns null.
         *
         * <p>Does not use session (cache) var. By default never returns null.</p>
         */
        public Locale resolveLocale() {
            Locale locale;
            locale = resolveExplicitLocale();
            if (locale != null) {
                return locale;
            }
            locale = resolveImplicitLocale();
            if (locale != null) {
                return locale;
            }
            return resolveDefaultLocale();
        }

        /**
         * Resolves explicitly chosen (or overriding) locale, meaning a locale you would set after-login, or null.
         *
         * <p>Meant for after-login use where you want to preserve a previously-set session locale (set anonymously) unless
         * the UserLogin.lastLocale was available (in which case it overrides the anonymous choice).</p>
         */
        public Locale resolveExplicitLocale() {
            return resolveUserLoginLocale();
        }

        /**
         * Resolves implicit user/client/browser locale, meaning a locale you would set as anonymous and less important than userLogin.
         */
        public Locale resolveImplicitLocale() {
            return resolveClientRequestLocale();
        }

        /**
         * Resolve a default locale - may be overridden by subclass.
         */
        public Locale resolveDefaultLocale() {
            return resolveSystemLocale();
        }

        public Locale resolveUserLoginLocale() {
            Locale locale = UtilMisc.asLocale(getUserLoginField("lastLocale"));
            return (locale != null) ? filterLocale(locale, false) : null;
        }

        /**
         * Gets locale from accept headers.
         *
         * <p>TODO: REVIEW: Unclear if default implementation should include this anymore (or return null) - for stores behavior
         *     can be overridden or filtered.</p>
         */
        public Locale resolveClientRequestLocale() {
            if (!checkClientRequest || request == null) {
                return null;
            }
            List<Locale> clientLocales = UtilHttp.getClientRequestLocales(request, true, true);
            return UtilValidate.isNotEmpty(clientLocales) ? filterLocale(clientLocales, false) : null;
        }

        public Locale resolveSystemLocale() {
            return Locale.getDefault();
        }

        /**
         * Locale filters for user login and client request locales - may be overridden by subclass to implement
         * and may also implement here in future (TODO?).
         */
        public Locale filterLocale(Object locale, Boolean exact) {
            return UtilMisc.asLocale(UtilMisc.firstSafe(locale));
        }

        public TimeZone getSessionTimeZone(AttrAccessOp accessOp) {
            return AttrHandler.this.getSessionTimeZone(request, session, accessOp, this);
        }

        /**
         * Resolves timeZone using override, user, default/fallback logic.
         *
         * <p>Does not use session (cache) var. By default never returns null.</p>
         */
        public TimeZone resolveTimeZone() {
            TimeZone timeZone;
            timeZone = resolveExplicitTimeZone();
            if (timeZone != null) {
                return timeZone;
            }
            timeZone = resolveImplicitTimeZone();
            if (timeZone != null) {
                return timeZone;
            }
            return resolveDefaultTimeZone();
        }

        public TimeZone resolveExplicitTimeZone() {
           return resolveUserLoginTimeZone();
        }

        public TimeZone resolveImplicitTimeZone() {
            return resolveClientRequestTimeZone();
        }

        /**
         * Resolve a default time zone - may be overridden by subclass.
         */
        public TimeZone resolveDefaultTimeZone() {
            return resolveSystemTimeZone();
        }

        public TimeZone resolveUserLoginTimeZone() {
            TimeZone timeZone = UtilDateTime.asTimeZone(getUserLoginField("lastTimeZone"));
            return (timeZone != null) ? filterTimeZone(timeZone, null) : null;
        }

        public TimeZone resolveClientRequestTimeZone() {
            // TODO?: Not implemented
            if (!checkClientRequest || request == null) {
                return null;
            }
            return null;
        }

        public TimeZone resolveSystemTimeZone() {
            return TimeZone.getDefault();
        }

        /**
         * Time zone filters for user login and client request time zones - may be overridden by subclass to implement
         * and may also implement here in future (TODO?).
         */
        public TimeZone filterTimeZone(Object timeZone, Boolean exact) {
            return UtilDateTime.asTimeZone(UtilMisc.firstSafe(timeZone));
        }

        public String getSessionCurrencyUom(AttrAccessOp accessOp) {
            return AttrHandler.this.getSessionCurrencyUom(request, session, accessOp, this);
        }

        /**
         * Resolves currencyUom using override, user, default/fallback logic.
         *
         * <p>Does not use session (cache) var. By default never returns null.</p>
         */
        public String resolveCurrencyUom() {
            String currencyUom;
            currencyUom = resolveExplicitCurrencyUom();
            if (currencyUom != null) {
                return currencyUom;
            }
            currencyUom = resolveImplicitCurrencyUom();
            if (currencyUom != null) {
                return currencyUom;
            }
            return resolveDefaultCurrencyUom();
        }

        public String resolveExplicitCurrencyUom() {
            return resolveUserLoginCurrencyUom();
        }

        public String resolveImplicitCurrencyUom() {
            return resolveClientRequestCurrencyUom();
        }

        /**
         * Resolve a default currency - may be overridden by subclass.
         */
        public String resolveDefaultCurrencyUom() {
            return resolveSystemCurrencyUom();
        }

        public String resolveUserLoginCurrencyUom() {
            String currencyUom = getUserLoginField("lastCurrencyUom");
            return (currencyUom != null) ? filterCurrencyUom(currencyUom, null) : null;
        }

        public String resolveClientRequestCurrencyUom() {
            // TODO?: Not implemented
            if (!checkClientRequest || request == null) {
                return null;
            }
            return null;
        }

        public String resolveSystemCurrencyUom() {
            return UtilProperties.getPropertyValue("general", "currency.uom.id.default", "USD");
        }

        /**
         * Currency filters for user login and client request currencies - may be overridden by subclass to implement
         * and may also implement here in future (TODO?).
         */
        public String filterCurrencyUom(Object currencyUom, Boolean exact) {
            return UtilMisc.firstSafe(currencyUom);
        }

        /**
         * Re-implementation of <code>org.ofbiz.product.product.ProductEvents#setDefaultStoreSettings</code>.
         *
         * <p>Intended for first-visit events.</p>
         *
         * <p>NOTE: currencyUom handled in StoreAttrHandler.StoreResolver</p>
         */
        public void setDefaultSessionSettings() {
            UtilHttp.setLocale(session, resolveLocale());
            UtilHttp.setTimeZone(session, resolveTimeZone());
        }

        /**
         * Re-implementation of <code>org.ofbiz.product.product.ProductEvents#setDefaultUserSettings</code>.
         *
         * <p>Intended for after-login events. For this reason, this method tries to preserve any previous or
         * explicitly-set session attributes and store defaults previously set unless another explicit locale
         * is found such as UserLogin.lastLocale. If only implicit or default locales are </p>
         *
         * <p>NOTE: currencyUom handled in StoreAttrHandler.StoreResolver</p>
         */
        public void setDefaultUserSettings() {
            Locale locale = resolveExplicitLocale();
            if (locale != null) {
                UtilHttp.setLocale(session, locale);
            } else {
                locale = resolveImplicitLocale();
                if (locale != null) {
                    UtilHttp.setLocaleIfNone(session, locale);
                } else {
                    locale = resolveDefaultLocale();
                    if (locale != null) {
                        UtilHttp.setLocaleIfNone(session, locale);
                    }
                }
            }

            TimeZone timeZone = resolveExplicitTimeZone();
            if (timeZone != null) {
                UtilHttp.setTimeZone(session, timeZone);
            } else {
                timeZone = resolveImplicitTimeZone();
                if (timeZone != null) {
                    UtilHttp.setTimeZoneIfNone(session, timeZone);
                } else {
                    timeZone = resolveDefaultTimeZone();
                    if (timeZone != null) {
                        UtilHttp.setTimeZoneIfNone(session, timeZone);
                    }
                }
            }
        }

        public void runBasicLoginEvents() {
            if (isUseBasicLoginEvent()) {
                setDefaultUserSettings();
            }
        }

        protected Map<String, Object> getUserLogin() {
            if (!checkUserLogin) {
                return null;
            }
            Map<String, Object> userLogin = this.userLogin;
            if (userLogin == null) {
                userLogin = AttrHandler.this.getUserLogin(session);
                if (cacheUserLogin) {
                    this.userLogin = userLogin;
                }
            }
            return userLogin;
        }

        protected <T> T getUserLoginField(String fieldName) {
            Map<String, Object> userLogin = getUserLogin();
            return (userLogin != null) ? UtilGenerics.cast(userLogin.get(fieldName)) : null;
        }
    }

}