package org.ofbiz.webapp.control;

import java.io.IOException;
import java.util.Collection;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.entity.Delegator;
import org.ofbiz.webapp.ExtWebappInfo;
import org.ofbiz.webapp.FullWebappInfo;
import org.ofbiz.webapp.OfbizUrlBuilder;
import org.ofbiz.webapp.WebAppUtil;
import org.ofbiz.webapp.website.WebSiteProperties;
import org.xml.sax.SAXException;

/**
 * SCIPIO: Request link utilities.
 */
public abstract class RequestLinkUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final Pattern jsessionIdPat = Pattern.compile("((;jsessionid=)([^\\?#]*))");

    public static final String HTTP_PROTO_COLON = "http:";
    public static final String HTTP_PROTO_COLONSLASH = "http://";
    public static final String HTTPS_PROTO_COLON = "https:";
    public static final String HTTPS_PROTO_COLONSLASH = "https://";

    protected RequestLinkUtil() {
    }

    /**
     * @deprecated 2018-08: In Scipio, there is never a jsessionId added anymore.
     */
    @Deprecated
    public static boolean containsJsessionId(String url) {
        return url.contains(";jsessionid=");
    }

    /**
     * @deprecated 2018-08: In Scipio, there is never a jsessionId added anymore.
     */
    @Deprecated
    public static String removeJsessionId(String url) {
        return jsessionIdPat.matcher(url).replaceFirst("");
    }

    /**
     * @deprecated 2018-08: In Scipio, there is never a jsessionId added anymore.
     */
    @Deprecated
    public static String getSessionId(String url) {
        Matcher m = jsessionIdPat.matcher(url);
        boolean found = m.find();
        if (found) {
            return m.group(3);
        }
        else {
            return null;
        }
    }

    /**
     * @deprecated 2018-08: In Scipio, there is never a jsessionId added anymore.
     */
    @Deprecated
    public static String setJsessionId(String url, String sessionId) {
        String jsessionIdStr = ";jsessionid=" + sessionId;

        Matcher m = jsessionIdPat.matcher(url);
        String regReplUrl = m.replaceFirst(jsessionIdStr);

        if (containsJsessionId(regReplUrl)) {
            return regReplUrl;
        }
        else {
            // This is ofbizUrl/RequestHanlder.makeLink behavior...
            StringBuilder newURL = new StringBuilder(url);

            int questionIndex = newURL.indexOf("?");
            if (questionIndex == -1) {
                newURL.append(jsessionIdStr);
            } else {
                newURL.insert(questionIndex, jsessionIdStr);
            }
            return newURL.toString();
        }
    }

    /**
     * @deprecated 2018-08: In Scipio, there is never a jsessionId added anymore.
     */
    @Deprecated
    public static String encodeURLNoJsessionId(String url, HttpServletResponse response) {
        return RequestLinkUtil.removeJsessionId(response.encodeURL(url));
    }

    public static String checkAddExternalLoginKey(String url, HttpServletRequest request, boolean escaped) {
        return checkAddExternalLoginKey(url, request, escaped ? "&amp;" : "&");
    }

    public static String checkAddExternalLoginKey(String url, HttpServletRequest request, String paramDelim) {
        String extLoginKey = (String) request.getAttribute("externalLoginKey");
        if (extLoginKey != null && !extLoginKey.isEmpty()) {
            url = url + (url.contains("?") ? paramDelim : "?") + "externalLoginKey=" + extLoginKey;
        }
        return url;
    }

    public static String removeQueryStringParam(String queryString, String paramName) {
        // WARNING: UNTESTED
        final Pattern pat = Pattern.compile("(^|[?&])" + paramName + "=[^#?;&]*");

        Matcher m = pat.matcher(queryString);
        String res = m.replaceAll("");

        if (queryString.startsWith("?") && !res.startsWith("?")) {
            return "?" + res;
        }
        else {
            return res;
        }
    }

    /**
     * Makes param string (no starting delimiter); intended specifically for new Scipio
     * link-building facilities, and may slightly differ from stock Ofbiz ones.
     */
    public static String makeParamString(Map<String, Object> params, String delim) {
        StringBuilder sb = new StringBuilder("");
        for(Map.Entry<String, Object> entry : params.entrySet()) {
            appendToParamString(sb, entry.getKey(), entry.getValue(), delim);
        }
        if (sb.length() >= delim.length()) {
            sb.delete(0, delim.length());
        }
        return sb.toString();
    }

    public static void appendToParamString(StringBuilder sb, String name, Object val, String delim) {
        if (val instanceof Collection) { // param with multiple values (rare)
            for(Object subVal : UtilGenerics.checkCollection(val)) {
                appendToParamStringAsString(sb, name, subVal, delim);
            }
        } else {
            appendToParamStringAsString(sb, name, val, delim);
        }
    }

    public static void appendToParamStringAsString(StringBuilder sb, String name, Object val, String delim) {
        sb.append(delim);
        sb.append(name);
        sb.append("=");
        if (val != null) {
            sb.append(val.toString());
        }
    }

    public static Boolean checkFullSecureOrStandard(HttpServletRequest request, HttpServletResponse response,
            Boolean interWebapp, Boolean fullPath, Boolean secure) {
        /* 2018-08-02: the target method does not use this anymore, so don't bother
        WebSiteProperties webSiteProps;
        try {
            webSiteProps = WebSiteProperties.from(request);
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return true;
        }*/
        return RequestHandler.checkFullSecureOrStandard(request, null, null, interWebapp, fullPath, secure);
    }

    public static Boolean checkFullSecureOrStandard(Delegator delegator, WebSiteProperties webSiteProps,
            Boolean interWebapp, Boolean fullPath, Boolean secure, Map<String, Object> context) {
        // what we can do here depends on whether we got a request/response or not
        // checkFullSecureOrStandard should handle the case where request was missing (treats as insecure current request)
        return RequestHandler.checkFullSecureOrStandard(null, webSiteProps, null, interWebapp, fullPath, secure);

        /* old logic, too simplistic (wrong and insecure compared to new RequestHandler logic)
        if (Boolean.TRUE.equals(secure)) {
            return Boolean.TRUE;
        } else if (Boolean.TRUE.equals(fullPath)) {
            return Boolean.FALSE;
        } else {
            return null;
        }
        */
    }

    public static Boolean checkFullSecureOrStandard(Delegator delegator, String webSiteId,
            Boolean interWebapp, Boolean fullPath, Boolean secure, Map<String, Object> context) {
        /* 2018-08-02: the target method does not use this anymore, so don't bother
        WebSiteProperties webSiteProps = null;
        if (webSiteId != null) {
            try {
                webSiteProps = WebSiteProperties.from(delegator, webSiteId);
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                return true;
            }
        }*/
        return checkFullSecureOrStandard(delegator, (WebSiteProperties) null, interWebapp, fullPath, secure, context);
    }

    public static String doLinkURLEncode(HttpServletRequest request, HttpServletResponse response, StringBuilder newURL, boolean interWebapp,
            FullWebappInfo targetWebappInfo, FullWebappInfo currentWebappInfo, boolean didFullStandard, boolean didFullSecure) {
        return RequestHandler.doLinkURLEncode(request, response, newURL, interWebapp, targetWebappInfo, currentWebappInfo, didFullStandard, didFullSecure);
    }

    public static String doLinkURLEncode(Delegator delegator, Locale locale, StringBuilder newURL, FullWebappInfo targetWebappInfo, FullWebappInfo currentWebappInfo,
            boolean didFullStandard, boolean didFullSecure, Map<String, Object> context) {
        return RequestHandler.doLinkURLEncode(delegator, locale, newURL, targetWebappInfo, currentWebappInfo, didFullStandard, didFullSecure, context);
    }

    /**
     * SCIPIO: Helper method, originally derived from catalog URL links, but needed repeatedly.
     * <p>
     * This method only supports linking within the current webapp and encoding using the current webapp.
     */
    public static String buildLinkHostPartAndEncode(HttpServletRequest request, HttpServletResponse response, Locale locale, FullWebappInfo targetWebappInfo, String url,
            Boolean fullPath, Boolean secure, Boolean encode, boolean includeWebappPathPrefix) throws IllegalArgumentException {

        FullWebappInfo currentWebappInfo = FullWebappInfo.fromRequest(request);
        if (targetWebappInfo == null) {
            targetWebappInfo = currentWebappInfo;
        } else {
            // 2018-08-08: force fullPath if server part differs
            if (!targetWebappInfo.equalsProtoHostPortWithHardDefaults(currentWebappInfo)) {
                fullPath = true;
            }
        }

        boolean didFullStandard = false;
        boolean didFullSecure = false;
        StringBuilder newURL = new StringBuilder();

        Boolean secureFullPathFlag = checkFullSecureOrStandard(request, response, false, fullPath, secure);
        if (secureFullPathFlag != null) {
            if (secureFullPathFlag) {
                didFullSecure = true;
            } else {
                didFullStandard = true;
            }
            try {
                targetWebappInfo.getOfbizUrlBuilder().buildHostPart(newURL, url, secureFullPathFlag, false);
            } catch (Exception e) {
                throw new IllegalStateException("Error building link host part for webapp: " + targetWebappInfo, e);
            }
        }

        if (includeWebappPathPrefix) {
            // 2018-07-27: must use webapp path prefix
            try {
                targetWebappInfo.getOfbizUrlBuilder().buildPathPartWithWebappPathPrefix(newURL, url);
            } catch (Exception e) {
                throw new IllegalStateException("Error building link path part for webapp: " + targetWebappInfo, e);
            }
        } else {
            newURL.append(url);
        }

        String res;
        if (!Boolean.FALSE.equals(encode)) {
            res = RequestHandler.doLinkURLEncode(request, response, newURL, false, targetWebappInfo, targetWebappInfo, didFullStandard, didFullSecure);
        } else {
            res = newURL.toString();
        }

        return res;
    }

    public static String buildLinkHostPartAndEncodeSafe(HttpServletRequest request, HttpServletResponse response, Locale locale, FullWebappInfo targetWebappInfo, String url,
            Boolean fullPath, Boolean secure, Boolean encode, boolean includeWebappPathPrefix) {
        try {
            return buildLinkHostPartAndEncode(request, response, locale, targetWebappInfo, url, fullPath, secure, encode, includeWebappPathPrefix);
        } catch(Exception e) {
            Debug.logError(e, module);
            return null;
        }
    }

    @Deprecated
    public static String buildLinkHostPartAndEncode(HttpServletRequest request, HttpServletResponse response, FullWebappInfo targetWebappInfo, String url,
            Boolean fullPath, Boolean secure, Boolean encode) throws WebAppConfigurationException, IOException {
        return buildLinkHostPartAndEncode(request, response, null, targetWebappInfo, url, fullPath, secure, encode, false);
    }

    /**
     * SCIPIO: Helper method, originally derived from catalog URL links, but needed repeatedly.
     * <p>
     * This method treats the given webSiteId as the "current" webapp, and links only within this
     * webapp and (theoretically) only encodes using this webapp.
     * WARN: In other words, this is NOT meant to handle inter-webapp links! It only han
     */
    public static String buildLinkHostPartAndEncode(Delegator delegator, Locale locale, FullWebappInfo targetWebappInfo, String url,
            Boolean fullPath, Boolean secure, Boolean encode, boolean includeWebappPathPrefix, FullWebappInfo currentWebappInfo,
            Map<String, Object> context) {

        boolean didFullStandard = false;
        boolean didFullSecure = false;
        StringBuilder newURL = new StringBuilder();

        // NOTE: this is always treated as inter-webapp, because we don't know our webapp
        Boolean secureFullPathFlag = checkFullSecureOrStandard(delegator, targetWebappInfo.getWebSiteProperties(),
                !targetWebappInfo.equals(currentWebappInfo), fullPath, secure, context);

        if (secureFullPathFlag != null) {
            if (secureFullPathFlag) {
                didFullSecure = true;
            } else {
                didFullStandard = true;
            }
            try {
                targetWebappInfo.getOfbizUrlBuilder().buildHostPart(newURL, url, Boolean.TRUE.equals(secureFullPathFlag), false);
            } catch (Exception e) {
                throw new IllegalStateException("Error building link host part for webapp: " + targetWebappInfo, e);
            }
        }

        if (includeWebappPathPrefix) {
            try {
                targetWebappInfo.getOfbizUrlBuilder().buildPathPartWithWebappPathPrefix(newURL, url);
            } catch (Exception e) {
                throw new IllegalStateException("Error building link path part for webapp: " + targetWebappInfo, e);
            }
        } else {
            newURL.append(url);
        }

        String res;
        if (!Boolean.FALSE.equals(encode)) {
            res = RequestHandler.doLinkURLEncode(delegator, locale, newURL, targetWebappInfo,
                    currentWebappInfo, didFullStandard, didFullSecure, context);
        } else {
            res = newURL.toString();
        }
        return res;
    }

    public static String buildLinkHostPartAndEncodeSafe(Delegator delegator, Locale locale, FullWebappInfo targetWebappInfo, String url,
            Boolean fullPath, Boolean secure, Boolean encode, boolean includeWebappPathPrefix, FullWebappInfo currentWebappInfo,
            Map<String, Object> context) {
        try {
            return buildLinkHostPartAndEncode(delegator, locale, targetWebappInfo, url, fullPath, secure, encode, includeWebappPathPrefix,
                    currentWebappInfo, context);
        } catch(Exception e) {
            Debug.logError(e, module);
            return null;
        }
    }

    @Deprecated
    public static String buildLinkHostPartAndEncode(Delegator delegator, String webSiteId, String url,
            Boolean fullPath, Boolean secure, Boolean encode, Map<String, Object> context) {
        FullWebappInfo webappInfo = FullWebappInfo.fromWebapp(ExtWebappInfo.fromWebSiteId(webSiteId), context);
        return buildLinkHostPartAndEncode(delegator, null, webappInfo, url, fullPath, secure, encode, false, webappInfo, context);
    }

    /**
     * SCIPIO: Rudimentarily builds host part of link, from request, as requested secure or not (or using current if null).
     * This overload uses the CURRENT WebSite in request to determine the host, unless explicit is passed, in which case that one is used.
     * Simple wrapper around {@link org.ofbiz.webapp.OfbizUrlBuilder#buildHostPart(Appendable, Boolean)}.
     * Added 2017-11-18.
     *
     * @param request the request, containing the current webSiteId
     * @param webSiteId optional explicit target webSiteId
     * @param secure if explicit true or false, creates https or http link, respectively; otherwise auto-determines from request ("current")
     */
    public static String buildLinkHostPart(HttpServletRequest request, Locale locale, String webSiteId, Boolean secure, boolean includeWebappPathPrefix) {
        StringBuilder newURL = new StringBuilder();
        FullWebappInfo targetWebappInfo = FullWebappInfo.fromWebapp(ExtWebappInfo.fromWebSiteId(webSiteId), request);
        if (secure == null) secure = RequestLinkUtil.isEffectiveSecure(request);
        try {
            targetWebappInfo.getOfbizUrlBuilder().buildHostPart(newURL, secure);
            if (includeWebappPathPrefix) {
                targetWebappInfo.getOfbizUrlBuilder().buildPathPartWithWebappPathPrefix(newURL, null);
            }
        } catch(Exception e) {
            throw new IllegalArgumentException(e);
        }
        return newURL.toString();
    }

    @Deprecated
    public static String buildLinkHostPart(HttpServletRequest request, String webSiteId, Boolean secure) throws WebAppConfigurationException, IOException {
        return buildLinkHostPart(request, null, webSiteId, secure, false);
    }

    /**
     * SCIPIO: Rudimentarily builds host part of link, from request, as requested secure or not (no automatic logic).
     * Same as {@link #buildLinkHostPart(HttpServletRequest, Locale, String, Boolean, boolean)} but throws no exceptions and returns null instead.
     * Added 2017-11-18.
     */
    public static String buildLinkHostPartSafe(HttpServletRequest request, Locale locale, String webSiteId, Boolean secure, boolean includeWebappPathPrefix) {
        try {
            return buildLinkHostPart(request, locale, webSiteId, secure, includeWebappPathPrefix);
        } catch (Exception e) {
            Debug.logError(e, "Error building link host part: " + e.getMessage(), module);
            return null; // null may allow default value operators to work in other langs
        }
    }

    @Deprecated
    public static String buildLinkHostPartSafe(HttpServletRequest request, String webSiteId, Boolean secure) {
        return buildLinkHostPartSafe(request, null, webSiteId, secure, false);
    }

    /**
     * SCIPIO: Rudimentarily builds host part of link, statically, as requested secure or not (no automatic logic).
     * This overload uses the explicit WebSite in request to determine the host, or the system defaults (url.properties) if no webSiteId passed.
     * Simple wrapper around {@link org.ofbiz.webapp.OfbizUrlBuilder#buildHostPart(Appendable, Boolean)}.
     * Added 2017-11-18.
     *
     * @param delegator the delegator
     * @param webSiteId optional explicit target webSiteId
     * @param secure if explicit true or false, creates https or http link, respectively; if null, uses a configured or common default
     */
    public static String buildLinkHostPart(Delegator delegator, Locale locale, String webSiteId, Boolean secure,
            boolean includeWebappPathPrefix, FullWebappInfo.Cache webappInfoCache) {
        StringBuilder newURL = new StringBuilder();
        OfbizUrlBuilder builder = FullWebappInfo.getOfbizUrlBuilderFromWebSiteIdOrDefaults(webSiteId, delegator, webappInfoCache);
        try {
            builder.buildHostPart(newURL, secure);
            if (includeWebappPathPrefix) {
                builder.buildPathPartWithWebappPathPrefix(newURL);
            }
        } catch(Exception e) {
            throw new IllegalArgumentException(e);
        }
        return newURL.toString();
    }

    @Deprecated
    public static String buildLinkHostPart(Delegator delegator, String webSiteId, Boolean secure) throws WebAppConfigurationException, IOException {
        return buildLinkHostPart(delegator, null, webSiteId, secure, false, null);
    }

    /**
     * SCIPIO: Rudimentarily builds host part of link, statically, as requested secure or not (no automatic logic).
     * Same as {@link #buildLinkHostPart(Delegator, String, Boolean)} but throws no exceptions and returns null instead.
     * Added 2017-11-18.
     */
    public static String buildLinkHostPartSafe(Delegator delegator, Locale locale, String webSiteId, Boolean secure, boolean includeWebappPathPrefix) {
        try {
            return buildLinkHostPart(delegator, locale, webSiteId, secure, includeWebappPathPrefix, null);
        } catch (Exception e) {
            Debug.logError(e, "Error building link host part: " + e.getMessage(), module);
            return null; // null may allow default value operators to work in other langs
        }
    }

    @Deprecated
    public static String buildLinkHostPartSafe(Delegator delegator, String webSiteId, Boolean secure) {
        return buildLinkHostPartSafe(delegator, null, webSiteId, secure, false);
    }

    public static String getWebSiteContextPath(Delegator delegator, String webSiteId) throws IllegalArgumentException {
        try {
            return WebAppUtil.getWebappInfoFromWebsiteId(webSiteId).getContextRoot();
        } catch (SAXException e) {
            throw new IllegalArgumentException(e);
        } catch (IOException e) {
            throw new IllegalArgumentException(e);
        }
    }

    /**
     * Builds link using RequestHandler.makeLinkAuto logic, convenience wrapper.
     */
    public static String makeLinkAuto(ServletContext servletContext, HttpServletRequest request, HttpServletResponse response, String uri) {
        return RequestHandler.makeLinkAuto(request, response, uri, null, null, null, null, null, null, null);
    }

    /**
     * Builds link using RequestHandler.makeLinkAuto logic, convenience wrapper.
     */
    public static String makeLinkAuto(ServletContext servletContext, HttpServletRequest request, HttpServletResponse response,
            String uri, Boolean fullPath, Boolean secure, Boolean encode) {
        return RequestHandler.makeLinkAuto(request, response, uri, null, null, null, null, fullPath, secure, encode);
    }

    /**
     * Builds link using RequestHandler.makeLinkAuto logic, convenience wrapper.
     */
    public static String makeLinkAuto(ServletContext servletContext, HttpServletRequest request, HttpServletResponse response,
            String uri, Boolean absPath, Boolean interWebapp, String webSiteId, Boolean controller, Boolean fullPath, Boolean secure, Boolean encode) {
        return RequestHandler.makeLinkAuto(request, response, uri, absPath, interWebapp, webSiteId, controller, fullPath, secure, encode);
    }

    /**
     * Builds link using RequestHandler.makeLinkAuto logic, convenience wrapper.
     */
    public static String makeLinkAuto(HttpServletRequest request, HttpServletResponse response, String uri) {
        return RequestHandler.makeLinkAuto(request, response, uri, null, null, null, null, null, null, null);
    }

    /**
     * Builds link using RequestHandler.makeLinkAuto logic, convenience wrapper.
     */
    public static String makeLinkAuto(HttpServletRequest request, HttpServletResponse response,
            String uri, Boolean fullPath, Boolean secure, Boolean encode) {
        return RequestHandler.makeLinkAuto(request, response, uri, null, null, null, null, fullPath, secure, encode);
    }

    /**
     * Builds link using RequestHandler.makeLinkAuto logic, convenience wrapper.
     */
    public static String makeLinkAuto(HttpServletRequest request, HttpServletResponse response,
            String uri, Boolean absPath, Boolean interWebapp, String webSiteId, Boolean controller, Boolean fullPath, Boolean secure, Boolean encode) {
        return RequestHandler.makeLinkAuto(request, response, uri, absPath, interWebapp, webSiteId, controller, fullPath, secure, encode);
    }

    /**
     * Returns {@link javax.servlet.http.HttpServletRequest#getRequestURL} + query string.
     */
    public static String getFullRequestURL(HttpServletRequest request) {
        StringBuffer url = request.getRequestURL();
        if (request.getQueryString() != null) url.append("?").append(request.getQueryString());
        return url.toString();
    }

    /**
     * Best-effort rebuilds the incoming URL that was requested by the client, before any forwards, but with support
     * for specifying a different protocol and port.
     * <p>
     * Implementation derived from {@link org.apache.catalina.connector.Request#getRequestURL()}.
     *
     * @param request
     * @param response
     * @param secure true: use https; false: use http; null: use same as original request url
     * @param staticHost true: use host from url.properties/WebSite; false/null: use host from request.getServerName
     * @param includeWebappPathPrefix true: include webapp path prefix from WebSite or url.properties configuration
     * @param useQueryString
     * @param handleErrors if true, swallow errors and use best available; if false, throw IllegalStateException
     * @throws IllegalStateException if handleErrors false and any entity or parsing error
     */
    public static String rebuildOriginalRequestURL(HttpServletRequest request, HttpServletResponse response, Locale locale,
            Boolean secure, Boolean staticHost, boolean includeWebappPathPrefix, boolean useQueryString, boolean handleErrors) throws IllegalStateException {
        // NOTE: derived from Tomcat 8 implementation of getRequestURL
        StringBuffer url = new StringBuffer();
        // DEV NOTE: it's better to go through webappInfo now, since
        // we have easier choice of WebSiteProperties or OfbizUrlBuilder through it
        FullWebappInfo webappInfo = null;
        WebSiteProperties props = null;
        String scheme;
        int port;
        if (secure != null) {
            scheme = secure ? "https" : "http";
            port = -1;
            try {
                webappInfo = FullWebappInfo.fromRequest(request);
                props = webappInfo.getWebSiteProperties();
                String portStr = secure ? props.getHttpsPort() : props.getHttpPort();
                if (portStr != null && !portStr.isEmpty()) {
                    port = Integer.parseInt(portStr);
                }
            } catch (Exception e) {
                if (handleErrors) {
                    Debug.logError(e, "rebuildOriginalRequestURL: Error getting web site properties for http/https port: " + e.getMessage(), module);
                } else {
                    throw new IllegalStateException("Error getting web site properties for http/https port: " + e.getMessage(), e);
                }
            }
            if (port < 0) {
                port = secure ? 443 : 80;
            }
        } else {
            scheme = request.getScheme();
            port = request.getServerPort();
        }
        if (port < 0) {
            port = 80;
        }
        url.append(scheme).append("://");
        String host;
        if (Boolean.TRUE.equals(staticHost)) {
            if (webappInfo == null) {
                try {
                    webappInfo = FullWebappInfo.fromRequest(request);
                    props = webappInfo.getWebSiteProperties();
                } catch (Exception e) {
                    if (handleErrors) {
                        Debug.logError(e, "rebuildOriginalRequestURL: Error getting web site properties for host name"
                                + " (using request.getServerName() instead): " + e.getMessage(), module);
                    } else {
                        throw new IllegalStateException("Error getting web site properties for host name: " + e.getMessage(), e);
                    }
                }
            }
            if (props != null) {
                host = scheme.equals("https") ? props.getHttpsHost() : props.getHttpHost();
                if (host == null || host.isEmpty()) host = request.getServerName();
            } else {
                host = request.getServerName();
            }
        } else {
            host = request.getServerName();
        }
        url.append(host);
        if ((scheme.equals("http") && (port != 80))
            || (scheme.equals("https") && (port != 443))) {
            url.append(':').append(port);
        }

        String requestURI = (String) request.getAttribute(RequestDispatcher.FORWARD_REQUEST_URI);
        String queryString = (String) request.getAttribute(RequestDispatcher.FORWARD_QUERY_STRING);
        if (requestURI == null) { // NOTE: must use this test for the queryString too
            requestURI = request.getRequestURI();
            queryString = request.getQueryString();
        }
        if (includeWebappPathPrefix) {
            OfbizUrlBuilder urlBuilder = null;
            try {
                if (webappInfo == null) {
                    webappInfo = FullWebappInfo.fromRequest(request);
                }
                urlBuilder = webappInfo.getOfbizUrlBuilder();
            } catch (Exception e) {
                if (handleErrors) {
                    Debug.logError(e, "rebuildOriginalRequestURL: Error getting url builder for webapp " + webappInfo + ": " + e.getMessage(), module);
                } else {
                    throw new IllegalStateException("Error getting url builder webapp " + webappInfo + ": " + e.getMessage(), e);
                }
            }
            if (urlBuilder != null) {
                try {
                    urlBuilder.buildPathPartWithWebappPathPrefix(url);
                } catch(Exception e) {
                    if (handleErrors) {
                        Debug.logError(e, "rebuildOriginalRequestURL: Error building webappPathPrefix for webapp " + webappInfo + ": " + e.getMessage(), module);
                    } else {
                        throw new IllegalStateException("Error building webappPathPrefix for webapp " + webappInfo + ": " + e.getMessage(), e);
                    }
                }
            }
        }
        url.append(requestURI);
        if (useQueryString && queryString != null) {
            url.append("?").append(queryString);
        }
        return url.toString();
    }

    @Deprecated
    public static String rebuildOriginalRequestURL(HttpServletRequest request, HttpServletResponse response,
            Boolean secure, boolean useQueryString) {
        return rebuildOriginalRequestURL(request, response, null, secure, null, true, useQueryString, true);
    }

    public static String getServletAndPathInfo(HttpServletRequest request) {
        if (request.getPathInfo() != null) return request.getServletPath() + request.getPathInfo();
        else return request.getServletPath();
    }

    /**
     * Returns the first path elem after the slash, or null if empty or root request.
     * WARN: assumes the first char is a slash - meant for servlet API methods: getServletPath(), getPathInfo().
     */
    public static String getFirstPathElem(String path) {
        if (path == null || path.length() <= 1) return null;

        int secondSlashIndex = path.indexOf('/', 1);
        if (secondSlashIndex >= 1) {
            path = path.substring(1, secondSlashIndex);
        } else {
            path = path.substring(1);
        }

        return path;
    }

    public static String getFirstPathInfoElem(HttpServletRequest request) {
        return getFirstPathElem(request.getPathInfo());
    }

    public static String getFirstServletAndPathInfoElem(HttpServletRequest request) {
        return getFirstPathElem(getServletAndPathInfo(request));
    }

    /**
     * Simple reusable check for <code>X-Forwarded-Proto</code> HTTPS header.
     * Does NOT include <code>request.isSecure()</code> check, do separate.
     * Added 2017-11-18.
     */
    public static boolean isForwardedSecure(HttpServletRequest request) {
        String forwardedProto = request.getHeader("X-Forwarded-Proto");
        return forwardedProto != null && "https".equals(forwardedProto.toLowerCase());
    }

    /**
     * Abstracted secure flag check. Currently checks:
     * <ul>
     * <li><code>X-Forwarded-Proto</code> HTTPS header <strong>OR</strong></li>
     * <li><code>request.isSecure()</code></li>
     * </ul>
     * Added 2017-11-18.
     */
    public static boolean isEffectiveSecure(HttpServletRequest request) {
        return request.isSecure() || isForwardedSecure(request);
    }

    /**
     * Checks if the given URL starts with specified protocol, fast.
     * @param url the url
     * @param protocol the protocol, lowercase (must be)
     */
    public static boolean isUrlProtocol(final String url, final String protocol) {
        final int protocolLength = protocol.length();
        if (url.length() < (protocolLength + 1)) return false;
        if (url.charAt(protocolLength) != ':') return false;
        return protocol.equalsIgnoreCase(url.substring(0, protocolLength)); // substring+equal avoid iterating whole url
    }
}
