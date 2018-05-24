package org.ofbiz.webapp.control;

import java.io.IOException;
import java.util.Collection;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
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
    
    protected RequestLinkUtil() {
    }
    
    public static boolean containsJsessionId(String url) {
        return url.contains(";jsessionid=");
    }

    public static String removeJsessionId(String url) {
        return jsessionIdPat.matcher(url).replaceFirst(""); 
    }
    
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
    
    public static String doLinkURLEncode(HttpServletRequest request, HttpServletResponse response, StringBuilder newURL, boolean interWebapp,
            boolean didFullStandard, boolean didFullSecure) {
        RequestHandler rh = RequestHandler.getRequestHandler(request.getServletContext());
        return rh.doLinkURLEncode(request, response, newURL, interWebapp, didFullStandard, didFullSecure);
    }
    
    /**
     * SCIPIO: Helper method, originally derived from catalog URL links, but needed repeatedly.
     */
    public static String buildLinkHostPartAndEncode(HttpServletRequest request, HttpServletResponse response, String url,
            Boolean fullPath, Boolean secure, Boolean encode) throws WebAppConfigurationException, IOException {
        
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
            
            OfbizUrlBuilder builder;
            try {
                builder = OfbizUrlBuilder.from(request);
            } catch (GenericEntityException e) {
                throw new IOException(e);
            } 

            builder.buildHostPart(newURL, url, secureFullPathFlag);
        }
        newURL.append(url);
        
        String res;
        if (!Boolean.FALSE.equals(encode)) {
            RequestHandler rh = RequestHandler.getRequestHandler(request.getServletContext());
            res = rh.doLinkURLEncode(request, response, newURL, false, didFullStandard, didFullSecure);
        } else {
            res = newURL.toString();
        }
        
        return res;
    }
    
    public static Boolean checkFullSecureOrStandard(HttpServletRequest request, HttpServletResponse response, 
            Boolean interWebapp, Boolean fullPath, Boolean secure) {
        WebSiteProperties webSiteProps;
        try {
            webSiteProps = WebSiteProperties.from(request);
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return true;
        }
        //RequestHandler rh = RequestHandler.getRequestHandler(request.getServletContext());
        return RequestHandler.checkFullSecureOrStandard(request, webSiteProps, null, interWebapp, fullPath, secure);
    }
    
    public static Boolean checkFullSecureOrStandard(Delegator delegator, WebSiteProperties webSiteProps,
            Boolean interWebapp, Boolean fullPath, Boolean secure, HttpServletRequest request, HttpServletResponse response) {
        // what we can do here depends on whether we got a request/response or not
        // checkFullSecureOrStandard should handle the case where request was missing (treats as insecure current request)
        return RequestHandler.checkFullSecureOrStandard(request, webSiteProps, null, interWebapp, fullPath, secure);
        
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
            Boolean interWebapp, Boolean fullPath, Boolean secure, HttpServletRequest request, HttpServletResponse response) {
        WebSiteProperties webSiteProps = null;
        if (webSiteId != null) {
            try {
                webSiteProps = WebSiteProperties.from(delegator, webSiteId);
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                return true;
            }
        }
        return checkFullSecureOrStandard(delegator, webSiteProps, interWebapp, fullPath, secure, request, response);
    }
    
    public static String buildLinkHostPartAndEncode(Delegator delegator, String webSiteId, String url,
            Boolean fullPath, Boolean secure, Boolean encode,
            HttpServletRequest request, HttpServletResponse response) throws WebAppConfigurationException, IOException {

        boolean didFullStandard = false;
        boolean didFullSecure = false;     
        StringBuilder newURL = new StringBuilder();
        
        // NOTE: this is always treated as inter-webapp, because we don't know our webapp
        Boolean secureFullPathFlag = checkFullSecureOrStandard(delegator, webSiteId, true, fullPath, secure, request, response);
        if (secureFullPathFlag != null) {
            if (secureFullPathFlag) {
                didFullSecure = true;
            } else {
                didFullStandard = true;
            }
            
            OfbizUrlBuilder builder;
            if (UtilValidate.isNotEmpty(webSiteId)) {
                WebappInfo webAppInfo;
                try {
                    webAppInfo = WebAppUtil.getWebappInfoFromWebsiteId(webSiteId);
                } catch (SAXException e) {
                    throw new IOException(e);
                }
                
                try {
                    builder = OfbizUrlBuilder.from(webAppInfo, delegator);
                } catch (SAXException e) {
                    throw new IOException(e);
                } catch (GenericEntityException e) {
                    throw new IOException(e);
                }
            } else {
                try {
                    // this will make a builder that uses default system web site properties
                    builder = OfbizUrlBuilder.from(null, null, delegator);
                } catch (SAXException e) {
                    throw new IOException(e);
                } catch (GenericEntityException e) {
                    throw new IOException(e);
                }
            }
            builder.buildHostPart(newURL, url, Boolean.TRUE.equals(secureFullPathFlag));
        }
        newURL.append(url);
        
        if (request != null && response != null) {
            String res;
            if (!Boolean.FALSE.equals(encode)) {
                RequestHandler rh = RequestHandler.getRequestHandler(request.getServletContext());
                res = rh.doLinkURLEncode(request, response, newURL, true, didFullStandard, didFullSecure);
            } else {
                res = newURL.toString();
            }
            
            return res;
        } else {
            return newURL.toString();
        }
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
    public static String buildLinkHostPart(HttpServletRequest request, String webSiteId, Boolean secure) throws WebAppConfigurationException, IOException {
        StringBuilder newURL = new StringBuilder();
        OfbizUrlBuilder builder;
        if (UtilValidate.isNotEmpty(webSiteId)) {
            builder = getExplicitOfbizUrlBuilder((Delegator) request.getAttribute("delegator"), webSiteId);
        } else {
            try {
                builder = OfbizUrlBuilder.from(request);
            } catch (GenericEntityException e) {
                throw new IOException(e);
            }
        }
        if (secure == null) secure = RequestLinkUtil.isEffectiveSecure(request);
        builder.buildHostPart(newURL, secure);
        return newURL.toString();
    }
    
    private static OfbizUrlBuilder getExplicitOfbizUrlBuilder(Delegator delegator, String webSiteId) throws WebAppConfigurationException, IOException {
        WebappInfo webAppInfo;
        try {
            webAppInfo = WebAppUtil.getWebappInfoFromWebsiteId(webSiteId);
        } catch (SAXException e) {
            throw new IOException(e);
        }
        try {
            return OfbizUrlBuilder.from(webAppInfo, delegator);
        } catch (SAXException e) {
            throw new IOException(e);
        } catch (GenericEntityException e) {
            throw new IOException(e);
        }
    }
    
    /**
     * SCIPIO: Rudimentarily builds host part of link, from request, as requested secure or not (no automatic logic). 
     * Same as {@link #buildLinkHostPart(HttpServletRequest, String, Boolean)} but throws no exceptions and returns null instead.
     * Added 2017-11-18.
     */
    public static String buildLinkHostPartSafe(HttpServletRequest request, String webSiteId, Boolean secure) {
        try {
            return buildLinkHostPart(request, webSiteId, secure);
        } catch (Exception e) {
            Debug.logError(e, "Error building link host part: " + e.getMessage(), module);
            return null; // null may allow default value operators to work in other langs
        }
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
    public static String buildLinkHostPart(Delegator delegator, String webSiteId, Boolean secure) throws WebAppConfigurationException, IOException {
        StringBuilder newURL = new StringBuilder();
        OfbizUrlBuilder builder;
        if (UtilValidate.isNotEmpty(webSiteId)) {
            builder = getExplicitOfbizUrlBuilder(delegator, webSiteId);
        } else {
            try {
                // this will make a builder that uses default system web site properties
                builder = OfbizUrlBuilder.from(null, null, delegator);
            } catch (SAXException e) {
                throw new IOException(e);
            } catch (GenericEntityException e) {
                throw new IOException(e);
            }
        }
        builder.buildHostPart(newURL, secure);
        return newURL.toString();
    }
    
    /**
     * SCIPIO: Rudimentarily builds host part of link, statically, as requested secure or not (no automatic logic). 
     * Same as {@link #buildLinkHostPart(Delegator, String, Boolean)} but throws no exceptions and returns null instead.
     * Added 2017-11-18.
     */
    public static String buildLinkHostPartSafe(Delegator delegator, String webSiteId, Boolean secure) {
        try {
            return buildLinkHostPart(delegator, webSiteId, secure);
        } catch (Exception e) {
            Debug.logError(e, "Error building link host part: " + e.getMessage(), module);
            return null; // null may allow default value operators to work in other langs
        }
    }
    
    public static String getWebSiteContextPath(Delegator delegator, String webSiteId) throws WebAppConfigurationException, IOException {
        WebappInfo webAppInfo;
        try {
            webAppInfo = WebAppUtil.getWebappInfoFromWebsiteId(webSiteId);
            
            return webAppInfo.getContextRoot();
        } catch (SAXException e) {
            throw new IOException(e);
        }
    }
    
    /**
     * Builds link using RequestHandler.makeLinkAuto logic, convenience wrapper.
     */
    public static String makeLinkAuto(ServletContext servletContext, HttpServletRequest request, HttpServletResponse response, String uri) {
        RequestHandler rh = RequestHandler.getRequestHandler(servletContext);
        return rh.makeLinkAuto(request, response, uri, null, null, null, null, null, null, null);
    }

    /**
     * Builds link using RequestHandler.makeLinkAuto logic, convenience wrapper.
     */
    public static String makeLinkAuto(ServletContext servletContext, HttpServletRequest request, HttpServletResponse response, 
            String uri, Boolean fullPath, Boolean secure, Boolean encode) {
        RequestHandler rh = RequestHandler.getRequestHandler(servletContext);
        return rh.makeLinkAuto(request, response, uri, null, null, null, null, fullPath, secure, encode);
    }
    
    /**
     * Builds link using RequestHandler.makeLinkAuto logic, convenience wrapper.
     */
    public static String makeLinkAuto(ServletContext servletContext, HttpServletRequest request, HttpServletResponse response, 
            String uri, Boolean absPath, Boolean interWebapp, String webSiteId, Boolean controller, Boolean fullPath, Boolean secure, Boolean encode) {
        RequestHandler rh = RequestHandler.getRequestHandler(servletContext);
        return rh.makeLinkAuto(request, response, uri, absPath, interWebapp, webSiteId, controller, fullPath, secure, encode);
    }
    
    /**
     * Builds link using RequestHandler.makeLinkAuto logic, convenience wrapper.
     */
    public static String makeLinkAuto(HttpServletRequest request, HttpServletResponse response, String uri) {
        RequestHandler rh = RequestHandler.getRequestHandler(request.getServletContext());
        return rh.makeLinkAuto(request, response, uri, null, null, null, null, null, null, null);
    }

    /**
     * Builds link using RequestHandler.makeLinkAuto logic, convenience wrapper.
     */
    public static String makeLinkAuto(HttpServletRequest request, HttpServletResponse response, 
            String uri, Boolean fullPath, Boolean secure, Boolean encode) {
        RequestHandler rh = RequestHandler.getRequestHandler(request.getServletContext());
        return rh.makeLinkAuto(request, response, uri, null, null, null, null, fullPath, secure, encode);
    }
    
    /**
     * Builds link using RequestHandler.makeLinkAuto logic, convenience wrapper.
     */
    public static String makeLinkAuto(HttpServletRequest request, HttpServletResponse response, 
            String uri, Boolean absPath, Boolean interWebapp, String webSiteId, Boolean controller, Boolean fullPath, Boolean secure, Boolean encode) {
        RequestHandler rh = RequestHandler.getRequestHandler(request.getServletContext());
        return rh.makeLinkAuto(request, response, uri, absPath, interWebapp, webSiteId, controller, fullPath, secure, encode);
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
     * Attemps to rebuild the URL that was requested by the client, before any forwards, but with support
     * for specifying the protocol and port.
     * This is a custom coded version of {@link #getFullOriginalURL}.
     */
    public static String rebuildOriginalRequestURL(HttpServletRequest request, HttpServletResponse response, Boolean secure, boolean includeQueryString) {
        // NOTE: derived from Tomcat 8 implementation of getRequestURL
        StringBuffer url = new StringBuffer();
        String scheme; 
        int port;
        if (secure != null) {
            scheme = secure ? "https" : "http";
            port = -1;
            try {
                WebSiteProperties props = WebSiteProperties.from(request);
                String portStr = secure ? props.getHttpsPort() : props.getHttpPort();
                if (portStr != null && !portStr.isEmpty()) {
                    port = Integer.parseInt(portStr);
                }
            } catch (Exception e) { // SCIPIO: just catch everything: GenericEntityException
                // If the entity engine is throwing exceptions, then there is no point in continuing.
                Debug.logError(e, "rebuildOriginalRequestURL: Exception thrown while getting web site properties http/https port: " + e.getMessage(), module);
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
        url.append(scheme);
        url.append("://");
        url.append(request.getServerName());
        if ((scheme.equals("http") && (port != 80))
            || (scheme.equals("https") && (port != 443))) {
            url.append(':');
            url.append(port);
        }
        
        String requestURI = (String) request.getAttribute(RequestDispatcher.FORWARD_REQUEST_URI);
        String queryString = (String) request.getAttribute(RequestDispatcher.FORWARD_QUERY_STRING);
        if (requestURI == null) { // NOTE: must use this test for the queryString too
            requestURI = request.getRequestURI();
            queryString = request.getQueryString();
        }
        url.append(requestURI);
        if (includeQueryString && queryString != null) {
            url.append("?").append(queryString);
        }
        return url.toString();
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
        return UtilValidate.isNotEmpty(forwardedProto) && "HTTPS".equals(forwardedProto.toUpperCase());
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
}
