package org.ofbiz.webapp.control;

import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.webapp.OfbizUrlBuilder;
import org.ofbiz.webapp.WebAppUtil;
import org.ofbiz.webapp.website.WebSiteProperties;
import org.xml.sax.SAXException;

/**
 * Cato: Request link utilities.
 */
public abstract class RequestLinkUtil {

    public static final String module = RequestLinkUtil.class.getName();
    
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
        String extLoginKey = (String) request.getAttribute("externalLoginKey");
        if (extLoginKey != null && !extLoginKey.isEmpty()) { 
            url = url + (url.contains("?") ? (escaped ? "&amp;" : "&") : "?") + "externalLoginKey=" + extLoginKey;
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
    
    public static String doLinkURLEncode(HttpServletRequest request, HttpServletResponse response, StringBuilder newURL, boolean interWebapp,
            boolean didFullStandard, boolean didFullSecure) {
        RequestHandler rh = RequestHandler.getRequestHandler(request.getServletContext());
        return rh.doLinkURLEncode(request, response, newURL, interWebapp, didFullStandard, didFullSecure);
    }
    
    /**
     * Cato: Helper method, originally derived from catalog URL links, but needed repeatedly.
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
            return false;
        }
        RequestHandler rh = RequestHandler.getRequestHandler(request.getServletContext());
        return rh.checkFullSecureOrStandard(request, webSiteProps, null, interWebapp, fullPath, secure);
    }
    
    public static Boolean checkFullSecureOrStandard(Delegator delegator, WebSiteProperties webSiteProps,
            Boolean interWebapp, Boolean fullPath, Boolean secure) {
        // FIXME: This case is too simplistic currently, check the website
        if (Boolean.TRUE.equals(secure)) {
            return Boolean.TRUE;
        } else if (Boolean.TRUE.equals(fullPath)) {
            return Boolean.FALSE;
        } else {
            return null;
        }
    }
    
    public static Boolean checkFullSecureOrStandard(Delegator delegator, String webSiteId,
            Boolean interWebapp, Boolean fullPath, Boolean secure) {
        // FIXME: This case is too simplistic currently, check the website
        if (Boolean.TRUE.equals(secure)) {
            return Boolean.TRUE;
        } else if (Boolean.TRUE.equals(fullPath)) {
            return Boolean.FALSE;
        } else {
            return null;
        }
    }
    
    public static String buildLinkHostPartAndEncode(Delegator delegator, String webSiteId, String url,
            Boolean fullPath, Boolean secure, Boolean encode) throws WebAppConfigurationException, IOException {
        StringBuilder newURL = new StringBuilder();
        
        // NOTE: this is always treated as inter-webapp, because we don't know our webapp
        Boolean secureFullPathFlag = checkFullSecureOrStandard(delegator, webSiteId, true, fullPath, secure);
        if (secureFullPathFlag != null) {
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
        return newURL.toString();
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
}
