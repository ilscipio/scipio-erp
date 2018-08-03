package com.ilscipio.scipio.ce.webapp.filter;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.webapp.website.WebSiteProperties;

/**
 * SCIPIO: Helper methods for URL rewriting and filtering.
 * Added 2017-08-14.
 */
public class UrlFilterHelper {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Sets some common request attributes needed by URL rewriting, for inbound rules.
     * NOTE: these should NOT be accessed by most webapps; is workaround for limitations in urlrewritefilter.
     */
    public void setCommonAttr(HttpServletRequest request, HttpServletResponse response) {
        ServletContext sc = request.getServletContext();
        request.setAttribute("scpCtrlServPath", sc.getAttribute("_CONTROL_SERVPATH_"));
        request.setAttribute("scpCtrlMapping", sc.getAttribute("_CONTROL_MAPPING_"));
        
        // cannot do this here, too risk of caching WebSiteProperties in req attribs prematurely 
        // and ruining the request for tenants
        //getSetWebappPathPrefix(request);
    }
    
    /**
     * @deprecated use {@link #setCommonAttr(HttpServletRequest, HttpServletResponse)}.
     */
    @Deprecated
    public void setCommonReqAttr(HttpServletRequest request, HttpServletResponse response) {
        setCommonAttr(request, response);
    }
    
    /**
     * Sets some common request attributes needed by URL rewriting, for outbound rules.
     * NOTE: these should NOT be accessed by most webapps; is workaround for limitations in urlrewritefilter.
     */
    public void setCommonOutboundAttr(HttpServletRequest request, HttpServletResponse response) {
        // TODO: REVIEW: is this re-assign needed here? will probably just waste cpu since
        // files will already call it manually
        //setCommonReqAttr(request, response);
        checkWebappContextPathAndSetAttr(request, response);
    }

    /**
     * @deprecated use {@link #setCommonOutboundReqAttr(HttpServletRequest, HttpServletResponse)}.
     */
    @Deprecated
    public void verifySameWebappContext(HttpServletRequest request, HttpServletResponse response) {
        setCommonOutboundAttr(request, response);
    }
    
    protected String getSetWebappPathPrefixForOutboundFilter(HttpServletRequest request) {
        WebSiteProperties webSiteProperties = WebSiteProperties.fromRequestFilterSafe(request); // 2018-07-31

        // FIXME: should get this through FullWebappInfo or OfbizUrlBuilder instead, but they don't have FilterSafe yet...
        boolean webappPathPrefixUrlBuild = webSiteProperties.isWebappPathPrefixUrlBuild(request.getServletContext());
        
        request.setAttribute("scpWebappPathPrefix", webSiteProperties.getWebappPathPrefix());
        request.setAttribute("scpWPPInUrl", webappPathPrefixUrlBuild ? "true" : "false");
        return webappPathPrefixUrlBuild ? webSiteProperties.getWebappPathPrefix() : "";
    }

    public boolean checkWebappContextPathAndSetAttr(HttpServletRequest request, HttpServletResponse response) {
        String outboundUrlStr = (String) request.getAttribute("urlFilter.outUrlWebapp.outUrl");
        String webappPathPrefix = getSetWebappPathPrefixForOutboundFilter(request); // 2018-08-03
        
        boolean sameContextPath = false;
        boolean wppInUrl = false;
        String wppFreeUrl = (outboundUrlStr != null) ? outboundUrlStr : "";
        if (outboundUrlStr != null) {
            String currentContextPath = webappPathPrefix + request.getContextPath();
            
            String urlPath = null;
            Matcher matcher = pathPat.matcher(outboundUrlStr);
            if (matcher.matches()) {
                String pathMatch = matcher.group(2);
                if (pathMatch == null) pathMatch = "";
                if (pathMatch.isEmpty()) {
                    urlPath = "/";
                } else {
                    urlPath = pathMatch;
                }
 
                if (urlPath.equals(currentContextPath)) {
                    sameContextPath = true;
                } else {
                    if (!currentContextPath.endsWith("/")) currentContextPath += "/";
                    sameContextPath = urlPath.startsWith(currentContextPath);
                }
                
                if (!webappPathPrefix.isEmpty()) {
                    String middle;
                    if (sameContextPath) {
                        middle = pathMatch.substring(webappPathPrefix.length());
                        wppInUrl = true;
                    } else {
                        // could have matching webappPathPrefix but different context, check now...
                        if (urlPath.equals(webappPathPrefix) || urlPath.startsWith(webappPathPrefix + "/")) {
                            middle = pathMatch.substring(webappPathPrefix.length());
                            wppInUrl = true;
                        } else {
                            middle = pathMatch;
                        }
                    }
                    StringBuilder sb = new StringBuilder();
                    if (matcher.group(1) != null) sb.append(matcher.group(1));
                    sb.append(middle);
                    if (matcher.group(3) != null) sb.append(matcher.group(3));
                    wppFreeUrl = sb.toString();
                }
            }
        }
        
        request.setAttribute("scpUrlOutSameCtx", sameContextPath ? "true" : "false");
        request.setAttribute("urlFilter.outUrlWebapp.isSameContext", sameContextPath ? "true" : "false"); // legacy
        request.setAttribute("scpWPPInUrl", wppInUrl ? "true" : "false");
        request.setAttribute("scpWPPFreeUrl", wppFreeUrl);
        return sameContextPath;
    }

    // TODO: ideally should optimize the regexp away
    private static final Pattern pathPat = Pattern.compile("^([^/]*//[^/]*)?(/.*?)?([?;#].*)?$");

    protected static String getPathFromUrl(String url) {
        String urlPath = null;
        Matcher matcher = pathPat.matcher(url);
        if (matcher.matches()) {
            String pathMatch = matcher.group(2);
            if (pathMatch.isEmpty()) {
                urlPath = "/";
            } else {
                urlPath = pathMatch;
            }
        }
        return urlPath;
    }
}
