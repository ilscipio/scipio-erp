package com.ilscipio.scipio.ce.webapp.filter;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.webapp.FullWebappInfo;
import org.ofbiz.webapp.OfbizUrlBuilder;

import com.ilscipio.scipio.ce.webapp.filter.urlrewrite.ScipioUrlRewriter;

/**
 * SCIPIO: Helper methods for URL rewriting and filtering (urlrewrite.xml).
 * Added 2017-08-14.
 */
public class UrlFilterHelper {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * When rendering from static render contexts (emails, etc.),
     * name of a request attribute (in the emulated HttpServletRequest)
     * containing a context map of the static render context.
     * <p>
     * NOTE: This attribute is empty during real webapp renders.
     */
    public static final String URL_REWRITE_CONTEXT = "scpUrlReCtx";
    
    public static final String URL_REWRITE_TARGET_WEBAPP = "scpUrlReTargetWebap";
    
    /**
     * Sets some common request attributes needed by URL rewriting, for both inbound and outbound rules.
     */
    public void setCommonAttr(HttpServletRequest request, HttpServletResponse response) {
        if (!Boolean.TRUE.equals(request.getAttribute("scpUrlReCommonSet"))) {
            ServletContext sc = request.getServletContext();
            request.setAttribute("scpCtrlServPath", sc.getAttribute("_CONTROL_SERVPATH_"));
            request.setAttribute("scpCtrlMapping", sc.getAttribute("_CONTROL_MAPPING_"));

            // cannot do this here, too risk of caching WebSiteProperties in req attribs prematurely
            // and ruining the request for tenants
            //getSetWebappPathPrefix(request);
            
            request.setAttribute("scpUrlReCommonSet", Boolean.TRUE);
        }
    }

    /**
     * Sets some common request attributes needed by URL rewriting, for inbound rules only.
     */
    public void setCommonAttrIn(HttpServletRequest request, HttpServletResponse response) {
        setCommonAttr(request, response);
    }
    
    /**
     * @deprecated use {@link #setCommonAttrIn(HttpServletRequest, HttpServletResponse)}.
     */
    @Deprecated
    public void setCommonReqAttr(HttpServletRequest request, HttpServletResponse response) {
        setCommonAttr(request, response);
    }
  
    /**
     * Sets some common request attributes needed by URL rewriting, for outbound rules only.
     */
    public void setCommonAttrOut(HttpServletRequest request, HttpServletResponse response) {
        // NOTE: 2018-08-06: We MUST run setCommonAttr here because in static/email
        // render context, only the outbound-rules will be executed.
        setCommonAttr(request, response);
        checkWebappContextPathAndSetAttr(request, response);
    }

    /**
     * @deprecated use {@link #setCommonAttrOut(HttpServletRequest, HttpServletResponse)}.
     */
    @Deprecated
    public void verifySameWebappContext(HttpServletRequest request, HttpServletResponse response) {
        setCommonAttrOut(request, response);
    }

    protected String getSetWebappPathPrefixForOutboundFilter(HttpServletRequest request) {
        try {
            FullWebappInfo webappInfo = FullWebappInfo.fromRequestFilterSafe(request); // 2018-07-31
            OfbizUrlBuilder urlInfo = webappInfo.getOfbizUrlBuilder();

            request.setAttribute("scpWebappPathPrefix", urlInfo.getWebappPathPrefix());
            request.setAttribute("scpWPPInUrl", urlInfo.isWebappPathPrefixUrlBuild() ? "true" : "false");

            return urlInfo.isWebappPathPrefixUrlBuild() ? urlInfo.getWebappPathPrefix() : "";
        } catch(Exception e) {
            Debug.logError("UrlFilterHelper: Error while fetching webapp info: " + e.toString(), module);
            return "";
        }
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

    public void doInterWebappUrlRewrite(HttpServletRequest request, HttpServletResponse response) {
        String url = (String) request.getAttribute("urlFilter.outUrlWebapp.outUrl");
        
        FullWebappInfo targetWebappInfo = (FullWebappInfo) request.getAttribute(URL_REWRITE_TARGET_WEBAPP);
        if (targetWebappInfo != null) {
            try {
                ScipioUrlRewriter rewriter = ScipioUrlRewriter.getForRequest(targetWebappInfo, request, response, true);
                url = rewriter.processOutboundUrl(url);
            } catch (Exception e) {
                Debug.logError("doInterWebappUrlRewrite: Error URL-encoding (rewriting) link for webapp " + targetWebappInfo 
                        + ": " + e.toString(), module); 
            }
        }
        
        request.setAttribute("scpUrlReOut", url);
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
