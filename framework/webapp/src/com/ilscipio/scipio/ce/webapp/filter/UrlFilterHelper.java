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
    public static final String SOURCE_CONTEXT = "scpUrlReSrcCtx";

    /**
     * When doing inter-webapp URL rewriting, name of a request attribute
     * containing the original HttpServletRequest of the source webapp.
     */
    public static final String SOURCE_REQUEST = "scpUrlReSrcReq";

    /**
     * For outbound rules, name of request attribute containing the FullWebappInfo
     * for the webapp to which the urlrewrite.xml file is associated.
     * <p>
     * This is NOT (necessarily) the webapp of the url currently being processed.
     * <p>
     * This is set through {@link #doInterWebappUrlRewrite} and
     * {@link com.ilscipio.scipio.ce.webapp.filter.urlrewrite.ScipioUrlRewriter#processOutboundUrl}
     * and read back by {@link #setCommonAttrOut} when inter-webapp urlrewrite.xml
     * outbound-rule emulation is triggered, because in those cases the request
     * may not be usable to determine the webapp to which urlrewrite.xml belongs.
     */
    public static final String URLREWRITE_CONF_WEBAPP = "scpUrlReCnfWebapp";

    /**
     * Name of optionally-present request attribute containing the FullWebappInfo
     * for the link currently being processed by outbound-rules.
     * <p>
     * Currently this is set by {@link org.ofbiz.webapp.control.RequestHandler#doLinkURLEncode}
     * as optimization (though practically necessary because it is complicated by webappPathPrefix when in URL).
     */
    public static final String OUT_URL_WEBAPP = "scpUrlOutWebapp";

    /**
     * Sets some common request attributes needed by URL rewriting, for both inbound and outbound rules.
     */
    public void setCommonAttr(HttpServletRequest request, HttpServletResponse response, FullWebappInfo webappInfo) {
        ServletContext sc = request.getServletContext();
        if (webappInfo != null) {
            // if specific target webapp, we can't optimize, we always have to set these
            request.setAttribute("scpCtrlServPath", webappInfo.getControlServletPath());
            request.setAttribute("scpCtrlMapping", webappInfo.getControlServletMapping());
        } else {
            if (!Boolean.TRUE.equals(request.getAttribute("scpUrlReCmnSet"))) {

                request.setAttribute("scpCtrlServPath", sc.getAttribute("_CONTROL_SERVPATH_"));
                request.setAttribute("scpCtrlMapping", sc.getAttribute("_CONTROL_MAPPING_"));

                request.setAttribute("scpUrlReCmnSet", Boolean.TRUE);
            }
        }
    }

    /**
     * Sets some common request attributes needed by URL rewriting, for inbound rules.
     */
    public void setCommonAttrIn(HttpServletRequest request, HttpServletResponse response) {
        setCommonAttr(request, response, null);
    }

    /**
     * @deprecated use {@link #setCommonAttrIn(HttpServletRequest, HttpServletResponse)}.
     */
    @Deprecated
    public void setCommonReqAttr(HttpServletRequest request, HttpServletResponse response) {
        setCommonAttr(request, response, null);
    }

    /**
     * Sets some common request attributes needed by URL rewriting, for outbound rules only.
     */
    public void setCommonAttrOut(HttpServletRequest request, HttpServletResponse response) {
        // NOTE: 2018-08-06: We MUST run setCommonAttr here because in static/email
        // render context, only the outbound-rules will be executed.

        // IMPORTANT: 2018-08-08: The webapp for this urlrewrite.xml may actually differ
        // from the "current" webapp in the request; in that case we have an explicit attribute
        // to get the real webapp for this urlrewrite.xml file
        FullWebappInfo webappInfo = (FullWebappInfo) request.getAttribute(URLREWRITE_CONF_WEBAPP);
        setCommonAttr(request, response, webappInfo);
        checkTargetWebapp(request, response, webappInfo);

        if (webappInfo != null) {
            request.setAttribute("scpUrlCtxPath", webappInfo.getContextPath());
        } else {
            request.setAttribute("scpUrlCtxPath", request.getContextPath());
        }
    }

    /**
     * @deprecated use {@link #setCommonAttrOut(HttpServletRequest, HttpServletResponse)}.
     */
    @Deprecated
    public void verifySameWebappContext(HttpServletRequest request, HttpServletResponse response) {
        setCommonAttrOut(request, response);
    }

    public void checkTargetWebapp(HttpServletRequest request, HttpServletResponse response) {
        checkTargetWebapp(request, response, (FullWebappInfo) request.getAttribute(URLREWRITE_CONF_WEBAPP));
    }
    /**
     * Checks the current outbound URL against the conf's current webapp to determine if it
     * should be processed by a different urlrewrite conf. Sets many request attributes.
     */
    public boolean checkTargetWebapp(HttpServletRequest request, HttpServletResponse response, FullWebappInfo webappInfo) {
        String outboundUrlStr = (String) request.getAttribute("urlFilter.outUrlWebapp.outUrl");

        // NOTE: outUrlWebappInfo may be null (consider optimization at best)
        FullWebappInfo outUrlWebappInfo = (FullWebappInfo) request.getAttribute(OUT_URL_WEBAPP);

        String webappPathPrefix = "";
        boolean wppInUrl = false;
        boolean sameContextPath = false;
        boolean wppMatched = false;
        String wppFreeUrl = (outboundUrlStr != null) ? outboundUrlStr : "";
        String pathMatch = "";

        try {
            if (webappInfo == null) {
                webappInfo = FullWebappInfo.fromRequestFilterSafe(request); // 2018-07-31
            }
            OfbizUrlBuilder urlInfo = webappInfo.getOfbizUrlBuilder();
            webappPathPrefix = urlInfo.getWebappPathPrefix();
            wppInUrl = urlInfo.isWebappPathPrefixUrlBuild();
        } catch(Exception e) {
            Debug.logError("UrlFilterHelper: Error while fetching webapp info: " + e.toString(), module);
        }

        if (outUrlWebappInfo != null && !outUrlWebappInfo.equals(webappInfo)) {

            ; // FAST ABORT: different target webapp (as reported by caller)

        } else if (outboundUrlStr != null) {
            // TODO?: here we could check for outUrlWebappInfo.equals(webappInfo)
            // and optimize? for now, appears safer to physically check to make sure the paths match
            // up, and have to anyway in order to extract the parts.
            
            String currentContextPath = webappPathPrefix
                    + (webappInfo != null ? webappInfo.getContextPath() : request.getContextPath());

            String urlPath = null;
            Matcher matcher = pathPat.matcher(outboundUrlStr);
            if (matcher.matches()) {
                pathMatch = matcher.group(2);
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

                if (wppInUrl && !webappPathPrefix.isEmpty()) {
                    String middle;
                    if (sameContextPath) {
                        middle = pathMatch.substring(webappPathPrefix.length());
                        wppMatched = true;
                    } else {
                        // could have matching webappPathPrefix but different context, check now...
                        if (urlPath.equals(webappPathPrefix) || urlPath.startsWith(webappPathPrefix + "/")) {
                            middle = pathMatch.substring(webappPathPrefix.length());
                            wppMatched = true;
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
        request.setAttribute("urlFilter.outUrlWebapp.isSameContext", sameContextPath ? "true" : "false"); // legacy compatibility

        request.setAttribute("scpWebappPathPrefix", webappPathPrefix);
        // scpWPPInUrl was ambiguous, scpWPPStrip is sufficient for now
        //request.setAttribute("scpWPPInUrl", wppInUrl ? "true" : "false");
        request.setAttribute("scpWPPFreeUrl", wppFreeUrl);
        // high-level control attributes, so we can control the urlrewrite.xml behavior from here
        // and adjust for new features
        request.setAttribute("scpWPPStrip", wppMatched ? "true" : "false");
        request.setAttribute("scpWPPReadd", (webappPathPrefix.length() > 0) ? "true" : "false");

        return sameContextPath;
    }

    public void doInterWebappUrlRewrite(HttpServletRequest request, HttpServletResponse response) {
        String url = (String) request.getAttribute("urlFilter.outUrlWebapp.outUrl");

        FullWebappInfo outUrlWebappInfo = (FullWebappInfo) request.getAttribute(OUT_URL_WEBAPP);

        // TODO?: this is very difficult and inefficient due to webappPathPrefix...
        //if (outUrlWebappInfo == null) {
            // BEST-EFFORT: caller gave us no hint, so TRY to determine target webapp
            //String outboundUrlStr = (String) request.getAttribute("urlFilter.outUrlWebapp.outUrl");
        //}

        if (outUrlWebappInfo != null) {
            try {
                ScipioUrlRewriter rewriter = ScipioUrlRewriter.getForRequest(outUrlWebappInfo, request, response, true);
                url = rewriter.processOutboundUrl(url, outUrlWebappInfo, request, response);
            } catch (Exception e) {
                Debug.logError("doInterWebappUrlRewrite: Error URL-encoding (rewriting) link for webapp " + outUrlWebappInfo
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

    /**
     * Returns the effective webapp for the urlrewrite.xml.
     * NOTE: This may be different than the webapp represented by the request!
     * <p>
     * If there is no URLREWRITE_CONF_WEBAPP request attrib, is assumed the current request's webapp
     * is the webapp for this urlrewrite conf.
     */
    public static FullWebappInfo getUrlRewriteConfWebapp(HttpServletRequest request) {
        FullWebappInfo targetWebappInfo = (FullWebappInfo) request.getAttribute(URLREWRITE_CONF_WEBAPP);
        if (targetWebappInfo != null) {
            try {
                targetWebappInfo = FullWebappInfo.fromRequestFilterSafe(request);
            } catch (Exception e) {
                Debug.logError(e, "getUrlRewriteConfWebapp: Error getting current webapp info: " + e.getMessage(), module);
            }
        }
        return targetWebappInfo;
    }
}
