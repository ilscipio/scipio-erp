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

    public static final String URL_REWRITE_TARGET_WEBAPP = "scpUrlReTgtWebapp";

    /**
     * Sets some common request attributes needed by URL rewriting, for both inbound and outbound rules.
     */
    public void setCommonAttr(HttpServletRequest request, HttpServletResponse response, FullWebappInfo targetWebappInfo) {
        ServletContext sc = request.getServletContext();
        if (targetWebappInfo != null) {
            // if specific target webapp, we can't optimize, we always have to set these
            request.setAttribute("scpCtrlServPath", targetWebappInfo.getControlServletPath());
            request.setAttribute("scpCtrlMapping", targetWebappInfo.getControlServletMapping());
        } else {
            if (!Boolean.TRUE.equals(request.getAttribute("scpUrlReCmnSet"))) {

                request.setAttribute("scpCtrlServPath", sc.getAttribute("_CONTROL_SERVPATH_"));
                request.setAttribute("scpCtrlMapping", sc.getAttribute("_CONTROL_MAPPING_"));

                request.setAttribute("scpUrlReCmnSet", Boolean.TRUE);
            }
        }
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

        // IMPORTANT: 2018-08-08: if a target webapp is set, we must use its info instead of
        // the original webapp in HttpServletRequest - it means this is an inter-webapp URL rewriting attempt
        // NOTE: This will depend on the implementation of ScipioUrlRewriter used in the end,
        // but it is never wrong to check this
        FullWebappInfo targetWebappInfo = (FullWebappInfo) request.getAttribute(URL_REWRITE_TARGET_WEBAPP);
        setCommonAttr(request, response, targetWebappInfo);
        checkWebappContextPathAndSetAttr(request, response, targetWebappInfo);

        if (targetWebappInfo != null) {
            request.setAttribute("scpUrlCtxPath", targetWebappInfo.getContextPath());
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

    public boolean checkWebappContextPathAndSetAttr(HttpServletRequest request, HttpServletResponse response, FullWebappInfo targetWebappInfo) {
        String outboundUrlStr = (String) request.getAttribute("urlFilter.outUrlWebapp.outUrl");

        String webappPathPrefixRaw = "";
        String webappPathPrefix = "";
        try {
            FullWebappInfo webappInfo = targetWebappInfo;
            if (webappInfo == null) {
                webappInfo = FullWebappInfo.fromRequestFilterSafe(request); // 2018-07-31
            }
            OfbizUrlBuilder urlInfo = webappInfo.getOfbizUrlBuilder();

            webappPathPrefixRaw = urlInfo.getWebappPathPrefix();
            request.setAttribute("scpWebappPathPrefix", webappPathPrefixRaw);
            request.setAttribute("scpWPPInUrl", urlInfo.isWebappPathPrefixUrlBuild() ? "true" : "false");

            webappPathPrefix = urlInfo.isWebappPathPrefixUrlBuild() ? webappPathPrefixRaw : "";
        } catch(Exception e) {
            Debug.logError("UrlFilterHelper: Error while fetching webapp info: " + e.toString(), module);
        }

        boolean sameContextPath = false;
        boolean wppInUrl = false;
        String wppFreeUrl = (outboundUrlStr != null) ? outboundUrlStr : "";
        if (outboundUrlStr != null) {
            String currentContextPath = webappPathPrefix
                    + (targetWebappInfo != null ? targetWebappInfo.getContextPath() : request.getContextPath());

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

        // high-level control attributes, so we can control the urlrewrite.xml behavior from here
        // and adjust for new features
        request.setAttribute("scpWPPStrip", wppInUrl ? "true" : "false");
        request.setAttribute("scpWPPReadd", (webappPathPrefixRaw.length() > 0) ? "true" : "false");

        return sameContextPath;
    }

    public void doInterWebappUrlRewrite(HttpServletRequest request, HttpServletResponse response) {
        String url = (String) request.getAttribute("urlFilter.outUrlWebapp.outUrl");

        FullWebappInfo targetWebappInfo = (FullWebappInfo) request.getAttribute(URL_REWRITE_TARGET_WEBAPP);
        if (targetWebappInfo != null) {
            try {
                ScipioUrlRewriter rewriter = ScipioUrlRewriter.getForRequest(targetWebappInfo, request, response, true);
                url = rewriter.processOutboundUrl(url, request, response);
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

    /**
     * Returns the effective target webapp's info for the urlrewrite.xml.
     * This may be different than the webapp represented by the request.
     */
    public static FullWebappInfo getEffectiveTargetWebapp(HttpServletRequest request) {
        FullWebappInfo targetWebappInfo = (FullWebappInfo) request.getAttribute(URL_REWRITE_TARGET_WEBAPP);
        if (targetWebappInfo != null) {
            try {
                targetWebappInfo = FullWebappInfo.fromRequest(request);
            } catch (Exception e) {
                Debug.logError(e, "getEffectiveTargetWebapp: Error getting current webapp info: " + e.getMessage(), module);
            }
        }
        return targetWebappInfo;
    }
}
