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

    // NOTE: these should be non-static methods (TODO: re-verify)

    public void verifySameWebappContext(HttpServletRequest request, HttpServletResponse response) {
        String outboundUrlStr = (String) request.getAttribute("urlFilter.outUrlWebapp.outUrl");
        boolean isSameContextPath = isSameWebappContext(request, outboundUrlStr);
        request.setAttribute("urlFilter.outUrlWebapp.isSameContext", isSameContextPath ? "true" : "false");
        //Debug.logInfo("isSameContext: " + outboundUrlStr + "? " + isSameContextPath, module);
    }

    public boolean isSameWebappContext(HttpServletRequest request, String outboundUrlStr) {
        if (outboundUrlStr != null) {
            String webappPathPrefix = WebSiteProperties.getWebappPathPrefixFilterSafe(request); // 2018-07-31
            String currentContextPath = webappPathPrefix + request.getContextPath();
            String urlPath = getPathFromUrl(outboundUrlStr);
            if (urlPath == null) return false;
            if (urlPath.equals(currentContextPath)) {
                return true;
            } else {
                if (!currentContextPath.endsWith("/")) currentContextPath += "/";
                return urlPath.startsWith(currentContextPath);
            }
        }
        return false;
    }

    // TODO: ideally should optimize the regexp away
    private static final Pattern pathPat = Pattern.compile("^([^/]*//[^/]*)?(/.*?)?([?;].*)?$");
    protected static String getPathFromUrl(String url) {
        String result = null;
        Matcher matcher = pathPat.matcher(url);
        if (matcher.matches()) {
            String pathMatch = matcher.group(2);
            if (pathMatch.isEmpty()) {
                result = "/";
            } else {
                result = pathMatch;
            }
        }
        return result;
    }

    /**
     * Sets some common request attributes needed by URL rewriting.
     * NOTE: these should NOT be accessed by most webapps; is workaround for limitations in urlrewritefilter.
     */
    public void setCommonReqAttr(HttpServletRequest request, HttpServletResponse response) {
        ServletContext sc = request.getServletContext();
        request.setAttribute("scpCtrlServPath", sc.getAttribute("_CONTROL_SERVPATH_"));
        request.setAttribute("scpCtrlMapping", sc.getAttribute("_CONTROL_MAPPING_"));
    }
}
