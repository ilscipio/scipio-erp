package com.ilscipio.scipio.ce.webapp.filter;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * SCIPIO: Helper methods for URL rewriting and filtering.
 * Added 2017-08-14.
 */
public class UrlFilterHelper {

    // NOTE: these should be non-static methods (TODO: re-verify)
    
    public void verifySameWebappContext(HttpServletRequest request, HttpServletResponse response) {
        String outboundUrlStr = (String) request.getAttribute("urlFilter.outUrlWebapp.outUrl");
        boolean isSameContextPath = isSameWebappContext(request, outboundUrlStr);
        request.setAttribute("urlFilter.outUrlWebapp.isSameContext", isSameContextPath ? "true" : "false");
    }
    
    public boolean isSameWebappContext(HttpServletRequest request, String outboundUrlStr) {
        if (outboundUrlStr != null) {
            String currentContextPath = request.getContextPath();
            String urlContextPath = getPathFromUrl(outboundUrlStr);
            if (urlContextPath == null) return false;
            if (urlContextPath.equals(currentContextPath)) {
                return true;
            } else {
                if (!currentContextPath.endsWith("/")) currentContextPath += "/";
                return urlContextPath.startsWith(currentContextPath);
            }
        }
        return false;
    }

    // FIXME: ideally should optimize the regexp away
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
