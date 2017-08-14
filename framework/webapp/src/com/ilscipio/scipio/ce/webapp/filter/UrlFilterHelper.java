package com.ilscipio.scipio.ce.webapp.filter;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * SCIPIO: Helper methods for URL rewriting and filtering.
 * Added 2017-08-14.
 */
public class UrlFilterHelper {

    public static void verifySameWebappContext(HttpServletRequest request, HttpServletResponse response) {
        String outboundUrlStr = (String) request.getAttribute("urlRewriteMod.outboundUrlWebapp.outboundUrl");
        boolean isSameContextPath = isSameWebappContext(request, outboundUrlStr);
        request.setAttribute("urlRewriteMod.outboundUrlWebapp.isSameContext", isSameContextPath ? "true" : "false");
    }
    
    public static boolean isSameWebappContext(HttpServletRequest request, String outboundUrlStr) {
        if (outboundUrlStr != null) {
            String currentContextPath = request.getContextPath();
            String urlContextPath = getPathFromUrl(outboundUrlStr);
            if (urlContextPath.equals(currentContextPath)) {
                return true;
            }
            else {
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
            }
            else {
                result = pathMatch;
            }
        }
        return result;
    }
}
