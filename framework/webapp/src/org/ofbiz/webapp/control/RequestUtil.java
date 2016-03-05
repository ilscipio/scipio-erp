package org.ofbiz.webapp.control;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletResponse;

/**
 * Cato: Request utilities.
 */
public abstract class RequestUtil {

    
    private static final Pattern jsessionIdPat = Pattern.compile("((;jsessionid=)([^\\?#]*))");
    
    protected RequestUtil() {
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
        return RequestUtil.removeJsessionId(response.encodeURL(url));
    }
}
