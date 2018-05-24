package com.ilscipio.scipio.cms.webapp;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

/**
 * WebappUtil - Cms webapp utilities.
 */
public abstract class CmsWebappUtil {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    private CmsWebappUtil() {}

    
    /**
     * Gets a system-wide unique ID for the current request. WARNING: We cheat and use current thread ID for now!
     * Note the name: "current".
     */
    public static long getCurrentRequestUniqueId(HttpServletRequest request) {
        return Thread.currentThread().getId();
    }
    
    private static final Pattern charsetPat = Pattern.compile("((^|;)\\s*charset=).*?(;|$)", Pattern.CASE_INSENSITIVE);
    
    public static String replaceHttpContentTypeCharset(String prevContentType, String newCharset) {
        if (prevContentType == null || prevContentType.trim().length() == 0) {
            return "charset=" + newCharset;
        } else {
            if (prevContentType.toLowerCase().contains("charset=")) {
                Matcher m = charsetPat.matcher(prevContentType);
                return m.replaceFirst("$1" + newCharset + "$3");
            } else {
                return prevContentType + "; charset=" + newCharset;
            }
        }
    }
    
    private static final Pattern splitPairsRawQueryPat = Pattern.compile("&");
    
    public static class QueryParam {
        public final String name;
        public final String value;
        
        public QueryParam(String name, String value) {
            this.name = name;
            this.value = value;
        }
    }
    
    public static List<QueryParam> splitRawQueryString(String queryString) {
        List<QueryParam> params = new ArrayList<>();
        if (queryString == null) {
            return params;
        }
        
        boolean startsWithDelim = (queryString.startsWith("?") || queryString.startsWith("&"));

        // Do it in two splits so malformed URLs don't cause haywire results
        String[] pairs = splitPairsRawQueryPat.split(startsWithDelim ? queryString.substring(1): queryString);
        if (pairs != null && pairs.length > 0) {
            for(String pair : pairs) {
                int sepIndex = pair.indexOf('=');
                if (sepIndex >= 1) {
                    params.add(new QueryParam(pair.substring(0, sepIndex), pair.substring(sepIndex+1)));
                }
            }
        }

        return params;
    }
    
    /**
     * URL-encodes arbitrary URL parts.
     * <p>
     * Loosely based on Ofbiz's {@link org.ofbiz.base.util.UtilHttp#urlEncodeArgs}.
     */
    public static Collection<String> urlEncode(Collection<String> parts, Collection<String> out) throws IllegalArgumentException {
        for(String part : parts) {
            try {
                out.add(UtilCodec.getUrlEncoder().encode(part));
            } catch(Exception e) {
                // Probably a bad argument
                throw new IllegalArgumentException("Error URL-encoding string: " + part, e);
            }
        }
        return out;
    }
    
    public static String urlEncode(String urlPart) throws IllegalArgumentException {
        try {
            return UtilCodec.getUrlEncoder().encode(urlPart);
        } catch(Exception e) {
            // Probably a bad argument
            throw new IllegalArgumentException("Error URL-encoding string: " + urlPart, e);
        }
    }
    
    public static String urlDecode(String urlPart) throws IllegalArgumentException {
        try {
            return UtilCodec.getUrlDecoder().decode(urlPart);
        } catch(Exception e) {
            // Probably a bad argument
            throw new IllegalArgumentException("Error URL-decoding string: " + urlPart, e);
        }
    }
    
    public static String restrictQueryStringParams(String queryString, Collection<String> paramNames, 
            boolean paramNamesEncoded) throws IllegalArgumentException {
        return restrictRemoveQueryStringParams(queryString, paramNames, paramNamesEncoded, true);
    }
    
    public static String removeQueryStringParams(String queryString, Collection<String> paramNames, 
            boolean paramNamesEncoded) throws IllegalArgumentException {
        return restrictRemoveQueryStringParams(queryString, paramNames, paramNamesEncoded, false);
    }
    
    private static String restrictRemoveQueryStringParams(String queryString, Collection<String> paramNames, 
            boolean paramNamesEncoded, boolean restrictMode) throws IllegalArgumentException {
        if (queryString == null) {
            return null;
        }
        
        StringBuilder newQuery = new StringBuilder("");
        
        Collection<String> properNames = paramNamesEncoded ? paramNames : urlEncode(paramNames, new HashSet<String>());
        
        List<QueryParam> params = splitRawQueryString(queryString);
        if (!params.isEmpty()) {
            for(QueryParam param : params) {
                boolean paramNamed = properNames.contains(param.name);
                if ((restrictMode && paramNamed) || (!restrictMode && !paramNamed)) { // If restrict false, use remove mode
                    newQuery.append("&");
                    newQuery.append(param.name);
                    newQuery.append("=");
                    newQuery.append(param.value);
                }
            }
        }
        
        boolean startsWithDelim = (queryString.startsWith("?") || queryString.startsWith("&"));
        if (startsWithDelim) {
            if (newQuery.length() > 0) {
                newQuery.setCharAt(0, queryString.charAt(0));
            } else {
                newQuery.append(queryString.charAt(0));
            }
        } else {
            if (newQuery.length() > 0) {
                newQuery.deleteCharAt(0);
            }
        }
        
        return newQuery.toString();
    }
    
    
    public static List<GenericValue> getWebSiteList(Delegator delegator, Set<String> webSiteIdSet) throws GenericEntityException {
        List<GenericValue> cmsWebSiteList = new ArrayList<>();
        final List<String> orderBy = Arrays.asList(new String[] {"siteName"});
        for(GenericValue webSite : delegator.findByAnd("WebSite", null, orderBy, true)) {
            if (webSiteIdSet.contains(webSite.getString("webSiteId"))) {
                cmsWebSiteList.add(webSite);
            }
        }
        return cmsWebSiteList;
    }
    
}
