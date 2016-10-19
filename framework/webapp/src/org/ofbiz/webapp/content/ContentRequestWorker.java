package org.ofbiz.webapp.content;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.webapp.taglib.ContentUrlTag;

/**
 * SCIPIO: new class for content request-related implementations.
 */
public abstract class ContentRequestWorker {
    
    /**
     * SCIPIO: builds a content link.
     * <p>
     * SCIPIO: added a urlDecode boolean and changed the default behavior to NOT url-decode (FALSE);
     * it should be done before storing in the database - if/when needed.
     * having default as true would be dangerous!
     */
    public static String makeContentLink(HttpServletRequest request, HttpServletResponse response, String uri, String imgSize, String webSiteId, Boolean urlDecode) {
        String requestUrl = uri;

        // SCIPIO: Our default behavior is NOT to decode unless requested, in contrast to stock Ofbiz
        if (Boolean.TRUE.equals(urlDecode)) {
            requestUrl = UtilCodec.getUrlDecoder().decode(requestUrl);
        }
        
        // If the URL starts with http(s) then there is nothing for us to do here
        // SCIPIO: FIXME?: We try to use a better test here, but we are forced to use the Permissive
        // version because - due to Ofbiz design of escaping - here we may receive a uri encoded 
        // in any language - highly problematic!
        if (UtilHttp.isFullUrlPerm(requestUrl)) {
            return requestUrl;
        }

        // make the link
        StringBuilder newURL = new StringBuilder();
        ContentUrlTag.appendContentPrefix(request, newURL, webSiteId);
        if ((newURL.length() > 0 && newURL.charAt(newURL.length() - 1) != '/') 
                && (requestUrl.length()> 0 && requestUrl.charAt(0) != '/')) {
            newURL.append('/');
        }

        if(UtilValidate.isNotEmpty(imgSize)){
            // SCIPIO: WARN/FIXME?: This hardcoded value check does NOT work properly even if it's
            // unhardcoded, because the uri may have been passed encoded in html or some other language.
            // It generally only works if the value is hardcoded straight into FTL, e.g.,
            // <#assign someUrl = "/images/defaultImage.jpg">
            // <@ofbizContentUrl>${someUrl}</@ofbizContentUrl>
            // In Scipio we should probably just avoid having such exceptions, but leaving this
            // here for now for legacy compatibility with old templates.
            if(!"/images/defaultImage.jpg".equals(requestUrl)){
                int index = requestUrl.lastIndexOf(".");
                if (index > 0) {
                    String suffix = requestUrl.substring(index);
                    String imgName = requestUrl.substring(0, index);
                    requestUrl = imgName + "-" + imgSize + suffix;
                }
            }
        }

        newURL.append(requestUrl);
        
        return newURL.toString();
    }
    
    public static String makeContentLink(HttpServletRequest request, HttpServletResponse response, String uri, String imgSize, String webSiteId) {
        return makeContentLink(request, response, uri, imgSize, webSiteId, null);
    }
    
    public static String makeContentLink(HttpServletRequest request, HttpServletResponse response, String uri, String imgSize) {
        return makeContentLink(request, response, uri, imgSize, null);
    }

}
