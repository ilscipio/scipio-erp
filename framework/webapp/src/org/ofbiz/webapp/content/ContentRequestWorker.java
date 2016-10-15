package org.ofbiz.webapp.content;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.webapp.taglib.ContentUrlTag;

/**
 * SCIPIO: new class for content request-related implementations.
 */
public abstract class ContentRequestWorker {

    public static String makeContentLink(HttpServletRequest request, HttpServletResponse response, String uri, String imgSize, String webSiteId) {
        String requestUrl = uri;

        // If the URL starts with http(s) then there is nothing for us to do here
        if (requestUrl.startsWith("http://") || requestUrl.startsWith("https://") || requestUrl.startsWith("//")) { // SCIPIO: better tests
            return requestUrl;
        }

        // SCIPIO: WARN: FIXME?: this is an Ofbiz kludge to get around Ofbiz's own UtilCodec behavior.
        // this should not have to happen here and consequences unknown at this time...
        requestUrl = UtilCodec.getDecoder("url").decode(requestUrl);

        // make the link
        StringBuilder newURL = new StringBuilder();
        ContentUrlTag.appendContentPrefix(request, newURL, webSiteId);
        if ((newURL.length() > 0 && newURL.charAt(newURL.length() - 1) != '/') 
                && (requestUrl.length()> 0 && requestUrl.charAt(0) != '/')) {
            newURL.append('/');
        }

        if(UtilValidate.isNotEmpty(imgSize)){
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
    
    public static String makeContentLink(HttpServletRequest request, HttpServletResponse response, String uri, String imgSize) {
        return makeContentLink(request, response, uri, imgSize, null);
    }

}
