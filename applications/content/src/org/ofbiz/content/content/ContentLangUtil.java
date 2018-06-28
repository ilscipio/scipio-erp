package org.ofbiz.content.content;

import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilCodec.SimpleEncoder;

/**
 * SCIPIO: ContentWrapper language handling encoder, to help implement {@link ContentWrapper}
 * method implementations.
 */
public class ContentLangUtil {
    
    /**
     * SCIPIO: Returns a content wrapper sanitizer for the given language, or
     * the "raw" encoder (no encoding) if null or not recognized.
     * <p>
     * <strong>UPDATE (2018-06-11):</strong> This method now returns a sanitizer
     * exactly as requested, i.e., HtmlEncoder for "html". Prior to 2018-06-11
     * this had been intentionally returning "raw" encoder for "html" and others (except "url")
     * as a backward-compatibility measure but there should be no code left in stock Scipio
     * for which this should cause any double-escaping.
     * <p>
     * <string>NOTE:</strong> In Scipio, the ContentWrapper get methods do not
     * return a StringWrapper, so if you request an "html" encoder, usually you
     * need to use <code>rawString(productContentWrapper.get(xxx, "html"))</code> 
     * around the call.
     * <p>
     * Should be used to implement {@link ContentWrapper#get(String, String)}.
     */
    public static SimpleEncoder getContentWrapperSanitizer(String lang) {
        SimpleEncoder encoder = UtilCodec.getEncoder(lang);
        return (encoder != null) ? encoder : UtilCodec.getRawEncoder();
        // Old pre-2018-08-11 code:
        //if ("url".equals(lang)) {
        //    return UtilCodec.getUrlEncoder();
        //} else {
        //    return UtilCodec.getRawEncoder();
        //}
    }
    
    /**
     * SCIPIO: Returns a content early sanitizer for the given language.
     * <p>
     * Should be used to implement {@link ContentWrapper#get(String, String)}.
     * <p>
     * NOTE: 2016-10-14: Currently the only language which <em>may</em> receive early sanitization is
     * "url". The URL <em>may</em> receive URL encoding of its parameters (only).
     * TODO?: Said URL parameter escaping is not currently implemented (is not urgent as this is
     * used to process values fresh out of database.
     * <p>
     * All others such as HTML are left as-is. HTML gets automatically escaped
     * by the screen renderer auto-escaping when returned from the {@link ContentWrapper#get(String, String)}
     * method in a freemarker template or across the data prep boundary (since it no longer returns a StringWrapper) 
     * during freemarker/screen rendering.
     */
    public static SimpleEncoder getEarlySanitizer(String lang) {
        if ("url".equals(lang)) {
            return UtilCodec.getUrlEncoder();
        } else {
            return UtilCodec.getRawEncoder();
        }
    }
}
