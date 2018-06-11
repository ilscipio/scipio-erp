package org.ofbiz.content.content;

import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilCodec.SimpleEncoder;

/**
 * SCIPIO: ContentWrapper language handling encoder, to help implement {@link ContentWrapper}
 * method implementations.
 */
public class ContentLangUtil {
    
    /**
     * SCIPIO: Returns a content early sanitizer for the given language.
     * <p>
     * Should be used to implement {@link ContentWrapper#get(String, String)}.
     * <p>
     * NOTE: 2016-10-14: Currently the only language which <em>may</em> receive sanitization is
     * "url". The URL <em>may</em> receive URL encoding of its parameters (only).
     * TODO?: Said URL parameter escaping is not currently implemented (is not urgent as this is
     * used to process values fresh out of database using {@link ContentWrapper}).
     * <p>
     * All others such as HTML are left as-is. HTML gets automatically escaped
     * by the screen renderer auto-escaping when returned from the {@link ContentWrapper#get(String, String)}
     * method in a freemarker template or across the data prep boundary (since it no longer returns a StringWrapper) 
     * during freemarker/screen rendering.
     */
    public static SimpleEncoder getContentWrapperSanitizer(String lang) {
        if ("url".equals(lang)) {
            return UtilCodec.getUrlEncoder();
        } else {
            return UtilCodec.getRawEncoder();
        }
    }
    
    public static SimpleEncoder getEarlySanitizer(String lang) {
        if ("url".equals(lang)) {
            return UtilCodec.getUrlEncoder();
        } else {
            return UtilCodec.getRawEncoder();
        }
    }
}
