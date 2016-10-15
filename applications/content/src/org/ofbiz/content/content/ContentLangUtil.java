package org.ofbiz.content.content;

import org.ofbiz.base.util.UtilCodec;

/**
 * SCIPIO: ContentWrapper language handling encoder.
 * <p>
 * Should be used to implement {@link ContentWrapper#get(String, String)}.
 */
public class ContentLangUtil {
    
    private static final RawContentSanitizer rawContentSanitizer = new RawContentSanitizer();
    private static final UrlContentSanitizer urlContentSanitizer = new UrlContentSanitizer();

    public static interface ContentSanitizer extends UtilCodec.SimpleEncoder {
        
    }
    
    public static class RawContentSanitizer implements ContentSanitizer {
        @Override
        public String encode(String original) {
            return UtilCodec.getRawEncoder().encode(original);
        }
    }
    
    public static class UrlContentSanitizer implements ContentSanitizer {
        @Override
        public String encode(String original) {
            // TODO: Here we should URL-encode the parameters (ONLY)
            return UtilCodec.getRawEncoder().encode(original);
        }
    }
    
    /**
     * SCIPIO: Returns a content early sanitizer for the given language.
     * <p>
     * TODO: 2016-10-14: Currently the ONLY language which will receive sanitization will be
     * "url". The URL will be parsed and its individual parameters will be URL-escaped in UTF-8.
     * PROPER URL ENCODE NOT YET IMPLEMENTED, TODO
     * <p>
     * All others such as HTML are left as-is. HTML gets automatically escaped
     * by the screen auto-escaping when returned from the {@link ContentWrapper#get(String, String)}
     * method (since it no longer returns a StringWrapper).
     */
    public static ContentSanitizer getEarlySanitizer(String langName) {
        if ("url".equals(langName)) {
            return urlContentSanitizer;
        } else {
            return rawContentSanitizer;
        }
    }
    
}
