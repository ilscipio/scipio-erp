package com.ilscipio.scipio.cms.util.fileType;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.tika.mime.MediaType;
import org.apache.tika.mime.MediaTypeRegistry;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

public class PreviewHelper {

    // these map mime-types AND implicitly their subtypes, to browser-recognized media types
    private static final Map<MediaType, String> audioMediaTypeGeneralizations = makeAudioMediaTypeGeneralizationsMap();
    private static final Map<MediaType, String> videoMediaTypeGeneralizations = makeVideoMediaTypeGeneralizationsMap();

    protected PreviewHelper(HttpServletRequest request, HttpServletResponse response) {
    }

    protected PreviewHelper(Delegator delegator, String userAgent) {
    }
    
    public static PreviewHelper getInstance(HttpServletRequest request, HttpServletResponse response) {
        return new PreviewHelper(request, response);
    }
    
    public static PreviewHelper getInstance(Delegator delegator, String userAgent) {
        return new PreviewHelper(delegator, userAgent);
    }
    
    private static Map<MediaType, String> makeAudioMediaTypeGeneralizationsMap() {
        Map<MediaType, String> map = new HashMap<>();
        
        // browser's don't recognize audio/vorbis, so this will return audio/ogg instead.
        map.put(TikaUtil.asNormMediaType("audio/ogg"), "audio/ogg");
        
        // potential others...
        map.put(TikaUtil.asNormMediaType("audio/webm"), "audio/webm");
        map.put(TikaUtil.asNormMediaType("audio/mpeg"), "audio/mpeg");
        map.put(TikaUtil.asNormMediaType("audio/x-flac"), "audio/flac");
        
        return map;
    }
    
    private static Map<MediaType, String> makeVideoMediaTypeGeneralizationsMap() {
        Map<MediaType, String> map = new HashMap<>();
        
        map.put(TikaUtil.asNormMediaType("video/webm"), "video/webm");
        map.put(TikaUtil.asNormMediaType("video/ogg"), "video/ogg");
        map.put(TikaUtil.asNormMediaType("video/mpeg"), "video/mpeg");
        
        return map;
    }
    
    /**
     * Translates a mime-type name/ID from a specific, accurate type to
     * a type supported by preview by most browsers, if one exists.
     * <p>
     * Uses Apache Tika.
     */
    public String getPreviewMediaType(String dataResourceTypeId, String mimeTypeId) {
        return getGenericMediaType(dataResourceTypeId, mimeTypeId);
    }
    
    public String getPreviewMediaType(GenericValue dataResource) {
        return getGenericMediaType(dataResource.getString("dataResourceTypeId"), dataResource.getString("mimeTypeId"));
    }
    
    /**
     * Translates a mime-type name/ID from a specific, accurate type to
     * a type supported by preview by most browsers, if one exists.
     * <p>
     * Uses Apache Tika.
     */
    public String getGenericMediaType(String dataResourceTypeId, String mimeTypeId) {
        if (UtilValidate.isEmpty(mimeTypeId)) {
            return null;
        }
        String result = mimeTypeId;
        
        MediaTypeRegistry registry = TikaUtil.getMediaTypeRegistry();
        MediaType mediaType = TikaUtil.asMediaType(mimeTypeId);
        MediaType normMediaType = registry.normalize(mediaType); // need to pass normalized one to registry methods
        
        if ("AUDIO_OBJECT".equals(dataResourceTypeId)) {
            for(Map.Entry<MediaType, String> entry : audioMediaTypeGeneralizations.entrySet()) {
                if (registry.isInstanceOf(normMediaType, entry.getKey())) {
                    return entry.getValue();
                }
            }
        } else if ("VIDEO_OBJECT".equals(dataResourceTypeId)) {
            for(Map.Entry<MediaType, String> entry : videoMediaTypeGeneralizations.entrySet()) {
                if (registry.isInstanceOf(normMediaType, entry.getKey())) {
                    return entry.getValue();
                }
            }
        }

        return result;
    }
    
}
