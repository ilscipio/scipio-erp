package com.ilscipio.scipio.cms.util.fileType;

import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.tika.mime.MediaType;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.PropertyMessage;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.cms.util.fileType.AbstractFileType.MagicNumber;
import com.ilscipio.scipio.cms.util.fileType.audio.AudioFileTypeResolver;
import com.ilscipio.scipio.cms.util.fileType.document.DocumentFileTypeResolver;
import com.ilscipio.scipio.cms.util.fileType.image.ImageFileTypeResolver;
import com.ilscipio.scipio.cms.util.fileType.video.VideoFileTypeResolver;
import com.ilscipio.scipio.common.util.TikaUtil;

/**
 * File type resolver. Currently supports both manual file type detection and detection through
 * Apache Tika library.
 * <p>
 * WARN: 2017-02-07: interface subject to change.
 * <p>
 * NOTE: 2017-02-08: the new default configuration is to use only Apache Tika
 * and non-strict types. non-strict means all video types named "video/*" are allowed, and such.
 */
public abstract class FileTypeResolver {
    
    public static final String module = FileTypeResolver.class.getName();
    
    public static final String IMAGE_TYPE = "IMAGE_OBJECT";
    public static final String VIDEO_TYPE = "VIDEO_OBJECT";
    public static final String AUDIO_TYPE = "AUDIO_OBJECT";
    public static final String DOCUMENT_TYPE = "DOCUMENT_OBJECT";

    private static final ResolverConfig defaultResolverConfig = ResolverConfig.fromSettings(null);
    
    protected final ResolverConfig resolverConfig;
    protected final Delegator delegator;
    
    protected FileTypeResolver(Delegator delegator, ResolverConfig resolverConfig) {
        this.delegator = delegator;
        this.resolverConfig = resolverConfig;
    }
    
    public static ResolverConfig getDefaultResolverConfig(Delegator delegator) {
        return defaultResolverConfig;
    }
    
    public static FileTypeResolver getInstance(Delegator delegator, String providedType) {
        return getInstance(delegator, providedType, getDefaultResolverConfig(delegator));
    }
    
    public static FileTypeResolver getInstance(Delegator delegator, String providedType, ResolverConfig resolverConfig) {
        if (providedType.equals(IMAGE_TYPE)) {
            return ImageFileTypeResolver.getInstance(delegator, resolverConfig);
        } else if (providedType.equals(VIDEO_TYPE)) {
            return VideoFileTypeResolver.getInstance(delegator, resolverConfig);
        } else if (providedType.equals(AUDIO_TYPE)) {
            return AudioFileTypeResolver.getInstance(delegator, resolverConfig);
        } else if (providedType.equals(DOCUMENT_TYPE)) {
            return DocumentFileTypeResolver.getInstance(delegator, resolverConfig);
        } else {
            throw new IllegalArgumentException("Cannot get a FileTypeResolver instance for this providedType: " + providedType);
        }
    }

    public ResolverConfig getResolverConfig() {
        return new ResolverConfig(resolverConfig); // immutable
    }
    
    
    /**
     * Finds media type and returns a string in xxx/yyy format similar to mime-type (usually will be a valid mime-type).
     * If unsupported, returns null.
     * <p>
     * 2017-02-07: This can now use either library (Apache Tika) or manually-coded detections.
     * <p>
     * NOTE: some subclasses override this.
     */
    public GenericValue findMimeType(ByteBuffer byteBuffer, String fileName) throws GeneralException {
        GenericValue mimeType = null;
        if (resolverConfig.isManualMediaTypeDetectionPrioritized()) {
            if (resolverConfig.isManualMediaTypeDetection()) {
                mimeType = findMimeTypeManual(byteBuffer, fileName);
                if (mimeType != null) {
                    return mimeType;
                }
            }
            if (resolverConfig.isLibraryMediaTypeDetection()) {
                mimeType = findMimeTypeLib(byteBuffer, fileName);
            }
        } else {
            if (resolverConfig.isLibraryMediaTypeDetection()) {
                mimeType = findMimeTypeLib(byteBuffer, fileName);
                if (mimeType != null) {
                    return mimeType;
                }
            }
            if (resolverConfig.isManualMediaTypeDetection()) {
                mimeType = findMimeTypeManual(byteBuffer, fileName);
                if (mimeType == null) {
                    mimeType = findMimeTypeForExtension(FileTypeUtil.getFileExtension(fileName));
                }
            }
        }

        return mimeType;
    }

    public abstract String getProvidedType();
    
    /**
     * Determines if allowed media type. The media types are the ones defined by Apache Tika (tika-mimetypes.xml),
     * which are basically mime-times (but we call them media type here to generalize), in the format
     * "xxx/yyy".
     */
    public boolean isAllowedMediaType(String mediaType) {
        if (UtilValidate.isEmpty(mediaType)) {
            return false;
        }
        return AllowMode.ALL.equals(resolverConfig.getAllowMode()) || isManualSupportedMediaType(mediaType) 
                || (AllowMode.PERMISSIVE.equals(resolverConfig.getAllowMode()) && isAllowedMediaTypePermissive(mediaType));
    }
    
    public abstract boolean isAllowedMediaTypePermissive(String mediaType);
    
    /**
     * Checks if a media/mime-type starts with a prefix, such as "audio/" or "video/",
     * or if any of its aliases starts with such a prefix.
     * <p>
     * used in permissive mode to be more permissive.
     */
    protected static boolean mediaTypeOrAliasHasPrefix(String mediaType, String prefix) {
        if (mediaType == null) return false;
        else if (mediaType.startsWith(prefix)) return true;
        else {
            for(String alias : TikaUtil.getMediaTypeAliasMimeTypeIds(mediaType)) {
                if (alias.startsWith(prefix)) return true;
            }
            return false;
        }
    }
    
    
    /**
     * Performs any necessary coercions on the media/mime-type, such as replacing "audio/" prefix by "video/"
     * or vice-versa, in manual mode.
     * @deprecated shouldn't really need or want this
     */
    @Deprecated
    public abstract String adjustMediaTypeManual(String mediaType);
    
    protected abstract List<AbstractFileType> getFileTypes();
    
    protected abstract Set<String> getManualSupportedMediaTypes();
    
    protected Set<String> collectManualSupportedMediaTypes(Delegator delegator, List<AbstractFileType> fileTypes) {
        // 2017-02-08: since tika, only collect these in strict mode, but usually shouldn't use strict mode anymore
        if (!AllowMode.STRICT.equals(resolverConfig.getAllowMode())) {
            return new HashSet<>();
        }
        Set<String> mediaTypes = new HashSet<>();
        for(AbstractFileType fileType : fileTypes) {
            for(MagicNumber magicNumber : fileType.getMagicNumbers()) {
                for(String extension : magicNumber.getExtensions()) {
                    String mediaType = magicNumber.getMediaType(delegator, extension);
                    if (mediaType != null) {
                        mediaType = adjustMediaTypeManual(mediaType);
                    }
                    if (mediaType != null) {
                        mediaTypes.add(mediaType);
                    }
                }
            }
        }
        return mediaTypes;
    }
    
    protected boolean isManualSupportedMediaType(String mediaType) {
        return getManualSupportedMediaTypes().contains(mediaType);
    }
    
    protected GenericValue findMimeTypeManual(ByteBuffer byteBuffer, String fileName) throws GeneralException {
        MagicNumber magicNumber = findMagicNumberManual(byteBuffer, fileName);
        if (magicNumber != null) {
            String extension = FileTypeUtil.getFileExtension(fileName);
            if (UtilValidate.isNotEmpty(extension) && magicNumber.getExtensions().contains(extension)) {
                GenericValue mimeType = findMimeTypeForExtension(extension);
                // NOTE: here if mimeType is missing from system; means a configuration problem
                if (mimeType == null) {
                    Debug.logError("Cms: Manual File Type Resolution configuration error: could not find MimeType for file extension '" 
                        + extension + "' for MagicNumber '" + magicNumber.getDescription()
                        + "' please review FileExtension and MimeType entities", module);
                }
                return mimeType;
            }
        } 
        return null;
    }
    
    protected String findMediaTypeManual(ByteBuffer byteBuffer, String fileName) {
        MagicNumber magicNumber = findMagicNumberManual(byteBuffer, fileName);
        if (magicNumber != null) { // redundant: && isAllowedMediaType(magicNumber.getMediaType())
            String mediaType = magicNumber.getMediaType(delegator, FileTypeUtil.getFileExtension(fileName));
            if (mediaType != null) {
                return adjustMediaTypeManual(mediaType);
            }
        } 
        return null;
    }
    
    protected final MagicNumber findMagicNumberManual(ByteBuffer byteBuffer, String fileName) {
        for (AbstractFileType fileType : getFileTypes()) {
            for (MagicNumber magicNumber : fileType.getMagicNumbers()) {
                StringBuilder contentBuilder = new StringBuilder();

                byte[] magicNumberBytes = FileTypeUtil.hexStringToByteArray(magicNumber.getMagicNumber());
                int magicNumberSize = magicNumberBytes.length;

                byte[] strippedFileContent = new byte[magicNumberSize];
                if (magicNumber.getOffset() > 0) {
                    byte[] fileBytes = byteBuffer.array();
                    // Scipio: This is only for debugging
//                    StringBuilder fileBuilder = new StringBuilder();
//                    for (byte b : fileBytes) {
//                        fileBuilder.append(String.format("%02X", b));
//                    }
//                    Debug.log("fileBytes content (magic number with offset) ====> " + fileBuilder.toString());
                    strippedFileContent = Arrays.copyOfRange(fileBytes, magicNumber.getOffset(), magicNumber.getOffset() + magicNumberSize);
                } else {
                    byteBuffer.get(strippedFileContent);
                }
                byteBuffer.rewind();

                ByteBuffer fileContentBuffer = ByteBuffer.allocate(magicNumberSize);
                fileContentBuffer.put(strippedFileContent);

                for (byte b : fileContentBuffer.array()) {
                    contentBuilder.append(String.format("%02X", b));
                }
                fileContentBuffer.rewind();

                String extractedContent = contentBuilder.toString();
                if (checkValidMagicNumber(magicNumber, extractedContent, fileType)) {
                    Debug.log("strippedFileContent " + extractedContent + " EQUALS magicNumberBuffer " + magicNumber.getMagicNumber() + " IS VALID FOR TYPE "
                            + fileType.getClass().getSimpleName(), module);
                    return magicNumber;
                } else {
                    Debug.log("strippedFileContent " + extractedContent + " NOT EQUALS magicNumberBuffer " + magicNumber.getMagicNumber() + " IS NOT VALID FOR "
                            + fileType.getClass().getSimpleName(), module);
                }
            }
        }
        return null;
    }

    protected final boolean checkValidMagicNumber(MagicNumber magicNumber, String extractedContent, AbstractFileType fileType) {
        boolean parseMagicNumberMask = false;
        for (int i = 0; i < magicNumber.getMagicNumberMask().length; i++) {
            if (magicNumber.getMagicNumberMask()[i] < Byte.MAX_VALUE) {
                parseMagicNumberMask = true;
                break;
            }
        }

        boolean isValid = false;
        if (!parseMagicNumberMask) {
            if (magicNumber.getMagicNumber().equals(extractedContent)) {
                isValid = true;
            }
        } else {
            byte[] ba = FileTypeUtil.hexStringToByteArray(magicNumber.getMagicNumber());
            byte[] ba1 = FileTypeUtil.hexStringToByteArray(extractedContent);

            if (ba.length == ba1.length && (ba.length == magicNumber.getMagicNumberMask().length)) {
                for (int i = 0; i < magicNumber.getMagicNumberMask().length; i++) {
                    int b = magicNumber.getMagicNumberMask()[i] & 0xFF;
                    Debug.log("MN Mask[" + i + "]:       hex ===> " + Integer.toHexString(b) + " binary ===> " + Integer.toBinaryString(b), module);

                    int mn = ba[i] & 0xFF;
                    int ec = ba1[i] & 0xFF;

                    // Straight comparison of the whole byte value
                    if (b == 255 && ba[i] == ba1[i]) {
                        isValid = true;
                    } else {
                        // Apply mask bits
                        int r1 = mn & b;
                        int r2 = ec & b;
                        if (r1 == r2) {
                            isValid = true;
                        } else {
                            isValid = false;
                        }
                    }
                    if (!isValid)
                        break;
                }
            }
        }

        return isValid;
    }
    
    /**
     * Finds mime-type based on extension ONLY, WITH adjustments/coercions/restrictions applied.
     */
    protected GenericValue findMimeTypeForExtension(String extension) throws GeneralException {
        GenericValue mimeType = null;
        if (UtilValidate.isNotEmpty(extension)) {
            GenericValue fileTypeExtension = delegator.findOne("FileExtension", true, UtilMisc.toMap("fileExtensionId", extension));
            if (UtilValidate.isNotEmpty(fileTypeExtension)) {
                List<GenericValue> mimeTypeList = fileTypeExtension.getRelated("MimeType", null, null, true);
                if (mimeTypeList.size() > 0)
                    mimeType = mimeTypeList.get(0);
            }
            if (mimeType != null) {
                String mimeTypeId = mimeType.getString("mimeTypeId");
                String adjustedMimeTypeId = adjustMediaTypeManual(mimeTypeId);
                if (!mimeTypeId.equals(adjustedMimeTypeId)) {
                    mimeType = delegator.findOne("MimeType", true, UtilMisc.toMap("mimeTypeId", adjustedMimeTypeId));
                }
            }
            
            if (mimeType != null) {
                // Check if the final result is an allowed type...
                if (!isAllowedMediaType(mimeType.getString("mimeTypeId"))) {
                    throw new FileTypeException(PropertyMessage.make("CMSErrorUiLabels", "CmsInvalidFileTypeForMediaCat", 
                            UtilMisc.toMap("mediaType", mimeType.getString("mimeTypeId"), "providedType", getProvidedType())));
                }
            }
        }
        return mimeType;
    }
    
    /**
     * Finds MimeType (through Apache Tika library), as string representation, based on filename and magic numbers.
     */
    protected GenericValue findMimeTypeLib(ByteBuffer byteBuffer, String fileName) throws GeneralException {
        MediaType mediaType = TikaUtil.findMediaTypeSafe(byteBuffer, fileName);
        
        if (mediaType != null) {
            String mimeTypeId = TikaUtil.getMimeTypeId(mediaType);
            // SPECIAL: here we assume Tika identified the file correctly.
            // so if it maps to a non-allowed type, we must throw error, NOT return null.
            if (!isAllowedMediaType(mimeTypeId)) {
                throw new FileTypeException(PropertyMessage.make("CMSErrorUiLabels", "CmsInvalidFileTypeForMediaCat", 
                        UtilMisc.toMap("mediaType", mimeTypeId, "providedType", getProvidedType())));
            }
        }
        
        // check if we have an exact match in system
        // NOTE: is possible there is no exact match in the system (though should try to minimize this)
        return TikaUtil.findEntityMimeTypeForMediaType(delegator, mediaType, true);
    }
    
    /**
     * Finds media type (through Apache Tika library), as string representation,
     * based on filename and magic numbers.
     */
    protected String findMediaTypeLib(ByteBuffer byteBuffer, String fileName) {
        MediaType mediaType = TikaUtil.findMediaTypeSafe(byteBuffer, fileName);
        return (mediaType != null) ? TikaUtil.getMimeTypeId(mediaType) : null;
    }
    
    public static class ResolverConfig {
        
        /**
         * whether should only allow explicitly recognized types, or more permissive media file types that fall
         * within known categories. 
         * <p>
         * only makes a difference if <code>libraryMediaTypeDetection</code> is true.
         * <p>
         * 2017-02-07: TODO: REVIEW DEFAULT
         * 
         * @see #isAllowedMediaType(String)
         */
        private AllowMode allowMode;
        
        private boolean libraryMediaTypeDetection;
        private boolean manualMediaTypeDetection;
        private boolean manualMediaTypeDetectionPrioritized;
        
        public ResolverConfig() {
            this.allowMode = AllowMode.PERMISSIVE;
            this.libraryMediaTypeDetection = true;
            this.manualMediaTypeDetection = false;
            this.manualMediaTypeDetectionPrioritized = false;
        }
        
        public ResolverConfig(ResolverConfig other) {
            this.allowMode = other.allowMode;
            this.libraryMediaTypeDetection = other.libraryMediaTypeDetection;
            this.manualMediaTypeDetection = other.manualMediaTypeDetection;
            this.manualMediaTypeDetectionPrioritized = other.manualMediaTypeDetectionPrioritized;
        }
        
        public static ResolverConfig fromSettings(Delegator delegator) {
            ResolverConfig.Builder builder = new ResolverConfig.Builder();
            builder.setAllowMode(AllowMode.getFromNameSafe(UtilProperties.getPropertyValue("cms.properties", "media.detect.allowMode", "permissive")));
            builder.setLibraryMediaTypeDetection(UtilProperties.getPropertyAsBoolean("cms.properties", "media.detect.libraryMediaTypeDetection", true));
            builder.setManualMediaTypeDetection(UtilProperties.getPropertyAsBoolean("cms.properties", "media.detect.manualMediaTypeDetection", false));
            builder.setManualMediaTypeDetectionPrioritized(UtilProperties.getPropertyAsBoolean("cms.properties", "media.detect.manualMediaTypeDetectionPrioritized", false));
            return builder.getConfig();
        }
        
        public AllowMode getAllowMode() {
            return allowMode;
        }
        public boolean isManualMediaTypeDetection() {
            return manualMediaTypeDetection;
        }
        public boolean isLibraryMediaTypeDetection() {
            return libraryMediaTypeDetection;
        }
        public boolean isManualMediaTypeDetectionPrioritized() {
            return manualMediaTypeDetectionPrioritized;
        }
        
        public static class Builder {
            private ResolverConfig config;

            public Builder(ResolverConfig config) {
                this.config = new ResolverConfig(config);
            }
            
            public Builder() {
                this.config = new ResolverConfig();
            }

            public void setAllowMode(AllowMode allowMode) {
                config.allowMode = allowMode != null ? allowMode : AllowMode.PERMISSIVE;
            }
            
            public void setManualMediaTypeDetection(boolean manualMediaTypeDetection) {
                config.manualMediaTypeDetection = manualMediaTypeDetection;
            }
            
            public void setLibraryMediaTypeDetection(boolean libraryMediaTypeDetection) {
                config.libraryMediaTypeDetection = libraryMediaTypeDetection;
            }
            
            public void setManualMediaTypeDetectionPrioritized(boolean manualMediaTypeDetectionPrioritized) {
                config.manualMediaTypeDetectionPrioritized = manualMediaTypeDetectionPrioritized;
            }
            
            public ResolverConfig getConfig() {
                ResolverConfig res = config;
                this.config = null; // invalidate builder so can't modify further
                return res;
            }
        }
    }
    
    public enum AllowMode {
        STRICT("strict"),
        PERMISSIVE("permissive"),
        ALL("all");
        
        private static final Map<String, AllowMode> nameMap;
        static {
            Map<String, AllowMode> map = new HashMap<>();
            for(AllowMode mode : AllowMode.values()) {
                map.put(mode.getName(), mode);
            }
            nameMap = map;     
        }
        
        private final String name;

        private AllowMode(String name) {
            this.name = name;
        }
        
        public String getName() {
            return name;
        }
        
        public static AllowMode getFromNameSafe(String name) {
            return nameMap.get(name);
        }
    }
}