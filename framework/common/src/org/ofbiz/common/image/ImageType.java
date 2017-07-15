package org.ofbiz.common.image;

import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.IndexColorModel;
import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;

/**
 * SCIPIO: Structure used to specify image types based on different image formats and conditions.
 * <p>
 * This is split into low-level image pixel type-based definitions in 
 * {@link ImageType.ImagePixelType} (<code>TYPE_XXX</code> constants)
 * and high-level complex ImageType definitions (this class itself and its constants).
 * <p>
 * String representation is one of these (properties example):
 * <ul>
 * <li><code>image.type.default={default=TYPE_INT_ARGB,noalpha=TYPE_INT_RGB}</code></li>
 * <li><code>image.type.default=TYPE_INT_ARGB</code></li>
 * </ul>
 * <p>
 * TODO: REVIEW: we only handle colorModel.hasAlpha - but what about colorModel.isAlphaPremultiplied()?
 * <p>
 * Added 2017-07-12.
 */
public class ImageType {

    /*
     * *************************************************************
     * Generic static fields
     * *************************************************************
     */
    
    public static final String module = ImageType.class.getName();
    private static final UtilCache<String, ImageType> strReprCache = 
            UtilCache.createUtilCache("common.image.imageType", 1000, 100000); // NOTE: we need limit because these could be passed from screens

    
    /*
     * *************************************************************
     * Low-level image pixel type definitions
     * *************************************************************
     */
    
    /**
     * Image pixel type constants, consisting of all the <code>TYPE_XXX</code> constants of
     * {@link java.awt.image.BufferedImage} plus additional ones supplementing them (informally).
     * <p>
     * NOTE: In libraries and ofbiz, "image pixel type" is usually referred to as
     * "image type", "imageType", "bufImgType", etc.
     * But all the types are based on pixel/int/char formats.
     * <p>
     * WARN: Do not access anything from BufferedImage through this class except the static TYPE_XXX constants;
     * extends here is easy hack to inherit all the constants - works out fine because no
     * statically-accessible members except the type constants (at current time).
     */
    public static abstract class ImagePixelType extends BufferedImage {
        private ImagePixelType(int width, int height, int imageType) { super(width, height, imageType); }

        /*
         * *************************************************************
         * Low-level image pixel type constants (BufferedImage TYPE_XXX)
         * *************************************************************
         */

        // DEV NOTE: we start at -10 for future/external interoperability
        
        /**
         * Forces preserving the input image type EVEN if it causes color loss
         * or is very slow.
         */
        public static final int TYPE_PRESERVE_ALWAYS = -10;
        
        /**
         * Attempts to preserve the input image type but ONLY if it causes no color loss.
         * WARN: 2017-07-14: current code may not always be able to guarantee this...
         */
        public static final int TYPE_PRESERVE_IF_LOSSLESS = -11;
        
        /**
         * Attempts to preserve the input image type but ONLY if it causes no noticeable
         * color loss (or only very minor, e.g. as would a high-quality JPG).
         */
        public static final int TYPE_PRESERVE_IF_LOWLOSS = -12;
        
        /**
         * This is used as a flag similar to passing null or an empty value - any extra
         * conversions are simply skipped. Only unavoidable conversions as part of the main imageop are performed.
         */
        public static final int TYPE_NOPRESERVE = -20;
        
        /*
         * *************************************************************
         * Context and reflection helpers
         * *************************************************************
         */
        
        // DEV NOTE: if adding more, make sure not already in use - if unsure, use
        // weird negative numbers
        
        public static Map<String, Integer> getTypeNameValueMap() {
            return ImageType.imagePixelTypeNameValueMap;
        }
        
        public static Map<Integer, String> getTypeValueNameMap() {
            return ImageType.imagePixelTypeValueNameMap;
        }

        /**
         * Gets {@link ImagePixelType} TYPE_XXX constant by name. 
         * Throws IllegalArgumentException if invalid name.
         */
        public static int getTypeConstant(String name) throws IllegalArgumentException {
            Integer value = ImageType.imagePixelTypeNameValueMap.get(name);
            if (value != null) return value;
            else throw new IllegalArgumentException(name + " is not a valid image pixel type TYPE_XXX constant name in " + ImagePixelType.class.getName());
        }
        
        /**
         * Gets {@link ImagePixelType} TYPE_XXX constant by name. 
         * Returns default if invalid name.
         */
        public static Integer getTypeConstant(String name, Integer defaultValue) {
            Integer value = ImageType.imagePixelTypeNameValueMap.get(name);
            return (value != null) ? value : defaultValue;
        }
        
        /**
         * Gets {@link ImagePixelType} TYPE_XXX constant name by value.
         * Throws IllegalArgumentException if invalid value.
         */
        public static String getTypeConstantName(int value) throws IllegalArgumentException {
            String name = ImageType.imagePixelTypeValueNameMap.get(value);
            if (name != null) return name;
            else throw new IllegalArgumentException(value + " is not a valid image pixel type TYPE_XXX constant value in " + ImagePixelType.class.getName());
        }
        
        /**
         * Gets {@link ImagePixelType} TYPE_XXX constant name by value.
         * Returns default if invalid value.
         */
        public static String getTypeConstantName(int value, String defaultValue) {
            String name = ImageType.imagePixelTypeValueNameMap.get(value);
            return (name != null) ? name : defaultValue;
        }
        
        /**
         * SCIPIO: Dynamically gets BufferedImage int constant by name.
         * Added 2017-07-12.
         */
        public static int getBufferedImageTypeConstant(String typeName) throws IllegalArgumentException {
            try {
                Field field = BufferedImage.class.getField(typeName);
                return field.getInt(null);
            } catch(Exception e) {
                throw new IllegalArgumentException("Buffered image type with name '" + typeName + "' does not exist in BufferedImage", e);
            }
        }
        
        /**
         * SCIPIO: Dynamically gets BufferedImage int constant by name.
         * Added 2017-07-12.
         */
        public static int getBufferedImageTypeConstant(String typeName, int defaultValue) {
            try {
                Field field = BufferedImage.class.getField(typeName);
                return field.getInt(null);
            } catch(Exception e) {
                Debug.logError(e, "Buffered image type with name '" + typeName + "' does not exist in BufferedImage", module);
                return defaultValue;
            }
        }
        
        
        /*
         * *************************************************************
         * Pixel type helpers
         * *************************************************************
         */
        
        public static boolean isTypeIndexed(int imagePixelType) {
            return imagePixelType == TYPE_BYTE_INDEXED || 
                    imagePixelType == TYPE_BYTE_BINARY;
        }
        
        public static boolean isTypeIndexed(BufferedImage image) {
            return isTypeIndexed(image.getType());
        }
        
        public static boolean isTypeCustom(int imagePixelType) {
            return imagePixelType == TYPE_CUSTOM;
        }
        
        public static boolean isTypeCustom(BufferedImage image) {
            return isTypeCustom(image.getType());
        }
        
        public static boolean isTypeIndexedOrCustom(int imagePixelType) {
            return imagePixelType == TYPE_BYTE_INDEXED || 
                    imagePixelType == TYPE_BYTE_BINARY || 
                    imagePixelType == TYPE_CUSTOM;
        }
        
        public static boolean isTypeIndexedOrCustom(BufferedImage image) {
            return isTypeIndexedOrCustom(image.getType());
        }
        
        public static boolean isTypePreserve(int imagePixelType) {
            return imagePixelType == TYPE_PRESERVE_IF_LOSSLESS || 
                    imagePixelType == TYPE_PRESERVE_ALWAYS ||
                    imagePixelType == TYPE_PRESERVE_IF_LOWLOSS;
        }
        
        public static boolean isTypeNoPreserve(int imagePixelType) {
            return imagePixelType == TYPE_NOPRESERVE;
        }
        
        public static boolean isTypeNoPreserveOrNull(Integer imagePixelType) {
            return imagePixelType == null || imagePixelType == TYPE_NOPRESERVE;
        }
        
        public static boolean isTypeSpecial(int imagePixelType) {
            return isTypePreserve(imagePixelType) || isTypeNoPreserve(imagePixelType);
        }
        
        /**
         * If targetPixelType is a TYPE_PRESERVE_XX, returns srcImagePixelType;
         * if it is TYPE_NOPRESERVE, returns currentPixelType (usually the result type of an image op);
         * otherwise returns targetPixelType.
         */
        public static int resolveTargetType(int targetPixelType, int srcImagePixelType, int currentPixelType) {
            if (isTypePreserve(targetPixelType)) return srcImagePixelType;
            else if (isTypeNoPreserve(targetPixelType)) return currentPixelType;
            else return targetPixelType;
        }

        public static int resolveTargetType(int targetPixelType, BufferedImage srcImage, int currentPixelType) {
            return resolveTargetType(targetPixelType, srcImage.getType(), currentPixelType);
        }
        
        /**
         * If targetPixelType is a TYPE_PRESERVE_XX, returns srcImagePixelType;
         * if it is TYPE_NOPRESERVE, throws IllegalArgumentException;
         * otherwise returns targetPixelType.
         */
        public static int resolveTargetType(int targetPixelType, int srcImagePixelType) {
            if (isTypePreserve(targetPixelType)) return srcImagePixelType;
            else if (isTypeNoPreserve(targetPixelType)) 
                throw new IllegalArgumentException("TARGET_NOPRESERVE or equivalent non-type was passed, but no current value available");
            else return targetPixelType;
        }

        public static int resolveTargetType(int targetPixelType, BufferedImage srcImage) {
            return resolveTargetType(targetPixelType, srcImage.getType());
        }
    }

    /**
     * Maps all {@link java.awt.image.BufferedImage} and {@link ImagePixelType} <code>TYPE_XXX</code>
     * constant definitions to their int values; for parsing.
     */
    private static final Map<String, Integer> imagePixelTypeNameValueMap = Collections.unmodifiableMap(
            populateClassIntFieldNameValueMapSafe(ImagePixelType.class, "TYPE_", new HashMap<String, Integer>()));
    /**
     * Maps all {@link java.awt.image.BufferedImage} and {@link ImagePixelType} <code>TYPE_XXX</code>
     * constant int values back to their names; for parsing.
     */
    private static final Map<Integer, String> imagePixelTypeValueNameMap = Collections.unmodifiableMap(
            UtilMisc.putAllReverseMapping(new HashMap<Integer, String>(), imagePixelTypeNameValueMap));

    
    /*
     * *************************************************************
     * High-level ImageType image type constants (ImageType)
     * *************************************************************
     */
    
    /**
     * Empty instance. 
     * NOTE: NOT strictly the only empty instance - use {@link #isEmpty()} for check.
     */
    public static final ImageType EMPTY = new ImageType();
    /**
     * Special preserve instance that instructs to always preserve the original image type,
     * EVEN if it results in color or data loss.
     * NOT strictly the only empty instance - use {@link #isEmpty()} for check.
     */
    public static final ImageType PRESERVE_ALWAYS = new ImageType(ImagePixelType.TYPE_PRESERVE_ALWAYS);
    /**
     * Special preserve instance that instructs to try to preserve the original image type,
     * UNLESS the preservation produces color or data loss.
     * NOT strictly the only empty instance - use {@link #isEmpty()} for check.
     * WARN: 2017-07-14: current code may not always be able to guarantee this...
     */
    public static final ImageType PRESERVE_IF_LOSSLESS = new ImageType(ImagePixelType.TYPE_PRESERVE_IF_LOSSLESS);
    /**
     * Special preserve instance that instructs to try to preserve the original image type,
     * UNLESS the preservation produces too much color or data loss.
     * NOT strictly the only empty instance - use {@link #isEmpty()} for check.
     */
    public static final ImageType PRESERVE_IF_LOWLOSS = new ImageType(ImagePixelType.TYPE_PRESERVE_IF_LOWLOSS);
    /**
     * Special no-preserve instance that prevents any extra conversions - similar to passing null or EMPTY.
     */
    public static final ImageType NOPRESERVE = new ImageType(ImagePixelType.TYPE_NOPRESERVE);
    /**
     * Generic TYPE_INT_ARGB_PRE for full RGB with fake alpha.
     * NOTE: This is the stock ofbiz setting, and was previously hardcoded throughout all java files.
     */
    public static final ImageType INT_ARGB_PRE = new ImageType(ImagePixelType.TYPE_INT_ARGB_PRE);
    /**
     * Generic TYPE_INT_ARGB preferred in thumbnailator and several libraries.
     */
    public static final ImageType INT_ARGB = new ImageType(ImagePixelType.TYPE_INT_ARGB);
    /**
     * Uses INT_ARGB for alpha-supporting images and INT_RGB for images without alpha.
     * This emulates the mortennobel lib behavior, but using different pixel arrangement.
     */
    public static final ImageType INT_ARGB_OR_RGB = new ImageType(ImagePixelType.TYPE_INT_ARGB, ImagePixelType.TYPE_INT_RGB);
    /**
     * Uses 4BYTE_ABGR for alpha-supporting images and 3BYTE_BGR for images without alpha.
     * This emulates the mortennobel lib behavior and is the precise types it uses.
     */
    public static final ImageType CHAR_ARGB_OR_RGB = new ImageType(ImagePixelType.TYPE_4BYTE_ABGR, ImagePixelType.TYPE_3BYTE_BGR);
    /**
     * SCIPIO: Global system default BufferedImage/ImageType type.
     * For use in operations where the input type is unspecified or unusable.
     * <p>
     * NOTE: Ofbiz originally used TYPE_INT_ARGB_PRE only for this, but other image libraries
     * appear to use something more like TYPE_INT_ARGB/TYPE_INT_RGB, so will use that for the time being.
     */
    public static final ImageType DEFAULT = getForObjectNonEmptySafe(UtilProperties.getPropertyValue(ImageUtil.IMAGECOMMON_PROP_RESOURCE, 
            ImageUtil.IMAGECOMMON_PROP_PREFIX+"type.default"), 
            INT_ARGB_OR_RGB, 
            ImageUtil.IMAGECOMMON_PROP_RESOURCE + " " + ImageUtil.IMAGECOMMON_PROP_PREFIX+"type.default");
    /**
     * SCIPIO: Global system default BufferedImage/ImageType direct/non-indexed RGB type.
     * This is usually the same as DEFAULT but this one guarantees no index/custom/weird type.
     * <p>
     * If you change DEFAULT to something weird, then you need to assign this one to something direct.
     * <p>
     * NOTE: Ofbiz originally used TYPE_INT_ARGB_PRE only for this, but other image libraries
     * appear to use something more like TYPE_INT_ARGB/TYPE_INT_RGB, so will use that for the time being.
     */
    public static final ImageType DEFAULT_DIRECT = getForObjectNonEmptySafe(UtilProperties.getPropertyValue(ImageUtil.IMAGECOMMON_PROP_RESOURCE, 
            ImageUtil.IMAGECOMMON_PROP_PREFIX+"type.default.direct"), 
            DEFAULT, 
            ImageUtil.IMAGECOMMON_PROP_RESOURCE + " " + ImageUtil.IMAGECOMMON_PROP_PREFIX+"type.default.direct");

    
    /*
     * *************************************************************
     * Instance fields
     * *************************************************************
     */
    
    // the Integer types here are stored outside map for faster read
    private final Integer pixelTypeDefault;
    private final Integer pixelTypeNoAlpha;
    private final Map<String, Integer> pixelTypes;
    
    
    /*
     * *************************************************************
     * Constructors and factories
     * *************************************************************
     */
    
    /**
     * Makes from default and noalpha.
     */
    public ImageType(Integer pixelTypeDefault, Integer pixelTypeNoAlpha) {
        this.pixelTypeDefault = pixelTypeDefault;
        this.pixelTypeNoAlpha = pixelTypeNoAlpha;
        Map<String, Integer> pixelTypes = new HashMap<>();
        pixelTypes.put("default", pixelTypeDefault);
        pixelTypes.put("noalpha", pixelTypeNoAlpha);
        this.pixelTypes = Collections.unmodifiableMap(pixelTypes);
    }
    
    /**
     * Makes from default only.
     */
    public ImageType(Integer pixelTypeDefault) {
        this.pixelTypeDefault = pixelTypeDefault;
        this.pixelTypeNoAlpha = null;
        Map<String, Integer> pixelTypes = new HashMap<>();
        pixelTypes.put("default", pixelTypeDefault);
        this.pixelTypes = Collections.unmodifiableMap(pixelTypes);
    }
    
    /**
     * Makes completely empty instance.
     */
    public ImageType() { 
        this.pixelTypeDefault = null;
        this.pixelTypeNoAlpha = null;
        this.pixelTypes = Collections.emptyMap();
    }
    
    /**
     * Makes from given map.
     */
    public ImageType(Map<String, Integer> pixelTypes) {
        this.pixelTypeDefault = pixelTypes.get("default");
        this.pixelTypeNoAlpha = pixelTypes.get("noalpha");
        this.pixelTypes = Collections.unmodifiableMap(new HashMap<>(pixelTypes));
    }
    
    /**
     * Gets for object which can be ImageType, Integer or String repr. Uses cache.
     * <p>
     * Never returns null (caller should check for null if it's important).
     */
    public static ImageType getForObject(Object obj) throws IllegalArgumentException {
        if (obj == null) return EMPTY;
        else if (obj instanceof ImageType) return (ImageType) obj;
        else if (obj instanceof Integer) return new ImageType((Integer) obj);
        else if (obj instanceof String) {
            return getForStrRepr((String) obj);
        } else {
            throw new IllegalArgumentException("Unrecognized type used to represent ImageType: " + obj.getClass().getName());
        }
    }
    
    public static ImageType getForObjectNonEmptySafe(Object obj, ImageType defaultValue, String resource) {
        try {
            ImageType imageType = getForObject(obj);
            if (imageType == null || !imageType.hasPixelTypeDefault()) {
                Debug.logWarning("Missing or incomplete ImageType definition in " + resource
                        + "; using default: " + defaultValue.toString(), module);
                return defaultValue;
            }
            return imageType;
        } catch(Exception e) {
            Debug.logError("Error in ImageType definition in " + resource + ": " + e.getMessage(), module);
            return defaultValue;
        }
    }

    protected static ImageType createFromObject(Object obj) throws IllegalArgumentException {
        if (obj == null) return EMPTY;
        else if (obj instanceof ImageType) return (ImageType) obj;
        else if (obj instanceof Integer) return new ImageType((Integer) obj);
        else if (obj instanceof String) {
            return createFromStrRepr((String) obj);
        } else {
            throw new IllegalArgumentException("Unrecognized type used to represent ImageType: " + obj.getClass().getName());
        }
    }
    
    /**
     * Constructs from string representation. Uses cache.
     * <p>
     * It can be either the name of a constant or a map-like representation following:
     * <code>{type=INT_XXX,typeNoAlpha=INT_XXX}</code>
     * <p>
     * If is a single constant name then it sets only the imageType field.
     * <p>
     * Trims everything.
     * <p>
     * Never returns null (caller should check for null if it's important).
     */
    public static ImageType getForStrRepr(String str) throws IllegalArgumentException {
        if (UtilValidate.isEmpty(str)) return EMPTY;
        ImageType imageType = strReprCache.get(str);
        if (imageType == null) {
            imageType = createFromStrRepr(str);
            strReprCache.put(str, imageType);
        }
        return imageType;
    }
    
    protected static ImageType createFromStrRepr(String str) throws IllegalArgumentException {
        if (UtilValidate.isEmpty(str)) return EMPTY;
        str = removeWhitespace(str);
        if (str.isEmpty()) return EMPTY;
        return createFromStrReprNormalized(str);
    }
    
    /**
     * Assumes no spaces and non-empty.
     */
    protected static ImageType createFromStrReprNormalized(String str) throws IllegalArgumentException {
        if (str.startsWith("{") && str.endsWith("}")) {
            String entries = str.substring(1, str.length() - 1);
            if (entries.isEmpty()) return EMPTY;
            return createFromMapInnerStrReprNormalized(entries, str);
        } else {
            return new ImageType(ImagePixelType.getTypeConstant(str));
        }
    }
    
    protected static ImageType createFromMapInnerStrReprNormalized(String entries, String origStr) throws IllegalArgumentException {
        String[] pairs = StringUtils.split(entries, ",");
        Map<String, Integer> intMap = new HashMap<>();
        for(String pair : pairs) {
            String[] nameValue = StringUtils.split(pair, "=", 2);
            if (nameValue.length != 2) throw new IllegalArgumentException("Invalid ImageType map string representation (missing '=' char): " + origStr);
            String name = nameValue[0];
            String value = nameValue[1];
            if (name.isEmpty()) throw new IllegalArgumentException("Invalid ImageType map string representation (name empty): " + origStr);
            if (value.isEmpty()) value = null;
            intMap.put(name, value != null ? ImagePixelType.getTypeConstant(value) : null);
        }
        return new ImageType(intMap);
    }
    
    
    /*
     * *************************************************************
     * Basic accessors
     * *************************************************************
     */
    
    /**
     * Returns true if completely empty instance (doesn't check nulls much).
     */
    public boolean isEmpty() {
        return pixelTypes.isEmpty();
    }
    
    public boolean hasPixelTypeDefault() {
        return getPixelTypeDefault() != null;
    }
    
    public boolean hasOnlyPixelTypeDefault() {
        return getPixelTypeDefault() != null && pixelTypes.size() == 1;
    }
    
    /**
     * The image type to use by default (no special conditions). 
     */
    public Integer getPixelTypeDefault() {
        return getPixelTypeDefaultRaw();
    }
    
    public Integer getPixelTypeDefaultRaw() {
        return pixelTypeDefault;
    }
    
    /**
     * Optional image type to use when the image has no alpha channel - optimization.
     */
    public Integer getPixelTypeNoAlpha() {
        return getPixelTypeNoAlphaRaw() != null ? getPixelTypeNoAlphaRaw() : getPixelTypeDefault();
    }
    
    public Integer getPixelTypeNoAlphaRaw() {
        return pixelTypeNoAlpha;
    }

    /**
     * Get pixel type by name: default, noalpha, ...
     */
    protected Integer getPixelType(String typeName) {
        Integer result = getPixelTypeRaw(typeName);
        return result != null ? result : getPixelTypeDefault();
    }
    
    protected Integer getPixelTypeRaw(String typeName) {
        return pixelTypes.get(typeName);
    }
    
    /**
     * As read-only map.
     */
    public Map<String, Integer> asMap() {
        return pixelTypes;
    }
    
    @Override
    public String toString() {
        if (hasOnlyPixelTypeDefault()) return makePixelTypeStrRepr(getPixelTypeDefault());
        else return makePixelTypeMapStrRepr(asMap());
    }
    
    public static String makePixelTypeStrRepr(Integer pixelType) {
        return ImagePixelType.getTypeConstantName(pixelType, "{}");
    }
    
    public static String makePixelTypeMapStrRepr(Map<String, Integer> pixelTypes) {
        StringBuilder sb = new StringBuilder("{");
        if (pixelTypes.containsKey("default")) { // show first
            sb.append("default=");
            sb.append(ImagePixelType.getTypeConstantName(pixelTypes.get("default"), ""));
        }
        for(Map.Entry<String, Integer> entry : pixelTypes.entrySet()) {
            String name = entry.getKey();
            if ("default".equals(name)) continue;
            if (sb.length() > 1) sb.append(",");
            sb.append(name);
            sb.append("=");
            sb.append(ImagePixelType.getTypeConstantName(entry.getValue(), ""));
        }
        sb.append("}");
        return sb.toString();
    }
    
    
    /*
     * *************************************************************
     * High-level queries
     * *************************************************************
     */
    
    /**
     * Resolves the image pixel type to use for the given image.
     * May return null.
     */
    public Integer getPixelTypeFor(BufferedImage image) {
        if (!image.getColorModel().hasAlpha()) return getPixelTypeNoAlpha();
        return getPixelTypeDefault();
    }
    
    /**
     * Resolves the image pixel type to use for the given color model.
     * May return null.
     */
    public Integer getPixelTypeFor(ColorModel colorModel) {
        if (!colorModel.hasAlpha()) return getPixelTypeNoAlpha();
        return getPixelTypeDefault();
    }
    
    
    /*
     * *************************************************************
     * Generic helpers
     * *************************************************************
     */

    protected static String removeWhitespace(String str) {
        return StringUtils.replace(str, " ", "");
    }
    
    // TODO: move to reflection class
    protected static Map<String, Integer> populateClassIntFieldNameValueMapSafe(Class<?> cls, String fieldNamePrefix, Map<String, Integer> nameValueMap) {
        try {
            for(Field field : ImagePixelType.class.getFields()) {
                if (field.getName().startsWith(fieldNamePrefix)) {
                    nameValueMap.put(field.getName(), field.getInt(null));
                }
            }
        } catch(Exception e) {
            Debug.logError(e, "Error inspecting fields of class " + cls.getName() + ": " + e.getMessage(), module);
        }
        return nameValueMap;
    }
    
    
    /*
     * *************************************************************
     * Image data/type helpers
     * *************************************************************
     */

    public static String printImageTypeInfo(BufferedImage image) {
        StringBuilder sb = new StringBuilder("[image pixel type: ");
        sb.append(ImagePixelType.getTypeConstantName(image.getType(), "(INVALID)"));
        ColorModel cm = image.getColorModel();
        sb.append("; bits per pixel: ");
        sb.append(cm.getPixelSize());
        
        if (image.getColorModel() instanceof IndexColorModel) {
            IndexColorModel icm = (IndexColorModel) cm;
            sb.append("; color array size: ");
            sb.append(icm.getMapSize());
        }
        
        sb.append("]");
        return sb.toString();
        
    }
}