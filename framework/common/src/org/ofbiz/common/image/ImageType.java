package org.ofbiz.common.image;

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.IndexColorModel;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;

/**
 * SCIPIO: Structure used to specify image types based on different image formats and conditions.
 * NOTE: This ImageType top-level class behaves more like a factory, for ImageTypeInfo instances,
 * but it also abstracts a versatile image type specification (that returns different value depending
 * on its contents and the source image available in context).
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
@SuppressWarnings("serial")
public class ImageType implements Serializable {

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
         * Same as TYPE_PRESERVE_IF_LOSSLESS except it will also only preserve if the operation is free in cost,
         * or in other words does not require extra conversion.
         */
        public static final int TYPE_PRESERVE_IF_LOSSLESS_FREE = -12;
        /**
         * Attempts to preserve the input image type but ONLY if it causes no noticeable
         * color loss (or only very minor, e.g. as would a high-quality JPG).
         */
        public static final int TYPE_PRESERVE_IF_LOWLOSS = -13;
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

        
        public static boolean isValidTypeConstantName(String name) {
            return imagePixelTypeNameValueMap.containsKey(name);
        }
        
        public static boolean isValidTypeConstant(int value) {
            return imagePixelTypeValueNameMap.containsKey(value);
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
        
        public static boolean isTypeDirect(int imagePixelType) {
            return !isTypeIndexedOrCustom(imagePixelType);
        }
        
        /**
         * Returns true mainly for "direct" non-indexed types that Graphics2D.drawImage has
         * no issues with, with respect to color loss.
         * If the type frequently results in color loss, this will return null.
         */
        public static boolean isTypeImageOpFriendly(int imagePixelType) {
            return !isTypeIndexedOrCustom(imagePixelType);
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
     * Same as PRESERVE_IF_LOSSLESS except it will also only preserve if the operation is free in cost,
     * or in other words does not require extra conversion.
     * @see #PRESERVE_IF_LOSSLESS
     */
    public static final ImageType PRESERVE_IF_LOSSLESS_FREE = new ImageType(ImagePixelType.TYPE_PRESERVE_IF_LOSSLESS_FREE);
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
     * Global system default BufferedImage/ImageType type.
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
     * Global system default BufferedImage/ImageType direct/non-indexed RGB type.
     * This is usually the same as DEFAULT but this one guarantees no index/custom/weird type.
     * <p>
     * If you change DEFAULT to something weird, then you need to assign this one to something direct.
     * <p>
     * NOTE: Ofbiz originally used TYPE_INT_ARGB_PRE only for this, but other image libraries
     * appear to use something more like TYPE_INT_ARGB/TYPE_INT_RGB, so will use that for the time being.
     */
    public static final ImageType DEFAULT_DIRECT = getForObjectNonEmptySafe(UtilProperties.getPropertyValue(ImageUtil.IMAGECOMMON_PROP_RESOURCE, 
            ImageUtil.IMAGECOMMON_PROP_PREFIX+"type.default.direct"), 
            ImagePixelType.isTypeDirect(DEFAULT.getPixelTypeDefault()) ? DEFAULT : INT_ARGB_OR_RGB, 
            ImageUtil.IMAGECOMMON_PROP_RESOURCE + " " + ImageUtil.IMAGECOMMON_PROP_PREFIX+"type.default.direct");
    /**
     * Global system default BufferedImage/ImageType for image operations. 
     * This should usually be a single TYPE_INT_ARGB OR TYPE_4BYTE_ABGR value to preserve all color components.
     * NOTE: ofbiz used TYPE_PRESERVE_IF_NOT_CUSTOM for this but that may not even work at this time.
     */
    public static final ImageType DEFAULT_IMAGEOP = getForObjectNonEmptySafe(UtilProperties.getPropertyValue(ImageUtil.IMAGECOMMON_PROP_RESOURCE, 
            ImageUtil.IMAGECOMMON_PROP_PREFIX+"type.default.imageop"), 
            DEFAULT_DIRECT, 
            ImageUtil.IMAGECOMMON_PROP_RESOURCE + " " + ImageUtil.IMAGECOMMON_PROP_PREFIX+"type.default.imageop");
    /**
     * Default targetType specified by the central common scaling image helper methods, such as
     * {@link ImageTransform#scaleImage}; the default for this is PRESERVE_IF_LOWLOSS.
     */
    public static final ImageType COMMON_SCALEIMAGE = getForObjectNonEmptySafe(UtilProperties.getPropertyValue(ImageUtil.IMAGECOMMON_PROP_RESOURCE, 
            ImageUtil.IMAGECOMMON_PROP_PREFIX+"type.common.scaleimage"), 
            PRESERVE_IF_LOWLOSS, 
            ImageUtil.IMAGECOMMON_PROP_RESOURCE + " " + ImageUtil.IMAGECOMMON_PROP_PREFIX+"type.common.scaleimage");

    
    /*
     * *************************************************************
     * Instance fields
     * *************************************************************
     */
    
    // the Integer types here are stored outside map for faster read
    private final ImageTypeInfo defaultInfo;
    private final ImageTypeInfo noAlphaInfo;
    private final Map<String, Integer> pixelTypes;
    private final Map<Integer, Integer> typeMappings;
    
    
    /*
     * *************************************************************
     * Constructors and factories
     * *************************************************************
     */
    
    /**
     * Makes from default and noalpha.
     */
    public ImageType(ImageTypeInfo defaultInfo, ImageTypeInfo noAlphaInfo) {
        this.defaultInfo = defaultInfo;
        this.noAlphaInfo = noAlphaInfo;
        Map<String, Integer> pixelTypes = new HashMap<>();
        pixelTypes.put("default", defaultInfo.getPixelType());
        pixelTypes.put("noalpha", noAlphaInfo.getPixelType());
        this.pixelTypes = Collections.unmodifiableMap(pixelTypes);
        this.typeMappings = Collections.emptyMap();
    }
    
    /**
     * Makes from default and noalpha.
     */
    public ImageType(Integer pixelTypeDefault, Integer pixelTypeNoAlpha) {
        this.defaultInfo = ImageTypeInfo.from(pixelTypeDefault);
        this.noAlphaInfo = ImageTypeInfo.from(pixelTypeNoAlpha);
        Map<String, Integer> pixelTypes = new HashMap<>();
        pixelTypes.put("default", pixelTypeDefault);
        pixelTypes.put("noalpha", pixelTypeNoAlpha);
        this.pixelTypes = Collections.unmodifiableMap(pixelTypes);
        this.typeMappings = Collections.emptyMap();
    }
    
    /**
     * Makes from default only.
     */
    public ImageType(Integer pixelTypeDefault) {
        this.defaultInfo = ImageTypeInfo.from(pixelTypeDefault);
        this.noAlphaInfo = ImageTypeInfo.EMPTY;
        Map<String, Integer> pixelTypes = new HashMap<>();
        pixelTypes.put("default", pixelTypeDefault);
        this.pixelTypes = Collections.unmodifiableMap(pixelTypes);
        this.typeMappings = Collections.emptyMap();
    }
    
    /**
     * Makes completely empty instance.
     */
    public ImageType() { 
        this.defaultInfo = ImageTypeInfo.EMPTY;
        this.noAlphaInfo = ImageTypeInfo.EMPTY;
        this.pixelTypes = Collections.emptyMap();
        this.typeMappings = Collections.emptyMap();
    }
    
    /**
     * Makes from given map and typemap.
     */
    public ImageType(Map<String, Integer> pixelTypes, Map<Integer, Integer> typeMappings) {
        pixelTypes = new HashMap<>(pixelTypes);
        this.defaultInfo = ImageTypeInfo.from(pixelTypes.get("default"));
        this.noAlphaInfo = ImageTypeInfo.from(pixelTypes.get("noalpha"));
        
        if (typeMappings == null || typeMappings.isEmpty()) typeMappings = Collections.emptyMap();
        else typeMappings = Collections.unmodifiableMap(new HashMap<>(typeMappings));
        this.typeMappings = typeMappings;
        
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
            if (imageType == null || imageType.isEmpty()) {
                Debug.logInfo("No ImageType definition specified for " + resource
                        + "; using default: " + defaultValue.toString(), module);
                return defaultValue;
            } else if (!imageType.hasPixelTypeDefault()) {
                Debug.logWarning("Incomplete ImageType definition in " + resource
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
        Map<Integer, Integer> typeMapping = null;
        for(String pair : pairs) {
            String[] nameValue = parseMapEntryStrReprNormalized(pair, origStr);
            if ("typemap".equals(nameValue[0]) && nameValue[1] != null) {
                typeMapping = parseTypeMapStrReprNormalized(nameValue[1], origStr);
            } else {
                intMap.put(nameValue[0], nameValue[1] != null ? ImagePixelType.getTypeConstant(nameValue[1]) : null);
            }
        }
        return new ImageType(intMap, typeMapping);
    }
    
    protected static Map<Integer, Integer> parseTypeMapStrReprNormalized(String entries, String origStr) throws IllegalArgumentException {
        if (entries.startsWith("{") && entries.endsWith("}")) {
            String[] pairs = StringUtils.split(entries, ",");
            Map<Integer, Integer> map = new HashMap<>();
            for(String pair : pairs) {
                String[] nameValue = parseMapEntryStrReprNormalized(pair, origStr);
                if (nameValue[1] != null) {
                    try {
                        map.put(ImagePixelType.getTypeConstant(nameValue[0]), ImagePixelType.getTypeConstant(nameValue[1]));
                    } catch(Exception e) {
                        throw new IllegalArgumentException("Invalid ImageType map string representation (typemap sub-map entry is not a valid type mapping, "
                                + "e.g. should be {...,typemap={TYPE_CUSTOM=TYPE_INT_ARGB,...},} - ), but found: " + origStr + "; cause: " + e.getMessage());
                    }
                } else {
                    throw new IllegalArgumentException("Invalid ImageType map string representation (typemap sub-map entry is not a valid type mapping, "
                            + "e.g. should be {...,typemap={TYPE_CUSTOM=TYPE_INT_ARGB,...},} - ), but found: " + origStr);
                }
            }
            return map;
        } else {
            throw new IllegalArgumentException("Invalid ImageType map string representation (typemap sub-map entry is not a valid type mapping, "
                    + "e.g. should be {...,typemap={TYPE_CUSTOM=TYPE_INT_ARGB,...},}), but found: " + origStr);
        }
    }
    
    protected static Map<String, String> parseMapStrReprNormalized(String entries, String origStr) throws IllegalArgumentException {
        if (entries.startsWith("{") && entries.endsWith("}")) {
            String[] pairs = StringUtils.split(entries, ",");
            Map<String, String> map = new HashMap<>();
            for(String pair : pairs) {
                String[] nameValue = parseMapEntryStrReprNormalized(pair, origStr);
                map.put(nameValue[0], nameValue[1]);
            }
            return map;
        } else {
            throw new IllegalArgumentException("Invalid ImageType map string representation: " + origStr);
        }
    }
    
    protected static String[] parseMapEntryStrReprNormalized(String pair, String origStr) throws IllegalArgumentException {
        String[] nameValue = StringUtils.split(pair, "=", 2);
        if (nameValue.length != 2) throw new IllegalArgumentException("Invalid ImageType map string representation (missing '=' char): " + origStr);
        String name = nameValue[0];
        String value = nameValue[1];
        if (name.isEmpty()) throw new IllegalArgumentException("Invalid ImageType map string representation (name empty): " + origStr);
        if (value.isEmpty()) value = null;
        nameValue[0] = name;
        nameValue[1] = value;
        return nameValue;
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
    
    /**
     * Returns true if there is a non-null pixel type for this instance.
     * This can be used as a more useful "non-empty" check.
     */
    public boolean hasPixelTypeDefault() {
        return getPixelTypeDefault() != null;
    }
    
    public boolean hasOnlyPixelTypeDefault() {
        return getPixelTypeDefault() != null && pixelTypes.size() == 1;
    }
    
    /**
     * Returns default type info or ImageTypeInfo.EMPTY (never null).
     */
    public ImageTypeInfo getDefaultInfo() {
        return defaultInfo;
    }
    
    /**
     * Returns noalpha type info, with fallback on default info or ImageTypeInfo.EMPTY (never null).
     */
    public ImageTypeInfo getNoAlphaInfo() {
        return noAlphaInfo != null ? noAlphaInfo : getDefaultInfo();
    }
    
    /**
     * Returns noalpha type info or ImageTypeInfo.EMPTY (never null).
     */
    public ImageTypeInfo getNoAlphaInfoRaw() {
        return noAlphaInfo != null ? noAlphaInfo : ImageTypeInfo.EMPTY;
    }
    
    /**
     * The image type to use by default (no conditions). 
     */
    public Integer getPixelTypeDefault() {
        return getPixelTypeDefaultRaw();
    }
    
    public Integer getPixelTypeDefaultRaw() {
        return defaultInfo.getPixelType();
    }
    
    /**
     * Optional image type to use when the image has no alpha channel - optimization.
     */
    public Integer getPixelTypeNoAlpha() {
        return getPixelTypeNoAlphaRaw() != null ? getPixelTypeNoAlphaRaw() : getPixelTypeDefault();
    }
    
    public Integer getPixelTypeNoAlphaRaw() {
        return noAlphaInfo.getPixelType();
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
    public Map<String, Object> asMap() {
        Map<String, Object> map = new HashMap<>();
        map.putAll(pixelTypes);
        if (!typeMappings.isEmpty()) {
            map.put("typemap", typeMappings);
        }
        return Collections.unmodifiableMap(map);
    }
    
    @Override
    public String toString() {
        if (hasOnlyPixelTypeDefault()) return makePixelTypeStrRepr(getPixelTypeDefault());
        else return makePixelTypeMapStrRepr(asMap());
    }
    
    public static String makePixelTypeStrRepr(Integer pixelType) {
        return ImagePixelType.getTypeConstantName(pixelType, "{}");
    }
    
    /**
     * TODO: REVIEW: sketchy
     */
    private static String makePixelTypeMapStrRepr(Map<?, ?> map) {
        StringBuilder sb = new StringBuilder("{");
        if (map.containsKey("default")) { // show first
            sb.append("default=");
            sb.append(ImagePixelType.getTypeConstantName((Integer) map.get("default"), ""));
        }
        makePixelTypeMapStrRepr(map, sb, "default");
        sb.append("}");
        return sb.toString();
    }
    
    private static void makePixelTypeMapStrRepr(Map<?, ?> map, StringBuilder sb, String... skipKeys) {
        for(Map.Entry<?, ?> entry : map.entrySet()) {
            String name;
            if (entry.getKey() instanceof String) {
                name = (String) entry.getKey();
            } else if (entry.getKey() instanceof Integer) {
                name = ImagePixelType.getTypeConstantName((Integer) entry.getKey(), "");
            } else {
                name = entry.getKey().toString();
            }
            if (Arrays.asList(skipKeys).contains(name)) continue;
            if (sb.length() > 1) sb.append(",");
            sb.append(name);
            sb.append("=");
            Object value = entry.getValue();
            if (value == null) {
                sb.append("");
            } else if (value instanceof String) {
                sb.append((String)value);
            } else if (value instanceof Integer) {
                sb.append(ImagePixelType.getTypeConstantName((Integer) entry.getValue(), ""));
            } else if (value instanceof Map) {
                sb.append("{");
                makePixelTypeMapStrRepr(UtilGenerics.checkMap(value), sb);
                sb.append("}");
            } else {
                sb.append(value);
            }
        }
    }
    
    
    /*
     * *************************************************************
     * High-level queries
     * *************************************************************
     */
    
    /**
     * The image type info needed to properly describe an image type.
     * NOTE: pixel type is NOT sufficient to describe type, that's why this exists, and
     * because can't get BufferedImage/ImagePixelType type easily from ColorModel.
     */
    public static class ImageTypeInfo implements Serializable {
        public static final ImageTypeInfo EMPTY = new ImageTypeInfo();
        private static Map<Integer, ImageTypeInfo> pixelTypeInfoCache = Collections.emptyMap(); // allows unique instances
        
        protected final Integer pixelType;
        protected final ColorModel colorModel;
        
        protected ImageTypeInfo(Integer pixelType, ColorModel colorModel) {
            this.pixelType = pixelType;
            this.colorModel = colorModel;
        }
        
        protected ImageTypeInfo(Integer pixelType) {
            this.pixelType = pixelType;
            this.colorModel = null;
        }
        
        protected ImageTypeInfo(BufferedImage image) {
            this.pixelType = image.getType();
            this.colorModel = image.getColorModel();
        }
        
        protected ImageTypeInfo() { this(null, null); }
        
        public static ImageTypeInfo from(Integer pixelType, ColorModel colorModel) throws IllegalArgumentException {
            if (colorModel != null) return new ImageTypeInfo(pixelType, colorModel);
            else if (pixelType == null) return EMPTY;
            else {
                ImageTypeInfo info = pixelTypeInfoCache.get(pixelType);
                if (info == null) {
                    if (!ImagePixelType.isValidTypeConstant(pixelType)) {
                        throw new IllegalArgumentException("Invalid ImagePixelType/BufferedImage TYPE_XXX constant value: " + pixelType);
                    }
                    info = new ImageTypeInfo(pixelType);
                    // NOTE: full copy + unmodifiableMap SHOULD ensure thread safety
                    Map<Integer, ImageTypeInfo> cache = new HashMap<>(pixelTypeInfoCache);
                    cache.put(pixelType, info);
                    pixelTypeInfoCache = Collections.unmodifiableMap(cache);
                }
                return info;
            }
        }
        
        public static ImageTypeInfo from(Integer pixelType) throws IllegalArgumentException {
            return from(pixelType, null);
        }
        
        public static ImageTypeInfo from(BufferedImage image) throws IllegalArgumentException {
            return from(image.getType(), image.getColorModel());
        }
        
        /**
         * TODO: NOT IMPLEMENTED - IS IT POSSIBLE??
         */
        public static ImageTypeInfo from(Image image) throws IllegalArgumentException {
            throw new UnsupportedOperationException("Cannot make ImageTypeInfo from " 
                    + Image.class.getName() + " class; please use BufferedImage");
        }
        
        public Integer getPixelType() {
            return pixelType;
        }
        public ColorModel getColorModel() {
            return colorModel;
        }
        
        protected boolean imageMatchesType(BufferedImage imageToTest) {
            return ImageType.imageMatchesType(imageToTest, getPixelType(), getColorModel());
        }
        
        protected boolean imageMatchesRequestedType(BufferedImage imageToTest, ImageTypeInfo imageType) {
            return ImageType.imageMatchesRequestedType(imageToTest, getPixelType(), getColorModel());
        }
        
        protected boolean imageMatchesRequestedType(BufferedImage imageToTest, BufferedImage srcImage) {
            return ImageType.imageMatchesRequestedType(imageToTest, getPixelType(), getColorModel(), srcImage);
        }
        
        protected ImageTypeInfo resolveTargetType(BufferedImage srcImage) {
            if (ImagePixelType.isTypePreserve(getPixelType())) return ImageTypeInfo.from(srcImage);
            else if (ImagePixelType.isTypeNoPreserve(getPixelType())) 
                throw new IllegalArgumentException("TARGET_NOPRESERVE or equivalent non-type was passed,"
                        + " but no 'current' image type value available from this overload");
            return this;
        }
        
    }
    
    /**
     * Resolves the image type info to use for the given image (RECOMMENDED OVERLOAD).
     * May return null.
     */
    public ImageTypeInfo getImageTypeInfoFor(BufferedImage image) {
        return getImageTypeInfoFor(image.getType(), image.getColorModel());
    }
    
    /**
     * Resolves the image type info to use for the given color model.
     * May return null.
     */
    public ImageTypeInfo getImageTypeInfoFor(Integer targetPixelType, ColorModel colorModel) {
        Integer resultPixelType = null;
        ColorModel resultColorModel = null; // TODO: DEV NOTE: this must NOT be the colorModel parameter!!!
        
        if (targetPixelType != null) resultPixelType = typeMappings.get(targetPixelType);
        if (resultPixelType == null && (colorModel != null && !colorModel.hasAlpha())) resultPixelType = getPixelTypeNoAlpha();
        if (resultPixelType == null) resultPixelType = getPixelTypeDefault();
        
        return new ImageTypeInfo(resultPixelType, resultColorModel);
    }
    
    /**
     * Resolves the image type info to use for the given color model.
     * May return null.
     */
    public ImageTypeInfo getImageTypeInfoFor(Integer targetPixelType) {
        return getImageTypeInfoFor(targetPixelType, null);
    }
    
    
    /**
     * Resolves the image pixel type to use for the given image (RECOMMENDED OVERLOAD).
     * May return null.
     */
    public Integer getPixelTypeFor(BufferedImage image) {
        return getPixelTypeFor(image.getType(), image.getColorModel());
    }
    
    /**
     * Resolves the image pixel type to use for the given color model.
     * May return null.
     */
    public Integer getPixelTypeFor(Integer targetPixelType, ColorModel colorModel) {
        if (targetPixelType != null) {
            Integer resultPixelType = typeMappings.get(targetPixelType);
            if (resultPixelType != null) return resultPixelType;
        }
        if (colorModel != null && !colorModel.hasAlpha()) return getPixelTypeNoAlpha();
        return getPixelTypeDefault();
    }
    
    /**
     * Resolves the image pixel type to use for the given color model.
     * May return null.
     */
    public Integer getPixelTypeFor(Integer targetPixelType) {
        return getPixelTypeFor(targetPixelType, null);
    }
    
    /**
     * TODO: always returns null... (RECOMMENDED OVERLOAD)
     */
    public ColorModel getColorModelFor(BufferedImage image) {
        return null;
    }
    
    /**
     * TODO: always returns null...
     */
    public ColorModel getColorModelFor(Integer targetPixelType, ColorModel colorModel) {
        return null;
    }
    
    /**
     * TODO: always returns null...
     */
    public ColorModel getColorModelFor(Integer targetPixelType) {
        return getColorModelFor(targetPixelType, null);
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

    /**
     * If targetPixelType is a TYPE_PRESERVE_XX, returns srcImagePixelType;
     * if it is TYPE_NOPRESERVE, returns currentPixelType (usually the result type of an image op);
     * otherwise returns targetPixelType.
     */
    public static ImageTypeInfo resolveTargetType(ImageTypeInfo targetType, BufferedImage srcImage) {
        return targetType.resolveTargetType(srcImage);
    }
    
    /**
     * If targetPixelType is a TYPE_PRESERVE_XX, returns srcImagePixelType;
     * if it is TYPE_NOPRESERVE, returns currentPixelType (usually the result type of an image op);
     * otherwise returns targetPixelType.
     */
    public static ImageTypeInfo resolveTargetType(int targetPixelType, ColorModel targetColorModel, BufferedImage srcImage) {
        return ImageTypeInfo.from(targetPixelType, targetColorModel).resolveTargetType(srcImage);
    }
    
    /**
     * Checks if the image type exactly matches the targetPixelType and targetColorModel (optional).
     * WARN: FIXME?: does not handle colorModel... this is a simplification for now.
     */
    public static boolean imageMatchesType(BufferedImage imageToTest, ImageTypeInfo imageType) {
        return imageType.imageMatchesType(imageToTest);
    }
    
    /**
     * Checks if the image type exactly matches the targetPixelType and targetColorModel (optional).
     * WARN: FIXME?: does not handle colorModel... this is a simplification for now.
     */
    public static boolean imageMatchesType(BufferedImage imageToTest, int targetPixelType, ColorModel targetColorModel) {
        return imageToTest.getType() == targetPixelType; // TODO: check color model
    }
    
    /**
     * Checks if the image type exactly matches the targetPixelType and targetColorModel (optional).
     * WARN: FIXME?: does not handle colorModel... this is a simplification for now.
     */
    public static boolean imageMatchesType(BufferedImage imageToTest, int targetPixelType) {
        return imageMatchesType(imageToTest, targetPixelType, null); // TODO: check color model
    }
    
    /**
     * Checks if the image type exactly matches the targetTypeImage.
     * WARN: FIXME?: does not handle colorModel... this is a simplification for now.
     */
    public static boolean imageMatchesType(BufferedImage imageToTest, BufferedImage targetTypeImage) {
        return imageToTest.getType() == targetTypeImage.getType(); // TODO: check color model
    }
    
    /**
     * (HIGH-LEVEL for post-image op) Checks if the image matches the requested output type.
     * WARN: FIXME?: does not handle colorModel... this is a simplification for now.
     * NOTE: here targetColorModel is NOT the source color model; it is used as-is.
     */
    public static boolean imageMatchesRequestedType(BufferedImage imageToTest, ImageTypeInfo imageType) {
        return imageType.imageMatchesType(imageToTest);
    }
    
    /**
     * (HIGH-LEVEL for post-image op) Checks if the image matches the requested output type.
     * WARN: FIXME?: does not handle colorModel... this is a simplification for now. cannot handle indexed images with different palettes!
     * NOTE: here targetColorModel is NOT the source color model; it is used as-is.
     */
    public static boolean imageMatchesRequestedType(BufferedImage imageToTest, int targetPixelType, ColorModel targetColorModel) {
        return imageMatchesType(imageToTest, targetPixelType, targetColorModel);
    }
    
    /**
     * (HIGH-LEVEL for post-image op) Checks if the image matches the requested output type, which for this method is simply the srcImage's type.
     * WARN: FIXME?: does not handle colorModel... this is a simplification for now. cannot handle indexed images with different palettes!
     */
    public static boolean imageMatchesRequestedType(BufferedImage imageToTest, BufferedImage srcImage) {
        return imageMatchesType(imageToTest, srcImage);
    }
    
    /**
     * (HIGH-LEVEL for post-image op) Checks if the image matches the requested output type.
     * WARN: FIXME?: does not handle colorModel... this is a simplification for now. cannot handle indexed images with different palettes!
     * NOTE: this variant resolves targetPixelType using {@link #resolveTargetType(int, BufferedImage)}.
     */
    public static boolean imageMatchesRequestedType(BufferedImage imageToTest, ImageTypeInfo imageType, BufferedImage srcImage) {
        return imageType.imageMatchesRequestedType(imageToTest, srcImage);
    }
    
    /**
     * (HIGH-LEVEL for post-image op) Checks if the image matches the requested output type.
     * WARN: FIXME?: does not handle colorModel... this is a simplification for now. cannot handle indexed images with different palettes!
     * NOTE: this variant resolves targetPixelType using {@link #resolveTargetType(int, BufferedImage)}.
     */
    public static boolean imageMatchesRequestedType(BufferedImage imageToTest, Integer targetPixelType, ColorModel targetColorMode, BufferedImage srcImage) {
        // THIS IS THE ACTUAL IMPLEMENTATION
        
        if (ImagePixelType.isTypePreserve(targetPixelType)) targetPixelType = resolveTargetType(targetPixelType, targetColorMode, srcImage).getPixelType();

        return imageToTest.getType() == targetPixelType;
    }

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