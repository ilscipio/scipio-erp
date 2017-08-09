package org.ofbiz.common.image;

import java.io.IOException;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.ofbiz.base.image.ImageVariantSelector;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.service.ServiceUtil;

/**
 * SCIPIO: Dedicated helper class to hold info from ImageProperties.xml files.
 * This is to avoid the constant re-parsing the ofbiz code does, so this may be cached and consulted in real-time.
 * TODO?: there may be need for a db-stored version due to the problems involved.
 * DEV NOTE: the ugly ImageVariantSelector pattern is due to ofbiz dependency
 * problems. Originally there was no real need for it.
 * Added 2017-08-04.
 */
@SuppressWarnings("serial")
public class ImageVariantConfig implements Serializable, ImageVariantSelector {

    public static final String module = ImageVariantConfig.class.getName();
    
    public static final ImageVariantConfig FALLBACK = new ImageVariantConfig("dummy", null, null, 
            UtilMisc.toList(new VariantInfo("thumbnail", 100, 100)));
    
    private static final Factory FACTORY = new Factory();
    private static final Cache CACHE = new Cache();
    private static final PathVariantConfig PATHVARIANTCFG = PathVariantConfig.fromPropertiesSafe("imagecommon", 
            "image.variant.selector.bypath.", CACHE); // always use cache for this... for now
    
    public static final String COMMON_IMAGEPROP_FILEPATH = "/framework/common/config/ImageProperties.xml";
    
    protected final String name;
    protected final String sourceType;
    protected final String location;
    protected final Map<String, VariantInfo> variantMap;
    protected final List<VariantInfo> variantList;
    protected transient Map<String, Map<String, String>> variantStringMap; // the original, for compat
    //protected transient VariantInfo biggestVariant;
    //protected transient VariantInfo smallestVariant;
    protected transient Map<String, VariantInfo> exactDimVariantMap; // maps "widthxheight" string to VariantInfo
    
    public ImageVariantConfig(String name, String sourceType, String location, Collection<VariantInfo> variants) {
        this(name, sourceType, location, variants, null); 
    }
    
    protected ImageVariantConfig(String name, String sourceType, String location, Collection<VariantInfo> variants, Map<String, Map<String, String>> variantStringMap) {
        this.name = name;
        this.sourceType = sourceType;
        this.location = location;
        // this ensures no duplicates
        Map<String, VariantInfo> variantMap = new LinkedHashMap<>();
        if (variants != null) {
            for(VariantInfo variant : variants) {
                if (variantMap.containsKey(variant.getName())) {
                    Debug.logWarning("Image variant config " + this.toString() + " has duplicate variant: " + variant.getName(), module);
                }
                variantMap.put(variant.getName(), variant);
            }
        }
        this.variantList = Collections.unmodifiableList(new ArrayList<>(variantMap.values()));
        this.variantMap = Collections.unmodifiableMap(variantMap);
        this.variantStringMap = (variantStringMap != null) ? Collections.unmodifiableMap(variantStringMap) : null;
        this.exactDimVariantMap = null;
    }
    
    public static class Factory implements ImageVariantSelector.Factory {
        @Override
        public ImageVariantConfig fromImagePropertiesMap(String name, String sourceType, String location, Map<String, Map<String, String>> imgPropsMap) {
            Collection<VariantInfo> dimMap = parseImagePropertiesMap(imgPropsMap);
            return new ImageVariantConfig(name, sourceType, location, dimMap, imgPropsMap);
        }
        
        @Override
        public ImageVariantConfig fromImagePropertiesXml(String imgPropsPath, Locale locale) throws IOException, IllegalArgumentException {
            String fullPath = getImagePropertiesFullPath(imgPropsPath);
            Map<String, Object> result = ImageTransform.getXMLValue(fullPath, locale);
            if (!ServiceUtil.isSuccess(result)) throw new IOException(ServiceUtil.getErrorMessage(result));
            Map<String, Map<String, String>> imgPropsMap = UtilGenerics.checkMap(result.get("xml"));
            // FIXME?: currently setting name and location to same, because the props don't h
            return fromImagePropertiesMap(imgPropsPath, "file", fullPath, imgPropsMap); 
        }
        
        @Override
        public ImageVariantConfig fromImagePropertiesXml(String imgPropsPath) throws IOException {
            return fromImagePropertiesXml(imgPropsPath, Locale.ENGLISH);
        }
        
        @Override
        public ImageVariantConfig fromResourceUrlPath(String path) throws IOException {
            return PATHVARIANTCFG.getVariantCfgForPath(path);
        }
        
        public static class FactorySource implements ImageVariantSelector.FactorySource {
            @Override
            public Factory getFactory() {
                return FACTORY;
            }
        }
    }
    
    public static class Cache extends Factory {

        private static final UtilCache<String, ImageVariantConfig> imgPropsPathCache = UtilCache.createUtilCache("common.image.variant.imgpropxml");
       
        
        @Override
        public ImageVariantConfig fromImagePropertiesMap(String name, String sourceType, String location,
                Map<String, Map<String, String>> imgPropsMap) {
            return FACTORY.fromImagePropertiesMap(name, sourceType, location, imgPropsMap);
        }

        @Override
        public ImageVariantConfig fromImagePropertiesXml(String imgPropsPath, Locale locale)
                throws IOException, IllegalArgumentException {
            ImageVariantConfig res = imgPropsPathCache.get(imgPropsPath);
            if (res == null) { // no need to sync
                res = FACTORY.fromImagePropertiesXml(imgPropsPath, locale);
                imgPropsPathCache.put(imgPropsPath, res);
            }
            return res;
        }

        @Override
        public ImageVariantConfig fromImagePropertiesXml(String imgPropsPath) throws IOException {
            return fromImagePropertiesXml(imgPropsPath, Locale.ENGLISH);
        }

        @Override
        public ImageVariantConfig fromResourceUrlPath(String path) throws IOException {
            return FACTORY.fromResourceUrlPath(path);
        }
        
        public static class FactorySource implements ImageVariantSelector.FactorySource {
            @Override
            public Cache getFactory() {
                return CACHE;
            }
        }
    }

    public static Factory getFactory() {
        return FACTORY;
    }
    
    public static Cache getCache() {
        return CACHE;
    }
    
    public static ImageVariantConfig fromImagePropertiesMap(String name, String sourceType, String location, Map<String, Map<String, String>> imgPropsMap) {
        return getCache().fromImagePropertiesMap(name, sourceType, location, imgPropsMap);
    }

    public static ImageVariantConfig fromImagePropertiesXml(String imgPropsPath, Locale locale) throws IOException, IllegalArgumentException {
        return getCache().fromImagePropertiesXml(imgPropsPath, locale);
    }
    
    public static ImageVariantConfig fromImagePropertiesXml(String imgPropsPath) throws IOException {
        return getCache().fromImagePropertiesXml(imgPropsPath);
    }
    
    private static List<VariantInfo> parseImagePropertiesMap(Map<String, Map<String, String>> map) throws NumberFormatException {
        List<VariantInfo> res = new ArrayList<>();
        for(Map.Entry<String, Map<String, String>> entry : map.entrySet()) {
            res.add(new VariantInfo(entry.getKey(), entry.getValue()));
        }
        return res;
    }
    
    /**
     * NOTE: currently this may be just a path, but could be something else.
     */
    public String getName() {
        return name;
    }

    /**
     * Supports: "file"
     */
    public String getSourceType() {
        return sourceType;
    }

    /**
     * A location to be able to find the original.
     */
    public String getLocation() {
        return location;
    }

    public VariantInfo getVariant(String name) {
        return variantMap.get(name);
    }
    
    public boolean hasVariant(String name) {
        return variantMap.get(name) != null;
    }
    
    public Set<String> getVariantNames() {
        return variantMap.keySet();
    }
    
    public Map<String, VariantInfo> getVariantMap() {
        return variantMap;
    }

    public List<VariantInfo> getVariantList() {
        return variantList;
    }
    
    /**
     * Returns the original string map for compatibility with old ofbiz methods.
     */
    public Map<String, Map<String, String>> getVariantStringMap() {
        Map<String, Map<String, String>> variantStringMap = this.variantStringMap;
        if (variantStringMap == null) { // FAST MUTABLE
            // rebuild this, needed for compat
            variantStringMap = new LinkedHashMap<>();
            for(VariantInfo variant : getVariantList()) {
                variantStringMap.put(variant.getName(), variant.propsToStringMap());
            }
            variantStringMap = Collections.unmodifiableMap(variantStringMap);
            this.variantStringMap = variantStringMap;
        }
        return variantStringMap;
    }

    public Map<String, VariantInfo> getExactDimVariantMap() {
        Map<String, VariantInfo> exactDimVariantMap = this.exactDimVariantMap;
        if (exactDimVariantMap == null) { // FAST MUTABLE
            exactDimVariantMap = new HashMap<>();
            for(VariantInfo variant : getVariantList()) {
                exactDimVariantMap.put(variant.getDimString(), variant);
            }
            exactDimVariantMap = Collections.unmodifiableMap(exactDimVariantMap);
            this.exactDimVariantMap = exactDimVariantMap;
        }
        return exactDimVariantMap;
    }

    /**
     * Fitting mode - influences best-image selection - meant
     * to be used in conjunction with css background-size property ("contain", "cover") or equivalent.
     * @see ImageVariantConfig#getCanvasBestVariant
     */
    public enum FitMode {
        /**
         * The default mode, selects the smallest variant that is slightly bigger
         * than the canvas dimensions (if possible).
         * This makes sense to use with css "contain".
         */
        MINIMUM("min"),
        /**
         * Selects the largest variant that is smaller than both canvas dimensions.
         */
        MAXIMUM("max"),;
        
        public static final FitMode DEFAULT = MINIMUM;
        
        private final String strName;
        
        private FitMode(String strName) {
            this.strName = strName;
        }

        public String getStrName() {
            return strName;
        }
        
        public static FitMode fromStrNameParamSafe(String strName) {
            if ("min".equals(strName) || "true".equals(strName)) return MINIMUM;
            else if ("max".equals(strName)) return MAXIMUM;
            else return null;
        }
    }
    
    /**
     * Tries to return the best variant for the canvas size, or null if there is no preferable option (usually
     * caller should use original in this case... unsure how to handle).
     * At least one of width and height must be specified.
     * NOTE: the html/css must be configured to match this; "min" meant could go with "contain".
     * @see FitMode
     */
    public VariantInfo getCanvasBestFitVariant(FitMode mode, Integer width, Integer height) {
        VariantInfo bestVariant = null;
        // OPTIMIZATION: check if we have an exact match (this could happen frequently in some cases)
        if (width != null && height != null) {
            bestVariant = getExactDimVariantMap().get(VariantInfo.getDimString(width, height));
            if (bestVariant != null) return bestVariant;
        }
        
        if (mode == null || mode == FitMode.MINIMUM) {
            // here we get the smallest variant that is larger than the canvas dimensions (so that we
            // make the browser resize it DOWN instead of up, making less detail loss)
            // NOTE: if there is no best, then we'll return null and just use the original.
            if (height == null) {
                // only width
                for(VariantInfo variant : getVariantList()) {
                    if (variant.getWidth() >= width) {
                        if (bestVariant == null) bestVariant = variant;
                        else {
                            int widthDiff = variant.getWidth() - bestVariant.getWidth();
                            if (widthDiff < 0) bestVariant = variant;
                        }
                    }
                }
            } else if (width == null) {
                // only height
                for(VariantInfo variant : getVariantList()) {
                    if (variant.getHeight() >= height) {
                        if (bestVariant == null) bestVariant = variant;
                        else {
                            int heightDiff = variant.getHeight() - bestVariant.getHeight();
                            if (heightDiff < 0) bestVariant = variant;
                        }
                    }
                }
            } else {
                // have both
                for(VariantInfo variant : getVariantList()) {
                    if (variant.getWidth() >= width && variant.getHeight() >= height) {
                        if (bestVariant == null) bestVariant = variant;
                        else {
                            int pixelNumDiff = variant.getNumPixels() - bestVariant.getNumPixels();
                            if (pixelNumDiff < 0) bestVariant = variant;
                            else if (pixelNumDiff == 0 && variant.getWidth() < bestVariant.getWidth()) bestVariant = variant;
                        }
                    }
                }
            }
        } else if (mode == FitMode.MAXIMUM) {
            // here we get the largest variant that is smaller that the canvas dimensions
            if (height == null) {
                // only width specified, so try to get closest to width
                for(VariantInfo variant : getVariantList()) {
                    if (variant.getWidth() <= width) {
                        if (bestVariant == null) bestVariant = variant;
                        else {
                            int widthDiff = variant.getWidth() - bestVariant.getWidth();
                            if (widthDiff > 0) bestVariant = variant;
                        }
                    }
                }
            } else if (width == null) {
                // only height specified, so try to get closest to height
                for(VariantInfo variant : getVariantList()) {
                    if (variant.getHeight() <= height) {
                        if (bestVariant == null) bestVariant = variant;
                        else {
                            int heightDiff = variant.getHeight() - bestVariant.getHeight();
                            if (heightDiff > 0) bestVariant = variant;
                        }
                    }
                }
            } else {
                // have both - return one with most pixels, but prefer width in rare case of same pixels
                for(VariantInfo variant : getVariantList()) {
                    if (variant.getWidth() <= width && variant.getHeight() <= height) {
                        if (bestVariant == null) bestVariant = variant;
                        else {
                            int pixelNumDiff = variant.getNumPixels() - bestVariant.getNumPixels();
                            if (pixelNumDiff > 0) bestVariant = variant;
                            else if (pixelNumDiff == 0 && variant.getWidth() > bestVariant.getWidth()) bestVariant = variant;
                        }
                    }
                }
            }
        } else {
            throw new UnsupportedOperationException("best-variant selection fit mode '" + mode.getStrName() + "' not yet supported");
        }
        return bestVariant;
    }
    
    public VariantInfo getCanvasBestFitVariant(String mode, Integer width, Integer height) {
        return getCanvasBestFitVariant(FitMode.fromStrNameParamSafe(mode), width, height);
    }

    /**
     * Checks if this config has same variants as the other; order IGNORED.
     */
    public boolean hasSameVariants(ImageVariantConfig other) {
        if (getVariantList().size() != other.getVariantList().size()) return false;
        for(VariantInfo variant : getVariantList()) {
            if (!variant.equalsProps(other.getVariant(variant.getName()))) return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "[name: " + getName() + ", location: " + getLocation() + "]";
    }
    
    public static class VariantInfo implements Serializable, ImageVariantSelector.VariantInfo { // TODO: move elsewhere
        private final String name;
        private final int width;
        private final int height;
        public VariantInfo(String name, int width, int height) {
            this.name = name;
            this.width = width;
            this.height = height;
        }
        private VariantInfo(String name, Map<String, String> map) throws NumberFormatException {
            this.name = name;
            this.width = Integer.parseInt(map.get("width"));
            this.height = Integer.parseInt(map.get("height"));
        }
        public String getName() {
            return name;
        }
        public int getWidth() {
            return width;
        }
        public int getHeight() {
            return height;
        }
        
        public int getNumPixels() {
            return getWidth()*getHeight();
        }
        
        public String getDimString() {
            return getWidth()+"x"+getHeight();
        }
        
        public static String getDimString(int width, int height) {
            return width+"x"+height;
        }
        
        @Override
        public String toString() {
            return getName()+"="+getDimString();
        }
        
        public boolean biggerEqualThan(int width, int height) {
            return this.width >= width && this.height >= height;
        }
        
        public boolean biggerEqualThan(VariantInfo other) {
            return biggerEqualThan(other.width, other.height);
        }
        
        @Override
        public boolean equals(Object other) {
            if (!(other instanceof VariantInfo)) return false;
            return equals((VariantInfo) other);
        }
        
        public boolean equals(VariantInfo other) {
            return equalsProps(other) && name.equals(other.name);
        }
        
        public boolean equalsProps(VariantInfo other) {
            return other != null && width == other.width && height == other.height;
        }
        
        public void propsToStringMap(Map<String, ? super String> map) {
            map.put("width", ""+width);
            map.put("height", ""+height);
        }
        
        public Map<String, String> propsToStringMap() {
            Map<String, String> map = new HashMap<>();
            propsToStringMap(map);
            return map;
        }
    }
    
    public static class PathVariantConfig implements Serializable {
        protected final Map<String, ImageVariantConfig> pathPrefixMap;
        protected final List<String> pathPrefixList;
        protected final ImageVariantConfig defaultCfg;
        
        public PathVariantConfig(Map<String, ImageVariantConfig> pathPrefixMap, ImageVariantConfig defaultCfg) {
            this.pathPrefixMap = Collections.unmodifiableMap(new HashMap<>(pathPrefixMap));
            this.pathPrefixList = Collections.unmodifiableList(new ArrayList<>(pathPrefixMap.keySet()));
            this.defaultCfg = defaultCfg;
        }

        public static PathVariantConfig fromProperties(String resource, String namePrefix, Factory cfgFactory) throws IOException, IllegalArgumentException {
            Map<String, String> entries = new HashMap<>();
            // get the .cfgfile entries
            Properties properties = UtilProperties.getProperties(resource);
            UtilProperties.putPropertiesWithPrefixSuffix(entries, properties, 
                    namePrefix, ".cfgfile", false, false, false);
            ImageVariantConfig defaultCfg = null;
            Map<String, ImageVariantConfig> pathPrefixMap = new LinkedHashMap<>();
            for(Map.Entry<String, String> entry : entries.entrySet()) {
                String name = entry.getKey();
                String cfgfile = entry.getValue();
                if ("default".equals(name)) {
                    defaultCfg = cfgFactory.fromImagePropertiesXml(cfgfile);
                    if (defaultCfg == null) throw new IllegalArgumentException("could not read: " + cfgfile);
                } else {
                    String pathPrefix = properties.getProperty(namePrefix + name + ".pathprefix");
                    if (pathPrefix != null) pathPrefix = pathPrefix.trim();
                    if (UtilValidate.isEmpty(pathPrefix)) Debug.logError("Empty pathprefix for [" + resource + "/" + namePrefix + "] (default entry will be used)", module);
                    else {
                        ImageVariantConfig cfg = cfgFactory.fromImagePropertiesXml(cfgfile);
                        if (cfg == null) throw new IllegalArgumentException("could not read: " + cfgfile);
                        if (pathPrefixMap.containsKey(pathPrefix)) Debug.logWarning("Duplicate pathprefix: " + pathPrefix + " for [" + resource + "/" + namePrefix + "]", module);
                        pathPrefixMap.put(pathPrefix, cfg);
                    }
                }
            }
            if (defaultCfg == null) throw new IllegalArgumentException("missing default variant configuration");
            return new PathVariantConfig(pathPrefixMap, defaultCfg);
        }
        
        public static PathVariantConfig fromPropertiesSafe(String resource, String namePrefix, Factory cfgFactory) {
            try {
                return fromProperties(resource, namePrefix, cfgFactory);
            } catch(Exception e) {
                Debug.logError("Unable to read image variant path mapping config [" + resource + "/" + namePrefix + "]", module);
                return new PathVariantConfig(new HashMap<String, ImageVariantConfig>(), null);
            }
        }
        
        public Map<String, ImageVariantConfig> getPathPrefixMap() {
            return pathPrefixMap;
        }

        public List<String> getPathPrefixList() {
            return pathPrefixList;
        }

        public ImageVariantConfig getDefaultCfg() {
            return defaultCfg;
        }

        public ImageVariantConfig getVariantCfgForPath(String path) {
            for(String pathPrefix : pathPrefixList) {
                if (path.startsWith(pathPrefix)) return pathPrefixMap.get(pathPrefix);
            }
            return defaultCfg;
        }
    }
     
    /**
     * If imgPropertyPath starts with component://, expands it to a full path; otherwise, assumes
     * it is a path from scipio project root and expands it to a full path using <code>ofbiz.home</code>.
     * @throws IllegalArgumentException 
     * @throws MalformedURLException 
     */
    public static String getImagePropertiesFullPath(String imgPropertyPath) throws IOException {
        try {
            if (FlexibleLocation.isUrlLocation(imgPropertyPath)) {
                return FlexibleLocation.resolveFileUrlAsPath(imgPropertyPath);
            } else {
                return System.getProperty("ofbiz.home") + imgPropertyPath;
            }
        } catch(IOException e) {
            throw e;
        } catch(Exception e) {
            throw new IOException(e);
        }
    }
    
    /**
     * SCIPIO: Returns the full path to the ImageProperties.xml file to use for common image size definitions.
     * Added 2017-08-08.
     */
    public static String getCommonImagePropertiesFullPath() throws IOException {
        return getImagePropertiesFullPath(COMMON_IMAGEPROP_FILEPATH);
    }
    
    public static String getCommonImagePropertiesPath() throws IOException {
        return COMMON_IMAGEPROP_FILEPATH;
    }
}
