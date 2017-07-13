package org.ofbiz.common.image.scaler;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Properties;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.image.ImageUtil;

/**
 * SCIPIO: ImageScaler instance registry.
 * <p>
 * FIXME: alias entries can't point to other alias entries (requires dep check)
 * <p>
 * Added 2017-07-10.
 */
public abstract class ImageScalers {

    public static final String module = ImageScalers.class.getName();
    public static final String IMAGE_PROP_SCALER_PREFIX = ImageUtil.IMAGEOP_PROP_PREFIX + "scaler.";
    
    public static final String SCALER_NAME_OPT = "scalerName";
    
    private static final Map<String, ImageScaler> scalers = Collections.unmodifiableMap(readScalers(
            ImageUtil.getAllPropertiesFiles(ImageUtil.IMAGEOP_PROP_RESOURCE), IMAGE_PROP_SCALER_PREFIX));
    private static final ImageScaler defaultScaler = extractDefaultScaler(scalers, true, true);
    
    protected ImageScalers() {
    }
    
    /**
     * Returns scaler for a given name.
     * The name can be an algorithm name or any other name chosen to represent the scaler,
     * as defined in image properties file.
     */
    public static ImageScaler getScaler(String name) {
        return scalers.get(name);
    }
    
    public static ImageScaler getScaler(Map<String, Object> scalingOptions) {
        return scalingOptions != null ? getScaler((String) scalingOptions.get(SCALER_NAME_OPT)) : null;
    }
    
    public static ImageScaler getScalerOrDefault(String name) {
        if (UtilValidate.isEmpty(name)) return getDefaultScaler();
        ImageScaler scaler = getScaler(name);
        if (scaler == null) {
            Debug.logWarning("Scaler not found for (algorithm or scaler) name: " + name + "; returning default", module);
        }
        return scaler != null ? scaler : getDefaultScaler();
    }
    
    public static ImageScaler getScalerOrDefault(Map<String, Object> scalingOptions) {
        return getScalerOrDefault(scalingOptions != null ? (String) scalingOptions.get(SCALER_NAME_OPT) : null);
    }
    
    public static ImageScaler getDefaultScaler() {
        return defaultScaler;
    }
    
    public static Map<String, ImageScaler> readScalers(Collection<Properties> propList, String propPrefix) {
        return ImageUtil.readImagePropsToImageOpMap(propList, propPrefix, ImageScaler.class);
    }
    
    public static ImageScaler extractDefaultScaler(Map<String, ImageScaler> scalerMap, boolean fallback, boolean log) {
        ImageScaler scaler = scalerMap.get("default");
        if (fallback && scaler == null) {
            Debug.logWarning("No default scaler configured: using stock default", module);
            try {
                scaler = new AwtImageScaler.Factory().getImageOpInst("java-awt", Collections.<String, Object>emptyMap());
            } catch(Exception e) {
                if (log) Debug.logError(e, "Could not instantiate stock default image scaler; scaling will fail: " + e.getMessage(), module);
            }
        }
        if (log && scaler != null) Debug.logInfo("Default image scaler instance: " + scaler.toString(), module);
        return scaler;
    }
}
