package org.ofbiz.common.image.scaler;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.common.image.ImageUtil;

/**
 * SCIPIO: Special composed/complex/delegating scaler that delegates to the other scalers
 * depending on certain conditions.
 * <p>
 * FIXME: bad recompilation forced when options are passed at runtime.
 * <p>
 * Added 2017-07-10.
 */
public class ComposedImageScaler extends AbstractImageScaler {

    public static final String module = ComposedImageScaler.class.getName();
    public static final String API_NAME = "composed";
    
    public static final Map<String, Object> DEFAULT_OPTIONS;
    static {
        Map<String, Object> options = makeOptionsMap();
        DEFAULT_OPTIONS = Collections.unmodifiableMap(options);
    }
    
    private static final FlexibleStringExpander EXDR_TRUE = FlexibleStringExpander.getInstance("${true}");
    private static final FlexibleStringExpander EXDR_FALSE = FlexibleStringExpander.getInstance("${false}");
    
    @SuppressWarnings("serial")
    protected static class OptionSetEntry implements Serializable {
        protected final String aliasName;
        protected final FlexibleStringExpander cond;
        protected final ImageScaler imageScaler;
        protected final Map<String, Object> options;
        protected OptionSetEntry(String aliasName, FlexibleStringExpander cond, ImageScaler imageScaler, Map<String, Object> options) {
            this.aliasName = aliasName;
            this.imageScaler = imageScaler;
            this.cond = cond;
            this.options = Collections.unmodifiableMap(options);
        }
    }
    
    protected Map<String, OptionSetEntry> compiledSets = null; // compiled on first run, optimization only
    
    protected ComposedImageScaler(AbstractImageScalerFactory<ComposedImageScaler> factory, String name, Map<String, Object> confOptions) {
        super(factory, name, confOptions);
    }

    public static class Factory extends AbstractImageScalerFactory<ComposedImageScaler> {

        @Override
        public ComposedImageScaler getImageOpInstStrict(String name, Map<String, Object> defaultScalingOptions) {
            return new ComposedImageScaler(this, name, defaultScalingOptions);
        }

        @Override
        public Map<String, Object> makeValidOptions(Map<String, Object> options) {
            Map<String, Object> validOptions = makeOptionsMap(options);
            // TODO?: currently can't do this very well in advance, unfortunately leaving the parsing
            // to real-time... otherwise option merging doesn't work well...
            return validOptions;
        }

        @Override protected String getApiName() { return API_NAME; }
        @Override public Map<String, Object> getDefaultOptions() { return DEFAULT_OPTIONS; }

        // must preverse option order for this scaler
        @Override protected Map<String, Object> makeOptionsMap(Map<String, Object> srcMap) { return new HashMap<>(srcMap); }
        @Override protected Map<String, Object> makeOptionsMap() { return new HashMap<>(); }
    }
    
    protected static Map<String, Object> makeOptionsMap(Map<String, Object> srcMap) { return new HashMap<>(srcMap); }
    protected static Map<String, Object> makeOptionsMap() { return new HashMap<>(); }
    
    @Override
    protected BufferedImage scaleImageCore(BufferedImage image, int targetWidth, int targetHeight,
            Map<String, Object> options) throws IOException {
        
        Map<String, OptionSetEntry> sets = UtilGenerics.checkMap(options.get("sets"));
        
        Map<String, Object> optionCtx = new HashMap<>();
        optionCtx.put("srcWidth", image.getWidth());
        optionCtx.put("srcHeight", image.getHeight());
        optionCtx.put("destWidth", targetWidth);
        optionCtx.put("destHeight", targetHeight);
        optionCtx.put("options", options);
        
        if (UtilValidate.isEmpty(sets)) throw new IllegalArgumentException("composed scaler " + toString(options) + " has no option sets");
        
        for(Map.Entry<String, OptionSetEntry> entry : sets.entrySet()) {
            String name = entry.getKey();
            OptionSetEntry optSet = entry.getValue();
            Object resObj = optSet.cond.expand(optionCtx);
            if (resObj instanceof Boolean) {
                if (ImageUtil.verboseOn()) Debug.logInfo("Composed image scaler option set " + name 
                        + " evaluated to " + resObj + " for scaler:\n  " + this.toString(options) + " and context:\n  " + optionCtx.toString(), module);
                if (Boolean.TRUE.equals(resObj)) {
                    Map<String, Object> subOptions = makeOptionsMap(optSet.options); 
                    subOptions.putAll(options); // this merge allows callers to pass generic options without knowing the specific set, to a limited extent
                    return optSet.imageScaler.scaleImage(image, targetWidth, targetHeight, subOptions);
                }
            } else {
                Debug.logError("Composed image scaler option set " + name 
                        + " did not evaluate to a Boolean for scaler " + this.toString(options) 
                        + "; evaluated to type: " + (resObj != null ? resObj.getClass().getName() : "null"), module);
            }
        }
        
        throw new IllegalStateException("composed scaler " + toString(options) + " failed to determine an image scaler to run");
    }
    
    protected Map<String, Object> getEffectiveScalingOptions(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) {
        if (options == null || options.isEmpty() || !containsSetsSettings(options)) {
            Map<String, OptionSetEntry> compiledSets = this.compiledSets;
            if (compiledSets == null) {
                compiledSets = compileSets(confDefOptions);
                compiledSets = Collections.unmodifiableMap(compiledSets);
                this.compiledSets = compiledSets;
            }
            Map<String, Object> mergedMap = makeOptionsMap(confDefOptions);
            if (options != null) {
                mergedMap.putAll(options); // ONLY if !containsSetsSettings(options) !!!
            }
            mergedMap.put("sets", compiledSets);
            return mergedMap;
        }
        else {
            // TODO: find some way to prevent constant recompile when specific set options are passed...
            Map<String, Object> mergedMap = makeOptionsMap(confDefOptions);
            mergedMap.putAll(options);
            mergedMap.put("sets", compileSets(mergedMap));
            return mergedMap;
        }
    }
    
    protected boolean containsSetsSettings(Map<String, Object> options) {
        // this is a semi-optimization that should make most cases faster... but dynamic cases slower...
        for(String key : options.keySet()) {
            if (key.startsWith("sets.")) return true;
        }
        return false;
    }
    
    protected Map<String, OptionSetEntry> compileSets(Map<String, Object> options) {
        Map<String, OptionSetEntry> optSetMap = new HashMap<>();
        for(Map.Entry<String, Object> entry : options.entrySet()) {
            String propName = entry.getKey();
            if (propName.startsWith("sets.") && propName.endsWith(".alias")) {
                String setName = propName.substring("sets.".length(), propName.length() - ".alias".length());
                String aliasName = (String) entry.getValue();
                if (aliasName != null) aliasName = aliasName.trim();
                if (UtilValidate.isEmpty(aliasName)) {
                    Debug.logWarning("Empty image op alias property in composed scaler sets: " + propName, module);
                    continue;
                }
                ImageScaler scaler = ImageScalers.getScalerOrDefault(aliasName);
                FlexibleStringExpander exdr;
                String exdrStr = (String) options.get("sets."+setName+".cond");
                if (exdrStr != null) exdrStr = exdrStr.trim();
                if (UtilValidate.isNotEmpty(exdrStr)) {
                    exdr = FlexibleStringExpander.getInstance(exdrStr);
                } else {
                    exdr = "default".equals(setName) ? EXDR_TRUE : EXDR_FALSE;
                }
                Map<String, Object> subOptions = ImageUtil.readImagePropOptions(options, "sets."+setName+".options.", makeOptionsMap());
                // TODO? we can just pass the sub-options dynamically for now... optimization issue...
                //ImageScaler derivedScaler
                OptionSetEntry setEntry = new OptionSetEntry(aliasName, exdr, scaler, subOptions);
                optSetMap.put(setName, setEntry);
            }
        }
        Map<String, OptionSetEntry> optSetMapRes = new LinkedHashMap<>();

        // setorder is required due to property files not maintaining order
        String setorder = (String) options.get("setorder");
        if (setorder != null) setorder = setorder.trim();
        if (UtilValidate.isNotEmpty(setorder)) {
            String[] setorderList = StringUtils.split(setorder, ",");
            for(String name : setorderList) {
                name = name.trim();
                OptionSetEntry optSetEntry = optSetMap.get(name);
                if (optSetEntry != null) {
                    optSetMapRes.put(name, optSetEntry);
                }
            }
        } else {
            for(Map.Entry<String, OptionSetEntry> entry : optSetMap.entrySet()) {
                if (!"default".equals(entry.getKey())) {
                    optSetMapRes.put(entry.getKey(), entry.getValue());
                }
            }
            OptionSetEntry defEntry = optSetMap.get("default");
            if (defEntry != null) {
                optSetMapRes.put("default", defEntry);
            }
        }
        return optSetMapRes;
    }

    @Override
    public boolean isNativeSupportedDestImagePixelType(int imagePixelType) {
        throw new UnsupportedOperationException("Can't determine isNativeSupportedDestImagePixelType");
    }
    
}
