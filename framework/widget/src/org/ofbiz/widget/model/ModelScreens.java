package org.ofbiz.widget.model;

import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.FileUtil;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.widget.WidgetWorker;
import org.w3c.dom.Element;

/**
 * SCIPIO: Represents a whole screens file.
 * <p>
 * This implements Map<String, ModelScreen> for backward-compatibility; the Map methods
 * all behave to ignore the elements that are not strictly "screen" element entries.
 */
@SuppressWarnings("serial")
public class ModelScreens implements Map<String, ModelScreen>, Serializable {

    public static final String module = ModelScreens.class.getName();
    
    /**
     * SCIPIO: Default name of the common screens file indicated to contain centralized reusable
     * settings, via {@link ModelScreenSettings.AutoIncludeSettings}.
     * <p>
     */
    public static final String COMMON_SCREENS_FILE = "CommonScreens.xml";
    private static final String SEP_COMMON_SCREENS_FILE = "/" + COMMON_SCREENS_FILE;
    
    protected final Map<String, ModelScreen> screenMap;
    protected final Map<String, ModelScreenGroup> screenGroupMap; // TODO: list of this
    protected final ModelScreenGroup rootGroup;
    
    // duplicated from rootGroups, for faster access
    protected final ModelScreenSettings effectiveSettings;
    
    public ModelScreens(Element rootElement, String sourceLocation) {
        this(rootElement, sourceLocation, true);
    }
    
    public ModelScreens(Element rootElement, String sourceLocation, boolean useAutoIncludeSettings) {
        Map<String, ModelScreen> screenMap = new HashMap<String, ModelScreen>();
        Map<String, ModelScreenGroup> screenGroupMap = new HashMap<String, ModelScreenGroup>();
        
        // SCIPIO: NOTE: 2016-10-30: The base code for this was originally moved from
        //   org.ofbiz.widget.model.ScreenFactory.readScreenDocument(Document, String)
        // but completely scrapped.
        
        ModelScreenGroup rootGroup = new ModelScreenGroup(rootElement, true, this, sourceLocation, useAutoIncludeSettings);
        for(ModelScreen modelScreen : rootGroup.getScreenList()) {
            screenMap.put(modelScreen.getName(), modelScreen);
        }
        screenGroupMap.put(rootGroup.getName(), rootGroup);
        
        List<? extends Element> childElements = UtilXml.childElementList(rootElement, "screen-group");
        for (Element childElement: childElements) {
            ModelScreenGroup modelScreenGroup = new ModelScreenGroup(childElement, false, this, sourceLocation, useAutoIncludeSettings);
            for(ModelScreen modelScreen : modelScreenGroup.getScreenList()) {
                screenMap.put(modelScreen.getName(), modelScreen);
            }
            if (screenGroupMap.containsKey(modelScreenGroup.getName())) {
                Debug.logWarning("Screen file [" + sourceLocation + 
                        "] contains more than one group with name [" + modelScreenGroup.getName() + "]; using last", module);
            } 
            screenGroupMap.put(modelScreenGroup.getName(), modelScreenGroup);
        }
        
        // NOTE: for performance reasons, in this class we leave the Collections.unmodifiableXxx
        // calls to the getters. 
        // This is safe because HashMap is read-only thread-safe after population when set in final field.
        // (doing extra copy here because screenMap/screenGroupMap may need to be changed to LinkedHashMap or other later 
        // during construct)
        this.screenMap = new HashMap<>(screenMap);
        this.screenGroupMap = new HashMap<>(screenGroupMap);
        this.rootGroup = rootGroup;
        this.effectiveSettings = rootGroup.getEffectiveSettings();
    }
    
    public ModelScreens() {
        this.screenMap = new HashMap<>();
        this.screenGroupMap = new HashMap<>();
        this.rootGroup = new ModelScreenGroup(null, true, this);
        this.effectiveSettings = rootGroup.getEffectiveSettings();
    }
    
    public Map<String, ModelScreen> getScreenMap() {
        return Collections.unmodifiableMap(screenMap);
    }
    
    public Map<String, ModelScreen> getRootScreenMap() {
        return rootGroup.getScreenMap();
    }
    
    public ModelScreenGroup getRootGroup() {
        return rootGroup;
    }

    public Map<String, ModelScreenGroup> getScreenGroupMap() {
        return Collections.unmodifiableMap(screenGroupMap);
    }
    
    /**
     * Returns settings map for root screens/group.
     */
    public Map<String, ModelScreenSettings> getScreenSettingsMap() {
        return rootGroup.getScreenSettingsMap();
    }
    
    /**
     * Returns effective settings for root screens/group.
     */
    public ModelScreenSettings getEffectiveSettings() {
        return effectiveSettings;
    }
    
    /**
     * Returns name settings for root screens/group.
     */
    public ModelScreenSettings getSettings(String name) {
        return rootGroup.screenSettingsMap.get(name);
    }
    
    /**
     * Gets any screen in file.
     */
    public ModelScreen getScreen(String name) {
        return screenMap.get(name);
    }
    
    /**
     * Gets screen from root only.
     */
    public ModelScreen getRootScreen(String name) {
        return rootGroup.screenMap.get(name);
    }
    
    /* Map interface methods */
    
    @Override
    public int size() {
        return screenMap.size();
    }

    @Override
    public boolean isEmpty() {
        return screenMap.isEmpty();
    }

    @Override
    public boolean containsKey(Object key) {
        return screenMap.containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        return screenMap.containsValue(value);
    }

    @Override
    public ModelScreen get(Object key) {
        return screenMap.get(key);
    }

    @Override
    public ModelScreen put(String key, ModelScreen value) {
        throw new UnsupportedOperationException("Cannot modify ModelScreens after construction");
    }

    @Override
    public ModelScreen remove(Object key) {
        throw new UnsupportedOperationException("Cannot modify ModelScreens after construction");
    }

    @Override
    public void putAll(Map<? extends String, ? extends ModelScreen> m) {
        throw new UnsupportedOperationException("Cannot modify ModelScreens after construction");
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException("Cannot modify ModelScreens after construction");
    }

    @Override
    public Set<String> keySet() {
        return Collections.unmodifiableMap(screenMap).keySet();
    }

    @Override
    public Collection<ModelScreen> values() {
        return Collections.unmodifiableMap(screenMap).values();
    }

    @Override
    public Set<java.util.Map.Entry<String, ModelScreen>> entrySet() {
        return Collections.unmodifiableMap(screenMap).entrySet();
    }
    
    public boolean isAutoIncludeSettingsConfigFile(String sourceLocation) {
        return sourceLocation.endsWith(SEP_COMMON_SCREENS_FILE);
    }
    
    /**
     * Checks for a CommonScreens.xml (or equivalent) file in same dir as sourceLocation;
     * if not found, checks parent dir; etc; up to top widget folder.
     */
    public String getAutoIncludeSettingsConfigFilePath(String sourceLocation) throws IllegalArgumentException {
        String basePath = WidgetWorker.getBaseWidgetFolderFromComponentPath(sourceLocation);
        String remPath = sourceLocation;
        int i;
        while((i = remPath.lastIndexOf('/')) >= basePath.length()) {
            remPath = remPath.substring(0, i);
            String configFilePath = remPath + SEP_COMMON_SCREENS_FILE;
            if (FileUtil.getFile(configFilePath).exists()) {
                return configFilePath;
            }
        }
        if (remPath.length() > basePath.length()) {
            String configFilePath = basePath + COMMON_SCREENS_FILE;
            if (FileUtil.getFile(configFilePath).exists()) {
                return configFilePath;
            }
        }
        return null;
    }
    
    /**
     * Represents any element child of top "screens" element in *Screens.xml file.
     */
    public interface ScreenEntry {

        /**
         * SCIPIO: Returns all the screens contained in this entry.
         */
        List<ModelScreen> getScreenList();
        
    }
}
