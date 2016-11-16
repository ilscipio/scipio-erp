package org.ofbiz.widget.model;

import java.io.IOException;
import java.io.Serializable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.widget.model.ModelScreenSettings.AutoIncludeSettings;
import org.ofbiz.widget.model.ModelScreenSettings.IncludeSettings;
import org.ofbiz.widget.model.ModelScreenWidget.DecoratorScreen;
import org.ofbiz.widget.model.ScreenFallback.FlexibleScreenFallbackSettings;
import org.ofbiz.widget.model.ScreenFallback.SimpleFlexibleScreenFallbackSettings;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

/**
 * SCIPIO: Model for new "screen-group" element, child of top "screens" element, and
 * containing screens ("screen" element or substitute).
 */
@SuppressWarnings("serial")
public class ModelScreenGroup extends ModelWidget implements ModelScreens.ScreenEntry {

    public static final String ROOT_GROUP_NAME = "root-screens";
    
    protected final ModelScreens modelScreens;
    
    protected final ModelScreenSettings effectiveSettings;
    protected final Map<String, ModelScreenSettings> screenSettingsMap;
    
    protected final Map<String, ModelScreen> screenMap;
    protected final List<ModelScreen> screenList;

    public ModelScreenGroup(String name, boolean rootGroup, ModelScreens modelScreens) {
        super(name != null ? name : (rootGroup ? ROOT_GROUP_NAME : ""));
        this.modelScreens = modelScreens;
        this.effectiveSettings = new ModelScreenSettings(ModelScreenSettings.DEFAULT_SETTINGS_NAME, true);
        Map<String, ModelScreenSettings> screenSettingsMap = new HashMap<String, ModelScreenSettings>();
        screenSettingsMap.put(this.effectiveSettings.getName(), this.effectiveSettings);
        this.screenSettingsMap = screenSettingsMap;
        Map<String, ModelScreen> screenMap = new HashMap<>();
        ArrayList<ModelScreen> screenList = new ArrayList<>();
        this.screenMap = screenMap;
        screenList.trimToSize();
        this.screenList = screenList;
    }
    
    public ModelScreenGroup(Element groupingElement, boolean rootGroup, ModelScreens modelScreens, String sourceLocation) {
        this(groupingElement, rootGroup, modelScreens, sourceLocation, true);
    }

    public ModelScreenGroup(Element groupingElement, boolean rootGroup, ModelScreens modelScreens, String sourceLocation, boolean useAutoIncludeSettings) {
        super(groupingElement, rootGroup ? ROOT_GROUP_NAME : null);
        this.modelScreens = modelScreens;

        Map<String, ModelScreenSettings> screenSettingsMap = new LinkedHashMap<String, ModelScreenSettings>();
        Map<String, ModelScreen> screenMap = new LinkedHashMap<>();
        
        // NOTE: auto settings only supported on root group currently (TODO? other groups? probably not worth it)
        if (rootGroup && useAutoIncludeSettings && !modelScreens.isAutoIncludeSettingsConfigFile(sourceLocation)) {
            String configFilePath = modelScreens.getAutoIncludeSettingsConfigFilePath(sourceLocation);
            if (configFilePath != null) {
                ModelScreens configScreens = null;
                try {
                    configScreens = ScreenFactory.getScreensFromLocation(configFilePath);
                } catch (Exception e) {
                    Debug.logError(e, "Error loading auto-include-settings config file [" + configFilePath + "] "
                            + "while loading file [" + sourceLocation + "]", module);
                }
                if (configScreens != null) {
                    AutoIncludeSettings autoIncludeSettings = configScreens.getEffectiveSettings().getAutoIncludeSettings();
                    if (autoIncludeSettings != null && !autoIncludeSettings.isEmpty()) {
                        // get the source location path relative to the dir the config file (CommonScreens.xml) is in
                        String configFileDir = configFilePath.substring(configFilePath.lastIndexOf('/') + 1);
                        String relScreenFileLoc = sourceLocation.substring(configFileDir.length());
                        if (autoIncludeSettings.appliesTo(relScreenFileLoc)) {
                            for(IncludeSettings inclSettings : autoIncludeSettings.getIncludeSettingsList()) {
                                addSettings(screenSettingsMap, inclSettings.getSettingsAlways());
                            }
                            for(ModelScreenSettings settings : autoIncludeSettings.getSettingsList()) {
                                addSettings(screenSettingsMap, settings);
                            }
                        }
                    }
                }
            }
        }
        
        List<? extends Element> childElements;
        childElements = UtilXml.childElementList(groupingElement, "include-settings");
        for (Element childElement: childElements) {
            IncludeSettings inclSettings = new IncludeSettings(childElement, sourceLocation);
            addSettings(screenSettingsMap, inclSettings.getSettingsAlways());
        }
        
        childElements = UtilXml.childElementList(groupingElement, "screen-settings");
        for (Element childElement: childElements) {
            ModelScreenSettings settings = new ModelScreenSettings(childElement, sourceLocation);
            addSettings(screenSettingsMap, settings);
        }
        
        // Merge all the active settings.
        ModelScreenSettings effectiveSettings = null;
        for(ModelScreenSettings settings : screenSettingsMap.values()) {
            if (settings.isActive()) {
                if (effectiveSettings != null) {
                    effectiveSettings = new ModelScreenSettings(effectiveSettings, settings);
                } else {
                    effectiveSettings = settings;
                }
            }
        }

        if (effectiveSettings == null) {
            String settingsName = ModelScreenSettings.DEFAULT_SETTINGS_NAME;
            effectiveSettings = new ModelScreenSettings(settingsName, true);
            if (screenSettingsMap.containsKey(settingsName)) {
                Debug.logWarning("Screen group already contains special [" + settingsName + "] screen-settings"
                        + " set to active=false, which we need to overwrite; the specified settings will be ignored "
                        + "completely and non-referenceable; using system defaults only", sourceLocation);
            }
        }
        screenSettingsMap.put(effectiveSettings.getName(), effectiveSettings);
        
        List<Element> screenElements = new ArrayList<>();
        childElements = UtilXml.childElementList(groupingElement, "include-screens");
        for (Element childElement: childElements) {
            IncludeScreens includeScreens = new IncludeScreens(childElement, sourceLocation);
            includeScreens.generateDelegatingScreensAsChildren(groupingElement);
        }
        screenElements.addAll(UtilXml.childElementList(groupingElement));
        for (Element childElement: screenElements) {
            if (ModelScreen.isScreenElement(childElement)) {
                // FIXME: Terrible ThreadLocal-based hack for setting default fallback settings
                FlexibleScreenFallbackSettings prevFallbackSettings = DecoratorScreen.getOverridingDefaultFallbackSettings();
                try {
                    FlexibleScreenFallbackSettings newSettings = effectiveSettings.getDecoratorScreenSettings().getDefaultDecoratorFallbackSettings();
                    DecoratorScreen.setOverridingDefaultFallbackSettings((prevFallbackSettings != null) ? 
                                    new SimpleFlexibleScreenFallbackSettings(prevFallbackSettings, newSettings) : newSettings);
                    
                    ModelScreen modelScreen = new ModelScreen(childElement, this, sourceLocation);
                    //Debug.logInfo("Read Screen with name: " + modelScreen.getName(), module);
                    screenMap.put(modelScreen.getName(), modelScreen);
                } finally {
                    DecoratorScreen.setOverridingDefaultFallbackSettings(prevFallbackSettings);
                }
            }
        }

        this.screenSettingsMap = Collections.unmodifiableMap(new HashMap<>(screenSettingsMap));
        this.screenMap = Collections.unmodifiableMap(new HashMap<>(screenMap));
        ArrayList<ModelScreen> screenList = new ArrayList<>(screenMap.values());
        screenList.trimToSize();
        this.screenList = Collections.unmodifiableList(screenList);
        this.effectiveSettings = effectiveSettings;
    }

    protected static void addSettings(Map<String, ModelScreenSettings> screenSettingsMap, ModelScreenSettings settings) {
        ModelScreenSettings existingSettings = screenSettingsMap.get(settings.getName());
        if (existingSettings != null) {
            ModelScreenSettings mergedSettings = new ModelScreenSettings(existingSettings, settings);
            // NOTE: screenSettingsMap is a LinkedHashMap and we have to do a remove first
            // in order to record a change in order here
            screenSettingsMap.remove(mergedSettings.getName());
            screenSettingsMap.put(mergedSettings.getName(), mergedSettings);
        } else {
            screenSettingsMap.put(settings.getName(), settings);
        }
    }
    
    public ModelScreenSettings getEffectiveSettings() {
        return effectiveSettings;
    }

    public Map<String, ModelScreenSettings> getScreenSettingsMap() {
        return screenSettingsMap;
    }

    public Map<String, ModelScreen> getScreenMap() {
        return screenMap;
    }

    @Override
    public List<ModelScreen> getScreenList() {
        return screenList;
    }
    
    ModelScreens getModelScreens() {
        return modelScreens;
    }

    @Override
    public void accept(ModelWidgetVisitor visitor) throws Exception {
        // TODO Auto-generated method stub
    }

    /**
     * Generates placeholder delegating screens, in the format:
     * <p>
     * <pre>
     * <code>
     * &lt;screen name="xxx"&gt;
     *     &lt;section&gt;
     *         &lt;widgets&gt;
     *             &lt;include-screens name="xxx" location="..."/&gt; 
     *         &lt;/widgets&gt;
     *     &lt;/section&gt;
     * &lt;/screen&gt;
     * </code>
     * </pre>
     */
    public static class IncludeScreens implements Serializable {
        protected final String location;
        protected final String name;
        protected final String groupName;
        protected final boolean recursive;
        
        public IncludeScreens(String location, String name, String groupName, boolean recursive) {
            this.location = location;
            this.name = name;
            this.groupName = groupName;
            this.recursive = recursive;
        }
        
        public IncludeScreens(Element inclScreensElement, String sourceLocation) {
            this.location = inclScreensElement.getAttribute("location");
            this.name = inclScreensElement.getAttribute("name");
            this.groupName = inclScreensElement.getAttribute("group-name");
            this.recursive = !"no".equals(inclScreensElement.getAttribute("recursive")); // default: full
            if (this.location.equals(sourceLocation)) { // NOTE: this check is poor, but it usually works with component:// paths
                throw new IllegalArgumentException("include-screens directive specifies location which is the same as "
                        + "the current file [" + sourceLocation + "]; must be different");
            }
        }
        
        public List<String> getScreenNameList() {
            try {
                URL screenFileUrl = FlexibleLocation.resolveLocation(location);
                if (screenFileUrl == null) {
                    throw new IllegalArgumentException("Could not resolve location to URL: " + location);
                }
                Document screenFileDoc = UtilXml.readXmlDocument(screenFileUrl, true, true);
                Element rootElement = screenFileDoc.getDocumentElement();

                List<Element> groupElements = new ArrayList<>();
                if (UtilValidate.isNotEmpty(groupName)) {
                    if (ROOT_GROUP_NAME.equals(groupName)) {
                        groupElements.add(rootElement);
                    } else {
                        for(Element groupElement : UtilXml.childElementList(rootElement, "screen-group")) {
                            if (groupName.equals(groupElement.getAttribute("name"))) {
                                groupElements.add(groupElement);
                                break;
                            }
                        }
                    }
                } else {
                    groupElements.add(rootElement);
                    groupElements.addAll(UtilXml.childElementList(rootElement, "screen-group"));
                }
                
                Set<String> resolvedScreenNames = new LinkedHashSet<>();
                for(Element groupElement : groupElements) {
                    List<? extends Element> childElements;
                    if (recursive) {
                        childElements = UtilXml.childElementList(groupElement, "include-screens");
                        for (Element childElement: childElements) {
                            IncludeScreens recInclScreens = new IncludeScreens(childElement, location);
                            for(String screenName : recInclScreens.getScreenNameList()) {
                                resolvedScreenNames.add(screenName);
                            }
                        }
                    }
                    
                    childElements = UtilXml.childElementList(groupElement);
                    for(Element screenElement : childElements) {
                        if (ModelScreen.isScreenElement(screenElement)) {
                            resolvedScreenNames.add(screenElement.getAttribute("name"));
                        }
                    }
                }
                
                List<String> resolvedScreenElementList = new ArrayList<>();
                if (UtilValidate.isNotEmpty(name)) {
                    if (!resolvedScreenNames.contains(name)) {
                        throw new IllegalArgumentException("Could not find screen with name [" + name + "] in location [" + location + "]");
                    }
                    resolvedScreenElementList.add(name);
                } else {
                    resolvedScreenElementList.addAll(resolvedScreenNames);
                }
                return resolvedScreenElementList;
            } catch(Exception e) {
                Debug.logError(e, "Could not include screen(s) from location [" + location + "], "
                        + " name [" + name + "] "
                        + " group-name [" + groupName + "] "
                        +" recursive [" + recursive + "] ",module);
                return new ArrayList<>();
            }
        }
        
        /**
         * Generates delegating placeholder screens, with no parent.
         */
        public List<Element> generateDelegatingScreens(Document document) {
            // Lookup the final models
            ModelScreens screens;
            try {
                screens = ScreenFactory.getScreensFromLocation(location);
            } catch (Exception e) {
                throw new IllegalStateException("include-screens: trying to generate delegate screens, "
                        + "but failed to read target screens fully-built models", e);
            }
            List<Element> delegScreens = new ArrayList<>();
            for(String screenName : getScreenNameList()) {
                ModelScreen screen = screens.getScreen(screenName);
                if (screen == null) {
                    throw new IllegalStateException("include-screens: trying to generate delegate screens with name [" + screenName + "]"
                            + "but it somehow does not seem to exist in the target location [" + location + "] which should have been read immediately before");
                }
                // TODO: optimize with model elements?
                Element screenElement;
                if (screen.isActionsOnly()) {
                    screenElement = createDelegatingActionsScreenElement(document, screenName, screenName, location);
                } else {
                    screenElement = createDelegatingWidgetsScreenElement(document, screenName, screenName, location);
                }
                delegScreens.add(screenElement);
            }
            return delegScreens;
        }
        
        /**
         * Generates delegating placeholder screens as children of specified element.
         */
        public void generateDelegatingScreensAsChildren(Element parentElement) {
            for(Element delegScreen : generateDelegatingScreens(parentElement.getOwnerDocument())) {
                parentElement.appendChild(delegScreen);
            }
        }

        public static Element createDelegatingWidgetsScreenElement(Document document, String localName, String targetName, String targetLocation) {
            Element screenElement = document.createElement("screen");
            screenElement.setAttribute("name", localName);
            Element sectionElement = document.createElement("section");
            screenElement.appendChild(sectionElement);
            Element widgetElement = document.createElement("widgets");
            sectionElement.appendChild(widgetElement);
            Element includeScreenElement = document.createElement("include-screen");
            widgetElement.appendChild(includeScreenElement);
            includeScreenElement.setAttribute("name", targetName);
            includeScreenElement.setAttribute("location", targetLocation);
            return screenElement;
        }
        
        public static Element createDelegatingActionsScreenElement(Document document, String localName, String targetName, String targetLocation) {
            Element screenElement = document.createElement("screen");
            screenElement.setAttribute("name", localName);
            Element actionsElement = document.createElement("actions");
            screenElement.appendChild(actionsElement);
            Element includeScreenActionsElement = document.createElement("include-screen-actions");
            actionsElement.appendChild(includeScreenActionsElement);
            includeScreenActionsElement.setAttribute("name", targetName);
            includeScreenActionsElement.setAttribute("location", targetLocation);
            return screenElement;
        }
        
        public String getLocation() {
            return location;
        }

        public String getName() {
            return name;
        }

        public String getGroupName() {
            return groupName;
        }

        public boolean isRecursive() {
            return recursive;
        }
    }
}
