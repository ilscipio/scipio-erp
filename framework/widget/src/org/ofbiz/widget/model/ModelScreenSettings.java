package org.ofbiz.widget.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.widget.model.ScreenFallback.FlexibleScreenFallbackSettings;
import org.ofbiz.widget.model.ScreenFallback.SimpleFlexibleScreenFallbackSettings;
import org.w3c.dom.Element;

/**
 * SCIPIO: Common screen settings and settings include directives.
 */
@SuppressWarnings("serial")
public class ModelScreenSettings extends ModelWidget {

    public static final String module = ModelScreenSettings.class.getName();

    public static final String DEFAULT_SETTINGS_NAME = "default-settings";

    private static final DecoratorScreenSettings defaultDecoratorScreenSettings = new DecoratorScreenSettings();
    
    protected final Boolean active;
    protected final AutoIncludeSettings autoIncludeSettings;
    protected final DecoratorScreenSettings decoratorScreenSettings;
    protected final RenderInitSettings renderInitSettings;
    protected final String location;
    
    public ModelScreenSettings(String name, Boolean active, String sourceLocation) {
        super(name);
        this.location = sourceLocation;
        this.active = active;
        this.autoIncludeSettings = null;
        this.decoratorScreenSettings = defaultDecoratorScreenSettings;
        this.renderInitSettings = null;
    }
    
    public ModelScreenSettings(Element settingsElement, String sourceLocation) {
        super(settingsElement);
        this.location = sourceLocation;
        List<? extends Element> childElementList;
        
        childElementList = UtilXml.childElementList(settingsElement, "auto-include-settings");
        if (UtilValidate.isNotEmpty(childElementList)) {
            this.autoIncludeSettings = new AutoIncludeSettings(childElementList.get(0), sourceLocation);
        } else {
            this.autoIncludeSettings = null;
        }  
        
        childElementList = UtilXml.childElementList(settingsElement, "decorator-screen-settings");
        if (UtilValidate.isNotEmpty(childElementList)) {
            this.decoratorScreenSettings = new DecoratorScreenSettings(childElementList.get(0), sourceLocation);
        } else {
            this.decoratorScreenSettings = defaultDecoratorScreenSettings;
        }  
        
        childElementList = UtilXml.childElementList(settingsElement, "render-init");
        if (UtilValidate.isNotEmpty(childElementList)) {
            this.renderInitSettings = new RenderInitSettings(this, childElementList.get(0), sourceLocation);
        } else {
            this.renderInitSettings = null;
        }  
        this.active = UtilMisc.booleanValue(settingsElement.getAttribute("active"));
    }
    
    // Copy constructor with overrides
    public ModelScreenSettings(ModelScreenSettings existing, String name, Boolean active) {
        super(UtilValidate.isNotEmpty(name) ? name : existing.getName());
        this.location = existing.location;
        if (active != null) {
            this.active = active;
        } else {
            this.active = existing.active;
        }
        this.autoIncludeSettings = (existing.autoIncludeSettings != null) ? existing.autoIncludeSettings : null;
        this.decoratorScreenSettings = new DecoratorScreenSettings(existing.decoratorScreenSettings);
        this.renderInitSettings = (existing.renderInitSettings != null) ? existing.renderInitSettings : null;
    }
    
    // Merge constructor
    public ModelScreenSettings(ModelScreenSettings existing, ModelScreenSettings override) {
        super(override.getName());
        this.location = override.location;
        if (override.active != null) {
            this.active = override.active;
        } else {
            this.active = existing.active;
        }
        this.decoratorScreenSettings = new DecoratorScreenSettings(existing.decoratorScreenSettings,
                override.decoratorScreenSettings);
        if (override.autoIncludeSettings != null) {
            if (existing.autoIncludeSettings != null) {
                this.autoIncludeSettings = new AutoIncludeSettings(existing.autoIncludeSettings,
                        override.autoIncludeSettings); 
            } else {
                this.autoIncludeSettings = override.autoIncludeSettings; 
            }
        } else {
            if (existing.autoIncludeSettings != null) {
                this.autoIncludeSettings = existing.autoIncludeSettings; 
            } else {
                this.autoIncludeSettings = null;
            }
        }
        if (override.renderInitSettings != null) {
            if (existing.renderInitSettings != null) {
                this.renderInitSettings = new RenderInitSettings(existing.renderInitSettings,
                        override.renderInitSettings); 
            } else {
                this.renderInitSettings = override.renderInitSettings; 
            }
        } else {
            if (existing.renderInitSettings != null) {
                this.renderInitSettings = existing.renderInitSettings; 
            } else {
                this.renderInitSettings = null;
            }
        }
    }
    
    public String getLocation() {
        return location;
    }
    
    public Boolean getActive() {
        return active;
    }

    public boolean isActive() {
        return !Boolean.FALSE.equals(active); // default is true
    }

    /**
     * Returns the auto-include settings or null if none defined.
     */
    public AutoIncludeSettings getAutoIncludeSettings() {
        return autoIncludeSettings;
    }

    public DecoratorScreenSettings getDecoratorScreenSettings() {
        return decoratorScreenSettings;
    }

    public RenderInitSettings getRenderInitSettings() {
        return renderInitSettings;
    }
    
    public List<ModelAction> getLocalRenderInitActions() {
        RenderInitSettings renderInitSettings = this.getRenderInitSettings();
        if(renderInitSettings == null) {
            return null;
        }
        RenderInitSettings.LocalSettings localSettings = renderInitSettings.getLocalSettings();
        if (localSettings == null) {
            return null;
        }
        return localSettings.getActions();
    }

    @Override
    public void accept(ModelWidgetVisitor visitor) throws Exception {
        // TODO Auto-generated method stub
    }

    public static class IncludeSettings implements Serializable {
        protected final String name;
        protected final String location;
        protected final String asName;
        protected final Boolean active;
        
        public IncludeSettings(String name, String location, String asName, Boolean active) {
            this.name = name;
            this.location = location;
            this.asName = asName;
            this.active = active;
        }

        public IncludeSettings(Element inclSettingsElement, String sourceLocation) {
            this.name = inclSettingsElement.getAttribute("name");
            if (!inclSettingsElement.getAttribute("location").isEmpty()) {
                this.location = inclSettingsElement.getAttribute("location");
            } else {
                this.location = sourceLocation;
            }
            this.asName = inclSettingsElement.getAttribute("as-name");
            this.active = UtilMisc.booleanValue(inclSettingsElement.getAttribute("active"));
        }
        
        public String getName() {
            return name;
        }
        
        public String getLocation() {
            return location;
        }
        
        public String getAsName() {
            return asName;
        }
        
        public Boolean getActive() {
            return active;
        }
        
        public ModelScreenSettings getSettings() {
            try {
                ModelScreens screen = ScreenFactory.getScreensFromLocation(location);
                ModelScreenSettings settings = screen.getSettings(name);
                if (settings != null) {
                    settings = new ModelScreenSettings(settings, asName, active);
                }
                return settings;
            } catch (Exception e) {
                Debug.logError(e, "Error including screen settings [" + name + "] at location [" + location + "]", module);
            }
            return null;
        }
        
        public ModelScreenSettings getSettingsAlways() throws IllegalArgumentException {
            try {
                ModelScreens screen = ScreenFactory.getScreensFromLocation(location);
                ModelScreenSettings settings = screen.getSettings(name);
                if (settings == null) {
                    throw new IllegalArgumentException("Settings not found with name [" + name + "] in file [" + location + "]");
                }
                return new ModelScreenSettings(settings, asName, active);
            } catch (Exception e) {
                throw new IllegalArgumentException("Error including screen settings [" + name + "] at location [" + location + "]", e);
            }
        }
    }

    public static class DecoratorScreenSettings implements Serializable {
        
        private static final FlexibleScreenFallbackSettings defaultDefaultDecoratorFallbackSettings = 
                new SimpleFlexibleScreenFallbackSettings("", "", null);
        
        protected final FlexibleScreenFallbackSettings defaultDecoratorFallbackSettings;
        
        public DecoratorScreenSettings(Element decSettingsElement, String sourceLocation) {
            this.defaultDecoratorFallbackSettings = new SimpleFlexibleScreenFallbackSettings(
                    decSettingsElement.getAttribute("default-fallback-name"),
                    decSettingsElement.getAttribute("default-fallback-location"),
                    UtilMisc.booleanValue(decSettingsElement.getAttribute("fallback-if-empty"))
                );
        }
        
        // Copy constructor
        public DecoratorScreenSettings(DecoratorScreenSettings existing) {
            this.defaultDecoratorFallbackSettings = existing.defaultDecoratorFallbackSettings;
        }
        
        // Merge constructor
        public DecoratorScreenSettings(DecoratorScreenSettings existing, DecoratorScreenSettings override) {
            this.defaultDecoratorFallbackSettings = new SimpleFlexibleScreenFallbackSettings(
                    existing.defaultDecoratorFallbackSettings,
                    override.defaultDecoratorFallbackSettings);
        }
        
        public DecoratorScreenSettings() {
            this.defaultDecoratorFallbackSettings = defaultDefaultDecoratorFallbackSettings;
        }
        
        public FlexibleScreenFallbackSettings getDefaultDecoratorFallbackSettings() {
            return defaultDecoratorFallbackSettings;
        }
    }
    
    public static class AutoIncludeSettings implements Serializable {
      
        private static final Pattern defaultFilePattern = Pattern.compile("^.*Screens\\.xml$");
        
        protected final Pattern filePattern;
        
        protected final List<IncludeSettings> includeSettingsList;
        protected final List<ModelScreenSettings> settingsList;
        
        public AutoIncludeSettings(Element autoSettingsElement, String sourceLocation) {
            if (!autoSettingsElement.getAttribute("file-pattern").isEmpty()) {
                this.filePattern = Pattern.compile(autoSettingsElement.getAttribute("file-pattern"));
            } else {
                this.filePattern = null;
            }
            
            ArrayList<IncludeSettings> includeSettingsList = new ArrayList<>();
            ArrayList<ModelScreenSettings> settingsList = new ArrayList<>();
            
            List<? extends Element> childElements = UtilXml.childElementList(autoSettingsElement, "include-settings");
            for (Element childElement: childElements) {
                IncludeSettings includeSettings = new IncludeSettings(childElement, sourceLocation);
                includeSettingsList.add(includeSettings);
            }
            
            childElements = UtilXml.childElementList(autoSettingsElement, "screen-settings");
            for (Element childElement: childElements) {
                ModelScreenSettings settings = new ModelScreenSettings(childElement, sourceLocation);
                settingsList.add(settings);
            }
            
            includeSettingsList.trimToSize();
            settingsList.trimToSize();
            this.includeSettingsList = Collections.unmodifiableList(includeSettingsList);
            this.settingsList = Collections.unmodifiableList(settingsList);
        }
        
        // Copy constructor
        public AutoIncludeSettings(AutoIncludeSettings existing) {
            // NOTE: all the members are final and read-only, so there's no point deep-copying
            this.filePattern = existing.filePattern;
            this.includeSettingsList = existing.includeSettingsList;
            this.settingsList = existing.settingsList;
        }
        
        // Merge constructor
        public AutoIncludeSettings(AutoIncludeSettings existing, AutoIncludeSettings override) {
            if (override.filePattern != null) {
                this.filePattern = override.filePattern;
            } else {
                this.filePattern = existing.filePattern;
            }
            ArrayList<IncludeSettings> includeSettingsList = new ArrayList<>();
            ArrayList<ModelScreenSettings> settingsList = new ArrayList<>();
            includeSettingsList.addAll(existing.includeSettingsList);
            includeSettingsList.addAll(override.includeSettingsList);
            settingsList.addAll(existing.settingsList);
            settingsList.addAll(override.settingsList);
            includeSettingsList.trimToSize();
            settingsList.trimToSize();
            this.includeSettingsList = Collections.unmodifiableList(includeSettingsList);
            this.settingsList = Collections.unmodifiableList(settingsList);
        }
        
        public AutoIncludeSettings() {
            this.filePattern = null;
            ArrayList<IncludeSettings> includeSettingsList = new ArrayList<>();
            ArrayList<ModelScreenSettings> settingsList = new ArrayList<>();
            includeSettingsList.trimToSize();
            settingsList.trimToSize();
            this.includeSettingsList = Collections.unmodifiableList(includeSettingsList);
            this.settingsList = Collections.unmodifiableList(settingsList);
        }

        public Pattern getFilePattern() {
            return filePattern;
        }

        public List<IncludeSettings> getIncludeSettingsList() {
            return includeSettingsList;
        }

        public List<ModelScreenSettings> getSettingsList() {
            return settingsList;
        }
        
        public boolean isEmpty() {
            return includeSettingsList.isEmpty() && settingsList.isEmpty();
        }
        
        public boolean appliesTo(String relScreenFileLoc) {
            if (filePattern != null) {
                return filePattern.matcher(relScreenFileLoc).matches();
            } else {
                return defaultFilePattern.matcher(relScreenFileLoc).matches();
            }
        }
        
    }
    
    public static class RenderInitSettings implements Serializable {
        
        protected final LocalSettings localSettings;
        
        public RenderInitSettings(ModelScreenSettings modelScreenSettings, Element autoSettingsElement, String sourceLocation) {
            List<? extends Element> childElements = UtilXml.childElementList(autoSettingsElement, "local");
            if (UtilValidate.isNotEmpty(childElements)) {
                this.localSettings = new LocalSettings(modelScreenSettings, childElements.get(0), sourceLocation);
            } else {
                this.localSettings = new LocalSettings();
            }
        }
        
        // Copy constructor
        public RenderInitSettings(RenderInitSettings existing) {
            this.localSettings = new LocalSettings(existing.localSettings);
        }
        
        // Merge constructor
        public RenderInitSettings(RenderInitSettings existing, RenderInitSettings override) {
            this.localSettings = new LocalSettings(existing.localSettings, override.localSettings);
        }
        
        public RenderInitSettings() {
            this.localSettings = new LocalSettings();
        }
     
        public LocalSettings getLocalSettings() {
            return localSettings;
        }

        public static class LocalSettings implements Serializable {
            
            protected final List<ModelAction> actions;
            
            public LocalSettings(ModelScreenSettings modelScreenSettings, Element autoSettingsElement, String sourceLocation) {
                ArrayList<ModelAction> actions = new ArrayList<>();
                List<? extends Element> childElements = UtilXml.childElementList(autoSettingsElement, "actions");
                for (Element childElement: childElements) {
                    actions.addAll(AbstractModelAction.readSubActions(modelScreenSettings, childElement));
                }
                actions.trimToSize();
                this.actions = Collections.unmodifiableList(actions);
            }
            
            // Copy constructor
            public LocalSettings(LocalSettings existing) {
                // NOTE: all the members are final and read-only, so there's no point deep-copying
                this.actions = existing.actions;
            }
            
            // Merge constructor
            public LocalSettings(LocalSettings existing, LocalSettings override) {
                ArrayList<ModelAction> actions = new ArrayList<>();
                actions.addAll(existing.actions);
                actions.addAll(override.actions);
                actions.trimToSize();
                this.actions = Collections.unmodifiableList(actions);
            }
            
            public LocalSettings() {
                this.actions = Collections.emptyList();
            }

            public List<ModelAction> getActions() {
                return actions;
            }
        }
    }

    @Override
    public String getContainerLocation() {
        return location;
    }
    
    @Override
    public String getWidgetType() {
        return "screen-settings";
    }
    
}
