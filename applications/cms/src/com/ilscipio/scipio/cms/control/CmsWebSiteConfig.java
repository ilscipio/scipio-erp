package com.ilscipio.scipio.cms.control;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletContext;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.webapp.ExtWebappInfo;
import org.ofbiz.webapp.control.ServletUtil;

/**
 * 2017: Dedicated WebSite config class containing all the web.xml context-params,
 * to avoid the non-stop lookups in the legacy code and make this easier to follow.
 */
@SuppressWarnings("serial")
public abstract class CmsWebSiteConfig implements Serializable {

    public static final String module = CmsWebSiteConfig.class.getName();
    
    private static final CmsWebSiteConfig HARD_DEFAULT = new CmsWebSiteVirtualConfig();
    private static final CmsWebSiteConfig DEFAULT = readDefaultConfig("cms", "webSiteConfig.defaults.", HARD_DEFAULT);

    /**
     * The fields on this class that represent configurable web.xml settings.
     */
    private static final List<Field> configFields;
    private static final Set<String> configOptionNames;

    static {
        Set<String> excludeFields = UtilMisc.toHashSet("webSiteId");
        ArrayList<Field> fields = new ArrayList<Field>(CmsWebSiteConfig.class.getDeclaredFields().length);
        Set<String> names = new LinkedHashSet<>();
        for(Field field : CmsWebSiteConfig.class.getDeclaredFields()) {
            if (excludeFields.contains(field.getName())) continue;
            if (Modifier.isStatic(field.getModifiers())) continue;
            fields.add(field);
            names.add(field.getName());
        }
        fields.trimToSize();
        configFields = Collections.unmodifiableList(fields);
        configOptionNames = Collections.unmodifiableSet(names);
    }
    
    // DEV NOTE: these class member names must match the web.xml option names (except "cms" prefix and first char case)
    private final boolean allowPreviewMode;
    private final String previewModeParamName;

    private final String defaultServletPath;
    private final String defaultSourceServletPath;
    private final String defaultForwardServletPath;
    private final String defaultTargetServletPath;
    private final String requestServletPath;
    
    private final boolean setResponseBrowserNoCacheCmsPage;
    private final boolean setResponseBrowserNoCacheScreen;    
    private final boolean setResponseBrowserNoCache;
    
    private final boolean alwaysUseDefaultForwardServletPath;
    private final boolean defaultForwardExtraPathInfo; 
    private final boolean defaultSourceFromContextRoot;
    private final boolean defaultForwardFromContextRoot;
    
    private final boolean useDefaultCmsPage;
    private final String defaultCmsPageId;

    private final boolean primaryPathFromContextRootDefault;
    private final boolean applyPrimaryPathFromContextRootDefaultAtStorage;
    
    private final boolean controlRootAlias;
    
    private final boolean mappingsIndexableDefault;
    
    /**
     * Hard defaults/fallbacks constructor.
     */
    protected CmsWebSiteConfig() {
        this.allowPreviewMode = false;
        this.previewModeParamName = "cmsPreviewMode";
        
        /*
         * NOTE: /control here is an emergency fallback used when we FAIL to detect the controller.
         */
        this.defaultServletPath = "/control";
        
        this.defaultSourceServletPath = this.defaultServletPath;
        this.defaultForwardServletPath = this.defaultServletPath;
        this.defaultTargetServletPath = this.defaultServletPath;
        this.requestServletPath = this.defaultServletPath;
        
        this.setResponseBrowserNoCacheCmsPage = false;
        this.setResponseBrowserNoCacheScreen = false;
        this.setResponseBrowserNoCache = false;
        
        this.alwaysUseDefaultForwardServletPath = false;
        this.defaultForwardExtraPathInfo = true;
        this.defaultSourceFromContextRoot = true;
        this.defaultForwardFromContextRoot = true;
        
        this.mappingsIndexableDefault = true;
        
        this.useDefaultCmsPage = false;
        this.defaultCmsPageId = null;
        
        this.primaryPathFromContextRootDefault = true;
        // TODO: REVIEW: unclear whether we really want this setting true here...
        this.applyPrimaryPathFromContextRootDefaultAtStorage = true;
        
        this.controlRootAlias = false;
    }
    
    /**
     * web.xml context-param constructor. Safe, does not throw exceptions.
     * May also work with other kind of maps as input.
     * <p>
     * If extWebappInfo is missing, it will attempt to fill in as much as possible.
     * At current time (2017-12-05), configuration can succeed without ExtWebappInfo as
     * long as <code>cmsDefaultServletPath</code> is set.
     */
    protected CmsWebSiteConfig(ConfigReaderBase config, CmsWebSiteConfig defaultConfig) {
        this.allowPreviewMode = config.getBoolean("allowPreviewMode", defaultConfig.isAllowPreviewMode());
        this.previewModeParamName = config.getString("previewModeParamName", defaultConfig.getPreviewModeParamName());

        // NOTE: dedicated getDefaultServletPath call so child class can override
        this.defaultServletPath = config.getDefaultServletPath("defaultServletPath", defaultConfig.getDefaultServletPath()); 
        
        this.defaultSourceServletPath = config.getServletPath("defaultSourceServletPath", defaultServletPath);
        this.defaultForwardServletPath = config.getServletPath("defaultForwardServletPath", defaultServletPath);
        this.defaultTargetServletPath = config.getServletPath("defaultTargetServletPath", defaultServletPath);
        this.requestServletPath = config.getServletPath("requestServletPath", defaultServletPath);

        this.setResponseBrowserNoCacheCmsPage = config.getBoolean("setResponseBrowserNoCacheCmsPage", defaultConfig.isSetResponseBrowserNoCacheCmsPage());
        this.setResponseBrowserNoCacheScreen = config.getBoolean("setResponseBrowserNoCacheScreen", defaultConfig.isSetResponseBrowserNoCacheScreen());
        this.setResponseBrowserNoCache = config.getBoolean("setResponseBrowserNoCache", defaultConfig.isSetResponseBrowserNoCache());
        
        this.alwaysUseDefaultForwardServletPath = config.getBoolean("alwaysUseDefaultForwardServletPath", defaultConfig.isAlwaysUseDefaultForwardServletPath());
        this.defaultForwardExtraPathInfo = config.getBoolean("defaultForwardExtraPathInfo", defaultConfig.getDefaultForwardExtraPathInfo());
        this.defaultSourceFromContextRoot = config.getBoolean("defaultSourceFromContextRoot", defaultConfig.getDefaultSourceFromContextRoot());
        this.defaultForwardFromContextRoot = config.getBoolean("defaultForwardFromContextRoot", defaultConfig.getDefaultForwardFromContextRoot());
        
        boolean useDefaultCmsPage = config.getBoolean("useDefaultCmsPage", defaultConfig.isUseDefaultCmsPage());
        String defaultCmsPageId = config.getString("defaultCmsPageId", defaultConfig.getDefaultCmsPageId());
        if (useDefaultCmsPage && UtilValidate.isEmpty(defaultCmsPageId)) {
            useDefaultCmsPage = false;
            defaultCmsPageId = null;
            // FIXME?: can't print webSiteId here (not yet set in subclass)
            Debug.logWarning("Cms: Default CMS page fallback was enabled in config (useDefaultCmsPage/cmsUseDefaultCmsPage), "
                    + "but no default CMS page specified (defaultCmsPageId/cmsDefaultCmsPageId); will treat as disabled", module); 
        }
        this.useDefaultCmsPage = useDefaultCmsPage;
        this.defaultCmsPageId = defaultCmsPageId;

        this.primaryPathFromContextRootDefault = config.getBoolean("primaryPathFromContextRootDefault", defaultConfig.getPrimaryPathFromContextRootDefault());
        this.applyPrimaryPathFromContextRootDefaultAtStorage = config.getBoolean("applyPrimaryPathFromContextRootDefaultAtStorage", defaultConfig.isApplyPrimaryPathFromContextRootDefaultAtStorage());

        this.controlRootAlias = config.getBoolean("controlRootAlias", defaultConfig.isControlRootAlias());
        
        this.mappingsIndexableDefault = config.getBoolean("mappingsIndexableDefault", defaultConfig.getMappingsIndexableDefault());
    }

    protected abstract String getLogMsgPrefix();
    
    /**
     * An abstract/reusable config.
     */
    public static class CmsWebSiteVirtualConfig extends CmsWebSiteConfig {
        private CmsWebSiteVirtualConfig() {
            super();
        }
        
        private CmsWebSiteVirtualConfig(ConfigReaderBase config, CmsWebSiteConfig defaultConfig) {
            super(config, defaultConfig);
        }
        
        @Override
        public String getWebSiteId() {
            return null;
        }

        @Override
        protected String getLogMsgPrefix() {
            return "Cms: Virtual website config: ";
        }
    }
    
    /**
     * Config for a real website.
     * DEV NOTE (self-note): cannot be subclassed well due to factory method heavy reliance.
     */
    public static class CmsWebSiteInstanceConfig extends CmsWebSiteConfig {
        private final String webSiteId;
        
        private CmsWebSiteInstanceConfig(String webSiteId) {
            super();
            this.webSiteId = webSiteId;
        }
        
        private CmsWebSiteInstanceConfig(String webSiteId, ExtWebappInfo extWebappInfo, ConfigReaderBase config,
                CmsWebSiteConfig defaultConfig) {
            // wrap the config to override the getDefaultServletPath method
            // if this pattern doesn't work later, just move it to a factory meth
            super(config, defaultConfig);
            this.webSiteId = webSiteId;
        }
        
        static CmsWebSiteInstanceConfig fromConfigReader(ExtWebappInfo extWebappInfo, ConfigReaderBase config,
                CmsWebSiteConfig defaultConfig) {
            String webSiteId = getWebSiteId(extWebappInfo, config, defaultConfig);
            if (webSiteId == null) {
                // try to get a hint of which webapp this is
                String localDispatcherName = config.getRawString("localDispatcherName");
                String webappDetailStr = "";
                if (localDispatcherName != null && localDispatcherName.length() > 0) {
                    webappDetailStr += " [localDispatcherName: " + localDispatcherName + "]";
                }
                if (extWebappInfo != null) { // this is a long shot, almost surely will be missing...
                    if (extWebappInfo.getWebappInfo() != null) {
                        String contextRoot = extWebappInfo.getWebappInfo().getContextRoot();
                        if (contextRoot != null && contextRoot.length() > 0) {
                            webappDetailStr += " [contextRoot: " + contextRoot + "]";
                        }
                    }
                }
                Debug.logError("Cms: Tried to read CMS website configuration for webapp"+webappDetailStr+", but the webapp appears"
                        + " to have no webSiteId - this webapp will not work with CMS (webSiteId required; must be set in web.xml)", module);
            }
            config = new WebSiteInstanceConfigReader(webSiteId, extWebappInfo, config, defaultConfig);
            return new CmsWebSiteInstanceConfig(webSiteId, extWebappInfo, config, defaultConfig);
        }
        
        private static String getWebSiteId(ExtWebappInfo extWebappInfo, ConfigReaderBase config, CmsWebSiteConfig defaultConfig) {
            String webSiteId = null;
            if (extWebappInfo != null) {
                webSiteId = extWebappInfo.getWebSiteId();
            }
            if (webSiteId == null) {
                webSiteId = config.getRawString("webSiteId");
            }
            if (webSiteId != null && webSiteId.isEmpty()) webSiteId = null;
            return webSiteId;
        }
        
        /**
         * Wraps config reader and overrides the {@link #getDefaultServletPath} method
         * to provide fallback on control servlet mapping.
         */
        private static class WebSiteInstanceConfigReader extends ConfigReaderWrapper {
            private final String webSiteId;
            private final ExtWebappInfo extWebappInfo;
            private final CmsWebSiteConfig defaultConfig;
            
            WebSiteInstanceConfigReader(String webSiteId, ExtWebappInfo extWebappInfo, ConfigReaderBase config, CmsWebSiteConfig defaultConfig) {
                super(config);
                this.webSiteId = webSiteId;
                this.extWebappInfo = extWebappInfo;
                this.defaultConfig = defaultConfig;
            }

            @Override
            public String getDefaultServletPath(String paramName, String defValue) {
                // SPECIAL OVERRIDE FOR WEBSITE INSTANCES
                String defaultServletPath = getServletPath(paramName, null);
                if (UtilValidate.isEmpty(defaultServletPath)) {
                    if (extWebappInfo != null) {
                        defaultServletPath = CmsControlUtil.normalizeServletPath(extWebappInfo.getControlServletMapping());
                        if (UtilValidate.isEmpty(defaultServletPath)) {
                            Debug.logWarning("Cms: Website '" + webSiteId + "': Could not determine a default value for cmsDefaultServletPath (web.xml)"
                                    + " - control servlet not found; using fallback '" + defaultConfig.getDefaultServletPath() 
                                    + "' as default - please configure cmsDefaultServletPath or check the webapp configuration and ensure the webapp is loaded"
                                    + " and ControlServlet is properly configured in web.xml", module);
                            defaultServletPath = defaultConfig.getDefaultServletPath();
                        } else {
                            Debug.logInfo("Cms: Website '" + webSiteId + "': WebSite config: defaultServletPath not set; using control servlet mapping as defaultServletPath: " + defaultServletPath, module);
                        }
                    } else {
                        Debug.logWarning("Cms: Website '" + webSiteId + "': Could not determine a default value for cmsDefaultServletPath (web.xml)"
                                + " - webapp descriptors are missing (ExtWebappInfo - can't determine control servlet mapping as default); using fallback '" + defaultConfig.getDefaultServletPath() 
                                + "' as default - please configure cmsDefaultServletPath or check the webapp configuration and ensure the webapp is loaded", module);
                        defaultServletPath = defaultConfig.getDefaultServletPath();
                    }
                }
                return defaultServletPath;
            }
        }
        
        @Override
        public String getWebSiteId() {
            return webSiteId;
        }

        @Override
        protected String getLogMsgPrefix() {
            return "Cms: Website '" + getWebSiteId() + "': ";
        }
    }
    
    
    /*
     * Factory methods (safe, do not throw exceptions)
     */
    
    public static CmsWebSiteConfig fromContextParams(ExtWebappInfo extWebappInfo, Map<String, ?> contextParams) {
        return CmsWebSiteInstanceConfig.fromConfigReader(extWebappInfo, ConfigReader.fromContextParams(contextParams), getDefault());
    }
    
    public static CmsWebSiteConfig fromContextParams(ExtWebappInfo extWebappInfo) {
        return fromContextParams(extWebappInfo, extWebappInfo.getContextParams());
    }
    
    public static CmsWebSiteConfig fromContextParams(String webSiteId) {
        ExtWebappInfo extWebappInfo = readExtWebappInfoSafe(webSiteId);
        if (extWebappInfo == null) {
            Debug.logWarning("Cms: Website '" + webSiteId 
                    + "': no webapp context-params available (could not determine core webapp descriptors (ExtWebappInfo)), will use pure default config", module);
            return new CmsWebSiteInstanceConfig(webSiteId);
        } else {
            return fromContextParams(extWebappInfo, extWebappInfo.getContextParams());
        }
    }
    
    public static CmsWebSiteConfig fromServletContext(ExtWebappInfo extWebappInfo, ServletContext servletContext) {
        return CmsWebSiteInstanceConfig.fromConfigReader(extWebappInfo, ConfigReader.fromServletContext(servletContext), getDefault());
    }
    
    public static CmsWebSiteConfig fromServletContext(String webSiteId, ServletContext servletContext) {
        return CmsWebSiteInstanceConfig.fromConfigReader(readExtWebappInfoSafe(webSiteId), ConfigReader.fromServletContext(servletContext), getDefault());
    }

    /**
     * Initializes config from ServletContext, with webSiteId extracted from the context-params
     * and used to lookup ExtWebappInfo.
     */
    public static CmsWebSiteConfig fromServletContext(ServletContext servletContext) {
        return fromServletContext(servletContext.getInitParameter("webSiteId"), servletContext);
    }
    
    /**
     * A copy of the default config ({@link #getDefault()}) but with the webSiteId inserted.
     */
    public static CmsWebSiteConfig fromDefaults(String webSiteId) {
        return new CmsWebSiteInstanceConfig(webSiteId);
    }
    
    private static ExtWebappInfo readExtWebappInfoSafe(String webSiteId) {
        ExtWebappInfo extWebappInfo = null;
        try {
            // NOTE: no caching allowed here!
            extWebappInfo = ExtWebappInfo.fromWebSiteIdNew(webSiteId);
        } catch (Exception e) {
            Debug.logError(e, "Cms: Website '" + webSiteId 
                    + "': error reading core webapp descriptors (ExtWebappInfo) - CMS website configuration may be incomplete: " + e.getMessage(), module);
        }
        return extWebappInfo;
    }
    
    /**
     * Returns default configuration from cms.properties.
     */
    public static CmsWebSiteConfig getDefault() {
        return DEFAULT;
    }
    
    public static CmsWebSiteConfig getHardDefault() {
        return HARD_DEFAULT;
    }
    
    protected static CmsWebSiteConfig readDefaultConfig(String resource, String propNamePrefix, CmsWebSiteConfig defaultConfig) {
        ConfigReaderBase src = ConfigReader.fromProperties(resource, propNamePrefix);
        return new CmsWebSiteVirtualConfig(src, defaultConfig);
    }
    
    
    /*
     * Settings
     */
 
    /**
     * The webSiteId, if applicable, or null.
     */
    public abstract String getWebSiteId();

    public boolean isAllowPreviewMode() {
        return allowPreviewMode;
    }

    public String getPreviewModeParamName() {
        return previewModeParamName;
    }

    public String getDefaultServletPath() {
        return defaultServletPath;
    }

    public String getDefaultSourceServletPath() {
        return defaultSourceServletPath;
    }

    public String getDefaultForwardServletPath() {
        return defaultForwardServletPath;
    }

    public String getDefaultTargetServletPath() {
        return defaultTargetServletPath;
    }
    
    public String getRequestServletPath() {
        return requestServletPath;
    }
    
    public boolean isSetResponseBrowserNoCacheCmsPage() {
        return setResponseBrowserNoCacheCmsPage;
    }

    public boolean isSetResponseBrowserNoCacheScreen() {
        return setResponseBrowserNoCacheScreen;
    }

    public boolean isSetResponseBrowserNoCache() {
        return setResponseBrowserNoCache;
    }

    public boolean isAlwaysUseDefaultForwardServletPath() {
        return alwaysUseDefaultForwardServletPath;
    }

    public boolean getDefaultForwardExtraPathInfo() {
        return defaultForwardExtraPathInfo;
    }

    public boolean getDefaultSourceFromContextRoot() {
        return defaultSourceFromContextRoot;
    }

    public boolean getDefaultForwardFromContextRoot() {
        return defaultForwardFromContextRoot;
    }

    public boolean isUseDefaultCmsPage() {
        return useDefaultCmsPage;
    }

    public String getDefaultCmsPageId() {
        return defaultCmsPageId;
    }

    public boolean getPrimaryPathFromContextRootDefault() {
        return primaryPathFromContextRootDefault;
    }
    
    public boolean isApplyPrimaryPathFromContextRootDefaultAtStorage() {
        return applyPrimaryPathFromContextRootDefaultAtStorage;
    }

    /**
     * Returns the cmsControlRootAlias web.xml setting; does NOT contain
     * the fallback logic - use {@link CmsWebSiteInfo#isControlRootAlias()} for that.
     */
    public boolean isControlRootAlias() {
        return controlRootAlias;
    }

    public boolean getMappingsIndexableDefault() {
        return mappingsIndexableDefault;
    }
    
    /**
     * Returns the web.xml config option names, excluding the "cms" prefix, and the first
     * letter is lowercase.
     */
    public static Set<String> getConfigOptionNames() {
        return configOptionNames;
    }

    
    /*
     * Output
     */

    /**
     * The fields on this class that represent configurable web.xml settings.
     */
    private List<Field> getConfigFields() {
        return configFields;
    }
    
    public Map<String, Object> toMap(Map<String, Object> map, boolean includeWebSiteId) {
        if (includeWebSiteId) map.put("webSiteId", getWebSiteId());
        
        for(Field field : getConfigFields()) {
            try {
                map.put(field.getName(), field.get(this));
            } catch (Exception e) {
                Debug.logError(e, "Cms: Website '" + getWebSiteId() 
                        + "' CmsWebSiteConfig error reading own internal field '" 
                        + field.getName() + "': " + e.getMessage(), module);
            }
        }
 
        return map;
    }
    
    /**
     * For logging/debugging.
     */
    public StringBuilder toStringDesc(boolean includeIntro, String introDelim, String entryPrefix, String entryColon, String delim) {
        StringBuilder sb = new StringBuilder();
        if (includeIntro) {
            if (getWebSiteId() != null) {
                sb.append("CMS website '");
                sb.append(getWebSiteId());
                sb.append("' configuration:");
            } else {
                sb.append("Default CMS website configuration:");
            }
            sb.append(introDelim);
        }
        
        String currDelim = "";
        for(Field field : getConfigFields()) {
            try {
                sb.append(currDelim);
                sb.append(entryPrefix);
                sb.append(field.getName());
                sb.append(entryColon);
                sb.append(field.get(this));
            } catch (Exception e) {
                Debug.logError(e, "Cms: Website '" + getWebSiteId() 
                        + "' CmsWebSiteConfig error reading own internal field '" 
                        + field.getName() + "': " + e.getMessage(), module);
            }
            currDelim = delim;
        }
        return sb;
    }
    
    public StringBuilder toStringDescSingleLine(boolean includeIntro) {
        return toStringDesc(includeIntro, " [", "", "=", ", ").append("]");
    }
    
    public StringBuilder toStringDescMultiLine(boolean includeIntro) {
        return toStringDesc(includeIntro, "\n", "    ", ": ", "\n");
    }

    public interface ConfigReaderBase {
        Object get(String name);
        Object getRaw(String name); // get with no prefix (bypass)
        String getRawString(String name);
        Boolean getBoolean(String paramName, Boolean defValue);
        String getString(String paramName, String defValue);
        String getServletPath(String paramName, String defValue);
        /**
         * Subclass may need to override this for special handling.
         */
        String getDefaultServletPath(String paramName, String defValue);
    }
    
    public static abstract class ConfigReaderWrapper implements ConfigReaderBase {
        protected final ConfigReaderBase config;
        public ConfigReaderWrapper(ConfigReaderBase config) { this.config = config; }
        @Override public Object get(String name) { return config.get(name); }
        @Override public Object getRaw(String name) { return config.getRaw(name); }
        @Override public String getRawString(String name) { return config.getRawString(name); }
        @Override public Boolean getBoolean(String paramName, Boolean defValue) { return config.getBoolean(paramName, defValue); }
        @Override public String getString(String paramName, String defValue) { return config.getString(paramName, defValue); }
        @Override public String getServletPath(String paramName, String defValue) { return config.getServletPath(paramName, defValue); }
        @Override public String getDefaultServletPath(String paramName, String defValue) { return config.getDefaultServletPath(paramName, defValue); }        
    }

    /**
     * Config reader, for CmsWebSiteConfig constructor.
     */
    public static abstract class ConfigReader implements Serializable, ConfigReaderBase {
        public static final String CONTEXT_PARAM_PREFIX = "cms";

        
        public static ConfigReaderBase fromContextParams(Map<String, ?> contextParams) {
            return new PrefixMapConfigReader(contextParams, CONTEXT_PARAM_PREFIX);
        }
        
        public static ConfigReaderBase fromServletContext(ServletContext servletContext) {
            return new PrefixMapConfigReader(ServletUtil.getContextParamsMapAdapter(servletContext), CONTEXT_PARAM_PREFIX);
        }
        
        public static ConfigReaderBase fromProperties(String resource, String propNamePrefix) {
            Map<String, Object> params = new HashMap<>();
            UtilProperties.putPropertiesWithPrefixSuffix(params, UtilProperties.getProperties(resource), 
                    propNamePrefix, null, true, false, false);
            return new MapConfigReader(params);
        }

        public static class MapConfigReader extends ConfigReader {
            protected final Map<String, ?> params;
            public MapConfigReader(Map<String, ?> params) { this.params = params; }
            @Override public Object get(String name) { return params.get(name); }
            @Override public Object getRaw(String name) { return params.get(name); }
        }
        public static class PrefixMapConfigReader extends MapConfigReader {
            protected final String namePrefix;
            public PrefixMapConfigReader(Map<String, ?> params, String namePrefix) {
                super(params);
                this.namePrefix = namePrefix;
            }
            protected String checkPrefixName(String name) {
                if (name.startsWith(namePrefix)) return name;
                else return namePrefix + name.substring(0, 1).toUpperCase() + name.substring(1);
            }
            @Override
            public Object get(String name) {
                return params.get(checkPrefixName(name));
            }
        }
        public static class OptionalPrefixMapConfigReader extends PrefixMapConfigReader {
            public OptionalPrefixMapConfigReader(Map<String, ?> params, String namePrefix) {
                super(params, namePrefix);
            }
            @Override
            public Object get(String name) {
                String prefixedName = checkPrefixName(name);
                if (params.containsKey(prefixedName)) {
                    return params.get(prefixedName);
                } else {
                    return params.get(name);
                }
            }
        }
        
        @Override
        public abstract Object get(String name);
        @Override
        public abstract Object getRaw(String name); // get with no prefix (bypass)
        @Override
        public String getRawString(String name) { return (String) getRaw(name); }

        @Override
        public Boolean getBoolean(String paramName, Boolean defValue) {
            Object value = get(paramName);
            return UtilMisc.booleanValueVersatile(value, defValue);
        }
        
        @Override
        public String getString(String paramName, String defValue) {
            Object value = get(paramName);
            String valueStr;
            if (value instanceof String) {
                valueStr = (String) value;
            } else if (value == null) {
                return defValue;
            } else {
                valueStr = value.toString();
            }
            return UtilValidate.isNotEmpty(valueStr) ? valueStr : defValue;
        }
        
        @Override
        public String getServletPath(String paramName, String defValue) {
            String value = (String) get(paramName); // must be string
            return CmsControlUtil.normalizeServletPath(UtilValidate.isNotEmpty(value) ? value : defValue);
        }
    
        /**
         * Subclass may need to override this for special handling.
         */
        @Override
        public String getDefaultServletPath(String paramName, String defValue) {
            return getServletPath(paramName, defValue);
        }
    }

}