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
public class CmsWebSiteConfig implements Serializable {

    public static final String module = CmsWebSiteConfig.class.getName();
    
    private static final CmsWebSiteConfig HARD_DEFAULT = new CmsWebSiteConfig(null);
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
    
    private final String webSiteId;
    
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
    protected CmsWebSiteConfig(String webSiteId) {
        this.webSiteId = webSiteId;
        
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
    protected CmsWebSiteConfig(ExtWebappInfo extWebappInfo, ConfigReader config, CmsWebSiteConfig defaultConfig) {
        String webSiteId = (extWebappInfo != null) ? extWebappInfo.getWebSiteId() : (String) config.getRaw("webSiteId");
        if (webSiteId != null && webSiteId.isEmpty()) webSiteId = null;
        this.webSiteId = webSiteId;
        
        this.allowPreviewMode = config.getBoolean("cmsAllowPreviewMode", defaultConfig.isAllowPreviewMode());
        this.previewModeParamName = config.getString("cmsPreviewModeParamName", defaultConfig.getPreviewModeParamName());
        
        String defaultServletPath = config.getServletPath("cmsDefaultServletPath", null);
        if (UtilValidate.isEmpty(defaultServletPath)) {
            if (extWebappInfo != null) {
                defaultServletPath = CmsControlUtil.normalizeServletPath(extWebappInfo.getControlServletMapping());
                if (UtilValidate.isEmpty(defaultServletPath)) {
                    Debug.logWarning("Cms: Website '" + getWebSiteId() + "': Could not determine a default value for cmsDefaultServletPath (web.xml)"
                            + " - control servlet not found; using fallback '" + defaultConfig.getDefaultServletPath() 
                            + "' as default - please configure cmsDefaultServletPath or check the webapp configuration and ensure the webapp is loaded"
                            + " and ControlServlet is properly configured in web.xml", module);
                    defaultServletPath = defaultConfig.getDefaultServletPath();
                }
            } else {
                Debug.logWarning("Cms: Website '" + getWebSiteId() + "': Could not determine a default value for cmsDefaultServletPath (web.xml)"
                        + " - webapp descriptors are missing (ExtWebappInfo - can't determine control servlet mapping as default); using fallback '" + defaultConfig.getDefaultServletPath() 
                        + "' as default - please configure cmsDefaultServletPath or check the webapp configuration and ensure the webapp is loaded", module);
                defaultServletPath = defaultConfig.getDefaultServletPath();
            }
        }
        this.defaultServletPath = defaultServletPath;
        this.defaultSourceServletPath = config.getServletPath("cmsDefaultSourceServletPath", defaultServletPath);
        this.defaultForwardServletPath = config.getServletPath("cmsDefaultForwardServletPath", defaultServletPath);
        this.defaultTargetServletPath = config.getServletPath("cmsDefaultTargetServletPath", defaultServletPath);
        this.requestServletPath = config.getServletPath("cmsRequestServletPath", defaultServletPath);

        this.setResponseBrowserNoCacheCmsPage = config.getBoolean("cmsSetResponseBrowserNoCacheCmsPage", defaultConfig.isSetResponseBrowserNoCacheCmsPage());
        this.setResponseBrowserNoCacheScreen = config.getBoolean("cmsSetResponseBrowserNoCacheScreen", defaultConfig.isSetResponseBrowserNoCacheScreen());
        this.setResponseBrowserNoCache = config.getBoolean("cmsSetResponseBrowserNoCache", defaultConfig.isSetResponseBrowserNoCache());
        
        this.alwaysUseDefaultForwardServletPath = config.getBoolean("cmsAlwaysUseDefaultForwardServletPath", defaultConfig.isAlwaysUseDefaultForwardServletPath());
        this.defaultForwardExtraPathInfo = config.getBoolean("cmsDefaultForwardExtraPathInfo", defaultConfig.getDefaultForwardExtraPathInfo());
        this.defaultSourceFromContextRoot = config.getBoolean("cmsDefaultSourceFromContextRoot", defaultConfig.getDefaultSourceFromContextRoot());
        this.defaultForwardFromContextRoot = config.getBoolean("cmsDefaultForwardFromContextRoot", defaultConfig.getDefaultForwardFromContextRoot());
        
        boolean useDefaultCmsPage = config.getBoolean("cmsUseDefaultCmsPage", defaultConfig.isUseDefaultCmsPage());
        String defaultCmsPageId = config.getString("cmsDefaultCmsPageId", defaultConfig.getDefaultCmsPageId());
        if (useDefaultCmsPage && UtilValidate.isEmpty(defaultCmsPageId)) {
            useDefaultCmsPage = false;
            defaultCmsPageId = null;
            Debug.logWarning("Cms: Website '" + getWebSiteId() + "': Default CMS page fallback was enabled in webapp config (cmsUseDefaultCmsPage), "
                    + "but no default CMS page specified (cmsDefaultCmsPageId); will treat as disabled", module); 
        }
        this.useDefaultCmsPage = useDefaultCmsPage;
        this.defaultCmsPageId = defaultCmsPageId;

        this.primaryPathFromContextRootDefault = config.getBoolean("cmsPrimaryPathFromContextRootDefault", defaultConfig.getPrimaryPathFromContextRootDefault());
        this.applyPrimaryPathFromContextRootDefaultAtStorage = config.getBoolean("applyPrimaryPathFromContextRootDefaultAtStorage", defaultConfig.isApplyPrimaryPathFromContextRootDefaultAtStorage());

        this.controlRootAlias = config.getBoolean("cmsControlRootAlias", defaultConfig.isControlRootAlias());
        
        this.mappingsIndexableDefault = config.getBoolean("cmsMappingsIndexableDefault", defaultConfig.getMappingsIndexableDefault());
    }


    
    /*
     * Factory methods (safe, do not throw exceptions)
     */
    
    public static CmsWebSiteConfig fromContextParams(ExtWebappInfo extWebappInfo, Map<String, ?> contextParams) {
        return new CmsWebSiteConfig(extWebappInfo, ConfigReader.fromContextParams(contextParams), getDefault());
    }
    
    public static CmsWebSiteConfig fromContextParams(ExtWebappInfo extWebappInfo) {
        return fromContextParams(extWebappInfo, extWebappInfo.getContextParams());
    }
    
    public static CmsWebSiteConfig fromContextParams(String webSiteId) {
        ExtWebappInfo extWebappInfo = readExtWebappInfoSafe(webSiteId);
        if (extWebappInfo == null) {
            Debug.logWarning("Cms: Website '" + webSiteId 
                    + "': no webapp context-params available (could not determine core webapp descriptors (ExtWebappInfo)), will use pure default config", module);
            return new CmsWebSiteConfig(webSiteId);
        } else {
            return fromContextParams(extWebappInfo, extWebappInfo.getContextParams());
        }
    }
    
    public static CmsWebSiteConfig fromServletContext(ExtWebappInfo extWebappInfo, ServletContext servletContext) {
        return new CmsWebSiteConfig(extWebappInfo, ConfigReader.fromServletContext(servletContext), getDefault());
    }
    
    public static CmsWebSiteConfig fromServletContext(String webSiteId, ServletContext servletContext) {
        return new CmsWebSiteConfig(readExtWebappInfoSafe(webSiteId), ConfigReader.fromServletContext(servletContext), getDefault());
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
        return new CmsWebSiteConfig(webSiteId);
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
        ConfigReader src = ConfigReader.fromProperties(resource, propNamePrefix);
        return new CmsWebSiteConfig(null, src, defaultConfig);
    }
    
    
    /*
     * Settings
     */

    /**
     * The webSiteId, extracted from the web.xml context-params specifically.
     */
    public String getWebSiteId() {
        return webSiteId;
    }
   
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
        if (includeWebSiteId) map.put("webSiteId", webSiteId);
        
        for(Field field : getConfigFields()) {
            try {
                map.put(field.getName(), field.get(this));
            } catch (Exception e) {
                Debug.logError(e, "Cms: Website '" + webSiteId 
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
                Debug.logError(e, "Cms: Website '" + webSiteId 
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

    /**
     * Config reader, for CmsWebSiteConfig constructor.
     */
    public static abstract class ConfigReader implements Serializable {
        public static final String CONTEXT_PARAM_PREFIX = "cms";

        
        public static ConfigReader fromContextParams(Map<String, ?> contextParams) {
            return new PrefixMapConfigReader(contextParams, CONTEXT_PARAM_PREFIX);
        }
        
        public static ConfigReader fromServletContext(ServletContext servletContext) {
            return new PrefixMapConfigReader(ServletUtil.getContextParamsMapAdapter(servletContext), CONTEXT_PARAM_PREFIX);
        }
        
        public static ConfigReader fromProperties(String resource, String propNamePrefix) {
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
        
        public abstract Object get(String name);
        public abstract Object getRaw(String name); // get with no prefix (bypass)
        
        public Boolean getBoolean(String paramName, Boolean defValue) {
            Object value = get(paramName);
            return UtilMisc.booleanValueVersatile(value, defValue);
        }
        
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
        
        public String getServletPath(String paramName, String defValue) {
            String value = (String) get(paramName); // must be string
            return CmsControlUtil.normalizeServletPath(UtilValidate.isNotEmpty(value) ? value : defValue);
        }
    
    }

}