package com.ilscipio.scipio.cms.control;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
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
public class CmsWebSiteConfig {

    public static final String module = CmsWebSiteConfig.class.getName();
    
    private static final CmsWebSiteConfig DEFAULT = new CmsWebSiteConfig(null);
    
    /**
     * 2016: WARN: this is now absolute last resort only; always try to lookup the real one before this!
     * @see #getControlServletPath
     */
    private static final String defaultServletPathDefault = "/control";
    // NOTE: global default is "Y" because this closest matches original code (that did not have primary process mappings)
    private static final String primaryPathFromContextRootDefaultStatic = 
            UtilMisc.indicatorValueVersatile(UtilProperties.getPropertyValue("cms", "cms.primaryPathFromContextRoot.default", "Y"), "Y");
    
    /**
     * The fields on this class that represent configurable web.xml settings.
     */
    private static final List<Field> configFields;
    private static final Set<String> configOptionNames;
    private static final Set<String> prefixedConfigOptionNames;

    static {
        Set<String> excludeFields = UtilMisc.toHashSet("webSiteId");
        ArrayList<Field> fields = new ArrayList<Field>(CmsWebSiteConfig.class.getDeclaredFields().length);
        Set<String> names = new LinkedHashSet<>();
        Set<String> prefixedNames = new LinkedHashSet<>();
        for(Field field : CmsWebSiteConfig.class.getDeclaredFields()) {
            if (excludeFields.contains(field.getName())) continue;
            if (Modifier.isStatic(field.getModifiers())) continue;
            fields.add(field);
            names.add(field.getName());
            prefixedNames.add(toCmsInitParamName(field.getName()));
        }
        fields.trimToSize();
        configFields = Collections.unmodifiableList(fields);
        configOptionNames = Collections.unmodifiableSet(names);
        prefixedConfigOptionNames = Collections.unmodifiableSet(prefixedNames);
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
    
    private final boolean controlRootAlias;
    
    private final boolean defaultIsIndexable;
    
    /**
     * Main constructor. Safe, does not throw exceptions.
     * <p>
     * If extWebappInfo is missing, it will attempt to fill in as much as possible.
     * At current time (2017-12-05), configuration can succeed without ExtWebappInfo as
     * long as <code>cmsDefaultServletPath</code> is set.
     */
    protected CmsWebSiteConfig(ExtWebappInfo extWebappInfo, Map<String, ?> contextParams) {
        String webSiteId = (extWebappInfo != null) ? extWebappInfo.getWebSiteId() : (String) contextParams.get("webSiteId");
        if (webSiteId != null && webSiteId.isEmpty()) webSiteId = null;
        this.webSiteId = webSiteId;
        
        this.allowPreviewMode = getCmsBoolInitParam(contextParams, "cmsAllowPreviewMode", getDefault().isAllowPreviewMode()); // 2016: new
        this.previewModeParamName = getCmsStringInitParam(contextParams, "cmsPreviewModeParamName", getDefault().getPreviewModeParamName());
        
        String defaultServletPath = getCmsServletPathInitParam(contextParams, "cmsDefaultServletPath", null);
        if (UtilValidate.isEmpty(defaultServletPath)) {
            if (extWebappInfo != null) {
                defaultServletPath = CmsControlUtil.normalizeServletPath(extWebappInfo.getControlServletMapping());
                if (UtilValidate.isEmpty(defaultServletPath)) {
                    Debug.logWarning("Cms: Website '" + getWebSiteId() + "': Could not determine a default value for cmsDefaultServletPath (web.xml)"
                            + " - control servlet not found; using fallback '" + getDefault().getDefaultServletPath() 
                            + "' as default - please configure cmsDefaultServletPath or check the webapp configuration and ensure the webapp is loaded"
                            + " and ControlServlet is properly configured in web.xml", module);
                    defaultServletPath = getDefault().getDefaultServletPath();
                }
            } else {
                Debug.logWarning("Cms: Website '" + getWebSiteId() + "': Could not determine a default value for cmsDefaultServletPath (web.xml)"
                        + " - webapp descriptors are missing (ExtWebappInfo - can't determine control servlet mapping as default); using fallback '" + getDefault().getDefaultServletPath() 
                        + "' as default - please configure cmsDefaultServletPath or check the webapp configuration and ensure the webapp is loaded", module);
                defaultServletPath = getDefault().getDefaultServletPath();
            }
        }
        this.defaultServletPath = defaultServletPath;
        this.defaultSourceServletPath = getCmsServletPathInitParam(contextParams, "cmsDefaultSourceServletPath", defaultServletPath);
        this.defaultForwardServletPath = getCmsServletPathInitParam(contextParams, "cmsDefaultForwardServletPath", defaultServletPath);
        this.defaultTargetServletPath = getCmsServletPathInitParam(contextParams, "cmsDefaultTargetServletPath", defaultServletPath);
        this.requestServletPath = getCmsServletPathInitParam(contextParams, "cmsRequestServletPath", defaultServletPath);

        this.setResponseBrowserNoCacheCmsPage = getCmsBoolInitParam(contextParams, "cmsSetResponseBrowserNoCacheCmsPage", getDefault().isSetResponseBrowserNoCacheCmsPage());
        this.setResponseBrowserNoCacheScreen = getCmsBoolInitParam(contextParams, "cmsSetResponseBrowserNoCacheScreen", getDefault().isSetResponseBrowserNoCacheScreen());
        this.setResponseBrowserNoCache = getCmsBoolInitParam(contextParams, "cmsSetResponseBrowserNoCache", getDefault().isSetResponseBrowserNoCache());
        
        this.alwaysUseDefaultForwardServletPath = getCmsBoolInitParam(contextParams, "cmsAlwaysUseDefaultForwardServletPath", getDefault().isAlwaysUseDefaultForwardServletPath());
        this.defaultForwardExtraPathInfo = getCmsBoolInitParam(contextParams, "cmsDefaultForwardExtraPathInfo", getDefault().getDefaultForwardExtraPathInfo());
        this.defaultSourceFromContextRoot = getCmsBoolInitParam(contextParams, "cmsDefaultSourceFromContextRoot", getDefault().getDefaultSourceFromContextRoot());
        this.defaultForwardFromContextRoot = getCmsBoolInitParam(contextParams, "cmsDefaultForwardFromContextRoot", getDefault().getDefaultForwardFromContextRoot());
        
        boolean useDefaultCmsPage = getCmsBoolInitParam(contextParams, "cmsUseDefaultCmsPage", getDefault().isUseDefaultCmsPage());
        String defaultCmsPageId = getCmsStringInitParam(contextParams, "cmsDefaultCmsPageId", getDefault().getDefaultCmsPageId());
        if (useDefaultCmsPage && UtilValidate.isEmpty(defaultCmsPageId)) {
            useDefaultCmsPage = false;
            defaultCmsPageId = null;
            Debug.logWarning("Cms: Website '" + getWebSiteId() + "': Default CMS page fallback was enabled in webapp config (cmsUseDefaultCmsPage), "
                    + "but no default CMS page specified (cmsDefaultCmsPageId); will treat as disabled", module); 
        }
        this.useDefaultCmsPage = useDefaultCmsPage;
        this.defaultCmsPageId = defaultCmsPageId;

        this.primaryPathFromContextRootDefault = getCmsBoolInitParam(contextParams, "cmsPrimaryPathFromContextRootDefault", getDefault().getPrimaryPathFromContextRootDefault());

        this.controlRootAlias = getCmsBoolInitParam(contextParams, "cmsControlRootAlias", getDefault().isControlRootAlias());
        
        this.defaultIsIndexable = getCmsBoolInitParam(contextParams, "cmsDefaultIsIndexable", getDefault().getDefaultIsIndexable());
    }

    /**
     * Default configuration constructor.
     */
    protected CmsWebSiteConfig(String webSiteId) {
        this.webSiteId = webSiteId;
        
        this.allowPreviewMode = false;
        this.previewModeParamName = "cmsPreviewMode";
        
        this.defaultServletPath = defaultServletPathDefault;
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
        
        this.defaultIsIndexable = true;
        
        this.useDefaultCmsPage = false;
        this.defaultCmsPageId = null;
        
        this.primaryPathFromContextRootDefault = UtilMisc.booleanValueVersatile(primaryPathFromContextRootDefaultStatic, false);
        
        this.controlRootAlias = false;
    }
    
    /*
     * Factory methods (safe, do not throw exceptions)
     */
    
    public static CmsWebSiteConfig fromContextParams(ExtWebappInfo extWebappInfo, Map<String, ?> contextParams) {
        return new CmsWebSiteConfig(extWebappInfo, contextParams);
    }
    
    public static CmsWebSiteConfig fromContextParams(ExtWebappInfo extWebappInfo) {
        return new CmsWebSiteConfig(extWebappInfo, extWebappInfo.getContextParams());
    }
    
    public static CmsWebSiteConfig fromContextParams(String webSiteId) {
        ExtWebappInfo extWebappInfo = readExtWebappInfoSafe(webSiteId);
        if (extWebappInfo == null) {
            Debug.logWarning("Cms: Website '" + webSiteId 
                    + "': no webapp context-params available (could not determine core webapp descriptors (ExtWebappInfo)), will use pure default config", module);
            return new CmsWebSiteConfig(webSiteId);
        } else {
            return new CmsWebSiteConfig(extWebappInfo, extWebappInfo.getContextParams());
        }
    }
    
    public static CmsWebSiteConfig fromServletContext(ExtWebappInfo extWebappInfo, ServletContext servletContext) {
        return new CmsWebSiteConfig(extWebappInfo, ServletUtil.getContextParamsMapAdapter(servletContext));
    }
    
    public static CmsWebSiteConfig fromServletContext(String webSiteId, ServletContext servletContext) {
        return fromContextParams(readExtWebappInfoSafe(webSiteId), ServletUtil.getContextParamsMapAdapter(servletContext));
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
    
    public static CmsWebSiteConfig getDefault() {
        return DEFAULT;
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
    
    public String getPrimaryPathFromContextRootDefaultIndicator() {
        return primaryPathFromContextRootDefault ? "Y" : "N";
    }

    /**
     * Returns the cmsControlRootAlias web.xml setting; does NOT contain
     * the fallback logic - use {@link CmsWebSiteInfo#isControlRootAlias()} for that.
     */
    public boolean isControlRootAlias() {
        return controlRootAlias;
    }

    public boolean getDefaultIsIndexable() {
        return defaultIsIndexable;
    }
    
    /**
     * Returns the web.xml config option names, excluding the "cms" prefix, and the first
     * letter is lowercase.
     */
    public static Set<String> getConfigOptionNames() {
        return configOptionNames;
    }
    
    /**
     * Returns the web.xml config option names, include the "cms" prefix and casing.
     */
    public static Set<String> getPrefixedConfigOptionNames() {
        return prefixedConfigOptionNames;
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

    
    /*
     * Helpers
     */

    public static String toCmsInitParamName(String paramName) {
        if (paramName.startsWith("cms")) {
            return paramName;
        } else {
            return "cms" + paramName.substring(0, 1).toUpperCase() + paramName.substring(1);
        }
    }
    
    public static Boolean getCmsBoolInitParam(Map<String, ?> contextParams, String paramName, Boolean defValue) {
        if (contextParams == null) return defValue;
        Object value = contextParams.get(toCmsInitParamName(paramName));
        return UtilMisc.booleanValueVersatile(value, defValue);
    }
    
    public static String getCmsStringInitParam(Map<String, ?> contextParams, String paramName, String defValue) {
        if (contextParams == null) return defValue;
        String value = (String) contextParams.get(toCmsInitParamName(paramName));
        return UtilValidate.isNotEmpty(value) ? value : defValue;
    }
    
    public static String getCmsServletPathInitParam(Map<String, ?> contextParams, String paramName, String defValue) {
        if (contextParams == null) return defValue;
        String value = (String) contextParams.get(toCmsInitParamName(paramName));
        return CmsControlUtil.normalizeServletPath(UtilValidate.isNotEmpty(value) ? value : defValue);
    }

}