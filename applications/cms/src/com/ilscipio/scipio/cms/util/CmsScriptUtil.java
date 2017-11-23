package com.ilscipio.scipio.cms.util;

import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.entity.Delegator;

import com.ilscipio.scipio.cms.content.CmsPageContext;
import com.ilscipio.scipio.cms.template.CmsRenderUtil;
import com.ilscipio.scipio.cms.template.CmsScriptTemplate;

/**
 * Cms script utilities, available to CMS script templates and any other code.
 * <p>
 * Analogous to GroovyUtil/ScriptUtil/WidgetScriptUtil in framework.
 */
public abstract class CmsScriptUtil {

    private static final String module = CmsScriptUtil.class.getName();
    
    protected CmsScriptUtil() {
    }

    public static Object runCmsScriptByName(String name, Map<String, Object> context) throws Exception {
        return runCmsScriptByName((Delegator) context.get("delegator"), name, null, isUseCache(context), context);
    }
    
    // TODO? private because currently no webSiteId on CmsScriptTemplate
    static Object runCmsScriptByName(String name, String webSiteId, Map<String, Object> context) throws Exception {
        return runCmsScriptByName((Delegator) context.get("delegator"), name, webSiteId, isUseCache(context), context);
    }
    
    public static Object runCmsScriptByName(Delegator delegator, String name, boolean useCache, Map<String, Object> context) throws Exception {
        return runCmsScriptByName(delegator, name, null, useCache, context);
    }
    
    // TODO? private because currently no webSiteId on CmsScriptTemplate
    static Object runCmsScriptByName(Delegator delegator, String name, String webSiteId, boolean useCache, Map<String, Object> context) throws Exception {
        CmsScriptTemplate scriptTemplate = CmsScriptTemplate.getWorker().findByName(delegator, name, webSiteId, true, useCache, (HttpServletRequest) context.get("request"));
        if (scriptTemplate == null) {
            throw new IllegalArgumentException("Could not locate CMS script with name: " + name);
        }
        return runCmsScript(scriptTemplate, context);
    }
    
    public static Object runCmsScriptById(String id, Map<String, Object> context) throws Exception {
        return runCmsScriptById((Delegator) context.get("delegator"), id, isUseCache(context), context);
    }
    
    public static Object runCmsScriptById(Delegator delegator, String id, boolean useCache, Map<String, Object> context) throws Exception {
        CmsScriptTemplate scriptTemplate = CmsScriptTemplate.getWorker().findById(delegator, id, useCache, (HttpServletRequest) context.get("request"));
        if (scriptTemplate == null) {
            throw new IllegalArgumentException("Could not locate CMS script with id: " + id);
        }
        return runCmsScript(scriptTemplate, context);
    }
    
    public static Object runCmsScript(CmsScriptTemplate scriptTemplate, Map<String, Object> context) throws Exception {
        return scriptTemplate.getExecutor().execute(context);
    }
    
    public static boolean isUseCache(Map<String, Object> context) {
        CmsPageContext pageContext = CmsRenderUtil.getPageContext(context);
        // ONLY USE CACHE IF LIVE MODE
        if (pageContext != null) return !pageContext.isPreview();
        return false;
    }

}
