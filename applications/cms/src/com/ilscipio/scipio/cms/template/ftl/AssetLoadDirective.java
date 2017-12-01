package com.ilscipio.scipio.cms.template.ftl;

import java.io.IOException;
import java.io.Serializable;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilRender.RenderExceptionMode;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.entity.Delegator;

import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;
import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.content.CmsPageContent;
import com.ilscipio.scipio.cms.content.CmsPageContext;
import com.ilscipio.scipio.cms.template.CmsAssetTemplate;
import com.ilscipio.scipio.cms.template.CmsAssetTemplate.AssetTemplateRenderer.AtRenderArgs;
import com.ilscipio.scipio.cms.template.CmsPageTemplate;
import com.ilscipio.scipio.cms.template.CmsRenderUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;

/**
 * A FreeMarker directive that inserts the content of an asset. The
 * directive is called with the import name of the asset. The asset is
 * evaluated as FreeMarker template with the content of the respective
 * variable as template model.
 * 
 * The method is called as follows: 
 * <pre>
 * {@code
 * <@asset name="assetImportName" /> 
 * <@asset id="10000" />
 * }
 * </pre>
 * These imply <code>def="import"</code> by default.
 * 
 * 2017-02-08: also now supports rendering arbitrary asset even if not linked to template.
 * <pre>
 * {@code
 * <@asset name="assetName" def="global"/> 
 * <@asset name="assetName" webSiteId="webSiteId" def="global"/>
 * <@asset id="10000" def="global"/>
 * }
 * </pre>
 * <p>
 * Parameters:
 * <ul>
 * <li>def - (import|global, default: import) The scope in which to look for the asset definition</li>
 * <li>mode - (standalone|include|include-template, default: standalone) The rendering and inclusion mode
 *     <ul>
 *       <li>standalone: The asset is rendered in full in its own scope and (Freemarker) environment, with the context pushed
 *          to protected the caller's scope.</li>
 *       <li>include: This SKIPS all context population and instead includes the asset's template
 *          in the caller's environment as if the Freemarker <code>#include</code> directive had been used.</li>
 *       <li>import: This SKIPS all context population and instead includes the asset's template
 *          in the caller's environment as if the Freemarker <code>#import</code> directive had been used.
 *          The "ns" parameter is used to specify the namespace name.</li>
 *     </ul>
 * </li>
 * <li>ns - If mode is "import", this is the namespace name.
 *     DEV NOTE: The name here is "ns" because "as" (which would be clearer) is a reserved word
 *         in Freemarker, and the IDE doesn't like it.</li>
 * <li>name - Name of import (if def="import") or name of asset itself (if def="global")</li>
 * <li>webSiteId - (optional) organizational webSiteId of asset itself (if def="global"), acts as filter
 *                 NOTE: if no webSiteId passed, it preferentially returns the records having no webSiteId.</li>
 * <li>id - Asset template ID (alternative to name)</li>
 * <li>ctxVars - ((map)) Variables to set in context, which will be discarded when invocation returns</li>
 * <li>attribs - ((map)) Map of overriding attributes/content to override the user-specified attributes/content (if any)</li>
 * <li>ovrdCtxVars - ((map)) Variables to set in context after page attributes/content, overriding them, and which will be discarded when invocation returns</li>
 * </ul>
 * <p>
 * Note that in the resulting context, ovrdCtxVars overrides attribs, and attribs override ctxVars.
 * The attribs make the render context slightly more complicated than a call using the scipio utility <code>@render</code> macro.
 * Here the ctxVars are the same as on <code>@render</code>, but with assets, the attributes crush
 * the incoming context vars. In turn, a second set of overriding vars, here ovrdCtxVars, may sometimes be 
 * useful or needed to bypass some of the behaviors and type handling inherent in the attribute processing,
 * which may otherwise affect results.
 * <p>
 * Thread-safe, immutable and global.
 * <p>
 * TODO?: <code>@cmsAsset</code>: possibility to invoke from other renderers could be useful to reuse (with def="global").
 */
public class AssetLoadDirective implements TemplateDirectiveModel, Serializable {

    private static final long serialVersionUID = -2664394439313786601L;

    public static final String module = AssetLoadDirective.class.getName();
    
    private static final AssetLoadDirective instance = new AssetLoadDirective();

    static final RenderExceptionMode assetLiveExceptionMode = CmsRenderUtil.getDirectiveLiveRenderExceptionMode(RenderExceptionMode.valueOfPermissive(UtilProperties.getPropertyValue("cms", 
            "render.live.exception.directive.asset.mode"))); 
    
    static final RenderExceptionMode nestedLiveExceptionMode;
    
    static {
        RenderExceptionMode nested = RenderExceptionMode.valueOfPermissive(UtilProperties.getPropertyValue("cms", 
                "render.live.exception.directive.nested.mode"));
        if (nested == null) nested = assetLiveExceptionMode;
        nestedLiveExceptionMode = nested;
    }
    
    public enum Mode {
        STANDALONE("standalone"),
        INCLUDE("include"),
        IMPORT("import");
        
        private static final Map<String, Mode> nameMap;
        static {
            Map<String, Mode> map = new HashMap<>();
            for(Mode mode : Mode.values()) {
                map.put(mode.name, mode);
            }
            nameMap = map;
        }
        
        private final String name;

        private Mode(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }
        
        public static Mode fromName(String name) {
            return nameMap.get(name);
        }
    }
    
    public AssetLoadDirective() {
        super();
    }

    public static AssetLoadDirective getInstance() {
        return instance;
    }
    
    /* (non-Javadoc)
     * @see freemarker.template.TemplateDirectiveModel#execute(freemarker.core.Environment, java.util.Map, freemarker.template.TemplateModel[], freemarker.template.TemplateDirectiveBody)
     */
    @SuppressWarnings("rawtypes")
    @Override
    public void execute(Environment env, Map paramsUntyped, TemplateModel[] loopVars, TemplateDirectiveBody body)
            throws TemplateException, IOException {
        Writer out = env.getOut();
        
        MapStack<String> context = CmsRenderUtil.getRenderContextAlways(env);
        CmsPageContext pageContext = CmsRenderUtil.getPageContext(context);
        CmsPageContent pageContent = CmsRenderUtil.getTopPageContent(context);
        CmsPageTemplate pageTemplate = CmsRenderUtil.getPageTemplate(context);
        
        boolean newCmsCtx = false;
        if (pageContext == null) {
            newCmsCtx = true;
            pageContext = CmsPageContext.makeFromGenericRequestContext(context);
            if (pageContent == null) {
                pageContent = new CmsPageContent((CmsPage) null);
            }
        }
        
        Map<String, TemplateModel> params = UtilGenerics.checkMap(paramsUntyped);
        // extract the asset name from freemarker template model
        String assetName = TransformUtil.getStringArg(params, "name");
        if (UtilValidate.isEmpty(assetName)) {
            assetName = null;
        }
        String assetId = TransformUtil.getStringArg(params, "id");
        if (UtilValidate.isEmpty(assetId)) {
            assetId = null;
        }
        
        Map<String, Object> ctxVars = TransformUtil.getMapArg(params, "ctxVars", null, false, true);
        Map<String, Object> attribs = TransformUtil.getMapArg(params, "attribs", null, false, true);
        Map<String, Object> ovrdCtxVars = TransformUtil.getMapArg(params, "ovrdCtxVars", null, false, true);

        boolean globalDef = "global".equals(TransformUtil.getStringNonEscapingArg(params, "def"));
        
        String webSiteId = TransformUtil.getStringNonEscapingArg(params, "webSiteId"); 
        boolean webSiteOptional = TransformUtil.getBooleanArg(params, "webSiteOptional", true); 
        
        String modeStr = TransformUtil.getStringNonEscapingArg(params, "mode");
        Mode mode;
        if (UtilValidate.isNotEmpty(modeStr)) {
            mode = Mode.fromName(modeStr);
            if (mode == null) {
                handleError(env, null, "mode argument must be one of: standalone, include, include-template");
                return;
            }
        } else {
            mode = Mode.STANDALONE;
        }
        String namespace = null;
        if (mode == Mode.IMPORT) {
            namespace = TransformUtil.getStringNonEscapingArg(params, "ns");
            if (UtilValidate.isEmpty(namespace)) {
                handleError(env, null, "import mode requires \"ns\" argument as namespace name");
                return;
            }
        }

        if (assetName == null && assetId == null) {
            handleError(env, null, "The name of the asset must be given as parameter \"name\", or the id as parameter \"id\"");
            return;
        } else {
            CmsAssetTemplate assetTemplate;
            CmsPageContent assetContent = null;
            try {
                Delegator delegator = pageContext.getDelegator();
                boolean useCache = !pageContext.isPreview();
                
                if (globalDef) {
                    if (assetName != null) {
                        assetTemplate = CmsAssetTemplate.getWorker().findByName(delegator, assetName, webSiteId, webSiteOptional, useCache, pageContext.getRequest());
                    } else {
                        assetTemplate = CmsAssetTemplate.getWorker().findById(delegator, assetId, useCache, pageContext.getRequest());
                    }
                    
                    if (mode == Mode.STANDALONE) {
                        assetContent = new CmsPageContent(pageContent.getPage());
                        // Set any content with attribs supplied to macro
                        if (attribs != null) {
                            assetContent.putAll(attribs);
                        }
                    }
                } else {
                    if (newCmsCtx || pageTemplate == null) {
                        throw new IllegalStateException("Current rendering context has no existing cmsPageContext or cmsPageTemplate"
                            + " - assets cannot be rendered in non-CMS context in non-global mode (did you mean to use def=\"global\"?)");
                    }
                    
                    // prepare content for asset
                    //Map<String, CmsAssetTemplate> assetTemplates = pageTemplate.getActiveAssetTemplates();
                    
                    if (assetName != null) {
                        assetTemplate = pageTemplate.getAssetTemplateByImportName(assetName);
                    } else {
                        assetTemplate = pageTemplate.getAssetTemplateById(assetId);
                    }
                    
                    if (mode == Mode.STANDALONE) {
                        // NOTE: this creates a shallow copy of the lower map level - important!
                        assetContent = pageContent.getAssetContent(assetName);
                        // Replace any content with attribs supplied to macro
                        if (attribs != null) {
                            assetContent.putAll(attribs);
                        }
                    }
                }
            } catch (Exception e) {
                // DEV NOTE: I don't think this ever gets triggered, it's just here for safety;
                handleError(env, e, "Please check asset reference (" 
                        + (assetName != null ? "name: '" + assetName + "'" : "id: '" + assetId + "'")
                        + ") and/or validity of asset template. Please check with your supervisor if all variables have been set correctly");
                return;
            }
                
            if (assetTemplate != null) {
                try {
                    // render asset
                    if (mode == Mode.STANDALONE) {
                        // currently, ALWAYS push context around asset render call
                        final boolean shareScope = false;
                        assetTemplate.getRenderer().processAndRender(new AtRenderArgs(out, context, assetContent, pageContext, ctxVars, ovrdCtxVars, shareScope, newCmsCtx));
                    } else if (mode == Mode.INCLUDE) {
                        assetTemplate.getRenderer().includeTemplate(env);
                    } else if (mode == Mode.IMPORT) {
                        assetTemplate.getRenderer().importTemplate(env, namespace);
                    } else {
                        throw new CmsException("error determining asset mode");
                    }
                } catch (Exception e) {
                    handleNestedError(env, e, "Please check asset reference (" 
                            + (assetName != null ? "name: '" + assetName + "'" : "id: '" + assetId + "'")
                            + ") and/or validity of asset template. Please check with your supervisor if all variables have been set correctly");
                    return;
                }
            } else {
                if (assetName != null) {
                    handleError(env, null, "Asset with name '" + assetName + "' not found. Please check with your supervisor"
                                + " if all variables have been set correctly.");
                    return;
                } else {
                    handleError(env, null, "Asset with id '" + assetId + "' not found. Please check with your supervisor"
                                + " if all variables have been set correctly");
                    return;
                }
            }
        }
    }
    
    /**
     * Handles a directive error.
     */
    public static boolean handleError(Environment env, Throwable t, String errorMsg) throws CmsException, TemplateException {
        return CmsRenderUtil.handleDirectiveError(env, "Asset rendering failed", t, errorMsg, assetLiveExceptionMode, module);
    }
    
    /**
     * Handles nested errors.
     */
    public static boolean handleNestedError(Environment env, Throwable t, String errorMsg) throws CmsException, TemplateException {
        return CmsRenderUtil.handleDirectiveError(env, "Asset rendering failed", t, errorMsg, nestedLiveExceptionMode, module);
    }
        
}