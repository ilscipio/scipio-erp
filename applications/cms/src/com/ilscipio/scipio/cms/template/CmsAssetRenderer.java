package com.ilscipio.scipio.cms.template;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.content.CmsPageContent;
import com.ilscipio.scipio.cms.content.CmsPageContext;
import com.ilscipio.scipio.cms.template.ftl.CmsAssetDirective;
import freemarker.core.Environment;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.collections.RenderMapStack;
import org.ofbiz.entity.Delegator;

import java.io.StringWriter;
import java.io.Writer;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Java-based CMS asset renderer, alternative to {@link CmsAssetDirective}.
 *
 * <p>SCIPIO: 3.0.0: Added.</p>
 */
public class CmsAssetRenderer {

    protected Map<String, Object> context; // NOTE: If this is not a MapStack, one is automatically create around this map
    protected CmsPageContext pageContext;
    protected CmsPage page; // NOTE: page is actually optional to the render in multiple cases (global def)
    protected CmsPageTemplate pageTemplate;
    protected CmsPageContent pageContent;

    protected String def; // "global"
    protected String webSiteId;
    protected Boolean webSiteOptional;
    protected String namespace;

    protected Map<String, Object> earlyCtxVars;
    protected Map<String, Object> attribs;
    protected Map<String, Object> attrOvrdCtxVars;
    protected Map<String, Object> finalOvrdCtxVars;

    protected String mode;
    protected Writer out;
    protected Environment env; // NOTE: This is unlikely to be very useful here, probably don't use

    public CmsAssetRenderer(CmsPageContext pageContext, CmsPage page, CmsPageContent pageContent) {
        this.pageContext = pageContext;
        this.page = page;
        this.pageContent = pageContent;
    }

    public CmsAssetRenderer(CmsPageContext pageContext, CmsPageTemplate pageTemplate, CmsPageContent pageContent) {
        this.pageContext = pageContext;
        this.pageTemplate = pageTemplate;
        this.pageContent = pageContent;
    }

    public CmsAssetRenderer(CmsPageContext pageContext, CmsPage page) {
        this.pageContext = pageContext;
        this.page = page;
    }

    public CmsAssetRenderer(CmsPageContext pageContext) {
        this.pageContext = pageContext;
    }


    public CmsAssetRenderer(Map<String, Object> context, CmsPage page) {
        this.context = context;
        this.page = page;
    }

    public CmsAssetRenderer(Map<String, Object> context) {
        this.context = context;
    }


    public CmsAssetRenderer() {
    }

    public Map<String, Object> context() {
        return context;
    }

    public CmsAssetRenderer context(Map<String, Object> context) {
        this.context = context;
        return this;
    }

    public CmsPageContext pageContext() {
        return pageContext;
    }

    public CmsAssetRenderer pageContext(CmsPageContext pageContext) {
        this.pageContext = pageContext;
        return this;
    }

    public CmsPage page() {
        return page;
    }

    /**
     * Sets the CMS page for the render, optional for "global" def and generally optional (due to CmsAssetDirective
     * not traditionally requiring it).
     */
    public CmsAssetRenderer page(CmsPage page) {
        this.page = page;
        return this;
    }

    public CmsPageTemplate pageTemplate() {
        return pageTemplate;
    }

    public CmsAssetRenderer pageTemplate(CmsPageTemplate pageTemplate) {
        this.pageTemplate = pageTemplate;
        return this;
    }

    public CmsPageContent pageContent() {
        return pageContent;
    }

    public CmsAssetRenderer pageContent(CmsPageContent pageContent) {
        this.pageContent = pageContent;
        return this;
    }

    public String def() {
        return def;
    }

    public CmsAssetRenderer def(String def) {
        this.def = def;
        return this;
    }

    public CmsAssetRenderer global(boolean global) {
        def(global ? "global" : null);
        return this;
    }

    public String webSiteId() {
        return webSiteId;
    }

    public CmsAssetRenderer webSiteId(String webSiteId) {
        this.webSiteId = webSiteId;
        return this;
    }

    public Boolean webSiteOptional() {
        return webSiteOptional;
    }

    public CmsAssetRenderer webSiteOptional(Boolean webSiteOptional) {
        this.webSiteOptional = webSiteOptional;
        return this;
    }

    public String namespace() {
        return namespace;
    }

    /**
     * Sets namespace, only for "import" (freemarker) render mode.
     */
    public CmsAssetRenderer namespace(String namespace) {
        this.namespace = namespace;
        return this;
    }

    public Map<String, Object> earlyCtxVars() {
        return earlyCtxVars;
    }

    public CmsAssetRenderer earlyCtxVars(Map<String, Object> earlyCtxVars) {
        this.earlyCtxVars = earlyCtxVars;
        return this;
    }

    public Map<String, Object> attribs() {
        return attribs;
    }

    public CmsAssetRenderer attribs(Map<String, Object> attribs) {
        this.attribs = attribs;
        return this;
    }

    public Map<String, Object> attrOvrdCtxVars() {
        return attrOvrdCtxVars;
    }

    public CmsAssetRenderer attrOvrdCtxVars(Map<String, Object> attrOvrdCtxVars) {
        this.attrOvrdCtxVars = attrOvrdCtxVars;
        return this;
    }

    public Map<String, Object> finalOvrdCtxVars() {
        return finalOvrdCtxVars;
    }

    public CmsAssetRenderer finalOvrdCtxVars(Map<String, Object> finalOvrdCtxVars) {
        this.finalOvrdCtxVars = finalOvrdCtxVars;
        return this;
    }

    public String mode() {
        return mode;
    }

    /**
     * Sets render mode, one of: standalone (default), include, include-template
     */
    public CmsAssetRenderer mode(String mode) {
        this.mode = mode;
        return this;
    }

    public Writer out() {
        return out;
    }

    /**
     * Sets the output writer for all calls, only for "standalone" render mode.
     */
    public CmsAssetRenderer out(Writer out) {
        this.out = out;
        return this;
    }

    public Environment env() {
        return env;
    }

    /**
     * Sets Freemarker environment, only for non-"standalone" render modes.
     */
    public CmsAssetRenderer env(Environment env) {
        this.env = env;
        return this;
    }

    public void renderById(String id, String def, Writer out) throws CmsException {
        render(id, null, def, out);
    }

    public void renderById(String id, boolean global, Writer out) throws CmsException {
        render(id, null, global ? "global" : "", out);
    }

    public void renderById(String id, Writer out) throws CmsException {
        render(id, null, null, out);
    }

    public void renderByName(String name, String def, Writer out) throws CmsException {
        render(null, name, def, out);
    }

    public void renderByName(String name, boolean global, Writer out) throws CmsException {
        render(null, name, global ? "global" : "", out);
    }

    public void renderByName(String name, Writer out) throws CmsException {
        render(null, name, null, out);
    }

    /**
     * Renders each asset of a page to a string, by ID.
     *
     * <p>NOTE: You may need to call {@link #global(boolean)} prior to this call depending on how these should
     * be rendered.</p>
     */
    public Map<String, String> renderPageAssetsById() throws CmsException {
        CmsPage page = this.page;
        CmsPageTemplate pageTemplate = this.pageTemplate;
        if (pageTemplate == null && page != null) {
            pageTemplate = page.getTemplate();
        }
        if (pageTemplate == null) {
            throw new IllegalArgumentException("Missing page template");
        }

        Map<String, String> renders = new LinkedHashMap<>();
        for (CmsAssetTemplate assetTemplate : pageTemplate.getAssetTemplates()) {
            String assetId = assetTemplate.getId();
            StringWriter writer = new StringWriter();
            renderById(assetId, writer);
            renders.put(assetId, writer.toString());
        }
        return renders;
    }

    /**
     * Renders each asset of a page to a string, by name.
     *
     * <p>NOTE: You may need to call {@link #global(boolean)} prior to this call depending on how these should
     * be rendered.</p>
     */
    public Map<String, String> renderPageAssetsByName() throws CmsException {
        CmsPage page = this.page;
        CmsPageTemplate pageTemplate = this.pageTemplate;
        if (pageTemplate == null && page != null) {
            pageTemplate = page.getTemplate();
        }
        if (pageTemplate == null) {
            throw new IllegalArgumentException("Missing page template");
        }

        Map<String, String> renders = new LinkedHashMap<>();
        for (CmsAssetTemplate assetTemplate : pageTemplate.getAssetTemplates()) {
            String assetName = assetTemplate.getImportName();
            StringWriter writer = new StringWriter();
            renderByName(assetName, writer);
            renders.put(assetName, writer.toString());
        }
        return renders;
    }

    private void render(String id, String name, String def, Writer out) throws CmsException {
        Map<String, Object> context = this.context;
        CmsPageContext pageContext = this.pageContext;
        CmsPageContent pageContent = this.pageContent;
        CmsPage page = this.page; // NOTE: null is supported here (due to CmsAssetDirective support)
        if (page == null && pageContent != null) {
            page = pageContent.getPage();
        }
        CmsPageTemplate pageTemplate = this.pageTemplate;
        if (pageTemplate == null && page != null) {
            pageTemplate = page.getTemplate();
        }

        // TODO: REVIEW: Contrary to CmsAssetDirective, typically we need this inverted because presence of CmsPageContext
        // is not indicative of context setup having been done or not
        //boolean newCmsCtx = false;
        if (pageContext == null) {
            if (context == null) {
                throw new IllegalArgumentException("Missing page context or render context");
            }
            pageContext = CmsPageContext.makeFromGenericContext(context);
        }
        if (pageContent == null) {
            pageContent = new CmsPageContent(page);
        }

        // extract the asset name from freemarker template model
        String assetName = name;
        if (UtilValidate.isEmpty(assetName)) {
            assetName = null;
        }
        String assetId = id;
        if (UtilValidate.isEmpty(assetId)) {
            assetId = null;
        }

        Map<String, Object> earlyCtxVars = this.earlyCtxVars;
        Map<String, Object> attribs = this.attribs;
        Map<String, Object> attrOvrdCtxVars = this.attrOvrdCtxVars;
        Map<String, Object> finalOvrdCtxVars = this.finalOvrdCtxVars;

        boolean globalDef = "global".equals(def != null ? def : this.def);

        String webSiteId = this.webSiteId;
        boolean webSiteOptional = !Boolean.FALSE.equals(this.webSiteOptional);

        String modeStr = this.mode;

        CmsAssetDirective.Mode mode;
        if (UtilValidate.isNotEmpty(modeStr)) {
            mode = CmsAssetDirective.Mode.fromName(modeStr);
            if (mode == null) {
                handleError(null, "mode argument must be one of: standalone, include, include-template");
                return;
            }
        } else {
            mode = CmsAssetDirective.Mode.STANDALONE;
        }
        String namespace = null;
        if (mode == CmsAssetDirective.Mode.IMPORT) {
            namespace = this.namespace;
            if (UtilValidate.isEmpty(namespace)) {
                handleError(null, "import mode requires namespace argument");
                return;
            }
        }
        if (assetName == null && assetId == null) {
            handleError(null, "Missing asset name or id");
            return;
        }

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

                if (mode == CmsAssetDirective.Mode.STANDALONE) {
                    assetContent = new CmsPageContent(page);
                    // Set any content with attribs supplied to macro
                    if (attribs != null) {
                        assetContent.putAll(attribs);
                    }
                }
            } else {
                // TODO: REVIEW: We must allow newCmsCtx here because the CmsAssetDirective logic about context preparation
                //  typically will not hold as the page is likely to be already specified.
                //  Should see if this logic should also be extended to CmsAssetDirective in some way.
                //if (newCmsCtx || pageTemplate == null) {
                if (pageTemplate == null) {
                    throw new CmsException("Current rendering context has no existing cmsPageContext or cmsPageTemplate"
                            + " - assets cannot be rendered in non-CMS context in non-global mode (did you mean to use def=\"global\"?)");
                }

                // prepare content for asset
                //Map<String, CmsAssetTemplate> assetTemplates = pageTemplate.getActiveAssetTemplates();

                if (assetName != null) {
                    assetTemplate = pageTemplate.getAssetTemplateByImportName(assetName);
                } else {
                    assetTemplate = pageTemplate.getAssetTemplateById(assetId);
                }

                if (mode == CmsAssetDirective.Mode.STANDALONE) {
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
            handleError(e, "Please check asset reference ("
                    + (assetName != null ? "name: '" + assetName + "'" : "id: '" + assetId + "'")
                    + ") and/or validity of asset template. Please check with your supervisor if all variables have been set correctly");
            return;
        }

        if (assetTemplate != null) {
            try {
                // render asset
                if (mode == CmsAssetDirective.Mode.STANDALONE) {
                    // TODO: per-asset share-scope setting (protectScope here)
                    MapStack<String> stackContext;
                    if (context instanceof MapStack) {
                        stackContext = UtilGenerics.cast(context);
                    } else if (context != null) {
                        stackContext = RenderMapStack.createRenderContext(context);
                    } else {
                        stackContext = RenderMapStack.createEmptyRenderContext();
                    }

                    if (out == null) {
                        out = this.out;
                    }
                    assetTemplate.getRenderer().processAndRender(
                            new CmsAssetTemplate.AssetTemplateRenderer.AtRenderArgs(out, stackContext, pageContext,
                                    page, assetContent, earlyCtxVars, attrOvrdCtxVars, finalOvrdCtxVars, true));
                } else if (mode == CmsAssetDirective.Mode.INCLUDE) {
                    assetTemplate.getRenderer().includeTemplate(env);
                } else if (mode == CmsAssetDirective.Mode.IMPORT) {
                    assetTemplate.getRenderer().importTemplate(env, namespace);
                } else {
                    throw new CmsException("error determining asset mode");
                }
            } catch (Exception e) {
                handleNestedError(e, "Please check asset reference (" + (assetName != null ? "name: '" + assetName + "'" : "id: '" + assetId + "'")
                        + ") and/or validity of asset template. Please check with your supervisor if all variables have been set correctly");
                return;
            }
        } else {
            if (assetName != null) {
                throw new CmsException("Asset with name '" + assetName + "' not found. Please check with your supervisor"
                        + " if all variables have been set correctly.");
            } else {
                throw new CmsException("Asset with id '" + assetId + "' not found. Please check with your supervisor"
                        + " if all variables have been set correctly");
            }
        }
    }

    /**
     * Handles a directive error.
     */
    protected void handleError(Throwable t, String errorMsg) throws CmsException {
        throw new CmsException("Asset rendering failed: " + errorMsg, t);
    }

    /**
     * Handles nested errors.
     */
    protected void handleNestedError(Throwable t, String errorMsg) throws CmsException {
        throw new CmsException("Asset rendering failed: " + errorMsg, t);
    }
}
