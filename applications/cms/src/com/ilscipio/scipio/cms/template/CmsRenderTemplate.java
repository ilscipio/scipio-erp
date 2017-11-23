/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/

package com.ilscipio.scipio.cms.template;

import java.io.IOException;
import java.io.Serializable;
import java.io.StringReader;
import java.io.Writer;
import java.net.URL;
import java.rmi.server.UID;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.collections.ResourceBundleMapWrapper;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.base.util.template.ScipioFtlWrappers;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.widget.renderer.ScreenRenderer;
import org.ofbiz.widget.renderer.ScreenStringRenderer;
import org.ofbiz.widget.renderer.macro.MacroScreenRenderer;
import org.ofbiz.widget.renderer.macro.MacroScreenViewHandler;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.content.CmsPageContent;
import com.ilscipio.scipio.cms.content.CmsPageContext;
import com.ilscipio.scipio.cms.data.Preloadable;
import com.ilscipio.scipio.cms.data.Preloadable.AbstractPreloadable;
import com.ilscipio.scipio.cms.util.Optional;

import freemarker.core.Environment;
import freemarker.core.Environment.Namespace;
import freemarker.ext.beans.BeansWrapper;
import freemarker.template.Configuration;
import freemarker.template.Template;

/**
 * Interface for any template meant to be rendered to an output (in contrast
 * to templates such as script templates that execute instead of rendering).
 */
public interface CmsRenderTemplate extends Serializable {

    public static final String module = CmsRenderTemplate.class.getName();
        
    public TemplateRenderer<?> getRenderer();
    
    /**
     * Template renderer nested (or inner) class base.
     * <p>
     * (workaround for multiple inheritance limitations).
     */
    @SuppressWarnings("serial")
    public static abstract class TemplateRenderer<T extends CmsComplexTemplate & CmsRenderTemplate> extends AbstractPreloadable implements Preloadable, Serializable {
        
        // 2017-02: this is now unacceptable: we must use dynamically built one that can
        // depend on theme selection.
//        protected static final ScreenStringRenderer screenStringRenderer;
//        static {
//            ScreenStringRenderer renderer = null;
//            try {
//                renderer = new MacroScreenRenderer(UtilProperties.getPropertyValue("widget", "screen.name"), UtilProperties.getPropertyValue("widget", "screen.screenrenderer"));
//            } catch (Throwable t) {
//                Debug.logError(t, module);
//            }
//            screenStringRenderer = renderer;
//        }
        
        protected static final Configuration fmConfig = makeConfiguration((BeansWrapper) ScipioFtlWrappers.getSystemObjectWrapperFactory().getExtendedWrapper(FreeMarkerWorker.version, "html"));

        public static final Set<String> cmsOfbizCtxSysVarNames = Collections.unmodifiableSet(new HashSet<>(Arrays.asList(new String [] {
                // stock ofbiz+scipio names
                "screens", "globalContext", "nullField", "parameters", "delegator", "dispatcher",
                "security", "locale", "userLogin", "nowTimestamp", 
                "delegator", "dispatcher", "security",
                "org.apache.catalina.jsp_classpath",
                "autoUserLogin",
                "userLogin", "timeZone", "request",
                "response", "session", "application",
                "javaScriptEnabled",
                "sessionAttributes",
                "requestAttributes",
                "JspTaglibs",
                "requestParameters",
                "Application",
                "Request",
                "checkLoginUrl",
                "externalLoginKey",
                "externalKeyParam",
                "eventMessageList",
                "errorMessageList",
                "serviceValidationException",
                "isError",
                "requestMethod",
        })));
        
        public static final Set<String> cmsNewCtxSysVarNames = Collections.unmodifiableSet(new HashSet<>(Arrays.asList(new String [] {
                // new cms names
                "cmsCtxSysVarNames",
                "cmsPageContext",
                "cmsPageContent",
                "cmsContent",
                "cmsPageTemplate",
                "cmsPage",
                "cmsPageId",
                "cmsIsPreview"
        })));
        
        // FIXME: 2016: we need to unhardcode this names list through an Ofbiz patch.
        // for now just hardcode because it's the best way unfortunately...
        // NOTE: this is NOT all... omits very reusable names like "partyGroup" and "person"...
        public static final Set<String> cmsCtxSysVarNames;
        static {
            Set<String> names = new HashSet<>();
            names.addAll(cmsOfbizCtxSysVarNames);
            names.addAll(cmsNewCtxSysVarNames);
            cmsCtxSysVarNames = Collections.unmodifiableSet(names);
        }
        
        // NOTE: Template not serializable!
        protected transient Optional<Template> fmTemplate = null; // NOTE: 2016: Optional is required for thread safety (preload)
        protected final T template;
        
        public TemplateRenderer(T template) {
            super();
            this.template = template;
        }

        /**
         * 2016: Loads ALL this object's content into the current instance.
         * <p>
         * WARN: IMPORTANT: AFTER THIS CALL, 
         * NO FURTHER CALLS ARE ALLOWED TO MODIFY THE INSTANCE IN MEMORY.
         * Essential for thread safety!!!
         */
        @Override
        public void preload(PreloadWorker preloadWorker) {
            super.preload(preloadWorker);
            
            // NOTE: the Freemarker template can't really be made explicitly immutable, but it should be thread-safe anyway.
            this.getFreeMarkerTemplate();
        }

        public static Configuration makeConfiguration(BeansWrapper wrapper) {
            Configuration cfg = FreeMarkerWorker.makeConfiguration(wrapper);
            cfg.setTemplateExceptionHandler(new CmsRenderUtil.CmsTemplateExceptionHandler());
            
            // Get CMS-specific directives
            // SCIPIO: TODO: delegate to FreeMarkerWorker and remove license notice
            // Transforms properties file set up as key=transform name, property=transform class name
            ClassLoader loader = Thread.currentThread().getContextClassLoader();
            Enumeration<URL> resources;
            try {
                resources = loader.getResources("cmsFreemarkerTransforms.properties");
            } catch (IOException e) {
                Debug.logError(e, "Could not load list of cmsFreemarkerTransforms.properties", module);
                throw UtilMisc.initCause(new InternalError(e.getMessage()), e);
            }
            while (resources.hasMoreElements()) {
                URL propertyURL = resources.nextElement();
                Debug.logInfo("loading properties: " + propertyURL, module);
                Properties props = UtilProperties.getProperties(propertyURL);
                if (UtilValidate.isEmpty(props)) {
                    Debug.logError("Unable to locate properties file " + propertyURL, module);
                } else {
                    loadTransforms(loader, props, cfg);
                }
            }

            return cfg;
        }
        
        public static Configuration getDefaultCmsConfig() {
            return fmConfig;
        }
        
        /**
         * Protected helper method.
         * <p>
         * SCIPIO: TODO: delegate to FreeMarkerWorker and remove license notice
         */
        protected static void loadTransforms(ClassLoader loader, Properties props, Configuration config) {
            for (Iterator<Object> i = props.keySet().iterator(); i.hasNext();) {
                String key = (String) i.next();
                String className = props.getProperty(key);
                if (Debug.verboseOn()) {
                    Debug.logVerbose("Adding FTL Transform " + key + " with class " + className, module);
                }
                try {
                    config.setSharedVariable(key, loader.loadClass(className).newInstance());
                } catch (Exception e) {
                    Debug.logError(e, "Could not pre-initialize dynamically loaded class: " + className + ": " + e, module);
                }
            }
        }
        
        public Template getTemplate() {
            return getFreeMarkerTemplate();
        }
        
        protected Template getFreeMarkerTemplate() {
            Optional<Template> fmTemplate = this.fmTemplate;
            if (fmTemplate == null) {
                try {
                    // UID trick, same as used by widgets to generate unique names
                    String name = template.getName() + "_" + new UID().toString();
                    fmTemplate = Optional.ofNullable(new Template(name, new StringReader(template.getTemplateBody()), fmConfig));
                } catch (IOException e) {
                    throw new CmsException("Freemarker template could not be retrieved from database", e);
                }
                this.fmTemplate = fmTemplate;
            }
            return fmTemplate.orElse(null);
        }

        // TODO?: clean up this class, constructors hard to follow (booleans)
        public static class RenderArgs {
            private Writer out;
            private Environment env; // NOTE: optional for standalone render (uses its own environment)
            private MapStack<String> context;
            private CmsPageContent content;
            private CmsPageContext pageContext;
            
            private Map<String, Object> earlyCtxVars; // early ctx vars, similar to ofbiz widget-style context passing
            private Map<String, Object> ovrdCtxVars; // attribute-overriding ctx vars
            
            private boolean skipSystemCtx;
            private boolean systemCtxCmsOnly = false;
            private boolean systemCtxNoPush = false;
            private boolean skipExtraCommonCtx;
            private boolean shareScope;
            
            private boolean newCmsCtx = true; // high-level flag mainly used by subclasses

            public RenderArgs() {
                this.skipSystemCtx = false;
                this.skipExtraCommonCtx = false;
                this.shareScope = false;
            }
            
            public RenderArgs(Writer out, Environment env, MapStack<String> context, CmsPageContent content, CmsPageContext pageContext,
                    Map<String, Object> earlyCtxVars, Map<String, Object> ovrdCtxVars, boolean skipSystemCtx, boolean skipExtraCommonCtx, boolean shareScope) {
                this.out = out;
                setEnv(env);
                this.env = env;
                this.context = context;
                this.content = content;
                this.pageContext = pageContext;
                this.earlyCtxVars = earlyCtxVars;
                this.ovrdCtxVars = ovrdCtxVars;
                this.skipSystemCtx = skipSystemCtx;
                this.skipExtraCommonCtx = skipExtraCommonCtx;
                this.shareScope = shareScope;
            }
            
            public RenderArgs(Writer out, MapStack<String> context, CmsPageContent content, CmsPageContext pageContext,
                    Map<String, Object> earlyCtxVars, Map<String, Object> ovrdCtxVars, boolean shareScope) {
                this(out, null, context, content, pageContext, earlyCtxVars, ovrdCtxVars, false, false, shareScope);
            }

            public RenderArgs(Writer out, MapStack<String> context, CmsPageContent content,
                    CmsPageContext pageContext, boolean shareScope) {
                this(out, null, context, content, pageContext, null, null, false, false, shareScope);
            }
            
            public RenderArgs(Environment env, MapStack<String> context, CmsPageContent content, CmsPageContext pageContext,
                    Map<String, Object> earlyCtxVars, Map<String, Object> ovrdCtxVars, boolean shareScope) {
                this(null, env, context, content, pageContext, earlyCtxVars, ovrdCtxVars, false, false, shareScope);
            }

            public RenderArgs(Environment env, MapStack<String> context, CmsPageContent content,
                    CmsPageContext pageContext, boolean shareScope) {
                this(null, env, context, content, pageContext, null, null, false, false, shareScope);
            }
            
            public Writer getOut() {
                return out;
            }
            public void setOut(Writer out) {
                this.out = out;
            }
            public Environment getEnv() {
                return env;
            }
            public void setEnv(Environment env) {
                this.env = env;
                if (out == null && env != null) {
                    this.out = env.getOut();
                }
            }
            public MapStack<String> getContext() {
                return context;
            }
            public void setContext(MapStack<String> context) {
                this.context = context;
            }
            public CmsPageContent getContent() {
                return content;
            }
            public void setContent(CmsPageContent content) {
                this.content = content;
            }
            public CmsPageContext getPageContext() {
                return pageContext;
            }
            public void setPageContext(CmsPageContext pageContext) {
                this.pageContext = pageContext;
            }
            public Map<String, Object> getEarlyCtxVars() {
                return earlyCtxVars;
            }
            public void setEarlyCtxVars(Map<String, Object> earlyCtxVars) {
                this.earlyCtxVars = earlyCtxVars;
            }
            public Map<String, Object> getOvrdCtxVars() {
                return ovrdCtxVars;
            }
            public void setOvrdCtxVars(Map<String, Object> lateCtxVars) {
                this.ovrdCtxVars = lateCtxVars;
            }
            public boolean isSkipSystemCtx() {
                return skipSystemCtx;
            }
            public void setSkipSystemCtx(boolean skipSystemCtx) {
                this.skipSystemCtx = skipSystemCtx;
            }
            public boolean isSystemCtxCmsOnly() {
                return systemCtxCmsOnly;
            }
            public void setSystemCtxCmsOnly(boolean systemCtxCmsOnly) {
                this.systemCtxCmsOnly = systemCtxCmsOnly;
            }
            public boolean isSystemCtxNoPush() {
                return systemCtxNoPush;
            }
            public void setSystemCtxNoPush(boolean systemCtxNoPush) {
                this.systemCtxNoPush = systemCtxNoPush;
            }
            public boolean isSkipExtraCommonCtx() {
                return skipExtraCommonCtx;
            }
            public void setSkipExtraCommonCtx(boolean skipExtraCommonCtx) {
                this.skipExtraCommonCtx = skipExtraCommonCtx;
            }
            public boolean isShareScope() {
                return shareScope;
            }
            public void setShareScope(boolean shareScope) {
                this.shareScope = shareScope;
            }
            public CmsPage getPage() {
                // workaround for lack of CmsPage property
                return content != null ? content.getPage() : null;
            }
            public boolean isNewCmsCtx() {
                return newCmsCtx;
            }
            public void setNewCmsCtx(boolean newCmsCtx) {
                this.newCmsCtx = newCmsCtx;
            }
        }
        
        /**
         * Merges the content with the template with consideration to the given
         * context information and renders to the passed writer.
         */
        public Object processAndRender(RenderArgs renderArgs) throws CmsException {
            final boolean protectScope = !renderArgs.isShareScope();
            if (protectScope) {
                renderArgs.getContext().push();
            }
            try {
                // Populate context
                populateRenderContext(renderArgs);
                
                // Render template
                renderTemplate(renderArgs.getOut(), renderArgs.getContext());
                
                return null;
            } finally {
                if (protectScope) {
                    renderArgs.getContext().pop();
                }
            }
        }
        
        /**
         * Fully populates the render context as necessary, depending highly on the
         * template type.
         * <p>
         * NOTE: subclass may need to override this (2017-02-20: using the flags instead for now).
         */
        public void populateRenderContext(RenderArgs renderArgs) {
            if (renderArgs.getContent().isImmutable()) { // this makes the error clearer
                throw new IllegalStateException("Tried to populate CMS page directives on immutable CmsPageContent");
            }
            try {
                if (!renderArgs.isSkipSystemCtx()) {
                    populateSystemRenderContext(renderArgs);
                }
                if (!renderArgs.isSkipExtraCommonCtx()) {
                    populateExtraCommonRenderContext(renderArgs);
                }
                populateWidgetRenderContext(renderArgs);
            } catch (Throwable t) {
                throw new CmsException("Error preparing context for template render: " + t.getMessage(), t);
            }
        }
        
        /**
         * Invokes the actual template rendering as a standalone template.
         * Assumes context already prepared.
         */
        public void renderTemplate(Writer out, Map<String, Object> context) throws CmsException {
            try {
                Template template = getFreeMarkerTemplate();
                // 2017-03-20: MUST USE FreeMarkerWorker due to possible environment snafus...
                //template.process(context, out);
                FreeMarkerWorker.renderTemplate(template, context, out);
            } catch (Throwable t) {
                throw new CmsException("Error rendering template: " + t.getMessage(), t);
            }
        }
        
        /**
         * Includes the template as if Freemarker <code>#include</code> directive had been used,
         * without running any context population.
         */
        public void includeTemplate(Environment env) throws CmsException {
            try {
                Template template = getFreeMarkerTemplate();
                env.include(template);
            } catch (Throwable t) {
                throw new CmsException("Error rendering template: " + t.getMessage(), t);
            }
        }
        
        /**
         * Imports the template as if Freemarker <code>#import</code> directive had been used,
         * without running any context population.
         */
        public Namespace importTemplate(Environment env, String namespace) throws CmsException {
            try {
                Template template = getFreeMarkerTemplate();
                return env.importLib(template, namespace);
            } catch (Throwable t) {
                throw new CmsException("Error rendering template: " + t.getMessage(), t);
            }
        }

        /**
         * Populates the core system render context, including new CMS objects considered necessary
         * to the core.
         * <p>
         * Includes all context population duties normally done in widget renderer
         * by MacroScreenViewHandler.render, ScreenRenderer.populateContextForRequest, and 
         * CMS-specific additions.
         * <p>
         * Does NOT contain extras such as common UI label maps - see populateExtraCmsSystemRenderContext.
         */
        protected void populateSystemRenderContext(RenderArgs renderArgs) {
            if (renderArgs.isSystemCtxCmsOnly()) {
                populateContextForRequestCmsOnly(renderArgs.getContext(), null, 
                        renderArgs.getContent(), renderArgs.getPageContext(), getRenderPageTemplate(renderArgs));
            } else {
                populateContextForRequest(renderArgs.getContext(), null, 
                        renderArgs.getContent(), renderArgs.getPageContext(), getRenderPageTemplate(renderArgs));
            }
            
            // IMPORTANT: here we undo the stack push done by populateContextForRequest, and re-do it at the end
            // to include more of our custom variables.
            // NOTE: the stock ofbiz MacroScreenViewHandler does NOT do this, but it can probably be considered a flaw.
            renderArgs.getContext().pop();
                
            if (!renderArgs.isSystemCtxCmsOnly()) {
                populateSystemViewHandlerRenderContext(renderArgs.getContext(), 
                        renderArgs.getPageContext().getRequest(), renderArgs.getPageContext().getResponse(), 
                        renderArgs.getOut());
            }
            
            if (!renderArgs.isSystemCtxNoPush()) {
                renderArgs.getContext().push();
            }
        }
        
        /**
         * Populates the system context with the extra context fields normally added by 
         * {@link org.ofbiz.widget.renderer.macro.MacroScreenViewHandler#render},
         * which are not covered by {@link org.ofbiz.widget.renderer.ScreenRenderer#populateContextForRequest}
         */
        public static void populateSystemViewHandlerRenderContext(MapStack<String> context, 
                HttpServletRequest request, HttpServletResponse response, Writer writer) {
            // NOTE: 2017-02: we must emulate MacroScreenViewHandler and use dynamic renderer here,
            // because of theme-switchable support.
            // DEV NOTE: IMPORTANT: keep this method in sync with the context-populate parts of
            // MacroScreenViewHandler.render
            final String screenRendererName = "screen";
            ScreenStringRenderer screenStringRenderer;
            try {
                screenStringRenderer = MacroScreenViewHandler.loadRenderers(request, response, 
                        screenRendererName, context, writer);
            } catch (Exception e) {
                throw new CmsException(e);
            }
            ScreenRenderer screens = ScreenRenderer.makeWithEnvAwareFetching(writer, context, screenStringRenderer);
            
            context.put("screens", screens);
            // SCIPIO: 2016-09-15: in addition, dump the screens renderer into the request attributes,
            // for some cases where only request is available
            request.setAttribute("screens", screens);
            // SCIPIO: new early encoder
            UtilCodec.SimpleEncoder simpleEncoder = UtilCodec.getEncoder(UtilProperties.getPropertyValue("widget", screenRendererName + ".encoder"));
            context.put("simpleEncoder", simpleEncoder);
            UtilCodec.SimpleEncoder simpleEarlyEncoder = UtilCodec.getEncoder(UtilProperties.getPropertyValue("widget", screenRendererName + ".earlyEncoder"));
            context.put("simpleEarlyEncoder", (simpleEarlyEncoder != null) ? simpleEarlyEncoder : simpleEncoder);
            
            try {
                // SPECIAL: 2017-03-09: must register initial context manually here because renderScreenBegin never called
                // TODO?: registerContext should be a ScreenStringRenderer method (no cast)
                ((MacroScreenRenderer) screenStringRenderer).registerContext(writer, context);
            } catch (Exception e) {
                throw new CmsException(e);
            }
        }
        
        protected CmsPageTemplate getRenderPageTemplate(RenderArgs renderArgs) {
            if (this.template instanceof CmsPageTemplate) {
                return (CmsPageTemplate) this.template;
            } else {
                // here, probably rendering asset... 
                return null;
            }
        }
        
        /**
         * Populates context for a new request. 
         * <p>
         * Based on {@link org.ofbiz.widget.renderer.ScreenRenderer#populateContextForRequest}.
         * <p>
         * DEV NOTE: MUST BE MAINTAINED TO MATCH <code>ScreenRenderer.populateContextForRequest</code>.
         * Try to keep as close as possible in every way to that method (down to code style),
         * to minimize chances of missing anything.
         */
        public static void populateContextForRequest(MapStack<String> context, ScreenRenderer screens, CmsPageContent pageContent, CmsPageContext pageContext, CmsPageTemplate pageTemplate) {
            // top request-/page-level cms-specific variables
            populateTopCmsContextVariables(context, pageContent, pageContext, pageTemplate);
            ScreenRenderer.populateContextForRequest(context, screens, 
                    pageContext.getRequest(), pageContext.getResponse(), pageContext.getServletContext());
        }
        
        public static void populateContextForRequestCmsOnly(MapStack<String> context, ScreenRenderer screens, CmsPageContent pageContent, CmsPageContext pageContext, CmsPageTemplate pageTemplate) {
            populateTopCmsContextVariables(context, pageContent, pageContext, pageTemplate);
            context.push();
        }
        
        public static void populateTopCmsContextVariables(MapStack<String> context, CmsPageContent pageContent, CmsPageContext pageContext, CmsPageTemplate pageTemplate) {
            context.put("cmsPageContext", pageContext);
            context.put("cmsPageContent", pageContent);
            context.put("cmsContent", pageContent); // NOTE: this one gets overridden after, but also here just in case
            context.put("cmsPageTemplate", pageTemplate);
            CmsPage page = (pageContent != null) ? pageContent.getPage() : null;
            context.put("cmsPage", page); // NOTE: mainly for debugging and such...
            context.put("cmsPageId", (page != null) ? page.getId() : null); // NOTE: mainly for debugging and such...
            context.put("cmsIsPreview", pageContext.isPreview());
            context.put("cmsCtxSysVarNames", cmsCtxSysVarNames);
        }

        /**
         * Extra CMS-specific common context prep, that for practical purposes may be considered as part of system context setup,
         * although could also not be considered as such, and is NOT included as part of the system context stack push.
         */
        protected void populateExtraCommonRenderContext(RenderArgs renderArgs) {
            // 2017-02-20: MOVED these executions here, as part of "system", because it makes them easier to understand this way.
            Set<String> contextSkipNames = CmsRenderUtil.getContextSystemVarNamesAlways(renderArgs.getContext());
            populateDefaultLabelMaps(renderArgs, contextSkipNames);
            // NOTE: 2017: we no longer run processor script at every asset invocation. only top page template does this. lessened in importance anyway.
            populateProcessorScript(renderArgs, contextSkipNames);
        }
        
        /**
         * Populates widget-level context, or in other words things that would be found
         * in actions section of a screen widget, such as scripts and page context. 
         * <p>
         * NOTE: widget is loose term; no relation to ofbiz widgets.
         * <p>
         * Subclass <em>could</em> need to override this.
         */
        protected void populateWidgetRenderContext(RenderArgs renderArgs) {
            // for assets, most of the scripts and page-level content should already be in context
            // at this point. we only need to do the asset content.
            Set<String> contextSkipNames = CmsRenderUtil.getContextSystemVarNamesAlways(renderArgs.getContext());
            populateScriptsAndContent(renderArgs, contextSkipNames);
        }
        
        protected void populateEarlyExtraCtxVars(RenderArgs renderArgs) {
            if (renderArgs.getEarlyCtxVars() != null) {
                renderArgs.getContext().putAll(renderArgs.getEarlyCtxVars());
            }
        }
        
        protected void populateOvrdExtraCtxVars(RenderArgs renderArgs) {
            if (renderArgs.getOvrdCtxVars() != null) {
                // apply and consume
                renderArgs.getContext().putAll(renderArgs.getOvrdCtxVars());
            }
        }
        
        /**
         * TODO?: REMOVE? this should be covered by the templates themselves, and forcing this
         * might introduce ordering issues.
         */
        protected void populateDefaultLabelMaps(RenderArgs renderArgs, Set<String> contextSkipNames) {
            /* Add uiLabelMaps (this is screen-/page-specific, may be overwritten, so after push)*/
            try{
                ResourceBundleMapWrapper uiLabelMap = UtilProperties.getResourceBundleMap("CommonUiLabels", 
                        UtilHttp.getLocale((HttpServletRequest) renderArgs.getContext().get("request")));
                renderArgs.getContext().put("uiLabelMap", uiLabelMap);
            } catch(Exception e) {
                Debug.logError(e, "Cms: Error loading CommonUiLabels: " + e.getMessage(), module);
            }
        }
        
        /**
         * @deprecated too simplistic - no expansion timing control.
         */
        @Deprecated
        protected void populateInitialPageContent(RenderArgs renderArgs, Set<String> contextSkipNames) {
            CmsPageContent content = renderArgs.getContent().normalizeForAttributes(template.getExpansionSortedAttributeTemplates(), renderArgs.getPageContext());
            content.transferToContext(renderArgs.getContext(), contextSkipNames);
            setUpdatePageContentInstance(renderArgs, content);
        }
        
        /**
         * @deprecated too simplistic - no expansion timing control.
         */
        @Deprecated
        protected void setUpdatePageContentInstance(RenderArgs renderArgs, CmsPageContent content) {
            renderArgs.setContent(content); // theoreticially needed, but in practice usually will end up being same instance...
            renderArgs.getContext().put("cmsContent", content);
        }
        
        /**
         * @deprecated too simplistic - no expansion timing control.
         */
        @Deprecated
        protected void populateExpandedPageContent(RenderArgs renderArgs, Set<String> contextSkipNames) {
            // NOTE: 2017: the source injection context is simply the rendering context. it already contains everything needed, can avoid more copies.
            // NOTE: injectVariableContent currently injects/replaces content in-place (into first map)
            CmsPageContent content = renderArgs.getContent().parseExpandAttributes(template.getExpansionSortedAttributeTemplates(), renderArgs.getContext(), renderArgs.getPageContext());
            // Save all updated (injected) content back into context (making sure not to override important names)
            content.transferToContext(renderArgs.getContext(), contextSkipNames);
            setUpdatePageContentInstance(renderArgs, content);
        }
        
        /**
         * NOTE: 2017: the CMS-specific processor script is no longer of major importance in Scipio,
         * because Scipio supports custom system-wide and webapp-specific global scripts.
         * It is preferable to reuse those mechanisms, unless the script is truly CMS-specific.
         */
        protected void populateProcessorScript(RenderArgs renderArgs, Set<String> contextSkipNames) {
            try {
                CmsScriptTemplate.getProcessorScriptExecutor().execute(renderArgs.getContext());
            } catch (Exception e) {
                throw new CmsException(e);
            }
        }
        
        /**
         * NOTE: does not do attributes/content, only use this if not using those at all.
         * @see #populateScriptsAndContent
         */
        protected void populateScripts(RenderArgs renderArgs, Set<String> contextSkipNames) {
            populateScripts(getSortedScriptTemplates(), renderArgs, contextSkipNames);
        }
        
        protected void populateScripts(List<CmsScriptTemplate> scriptTemplates, RenderArgs renderArgs, Set<String> contextSkipNames) {
            if (scriptTemplates != null) {
                for (CmsScriptTemplate scriptTemplate : scriptTemplates) {
                    populateScript(scriptTemplate, renderArgs, contextSkipNames);
                }
            }
        }
        
        protected void populateScript(CmsScriptTemplate scriptTemplate, RenderArgs renderArgs, Set<String> contextSkipNames) {
            try {
                scriptTemplate.getExecutor().execute(renderArgs.getContext());
            } catch (Exception e) {
                throw new CmsException(e);
            }
        }
        
        /**
         * Common script + attribute population code common to both assets and pages.
         * Does not include script processor.
         * <p>
         * this will execute attributes and scripts in the precise right order, and also run
         * the "last" extra context vars, which are set after the last attributes (to
         * allow overriding them).
         * <p>
         * NOTE: 2017: this rewrites the old PageContent logic so that instead of dumping the page content
         * contexts blindly with putAll, we handle each CmsAttributeTemplate explicitly.
         * this also fixes the problem of unspecified attributes receiving arbitrary values through
         * the inherited context.
         * this also means we ditch any page content that does not apply to the exact attribute
         * definitions we are using, we never get any leftover values from prior definitions
         * and we prevent undefined and arbitrary behavior.
         */
        protected void populateScriptsAndContent(RenderArgs renderArgs, Set<String> contextSkipNames) {
            // old code order (too simplistic)
//            populateEarlyExtraCtxVars(renderArgs);
//            populateInitialPageContent(renderArgs, contextSkipNames);
//            populateExpandedPageContent(renderArgs, contextSkipNames);
//            populateLateExtraCtxVars(renderArgs);
//            populateScripts(renderArgs, contextSkipNames);
            
            // read attribs and scripts
            List<CmsAttributeTemplate> attribs = orEmpty(getSortedAttributeTemplates());
            Iterator<CmsAttributeTemplate> attribIt = attribs.iterator();
            List<CmsScriptTemplate> scripts = orEmpty(getSortedScriptTemplates());
            Iterator<CmsScriptTemplate> scriptIt = scripts.iterator();

            // populate early context vars (usually passed through macro call such as @asset ctxVars={...})
            populateEarlyExtraCtxVars(renderArgs);
            
            // TODO: review if needed, but seeing no need for this since handling attributes explicitly.
            //renderArgs.getContent().normalizeForAttributes(template.getExpansionSortedAttributeTemplates(), renderArgs.getPageContext());
            
            // initial populate late context vars (usually passed through macro call such as @asset ovrdCtxVars={...})
            // NOTE: "overriding/late" extra context vars: these are meant to override attributes.
            // but there may be key names that don't match any attributes; those ones we will
            // simply set immediately.
            // TODO: REVIEW: to simplify, for now just dump all the late vars, basically twice for
            // those that match attribs.
            populateOvrdExtraCtxVars(renderArgs);
            
            // Expand & transfer attributes to context, and run scripts, in the global order
            // defined by CmsAttributeTemplate.expandPosition together with CmsScriptTemplate.inputPosition.
            CmsAttributeTemplate attrib = attribIt.hasNext() ? attribIt.next() : null;
            CmsScriptTemplate script = scriptIt.hasNext() ? scriptIt.next() : null;
            while(attrib != null && script != null) {
                if (attrib.expandsBefore(script)) {
                    populateAttributeOrOvrd(attrib, renderArgs, contextSkipNames);
                    attrib = attribIt.hasNext() ? attribIt.next() : null;
                } else {
                    populateScript(script, renderArgs, contextSkipNames);
                    script = scriptIt.hasNext() ? scriptIt.next() : null;
                }
            }
            while(attrib != null) {
                populateAttributeOrOvrd(attrib, renderArgs, contextSkipNames);
                attrib = attribIt.hasNext() ? attribIt.next() : null;
            }
            while(script != null) {
                populateScript(script, renderArgs, contextSkipNames);
                script = scriptIt.hasNext() ? scriptIt.next() : null;
            }
        }
        
        /**
         * Expands (if applicable) the attribute and transfers it into context. If an ovrdCtxVar exists
         * for the same name, it replaces the attribute instead.
         */
        protected void populateAttributeOrOvrd(CmsAttributeTemplate attributeTemplate, RenderArgs renderArgs, Set<String> contextSkipNames) {
            String name = attributeTemplate.getName();
            if (contextSkipNames.contains(name)) {
                // this is not always fatal or a problem, but can cause serious issues in some cases
                Debug.logWarning("Cms: Attribute template '" + name + "' (id: " + attributeTemplate.getId() 
                    + ") uses the name of a reserved system context variable (consider renaming)", module);
            } 
            if (renderArgs.getOvrdCtxVars() != null && renderArgs.getOvrdCtxVars().containsKey(name)) {
                // overriding var def
                // NOTE: this should already be in context due to populateOvrdExtraCtxVars call, 
                // but put it again to be safe.
                renderArgs.getContext().put(name, renderArgs.getOvrdCtxVars().get(name));
            } else {
                // parse and expand the attribute. 
                // NOTE: this both re-stores it in the CmsPageContent (cmsContent)
                // and in the top-level context.
                renderArgs.getContext().put(name, renderArgs.getContent().parseExpandAttribute(attributeTemplate, 
                        renderArgs.getContext(), renderArgs.getPageContext(), true));
            }
        }
        
        private static <T> List<T> orEmpty(List<T> list) {
            return list != null ? list : Collections.<T> emptyList();
        }
        
        protected List<CmsScriptTemplate> getSortedScriptTemplates() { // sub-classes should override.
            return null;
        }
        
        protected List<CmsAttributeTemplate> getSortedAttributeTemplates() {
            return template.getExpansionSortedAttributeTemplates();
        }
        
        /**
         * Sets the standard Context variables dynamically
         * TODO: Check whether or not this is still being used - if it is, the returned object doesn't seem to be the correct one.
         * I think scriptContext must be added back to content somehow for it to work...
         */
        public CmsPageContent populateBasicContextVariables(CmsPageContent content, CmsPageContext pageContext) {        
            try {
                Map<String, Object> scriptContext = new HashMap<>();
                HttpServletRequest request = pageContext.getRequest();
                HttpServletResponse response = pageContext.getResponse();
                scriptContext.put("request", request);
                scriptContext.put("response", response);
                HttpSession session = request.getSession();
                scriptContext.put("session", session);
                scriptContext.put("dispatcher", request.getAttribute("dispatcher"));
                scriptContext.put("delegator", request.getAttribute("delegator"));
                scriptContext.put("security", request.getAttribute("security"));
                scriptContext.put("locale", UtilHttp.getLocale(request));
                scriptContext.put("timeZone", UtilHttp.getTimeZone(request));
                scriptContext.put("userLogin", session.getAttribute("userLogin"));
                scriptContext.put("parameters", UtilHttp.getCombinedMap(request, UtilMisc.toSet("delegator", "dispatcher", "security", "locale", "timeZone", "userLogin")));
                // make sure the "nullField" object is in there for entity ops; note this is nullField and not null because as null causes problems in FreeMarker and such...
                scriptContext.put("nullField", GenericEntity.NULL_FIELD);
                scriptContext.put("StringUtil", StringUtil.INSTANCE);

                scriptContext.put("cmsPageContext", pageContext);
                scriptContext.put("cmsContent",content);
                CmsScriptTemplate.getProcessorScriptExecutor().executeSafe(scriptContext);
            } catch (Exception e) {
                Debug.logError(e, module);
            }
            return content;
        }
    }
}
