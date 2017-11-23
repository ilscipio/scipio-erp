package com.ilscipio.scipio.cms.template;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletContext;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilRender;
import org.ofbiz.base.util.UtilRender.RenderExceptionMode;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.base.util.template.FreeMarkerWorker.OFBizTemplateExceptionHandler;

import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.content.CmsPageContent;
import com.ilscipio.scipio.cms.content.CmsPageContext;
import com.ilscipio.scipio.cms.control.CmsControlUtil;

import freemarker.core.Environment;
import freemarker.ext.util.WrapperTemplateModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;

public abstract class CmsRenderUtil {

    public static final String module = CmsRenderUtil.class.getName();
    
    private static final RenderExceptionMode liveExceptionMode = RenderExceptionMode.valueOfPermissive(UtilProperties.getPropertyValue("cms.properties", 
            "render.live.exception.mode"));
    
    static final RenderExceptionMode directiveLiveExceptionMode = RenderExceptionMode.valueOfPermissive(UtilProperties.getPropertyValue("cms.properties", 
            "render.live.exception.directive.mode"));
    
    protected CmsRenderUtil() {
    }

    // ERROR HANDLING
    
    public static RenderExceptionMode getLiveExceptionMode() {
        if (liveExceptionMode != null) return liveExceptionMode;
        return UtilRender.getGlobalRenderExceptionMode();
    }
    
    public static RenderExceptionMode getLiveExceptionMode(ServletContext servletContext) { // NOTE: never returns null
        if (servletContext != null) {
            RenderExceptionMode mode = RenderExceptionMode.valueOfPermissive(servletContext.getAttribute(UtilRender.RENDER_EXCEPTION_MODE_VAR));
            if (mode != null) return mode;
        }
        return getLiveExceptionMode();
    }

    /**
     * FIXME: this is using custom behavior that should be found outside CMS...
     * TODO: missing security options such as triggering HTTP 500... belongs outside CMS...
     */
    public static class CmsTemplateExceptionHandler extends OFBizTemplateExceptionHandler {

        @Override
        protected void handleTemplateExceptionDebug(TemplateException te, Environment env, Writer out) throws TemplateException {
            CmsTemplateException cte = CmsTemplateException.getFromExceptionOrCauseDeep(te);
            if (cte != null) {
                handleTemplateExceptionDebugCms(cte, te, env, out);
            } else {
                super.handleTemplateExceptionDebug(te, env, out);
            }
        }
        
        protected void handleTemplateExceptionDebugCms(CmsTemplateException cte, TemplateException te, Environment env, Writer out) throws TemplateException {
            // TODO? no localization support... should follow freemarker (?)...
            String msg = encode(cte.getFriendlyMessage(), env);
            if (msg != null) {
                String label = encode(cte.getFriendlyMessageLabel(), env);
                if (label != null) {
                    msg = makeCmsMsgMarkup(label, msg);
                } else {
                    msg = makeCmsMsgMarkup(msg);
                }
            } else {
                msg = encode(cte.getMessage(), env);
                msg = makeCmsMsgMarkup(msg);
            }
            write(out, msg);
        }
        
        protected boolean isPreviewMode(Environment env) {
            CmsPageContext pageContext = CmsRenderUtil.getBestPageContext(env);
            return pageContext != null && pageContext.isPreview();
        }
        
        protected String makeCmsMsgMarkup(String label, String msg) {
            return "<b>" + label + ":</b> " + msg;
        }
        
        protected String makeCmsMsgMarkup(String msg) {
            return "<b>CMS error:</b> " + msg;
        }
        
        protected void handleTemplateExceptionDebugLegacy(TemplateException te, Environment env, Writer out) throws TemplateException {
            String stackTrace = getStackTrace(te);
            stackTrace = encode(stackTrace, env);
            write(out, stackTrace);
        }
        
        protected String getStackTrace(TemplateException te) {
            StringWriter tempWriter = new StringWriter();
            PrintWriter pw = new PrintWriter(tempWriter, true);
            te.printStackTrace(pw);
            return tempWriter.toString();
        }
        
        protected String encode(String msg, Environment env) {
            if (msg == null) return msg;
            // TODO: REVIEW: this context fetch from stock is an example of why it currently doesn't matter
            // that we rely on context for security - it's like that everywhere. 
            UtilCodec.SimpleEncoder simpleEncoder = FreeMarkerWorker.getWrappedObject("simpleEncoder", env);
            if (simpleEncoder != null) {
                msg = simpleEncoder.encode(msg);
            }
            return msg;
        }
        
        protected void write(Writer out, String msg) {
            try {
                out.write(msg);
            } catch (IOException e) {
                Debug.logError(e, module);
            }
        }
    }

    /**
     * Error handling for markup-generating transforms and code running within FTL (non-FTL also available),
     * a.k.a. user directives. 
     * <p>
     * DEV NOTE: DO NOT USE THIS FOR RENDERER/INTERNAL CODE.
     * <p>
     * DEV NOTE: performance doesn't matter here, so we re-fetch variables to simplify callers.
     * <p>
     * DEV NOTE: this centralizes the decision of whether to throw unhandled CmsException or
     * Freemarker-handled TemplateException.
     * <p>
     * TODO?: none of these are equipped to handle localized output; would need overloads with
     * PropertyMessage + Locale because logs are always english. we should ideally follow what
     * Freemarker normally does.
     */
    public static boolean handleDirectiveError(Environment env, String label, Throwable t, String msg, RenderExceptionMode liveExMode, String module) throws TemplateException {
        try {
            Map<String, Object> context = null;
            CmsPageContext pageContext = null;
            CmsPageTemplate pageTemplate = null;
        
            context = CmsRenderUtil.getRenderContext(env);
            if (context != null) {
                // don't use Always on the following...
                pageContext = CmsRenderUtil.getPageContext(context);
                pageTemplate = CmsRenderUtil.getPageTemplate(context);
            } else {
                Debug.logWarning("Cms: While handling error, context map was not found in Freemarker environment", module);
            }

            return handleDirectiveError(pageContext, pageTemplate, label, t, msg, liveExMode, module);
        } catch(CmsTemplateException e) {
            throw new TemplateException(e.getMessage(), e, env);
        }
    }

    public static boolean handleDirectiveError(CmsPageContext pageContext, CmsPageTemplate pageTemplate, 
            String label, Throwable t, String msg, RenderExceptionMode liveExMode, String module) throws CmsTemplateException {
        String sysMsg = msg;
        String userMsg = msg;
        if (pageContext != null) {
            String tmplIdStr = " [template: " + pageTemplate.getName() + "]";
            sysMsg += tmplIdStr;
            if (pageContext.getRequest() != null) {
                String logIdStr = CmsControlUtil.getReqLogIdStr(pageContext.getRequest());
                if (UtilValidate.isNotEmpty(logIdStr)) {
                    sysMsg += " [" + logIdStr + "]";
                }
            }
        }
        return handleDirectiveError(label, t, sysMsg, userMsg, (pageContext != null ? pageContext.isPreview() : false), liveExMode, module);
    }
    
    public static boolean handleDirectiveError(String label, Throwable t, String msg,
            boolean isPreview, RenderExceptionMode liveExMode, String module) throws CmsTemplateException {
        return handleDirectiveError(label, t, msg, isPreview, liveExMode, module);
    }
    
    public static boolean handleDirectiveError(String label, Throwable t, String sysMsg, String userMsg,
            boolean isPreview, RenderExceptionMode liveExMode, String module) throws CmsTemplateException {
        String msg;
        if (t != null) {
            msg = label + ": " + sysMsg + ": " + t.getMessage();
        } else {
            msg = label + ": " + sysMsg;
        }
        CmsTemplateException ex = (t != null) ? new CmsTemplateException(msg, t) : new CmsTemplateException(msg);
        if (!isPreview && liveExMode != null) {
            ex.setRenderExceptionMode(liveExMode);
        } else {
            ; // already in request/servlet context
        }
        ex.setFriendlyMessageLabel(label);
        ex.setFriendlyMessage(userMsg + (t != null ? ": " + t.getMessage() : ""));
        throw ex;
    }
    
    /**
     * Get directive live render exception mode, or null if no special config for directives.
     * If specified specificLiveExMode is null, uses the generic one.
     */
    public static RenderExceptionMode getDirectiveLiveRenderExceptionMode(RenderExceptionMode specificLiveExMode) {
        if (specificLiveExMode != null) return specificLiveExMode;
        else return getDirectiveLiveRenderExceptionMode();
    }
    
    /**
     * Returns the generic directive live exception mode, or null if no special config for directives.;
     */
    public static RenderExceptionMode getDirectiveLiveRenderExceptionMode() {
        return directiveLiveExceptionMode;
    }
    
    /**
     * Special helper for directives to check what the render mode is going to be when exception is caught.
     * This can be used for alternate output.
     * WARN: this must be maintained carefully so it matches behavior of our modified OFBizTemplateExceptionHandler
     * and methods above.
     */
    public static RenderExceptionMode getDirectiveRenderExceptionMode(Environment env, RenderExceptionMode liveExMode) {
        MapStack<String> context = null;
        CmsPageContext pageContext = null;
        try {
            context = CmsRenderUtil.getRenderContextAlways(env);
            pageContext = CmsRenderUtil.getPageContextAlways(context);
        } catch(Throwable t) {
            Debug.logError(t, "Cms: isDirectiveCaughtRenderExceptionMode: internal error: could not fetch all page context info from environment", module);
        }
        boolean isPreview = false;
        if (pageContext != null) {
            isPreview = pageContext.isPreview();
        }
        if (!isPreview && liveExMode != null) return liveExMode;
        return UtilRender.getRenderExceptionMode(pageContext != null ? pageContext.getRequest() : null);
    }
    

    // CONTEXT VARIABLE ACCESSORS
    
    @SuppressWarnings("unchecked")
    public static MapStack<String> getRenderContext(Environment env) throws TemplateModelException {
        TemplateModel pcm = env.getVariable("context");
        if (pcm != null && pcm instanceof WrapperTemplateModel) {
            Object obj = ((WrapperTemplateModel) pcm).getWrappedObject();
            if (obj instanceof MapStack) {
                return (MapStack<String>) obj;
            }
        }
        return null;
    }

    public static MapStack<String> getRenderContextAlways(Environment env) throws IllegalStateException, TemplateModelException {
        MapStack<String> inst = getRenderContext(env);
        if (inst == null) {
            throw new IllegalStateException("Missing or invalid render context in freemarker environment");
        }
        return inst;
    }

    @SuppressWarnings("unchecked")
    public static Map<String, Object> getRenderContextGeneric(Environment env) throws TemplateModelException {
        TemplateModel pcm = env.getVariable("context");
        if (pcm != null && pcm instanceof WrapperTemplateModel) {
            Object obj = ((WrapperTemplateModel) pcm).getWrappedObject();
            if (obj instanceof MapStack) {
                return (Map<String, Object>) obj;
            }
        }
        return null;
    }

    public static Map<String, Object> getRenderContextGenericAlways(Environment env) throws IllegalStateException, TemplateModelException {
        Map<String, Object> inst = getRenderContext(env);
        if (inst == null) {
            throw new IllegalStateException("Missing or invalid render context in freemarker environment");
        }
        return inst;
    }

    public static CmsPage getPage(Map<String, ?> context) {
        Object obj = context.get("cmsPage");
        if (obj instanceof CmsPage) {
            return (CmsPage) obj;
        }
        return null;
    }

    public static CmsPage getPageAlways(Map<String, ?> context) throws IllegalStateException {
        CmsPage inst = getPage(context);
        if (inst == null) {
            throw new IllegalStateException("Missing or invalid cmsPage in rendering context");
        }
        return inst;
    }

    public static CmsPageContext getPageContext(Map<String, ?> context) {
        Object obj = context.get("cmsPageContext");
        if (obj instanceof CmsPageContext) {
            return (CmsPageContext) obj;
        }
        return null;
    }

    public static CmsPageContext getPageContextAlways(Map<String, ?> context) throws IllegalStateException {
        CmsPageContext inst = getPageContext(context);
        if (inst == null) {
            throw new IllegalStateException("Missing or invalid cmsPageContext in rendering context");
        }
        return inst;
    }

    public static CmsPageContext getBestPageContext(Environment env) {
        CmsPageContext pageContext = null;
        Map<String, Object> context = null;
        try {
            context = getRenderContextGeneric(env);
        } catch (Exception e) {
            ;
        }
        if (context != null) {
            pageContext = getPageContext(context);
            if (pageContext != null) {
                return pageContext;
            }
        }
        TemplateModel model = null;
        try {
            model = env.getVariable("cmsPageContext");
            Object obj = LangFtlUtil.unwrapOrNull(model);
            if (obj instanceof CmsPageContext) {
                return (CmsPageContext) obj;
            }
        } catch (Exception e) {
            ;
        }
        return null;
    }

    public static CmsPageContent getTopPageContent(Map<String, ?> context) {
        Object obj = context.get("cmsPageContent");
        if (obj instanceof CmsPageContent) {
            return (CmsPageContent) obj;
        }
        return null;
    }

    public static CmsPageContent getTopPageContentAlways(Map<String, ?> context) throws IllegalStateException {
        CmsPageContent inst = getTopPageContent(context);
        if (inst == null) {
            throw new IllegalStateException("Missing or invalid cmsPageContent in rendering context");
        }
        return inst;
    }

    public static CmsPageContent getCurrentContent(Map<String, ?> context) {
        Object obj = context.get("cmsContent");
        if (obj instanceof CmsPageContent) {
            return (CmsPageContent) obj;
        }
        return null;
    }

    public static CmsPageContent getCurrentContentAlways(Map<String, ?> context) throws IllegalStateException {
        CmsPageContent inst = getCurrentContent(context);
        if (inst == null) {
            throw new IllegalStateException("Missing or invalid current (page or asset) cmsContent in rendering context");
        }
        return inst;
    }

    public static CmsPageTemplate getPageTemplate(Map<String, ?> context) {
        Object obj = context.get("cmsPageTemplate");
        if (obj instanceof CmsPageTemplate) {
            return (CmsPageTemplate) obj;
        }
        return null;
    }

    public static CmsPageTemplate getPageTemplateAlways(Map<String, ?> context) throws IllegalStateException {
        CmsPageTemplate inst = getPageTemplate(context);
        if (inst == null) {
            throw new IllegalStateException("Missing or invalid cmsPageTemplate in rendering context");
        }
        return inst;
    }

    @SuppressWarnings("unchecked")
    public static Set<String> getContextSystemVarNames(Map<String, ?> context) {
        Object obj = context.get("cmsCtxSysVarNames");
        if (obj instanceof Set) {
            return (Set<String>) obj;
        }
        return null;
    }

    public static Set<String> getContextSystemVarNamesAlways(Map<String, ?> context) throws IllegalStateException {
        Set<String> inst = getContextSystemVarNames(context);
        if (inst == null) {
            throw new IllegalStateException("Missing or invalid context system var names in rendering context");
        }
        return inst;
    }
}
