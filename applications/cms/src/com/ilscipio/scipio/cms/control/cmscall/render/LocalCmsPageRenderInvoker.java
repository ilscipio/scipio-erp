package com.ilscipio.scipio.cms.control.cmscall.render;

import java.io.StringWriter;
import java.io.Writer;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilRender;
import org.ofbiz.base.util.UtilRender.RenderExceptionMode;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.content.CmsPageContext;
import com.ilscipio.scipio.cms.control.CmsControlUtil;
import com.ilscipio.scipio.cms.control.CmsView;
import com.ilscipio.scipio.cms.control.cmscall.CmsCallType;
import com.ilscipio.scipio.cms.template.CmsRenderUtil;
import com.ilscipio.scipio.cms.template.RendererType;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.control.RequestHandlerException;
import org.ofbiz.webapp.view.ViewHandler;
import org.ofbiz.webapp.view.ViewHandlerException;

public class LocalCmsPageRenderInvoker extends RenderInvoker {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public LocalCmsPageRenderInvoker(ServletContext servletCtx) {
        super(servletCtx);
    }

    public static LocalCmsPageRenderInvoker getRenderInvoker(ServletContext servletCtx) {
        // Using only HttpClient-based invoker for now
        return new LocalCmsPageRenderInvoker(servletCtx);
    }

    @Override
    public void invokeCmsRendering(HttpServletRequest request, HttpServletResponse response, ServletContext servletCtx,
            CmsPage cmsPage, CmsView view, String webSiteId, CmsCallType renderMode, Writer writer) throws Exception {
        CmsPageContext cmsPageContext = CmsPageContext.fromRequest(request, response,
                webSiteId, renderMode == CmsCallType.OFBIZ_PREVIEW, RendererType.CMS);
        invokeCmsRendering(request, response, servletCtx, cmsPage, cmsPageContext, writer, null);
    }

    @Override
    public void invokeCmsRendering(ViewHandler.ViewRenderContext vrctx, CmsPage cmsPage, CmsView view, String webSiteId,
                                   CmsCallType renderMode) throws Exception {
        CmsPageContext cmsPageContext = CmsPageContext.fromRequest(vrctx.request(), vrctx.response(),
                webSiteId, renderMode == CmsCallType.OFBIZ_PREVIEW, RendererType.CMS);
        invokeCmsRendering(vrctx.request(), vrctx.response(), servletCtx, cmsPage, cmsPageContext, vrctx.writer(), vrctx);
    }

    private void invokeCmsRendering(HttpServletRequest request, HttpServletResponse response, ServletContext servletCtx,
            CmsPage cmsPage, CmsPageContext cmsPageContext, Writer writer, ViewHandler.ViewRenderContext vrctx) throws Exception {
        if (cmsPage == null) {
            throw new CmsException("No CMS page available for CMS rendering");
        }
        if (cmsPageContext == null) {
            throw new CmsException("No CMS page context available for CMS rendering");
        }

        Writer origWriter = writer;
        if (CmsUtil.verboseOn()) {
            if (origWriter instanceof StringWriter) {
                ; // already prepped
            } else {
                writer = new StringWriter();
            }
        }

        String cmsPageVersionId = null;
        if (cmsPageContext.isPreview()) {
            cmsPageVersionId = CmsControlUtil.getPagePreviewVersionId(request);
        }

        // 2017-03-23: must set the appropriate error handling mode
        // TODO: REVIEW: security of this is not absolute (context + request), but it is still a relative
        // improvement over Ofbiz
        // TODO?: REVIEW: it's possible this should be in CmsRenderTemplate... or not...
        RenderExceptionMode exMode = cmsPageContext.isPreview() ? RenderExceptionMode.DEBUG :
            CmsRenderUtil.getLiveExceptionMode(request.getServletContext());
        request.setAttribute(UtilRender.RENDER_EXCEPTION_MODE_VAR, exMode);
        if (cmsPageContext.isPreview()) {
            // set RequestHandler.makeLink log level to something less drastic,
            // because missing controller request URIs are regular occurrence
            // on /website and /backendsite
            request.setAttribute("_SCP_LINK_ERROR_LEVEL_", Debug.WARNING);
        }

        request.setAttribute("cmsPage", cmsPage);
        request.setAttribute("cmsPageContext", cmsPageContext);

        // SCIPIO: 2.1.0: pre-screen-render event
        if (vrctx != null && vrctx.controllerConfig() != null) {
            try {
                RequestHandler.runEvents(vrctx, "pre-screen-render", vrctx.controllerConfig().getPreScreenRenderEventList(), false);
            } catch (GeneralException e) {
                Debug.logError(e, "Exception thrown reading/running pre-screen-render events: ", module);
                throw new ViewHandlerException(e);
            }
        }

        cmsPage.getRenderer().processAndRender(writer, cmsPageContext, cmsPageVersionId);
        writer.flush();

        // SCIPIO: 2.1.0: post-screen-render event
        if (vrctx != null && vrctx.controllerConfig() != null) {
            try {
                RequestHandler.runEvents(vrctx, "post-screen-render", vrctx.controllerConfig().getPostScreenRenderEventList(), false);
            } catch (GeneralException e) {
                Debug.logError(e, "Exception thrown reading/running post-screen-render events: ", module);
                throw new ViewHandlerException(e);
            }
        }

        if (CmsUtil.verboseOn() && (writer instanceof StringWriter)) {
            StringWriter sw = (StringWriter) writer;
            String outStr = sw.toString();
            if (outStr.trim().isEmpty()) {
                throw new CmsException("Cms: Render attempt produced no output");
            } else {
                Debug.logInfo("Cms: Rendered page '" + cmsPage.getId() + "'; response length (chars): " + outStr.length()
                    + CmsControlUtil.getReqLogIdDelimStr(request), module);
                if (origWriter instanceof StringWriter) {
                    ; // already printed
                } else {
                    origWriter.append(outStr);
                    origWriter.flush();
                    response.getWriter();
                }
            }
        }
    }

}
