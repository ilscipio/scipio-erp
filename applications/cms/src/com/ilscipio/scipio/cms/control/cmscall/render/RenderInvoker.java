package com.ilscipio.scipio.cms.control.cmscall.render;

import java.io.Writer;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.control.CmsView;
import com.ilscipio.scipio.cms.control.cmscall.CmsCallException;
import com.ilscipio.scipio.cms.control.cmscall.CmsCallType;
import com.ilscipio.scipio.cms.control.cmscall.CmsInvoker;

public abstract class RenderInvoker extends CmsInvoker {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    protected RenderInvoker(ServletContext servletCtx) {
        super(servletCtx);
    }
    
    public static RenderInvoker getRenderInvoker(ServletContext servletCtx) {
        // Using local CMS page
        return LocalCmsPageRenderInvoker.getRenderInvoker(servletCtx);
    }
    
    // 2016: better to pass CmsPage for local
//    public abstract void invokeCmsRendering(HttpServletRequest request, HttpServletResponse response, 
//            ServletContext servletCtx, CmsPageInfo page, CmsView view, String webSiteId) throws CmsCallException, Exception;
    
    public abstract void invokeCmsRendering(HttpServletRequest request, HttpServletResponse response, 
            ServletContext servletCtx, CmsPage cmsPage, CmsView view, String webSiteId, CmsCallType renderMode, Writer writer) throws CmsCallException, Exception;
    
}
