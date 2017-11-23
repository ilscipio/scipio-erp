package com.ilscipio.scipio.cms.control.cmscall;

import javax.servlet.ServletContext;

/**
 * Simply abstracts CMS invocation mechanisms.
 */
public abstract class CmsInvoker {

    public static final String module = CmsInvoker.class.getName();
    
    protected final ServletContext servletCtx;

    public CmsInvoker(ServletContext servletCtx) {
        this.servletCtx = servletCtx;
    }

}
