package com.ilscipio.scipio.cms.control.cmscall;

import javax.servlet.ServletContext;

/**
 * Simply abstracts CMS invocation mechanisms.
 */
public abstract class CmsInvoker {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    protected final ServletContext servletCtx;

    public CmsInvoker(ServletContext servletCtx) {
        this.servletCtx = servletCtx;
    }

}
