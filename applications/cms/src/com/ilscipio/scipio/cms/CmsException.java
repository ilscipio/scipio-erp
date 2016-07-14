package com.ilscipio.scipio.cms;

import org.ofbiz.base.util.Debug;

@SuppressWarnings("serial")
public class CmsException extends RuntimeException {

    public CmsException(String msg, Throwable e, String module) {
        super(msg, e);
        Debug.logError(msg + e, module);
    }

    public CmsException(String arg0, String module) {
        super(arg0);
    }
}
