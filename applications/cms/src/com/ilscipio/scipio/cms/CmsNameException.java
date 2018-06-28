package com.ilscipio.scipio.cms;

import org.ofbiz.base.util.PropertyMessage;

@SuppressWarnings("serial")
public class CmsNameException extends CmsInputException {

    public CmsNameException(String msg, Throwable e) {
        super(msg, e);
    }

    public CmsNameException(String msg) {
        super(msg);
    }

    public CmsNameException(Throwable e) {
        super(e);
    }

    public CmsNameException(String msg, PropertyMessage propMsg, Throwable e) {
        super(msg, propMsg, e);
    }

    public CmsNameException(String msg, PropertyMessage propMsg) {
        super(msg, propMsg);
    }

    public CmsNameException(PropertyMessage propMsg, Throwable e) {
        super(propMsg, e);
    }

    public CmsNameException(PropertyMessage propMsg) {
        super(propMsg);
    }

}
