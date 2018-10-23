package com.ilscipio.scipio.cms;

import org.ofbiz.base.util.PropertyMessage;

/**
 * Exception thrown to pass around input errors.
 */
@SuppressWarnings("serial")
public class CmsInputException extends CmsException {

    public CmsInputException(PropertyMessage propMsg, Throwable e) {
        super(propMsg, e);
    }

    public CmsInputException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public CmsInputException(String msg, PropertyMessage propMsg, Throwable e) {
        super(msg, propMsg, e);
    }

    public CmsInputException(String msg, PropertyMessage propMsg) {
        super(msg, propMsg);
    }

    public CmsInputException(String msg, Throwable e) {
        super(msg, e);
    }

    public CmsInputException(String msg) {
        super(msg);
    }

    public CmsInputException(Throwable e) {
        super(e);
    }

}
