package com.ilscipio.scipio.cms.control.cmscall;

import org.ofbiz.base.util.PropertyMessage;

import com.ilscipio.scipio.cms.CmsException;

/**
 * Exceptions thrown during or related to CMS invocations.
 */
@SuppressWarnings("serial")
public class CmsCallException extends CmsException {

    public CmsCallException(PropertyMessage propMsg, Throwable e) {
        super(propMsg, e);
    }

    public CmsCallException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public CmsCallException(String msg, PropertyMessage propMsg, Throwable e) {
        super(msg, propMsg, e);
    }

    public CmsCallException(String msg, PropertyMessage propMsg) {
        super(msg, propMsg);
    }

    public CmsCallException(String msg, Throwable e) {
        super(msg, e);
    }

    public CmsCallException(String msg) {
        super(msg);
    }

    public CmsCallException(Throwable e) {
        super(e);
    }
}
