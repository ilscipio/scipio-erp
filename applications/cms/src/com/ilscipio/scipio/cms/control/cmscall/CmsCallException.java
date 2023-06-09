package com.ilscipio.scipio.cms.control.cmscall;

import org.ofbiz.base.util.PropertyMessage;

import com.ilscipio.scipio.cms.CmsException;

import java.util.Collection;

/**
 * Exceptions thrown during or related to CMS invocations.
 */
@SuppressWarnings("serial")
public class CmsCallException extends CmsException {

    public CmsCallException() {
    }

    public CmsCallException(String msg) {
        super(msg);
    }

    public CmsCallException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public CmsCallException(Throwable nested) {
        super(nested);
    }

    public CmsCallException(String msg, Collection<?> messageList) {
        super(msg, messageList);
    }

    public CmsCallException(String msg, Collection<?> messageList, Throwable nested) {
        super(msg, messageList, nested);
    }

    public CmsCallException(Collection<?> messageList, Throwable nested) {
        super(messageList, nested);
    }

    public CmsCallException(Collection<?> messageList) {
        super(messageList);
    }

    public CmsCallException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public CmsCallException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }

}
