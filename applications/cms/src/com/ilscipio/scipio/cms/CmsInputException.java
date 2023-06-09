package com.ilscipio.scipio.cms;

import org.ofbiz.base.util.PropertyMessage;

import java.util.Collection;

/**
 * Exception thrown to pass around input errors.
 */
@SuppressWarnings("serial")
public class CmsInputException extends CmsException {

    public CmsInputException() {
    }

    public CmsInputException(String msg) {
        super(msg);
    }

    public CmsInputException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public CmsInputException(Throwable nested) {
        super(nested);
    }

    public CmsInputException(String msg, Collection<?> messageList) {
        super(msg, messageList);
    }

    public CmsInputException(String msg, Collection<?> messageList, Throwable nested) {
        super(msg, messageList, nested);
    }

    public CmsInputException(Collection<?> messageList, Throwable nested) {
        super(messageList, nested);
    }

    public CmsInputException(Collection<?> messageList) {
        super(messageList);
    }

    public CmsInputException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public CmsInputException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }

}
