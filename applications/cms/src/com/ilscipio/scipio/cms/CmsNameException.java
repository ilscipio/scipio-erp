package com.ilscipio.scipio.cms;

import org.ofbiz.base.util.PropertyMessage;

import java.util.Collection;

@SuppressWarnings("serial")
public class CmsNameException extends CmsInputException {

    public CmsNameException() {
    }

    public CmsNameException(String msg) {
        super(msg);
    }

    public CmsNameException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public CmsNameException(Throwable nested) {
        super(nested);
    }

    public CmsNameException(String msg, Collection<?> messageList) {
        super(msg, messageList);
    }

    public CmsNameException(String msg, Collection<?> messageList, Throwable nested) {
        super(msg, messageList, nested);
    }

    public CmsNameException(Collection<?> messageList, Throwable nested) {
        super(messageList, nested);
    }

    public CmsNameException(Collection<?> messageList) {
        super(messageList);
    }

    public CmsNameException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public CmsNameException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }
}
