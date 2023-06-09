package com.ilscipio.scipio.cms;

import org.ofbiz.base.util.PropertyMessage;

import java.util.Collection;

@SuppressWarnings("serial")
public class CmsPermissionException extends CmsException {

    public CmsPermissionException() {
    }

    public CmsPermissionException(String msg) {
        super(msg);
    }

    public CmsPermissionException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public CmsPermissionException(Throwable nested) {
        super(nested);
    }

    public CmsPermissionException(String msg, Collection<?> messageList) {
        super(msg, messageList);
    }

    public CmsPermissionException(String msg, Collection<?> messageList, Throwable nested) {
        super(msg, messageList, nested);
    }

    public CmsPermissionException(Collection<?> messageList, Throwable nested) {
        super(messageList, nested);
    }

    public CmsPermissionException(Collection<?> messageList) {
        super(messageList);
    }

    public CmsPermissionException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public CmsPermissionException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }

}
