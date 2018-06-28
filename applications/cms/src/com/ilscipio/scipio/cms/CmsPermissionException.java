package com.ilscipio.scipio.cms;

import org.ofbiz.base.util.PropertyMessage;

@SuppressWarnings("serial")
public class CmsPermissionException extends CmsException {

    public CmsPermissionException(PropertyMessage propMsg, Throwable e) {
        super(propMsg, e);
    }

    public CmsPermissionException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public CmsPermissionException(String msg, PropertyMessage propMsg, Throwable e) {
        super(msg, propMsg, e);
    }

    public CmsPermissionException(String msg, PropertyMessage propMsg) {
        super(msg, propMsg);
    }

    public CmsPermissionException(String msg, Throwable e) {
        super(msg, e);
    }

    public CmsPermissionException(String msg) {
        super(msg);
    }

    public CmsPermissionException(Throwable e) {
        super(e);
    }

}
