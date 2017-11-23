package com.ilscipio.scipio.cms.util.fileType;

import org.ofbiz.base.util.PropertyMessage;

import com.ilscipio.scipio.cms.CmsException;

@SuppressWarnings("serial")
public class FileTypeException extends CmsException {

    public FileTypeException(String msg, Throwable e) {
        super(msg, e);
    }

    public FileTypeException(String msg) {
        super(msg);
    }

    public FileTypeException(Throwable e) {
        super(e);
    }

    public FileTypeException(String msg, PropertyMessage propMsg, Throwable e) {
        super(msg, propMsg, e);
    }

    public FileTypeException(String msg, PropertyMessage propMsg) {
        super(msg, propMsg);
    }

    public FileTypeException(PropertyMessage propMsg, Throwable e) {
        super(propMsg, e);
    }

    public FileTypeException(PropertyMessage propMsg) {
        super(propMsg);
    }

}
