package com.ilscipio.scipio.common.util.fileType;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.PropertyMessage;

@SuppressWarnings("serial")
public class FileTypeException extends GeneralException {

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
        // super(msg, propMsg, e);
    }

    public FileTypeException(String msg, PropertyMessage propMsg) {
        // super(msg, propMsg);
    }

    public FileTypeException(PropertyMessage propMsg, Throwable e) {
        super(propMsg, e);
    }

    public FileTypeException(PropertyMessage propMsg) {
        super(propMsg);
    }

}
