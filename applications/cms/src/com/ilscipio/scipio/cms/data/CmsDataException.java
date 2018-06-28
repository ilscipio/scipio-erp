package com.ilscipio.scipio.cms.data;

import org.ofbiz.base.util.PropertyMessage;

import com.ilscipio.scipio.cms.CmsException;

/**
 * CMS data exception.
 */
@SuppressWarnings("serial")
public class CmsDataException extends CmsException {

    public CmsDataException(PropertyMessage propMsg, Throwable e) {
        super(propMsg, e);
    }

    public CmsDataException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public CmsDataException(String msg, PropertyMessage propMsg, Throwable e) {
        super(msg, propMsg, e);
    }

    public CmsDataException(String msg, PropertyMessage propMsg) {
        super(msg, propMsg);
    }

    public CmsDataException(String msg, Throwable e) {
        super(msg, e);
    }

    public CmsDataException(String msg) {
        super(msg);
    }

    public CmsDataException(Throwable e) {
        super(e);
    }

    
    /**
     * Thrown when candidate keys clash which are not enforced otherwise in schema.
     */
    public static class CmsUniqueDataException extends CmsDataException {

        private static final long serialVersionUID = 4868713020052920312L;

        public CmsUniqueDataException(PropertyMessage propMsg, Throwable e) {
            super(propMsg, e);
        }

        public CmsUniqueDataException(PropertyMessage propMsg) {
            super(propMsg);
        }

        public CmsUniqueDataException(String msg, PropertyMessage propMsg, Throwable e) {
            super(msg, propMsg, e);
        }

        public CmsUniqueDataException(String msg, PropertyMessage propMsg) {
            super(msg, propMsg);
        }

        public CmsUniqueDataException(String msg, Throwable e) {
            super(msg, e);
        }

        public CmsUniqueDataException(String msg) {
            super(msg);
        }

        public CmsUniqueDataException(Throwable e) {
            super(e);
        }

    }
}
