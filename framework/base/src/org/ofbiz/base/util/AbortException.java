package org.ofbiz.base.util;

import java.util.List;

/** General operation abort exception (SCIPIO). */
public class AbortException extends GeneralException {
    public AbortException() {
    }

    public AbortException(String msg) {
        super(msg);
    }

    public AbortException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public AbortException(Throwable nested) {
        super(nested);
    }

    public AbortException(String msg, List<?> messages) {
        super(msg, messages);
    }

    public AbortException(String msg, List<?> messages, Throwable nested) {
        super(msg, messages, nested);
    }

    public AbortException(List<?> messages, Throwable nested) {
        super(messages, nested);
    }

    public AbortException(List<?> messages) {
        super(messages);
    }

    public AbortException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public AbortException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }
}
