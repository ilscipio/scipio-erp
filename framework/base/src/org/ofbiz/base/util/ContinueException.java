package org.ofbiz.base.util;

import java.util.List;

/** General operation continue or recoverable exception (SCIPIO). */
public class ContinueException extends ControlException {
    public ContinueException() {
    }

    public ContinueException(String msg) {
        super(msg);
    }

    public ContinueException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public ContinueException(Throwable nested) {
        super(nested);
    }

    public ContinueException(String msg, List<?> messages) {
        super(msg, messages);
    }

    public ContinueException(String msg, List<?> messages, Throwable nested) {
        super(msg, messages, nested);
    }

    public ContinueException(List<?> messages, Throwable nested) {
        super(messages, nested);
    }

    public ContinueException(List<?> messages) {
        super(messages);
    }

    public ContinueException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public ContinueException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }
}
