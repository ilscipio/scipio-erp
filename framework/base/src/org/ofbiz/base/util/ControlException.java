package org.ofbiz.base.util;

import java.util.List;

/**
 * Control operation (SCIPIO).
 * @see AbortException
 * @see ContinueException
 */
public class ControlException extends GeneralException {
    public ControlException() {
    }

    public ControlException(String msg) {
        super(msg);
    }

    public ControlException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public ControlException(Throwable nested) {
        super(nested);
    }

    public ControlException(String msg, List<?> messages) {
        super(msg, messages);
    }

    public ControlException(String msg, List<?> messages, Throwable nested) {
        super(msg, messages, nested);
    }

    public ControlException(List<?> messages, Throwable nested) {
        super(messages, nested);
    }

    public ControlException(List<?> messages) {
        super(messages);
    }

    public ControlException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public ControlException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }
}
