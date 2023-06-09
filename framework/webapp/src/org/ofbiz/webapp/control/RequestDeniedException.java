package org.ofbiz.webapp.control;

import org.ofbiz.base.util.PropertyMessage;

import java.util.Collection;

/**
 * Thrown when a request is denied for security reasons (usually only reported in logs).
 *
 * <p>SCIPIO: 3.0.0: Added to decrease logging verbosity.</p>
 */
public class RequestDeniedException extends InvalidRequestException {

    public RequestDeniedException() {
    }

    public RequestDeniedException(String msg) {
        super(msg);
    }

    public RequestDeniedException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public RequestDeniedException(Throwable nested) {
        super(nested);
    }

    public RequestDeniedException(String msg, Collection<?> messageList) {
        super(msg, messageList);
    }

    public RequestDeniedException(String msg, Collection<?> messageList, Throwable nested) {
        super(msg, messageList, nested);
    }

    public RequestDeniedException(Collection<?> messageList, Throwable nested) {
        super(messageList, nested);
    }

    public RequestDeniedException(Collection<?> messageList) {
        super(messageList);
    }

    public RequestDeniedException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public RequestDeniedException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }

}
