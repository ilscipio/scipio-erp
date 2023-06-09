package org.ofbiz.webapp.control;

import org.ofbiz.base.util.PropertyMessage;

import java.util.Collection;

/**
 * Thrown when a request is missing or generally invalid.
 *
 * <p>SCIPIO: 3.0.0: Added to decrease logging verbosity.</p>
 */
public class InvalidRequestException extends RequestHandlerException {

    public InvalidRequestException() {
    }

    public InvalidRequestException(String msg) {
        super(msg);
    }

    public InvalidRequestException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public InvalidRequestException(Throwable nested) {
        super(nested);
    }

    public InvalidRequestException(String msg, Collection<?> messageList) {
        super(msg, messageList);
    }

    public InvalidRequestException(String msg, Collection<?> messageList, Throwable nested) {
        super(msg, messageList, nested);
    }

    public InvalidRequestException(Collection<?> messageList, Throwable nested) {
        super(messageList, nested);
    }

    public InvalidRequestException(Collection<?> messageList) {
        super(messageList);
    }

    public InvalidRequestException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public InvalidRequestException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }

}
