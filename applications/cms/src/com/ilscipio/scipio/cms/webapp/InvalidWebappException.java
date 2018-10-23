package com.ilscipio.scipio.cms.webapp;


/**
 * Exception thrown when an otherwise valid webapp ID of any sort is used to and
 * fails to resolve to a webapp.
 */
@SuppressWarnings("serial")
public class InvalidWebappException extends WebappException {

    public InvalidWebappException(String message) {
        super(message);
    }

    public InvalidWebappException(Throwable cause) {
        super(cause);
    }

    public InvalidWebappException(String message, Throwable cause) {
        super(message, cause);
    }

}
