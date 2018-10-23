package com.ilscipio.scipio.cms.webapp;

@SuppressWarnings("serial")
public class WebappException extends RuntimeException {

    public WebappException(String message) {
        super(message);
    }

    public WebappException(Throwable cause) {
        super(cause);
    }

    public WebappException(String message, Throwable cause) {
        super(message, cause);
    }

}
