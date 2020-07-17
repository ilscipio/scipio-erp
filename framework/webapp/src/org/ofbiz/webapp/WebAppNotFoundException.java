package org.ofbiz.webapp;

public class WebAppNotFoundException extends IllegalArgumentException { // SCIPIO
    public WebAppNotFoundException() {
    }

    public WebAppNotFoundException(String s) {
        super(s);
    }

    public WebAppNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }

    public WebAppNotFoundException(Throwable cause) {
        super(cause);
    }
}
