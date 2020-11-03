package org.ofbiz.base.util;

import java.io.IOException;

/**
 * An IOException during an I/O operation intended to be retryable (SCIPIO).
 */
public class RetryableIOException extends IOException {
    public RetryableIOException() {
    }

    public RetryableIOException(String message) {
        super(message);
    }

    public RetryableIOException(String message, Throwable cause) {
        super(message, cause);
    }

    public RetryableIOException(Throwable cause) {
        super(cause);
    }
}
