package org.ofbiz.base.util;

/**
 * SCIPIO: Generic IllegalArgumentException that indicates a record was not found, such as an entity.
 */
public class RecordNotFoundException extends IllegalArgumentException {

    public RecordNotFoundException() {
    }

    public RecordNotFoundException(String s) {
        super(s);
    }

    public RecordNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }

    public RecordNotFoundException(Throwable cause) {
        super(cause);
    }

}
