package org.ofbiz.base.util.cache;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.PropertyMessage;

import java.util.List;

/**
 * General cache and {@link UtilCache} exception.
 * <p>SCIPIO: 2.1.0: Added generalized cache exception.</p>
 */
public class CacheException extends GeneralException {

    public CacheException() {
    }

    public CacheException(String msg) {
        super(msg);
    }

    public CacheException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public CacheException(Throwable nested) {
        super(nested);
    }

    public CacheException(String msg, List<?> messages) {
        super(msg, messages);
    }

    public CacheException(String msg, List<?> messages, Throwable nested) {
        super(msg, messages, nested);
    }

    public CacheException(List<?> messages, Throwable nested) {
        super(messages, nested);
    }

    public CacheException(List<?> messages) {
        super(messages);
    }

    public CacheException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public CacheException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }

}
