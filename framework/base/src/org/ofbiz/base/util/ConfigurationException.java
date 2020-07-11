package org.ofbiz.base.util;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.PropertyMessage;

import java.util.List;

public class ConfigurationException extends GeneralException {
    public ConfigurationException() {
    }

    public ConfigurationException(String msg) {
        super(msg);
    }

    public ConfigurationException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public ConfigurationException(Throwable nested) {
        super(nested);
    }

    public ConfigurationException(String msg, List<?> messages) {
        super(msg, messages);
    }

    public ConfigurationException(String msg, List<?> messages, Throwable nested) {
        super(msg, messages, nested);
    }

    public ConfigurationException(List<?> messages, Throwable nested) {
        super(messages, nested);
    }

    public ConfigurationException(List<?> messages) {
        super(messages);
    }

    public ConfigurationException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public ConfigurationException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }
}
