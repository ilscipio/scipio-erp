package org.ofbiz.base.util;

import java.util.Locale;
import java.util.Map;

/**
 * SCIPIO: Used to delay property message evaluation.
 * <p>
 * New class (moved to org.ofbiz.base.util 2017-02-08).
 */
public abstract class PropertyMessage {

    protected abstract String getMessageImpl(Locale locale);
    
    public String getMessage(Locale locale) {
        return getMessageImpl(locale != null ? locale : Locale.getDefault());
    }
    
    public String getMessage() {
        return getMessage(Locale.getDefault());
    }
    
    public String getMessage(Map<String, ?> context) {
        return getMessage((Locale) context.get("locale"));
    }
    
    public static PropertyMessage make(String resource, String propertyName, Map<String, ?> propArgs, String prefix, String suffix) {
        return new DynamicPropertyMessage(resource, propertyName, propArgs, prefix, suffix);
    }
    
    public static PropertyMessage make(String resource, String propertyName, Map<String, ?> propArgs) {
        return new DynamicPropertyMessage(resource, propertyName, propArgs, null, null);
    }
    
    public static PropertyMessage make(String resource, String propertyName, String prefix, String suffix) {
        return new DynamicPropertyMessage(resource, propertyName, null, prefix, suffix);
    }
    
    public static PropertyMessage make(String resource, String propertyName) {
        return new DynamicPropertyMessage(resource, propertyName, null, null, null);
    }
    
    public static PropertyMessage makeFromStatic(String message) {
        return new StaticPropertyMessage(message);
    }
    
    public static PropertyMessage makeFromException(Throwable t) {
        return new ExceptionPropertyMessage(t);
    }
    
    
    public static class StaticPropertyMessage extends PropertyMessage {
        private final String message;

        protected StaticPropertyMessage(String message) {
            this.message = message;
        }

        @Override
        protected String getMessageImpl(Locale locale) {
            return message;
        }
    }
    
    public static class ExceptionPropertyMessage extends PropertyMessage {
        private final Throwable t;

        protected ExceptionPropertyMessage(Throwable t) {
            this.t = t;
        }

        @Override
        protected String getMessageImpl(Locale locale) {
            return t.getMessage();
        }
    }
    
    public static class DynamicPropertyMessage extends PropertyMessage {
        private final String resource;
        private final String propertyName;
        private final Map<String, ?> propArgs;
        
        private final String prefix;
        private final String suffix;
        
        protected DynamicPropertyMessage(String resource, String propertyName, Map<String, ?> propArgs, String prefix, String suffix) {
            this.resource = resource;
            this.propertyName = propertyName;
            this.propArgs = propArgs;
            this.prefix = prefix;
            this.suffix = suffix;
        }
        
        @Override
        protected String getMessageImpl(Locale locale) {
            if (propArgs != null) {
                return (prefix != null ? prefix : "") + 
                        UtilProperties.getMessage(resource, propertyName, propArgs, locale) + (suffix != null ? suffix : "");
            } else {
                return (prefix != null ? prefix : "") + 
                        UtilProperties.getMessage(resource, propertyName, locale) + (suffix != null ? suffix : "");
            }
        }
    }
}
