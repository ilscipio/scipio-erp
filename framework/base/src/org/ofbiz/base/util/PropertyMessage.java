package org.ofbiz.base.util;

import java.io.Serializable;
import java.util.Locale;
import java.util.Map;

/**
 * SCIPIO: Used to delay property message evaluation.
 * <p>
 * New class (moved to org.ofbiz.base.util 2017-02-08).
 */
@SuppressWarnings("serial")
public abstract class PropertyMessage implements Serializable {

    protected abstract String getMessageImpl(Locale locale);
    
    /**
     * Gets message in the specified locale, or the default sys locale if null.
     */
    public String getMessage(Locale locale) {
        return getMessageImpl(locale != null ? locale : Locale.getDefault());
    }

    /**
     * Gets message in the locale of the named locale field, or the default sys locale if null.
     */
    public String getMessage(Map<String, ?> context, String localeFieldName) {
        return getMessage((Locale) context.get(localeFieldName));
    }
    
    /**
     * Gets message in the locale of the "locale" field, or the default sys locale if null.
     */
    public String getMessage(Map<String, ?> context) {
        return getMessage(context, "locale");
    }
    
    /**
     * Gets message in default sys locale. 
     */
    public String getDefaultLocaleMessage() {
        return getMessage(Locale.getDefault());
    }
    
    /**
     * Gets message in the log language (always english in scipio/ofbiz).
     */
    public String getLogMessage() { // logs always in english
        return getMessage(Locale.ENGLISH);
    }
    
    /**
     * Gets message in the <code>Locale.ENGLISH</code> locale (convenience method).
     */
    public String getEnMessage() {
        return getMessage(Locale.ENGLISH);
    }
    
    /**
     * Gets message in the <code>Locale.GERMAN</code> locale (convenience method).
     */
    public String getDeMessage() {
        return getMessage(Locale.GERMAN);
    }
    
    /**
     * Gets message in default sys locale.
     * @deprecated 2017-11: ambiguous; use {@link #getDefaultLocaleMessage} or {@link #getLogMessage} instead.
     */
    @Deprecated
    public String getMessage() {
        return getMessage(Locale.getDefault());
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
