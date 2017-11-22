package org.ofbiz.base.util;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * SCIPIO: Used to delay property message evaluation.
 * <p>
 * New class (moved to org.ofbiz.base.util 2017-02-08).
 */
@SuppressWarnings("serial")
public abstract class PropertyMessage implements Serializable {

    public static final String module = PropertyMessage.class.getName();
    
    protected abstract String getMessageImpl(Locale locale);
    
    public static Locale getDefPropLocale() {
        return UtilProperties.getFallbackLocale();
    }
    
    public static Locale getDefSysLocale() {
        return Locale.getDefault();
    }
    
    public static Locale getLogLocale() {
        return Locale.ENGLISH;
    }
    
    public static Locale getDefExLocale() {
        return Locale.ENGLISH;
    }
    
    /**
     * Gets message in the specified locale, or in the default/fallback property locale if null ({@link UtilProperties#getFallbackLocale()}).
     */
    public String getMessage(Locale locale) {
        return getMessageImpl(locale != null ? locale : getDefPropLocale());
    }

    /**
     * Gets message in the locale of the named locale field of context, or in the default/fallback property locale if null ({@link UtilProperties#getFallbackLocale()}).
     */
    public String getMessage(Map<String, ?> context, String localeFieldName) {
        return getMessage((Locale) context.get(localeFieldName));
    }
    
    /**
     * Gets message in the locale of the common "locale" field of context, or in the default/fallback property locale if null ({@link UtilProperties#getFallbackLocale()}).
     */
    public String getMessage(Map<String, ?> context) {
        return getMessage(context, "locale");
    }
    
    /**
     * Gets message in default/fallback locale specified by locale.properties.fallback in general.properties ({@link UtilProperties#getFallbackLocale()}).
     */
    public String getDefPropLocaleMessage() {
        return getMessage(getDefPropLocale());
    }
    
    /**
     * Gets message in default system locale ({@link Locale#getDefault()}).
     */
    public String getDefSysLocaleMessage() {
        return getMessage(getDefSysLocale());
    }
    
    /**
     * Gets message in the log language (always {@link Locale#ENGLISH} in scipio/ofbiz).
     */
    public String getLogMessage() {
        return getMessage(getLogLocale());
    }
    
    /**
     * Gets message in the exception language (always {@link Locale#ENGLISH} in scipio/ofbiz).
     */
    public String getDefExLocaleMessage() {
        return getMessage(getDefExLocale());
    }
    
    /**
     * Gets message in default sys locale.
     * @deprecated 2017-11: ambiguous; use {@link #getDefaultSystemLocaleMessage} or {@link #getLogMessage} instead.
     */
    @Deprecated
    public String getMessage() {
        return getMessage(Locale.getDefault());
    }
    
    public static List<String> getMessages(Collection<PropertyMessage> propMsgs, Locale locale) {
        if (propMsgs == null) return null;
        List<String> msgs = new ArrayList<>(propMsgs.size());
        for(PropertyMessage propMsg : propMsgs) {
            if (propMsg == null) continue;
            String msg = propMsg.getMessage(locale);
            if (msg != null) msgs.add(msg);
        }
        return msgs;
    }
    
    public static List<String> getDefPropLocaleMessages(Collection<PropertyMessage> propMsgs) {
        return getMessages(propMsgs, getDefPropLocale());
    }
    
    public static List<String> getDefSysLocaleMessages(Collection<PropertyMessage> propMsgs) {
        return getMessages(propMsgs, getDefSysLocale());
    }
    
    public static List<String> getLogMessages(Collection<PropertyMessage> propMsgs) {
        return getMessages(propMsgs, getLogLocale());
    }
    
    public static List<String> getDefExLocaleMessages(Collection<PropertyMessage> propMsgs) {
        return getMessages(propMsgs, getDefExLocale());
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
    
    /**
     * Makes an appropriate PropertyMessage instance for the object instance type.
     * @param message the source message object
     * @param strict if true and not a String, Throwable or PropertyMessage, throws IllegalArgumentException; 
     *               if false, unknown type gets toString() and wrapped in StaticPropertyMessage
     */
    public static PropertyMessage makeAuto(Object message, boolean strict) {
        if (message == null) return null;
        else if (message instanceof PropertyMessage) return (PropertyMessage) message;
        else if (message instanceof String) return new StaticPropertyMessage((String) message);
        else if (message instanceof Throwable) return new ExceptionPropertyMessage((Throwable) message);
        else {
            if (strict) throw new IllegalArgumentException("cannot convert object to PropertyMessage, unknown type: " + message.getClass().getName());
            else {
                try {
                    return new StaticPropertyMessage(message.toString());
                } catch(Throwable t) {
                    // this will likely never happen, it's just to be safe in case toString() explodes on some instance
                    Debug.logError(t, "PropertyMessage.makeAuto: Unexpected error converting message to PropertyMessage instance (error message swallowed): " + t.getMessage(), module);
                    return null;
                }
            }
        }
    }
    
    /**
     * SCIPIO: Returns a new list where each given message is converted to a PropertyMessage instance
     * if it is not already one.
     * @param messages the source messages objects
     * @param strict if true and not a String, Throwable or PropertyMessage, throws IllegalArgumentException; 
     *               if false, safe mode, unknown type gets toString() and wrapped in StaticPropertyMessage, and no exceptions are thrown (logged instead)
     */
    public static List<PropertyMessage> makeAutoFromList(Collection<?> messages, boolean strict) {
        if (messages == null) return null;
        List<PropertyMessage> propMsgs = new ArrayList<>(messages.size());
        for(Object msg : propMsgs) {
            if (msg == null) {
                if (strict) {
                    throw new NullPointerException("PropertyMessage.makeAutoFromList: encountered null message in list");
                } else {
                    Debug.logWarning("PropertyMessage.makeAutoFromList: null message encountered; filtering out (caller may not be creating error messages properly)", module);
                    continue;
                }
            }
            PropertyMessage propMsg = makeAuto(msg, strict);
            if (propMsg != null) {
                propMsgs.add(propMsg);
            }
        }
        return propMsgs;
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
