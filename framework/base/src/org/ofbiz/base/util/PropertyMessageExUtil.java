package org.ofbiz.base.util;

import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * SCIPIO: Utilities for implementing and using {@link PropertyMessageEx}.
 */
public abstract class PropertyMessageExUtil {
    protected PropertyMessageExUtil() {
    }
    
    /**
     * SCIPIO: Safely makes a PropertyMessage list for storage in an exception.
     * If null given, returns null.
     */
    public static List<PropertyMessage> makePropertyMessageList(Collection<?> messages) {
        return PropertyMessage.makeListAutoSafe(messages);
    }
    
    /**
     * SCIPIO: Safely makes a PropertyMessage list for storage in an exception.
     * The argument can be a single message or collection.
     * If null given, returns null.
     */
    public static List<PropertyMessage> makePropertyMessageList(Object messageOrList) {
        return PropertyMessage.makeListAutoSafe(messageOrList);
    }
    
    /**
     * Returns the localized property exception message, or fallback on non-localized detail message.
     */
    public static String getExceptionMessage(Throwable t, Locale locale) {
        if (t instanceof PropertyMessageEx) {
            PropertyMessageEx propEx = (PropertyMessageEx) t;
            String message = propEx.getPropertyMessage().getMessage(locale);
            if (UtilValidate.isNotEmpty(message)) return message;
        }
        return t.getMessage();
    }
    
    /**
     * Returns the localized property exception message, or fallback on non-localized detail message.
     */
    public static String getExceptionMessage(Throwable t, Map<String, ?> context) {
        return getExceptionMessage(t, (Locale) context.get("locale"));
    }
    
    /**
     * Returns the property exception message in english, or fallback on non-localized detail message.
     * NOTE: Depending on the exception implementation, this may be redundant, and in some cases
     * it can be better to simply call {@code t.getMessage()} instead of this.
     */
    public static String getDefExLocaleExceptionMessage(Throwable t) {
        if (t instanceof PropertyMessageEx) {
            PropertyMessageEx propEx = (PropertyMessageEx) t;
            String message = propEx.getPropertyMessage().getMessage(PropertyMessage.getDefExLocale());
            if (UtilValidate.isNotEmpty(message)) return message;
        }
        return t.getMessage();
    }
}