package org.ofbiz.base.util;

import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * SCIPIO: Utilities for implementing and using {@link PropertyMessageEx}.
 */
public abstract class PropertyMessageExUtil {

    public static final String MSG_INTRO_DELIM = ": ";

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
        return getExceptionMessage(t, PropertyMessage.getLocale(context));
    }

    /**
     * Returns the localized property exception message list, or null if none.
     */
    public static List<String> getExceptionMessageList(Throwable t, Locale locale) {
        if (t instanceof PropertyMessageEx) {
            PropertyMessageEx propEx = (PropertyMessageEx) t;
            return PropertyMessage.getMessages(propEx.getPropertyMessageList(), locale);
        }
        return null;
    }

    /**
     * Returns the localized property exception message list, or null if none.
     */
    public static List<String> getExceptionMessageList(Throwable t, Map<String, ?> context) {
        return getExceptionMessageList(t, PropertyMessage.getLocale(context));
    }

    /**
     * Returns the property exception message in log locale (english), or fallback on non-localized detail message.
     * NOTE: Depending on the exception implementation, this may be redundant, and in some cases
     * it can be better to simply call {@code t.getMessage()} instead of this.
     */
    public static String getLogLocaleExceptionMessage(Throwable t) {
        if (t instanceof PropertyMessageEx) {
            PropertyMessageEx propEx = (PropertyMessageEx) t;
            String message = propEx.getPropertyMessage().getMessage(PropertyMessage.getLogLocale());
            if (UtilValidate.isNotEmpty(message)) return message;
        }
        return t.getMessage();
    }

    /**
     * Returns the property exception message in exception locale (english), or fallback on non-localized detail message.
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

    /**
     * Makes a localized service message from the given message intro plus the property message from the exception OR
     * detail message if none (abstraction).
     * Does NOT include any message lists the exception may contain (use {@link #makeServiceMessageList}).
     */
    public static String makeServiceMessage(PropertyMessage messageIntro, Throwable t, Locale locale) {
        return PropertyMessage.combineMessages(locale, MSG_INTRO_DELIM, messageIntro, getExceptionMessage(t, locale));
    }

    /**
     * Makes a localized service message from the given message intro plus the property message from the exception OR
     * detail message if none (abstraction).
     * Does NOT include any message lists the exception may contain (use {@link #makeServiceMessageList}).
     */
    public static String makeServiceMessage(String messageIntro, Throwable t, Locale locale) {
        return makeServiceMessage(PropertyMessage.makeFromStatic(messageIntro), t, locale);
    }

    /**
     * Extracts any message lists from the exception and localizes them for service message list (abstraction).
     */
    public static List<String> makeServiceMessageList(Throwable t, Locale locale) {
        return getExceptionMessageList(t, locale);
    }

    /**
     * Makes a log-language (english) message from the given message intro plus the property message from the exception OR
     * detail message if none (abstraction).
     */
    public static String makeLogMessage(PropertyMessage messageIntro, Throwable t) {
        return PropertyMessage.combineLogMessages(MSG_INTRO_DELIM, messageIntro, getLogLocaleExceptionMessage(t));
    }

    /**
     * Makes a log-language (english) message from the given message intro plus the property message from the exception OR
     * detail message if none (abstraction).
     */
    public static String makeLogMessage(String messageIntro, Throwable t) {
        return makeLogMessage(PropertyMessage.makeFromStatic(messageIntro), t);
    }

}