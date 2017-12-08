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
        return getExceptionMessageList(t, (Locale) context.get("locale"));
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
    
//    /**
//     * Creates a service error result from the exception containing the main exception message
//     * and message list, taken from {@link PropertyMessageEx}, with optional intro message.
//     */
//    public static Map<String, Object> makeServiceErrorResult(PropertyMessage messageIntro, Throwable t, Locale locale) {
//        return ServiceUtil.returnError(makeServiceMessage(messageIntro, t, locale), getExceptionMessageList(t, locale));
//    }
//    
//    /**
//     * Creates a service error result from the exception containing the main exception message
//     * and message list, taken from {@link PropertyMessageEx}, with optional intro message.
//     */
//    public static Map<String, Object> makeServiceErrorResult(String messageIntro, Throwable t, Locale locale) {
//        return makeServiceErrorResult(PropertyMessage.makeFromStatic(messageIntro), t, locale);
//    }
//    
//    /**
//     * Creates a service error result from the exception containing the main exception message
//     * and message list, taken from {@link PropertyMessageEx}.
//     */
//    public static Map<String, Object> makeServiceErrorResult(Throwable t, Locale locale) {
//        return makeServiceErrorResult((PropertyMessage) null, t, locale);
//    }
//    
//    /**
//     * Creates a service failure result from the exception containing the main exception message
//     * and message list, taken from {@link PropertyMessageEx}, with optional intro message.
//     */
//    public static Map<String, Object> makeServiceFailureResult(PropertyMessage messageIntro, Throwable t, Locale locale) {
//        return ServiceUtil.returnFailure(makeServiceMessage(messageIntro, t, locale), getExceptionMessageList(t, locale));
//    }
//    
//    /**
//     * Creates a service failure result from the exception containing the main exception message
//     * and message list, taken from {@link PropertyMessageEx}, with optional intro message.
//     */
//    public static Map<String, Object> makeServiceFailureResult(String messageIntro, Throwable t, Locale locale) {
//        return makeServiceFailureResult(PropertyMessage.makeFromStatic(messageIntro), t, locale);
//    }
//    
//    /**
//     * Creates a service failure result from the exception containing the main exception message
//     * and message list, taken from {@link PropertyMessageEx}.
//     */
//    public static Map<String, Object> makeServiceFailureResult(Throwable t, Locale locale) {
//        return makeServiceFailureResult((PropertyMessage) null, t, locale);
//    }
    
    public static String makeServiceMessage(PropertyMessage messageIntro, Throwable t, Locale locale) {
        String msg = null;
        if (messageIntro != null) msg = messageIntro.getMessage(locale);
        String exMsg = getExceptionMessage(t, locale);
        if (UtilValidate.isNotEmpty(msg)) {
            if (UtilValidate.isNotEmpty(exMsg)) msg += ": " + exMsg;
        } else {
            msg = exMsg;
        }
        return msg;
    }
    
}