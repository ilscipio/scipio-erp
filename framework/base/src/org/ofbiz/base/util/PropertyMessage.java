package org.ofbiz.base.util;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
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

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected abstract String getMessageImpl(Locale locale);


    /*
     * ****************************************
     * Generic static fields and helpers
     * ****************************************
     */

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

    public static Locale getLocale(Map<String, ?> context) {
        return (Locale) context.get("locale");
    }


    /*
     * ****************************************
     * Message evaluation
     * ****************************************
     */

    /**
     * Evaluates message in the specified locale, or in the default/fallback property locale if null ({@link UtilProperties#getFallbackLocale()}).
     */
    public String getMessage(Locale locale) {
        return getMessageImpl(locale != null ? locale : getDefPropLocale());
    }

    /**
     * Evaluates message in the locale of the named locale field of context, or in the default/fallback property locale if null ({@link UtilProperties#getFallbackLocale()}).
     */
    public String getMessage(Map<String, ?> context, String localeFieldName) {
        return getMessage((Locale) context.get(localeFieldName));
    }

    /**
     * Evaluates message in the locale of the common "locale" field of context, or in the default/fallback property locale if null ({@link UtilProperties#getFallbackLocale()}).
     */
    public String getMessage(Map<String, ?> context) {
        return getMessage(context, "locale");
    }

    /**
     * Evaluates message in default/fallback locale specified by locale.properties.fallback in general.properties ({@link UtilProperties#getFallbackLocale()}).
     */
    public String getDefPropLocaleMessage() {
        return getMessage(getDefPropLocale());
    }

    /**
     * Evaluates message in default system locale ({@link Locale#getDefault()}).
     */
    public String getDefSysLocaleMessage() {
        return getMessage(getDefSysLocale());
    }

    /**
     * Evaluates message in the log language (always {@link Locale#ENGLISH} in scipio/ofbiz).
     */
    public String getLogMessage() {
        return getMessage(getLogLocale());
    }

    /**
     * Evaluates message in the exception language (always {@link Locale#ENGLISH} in scipio/ofbiz).
     */
    public String getDefExLocaleMessage() {
        return getMessage(getDefExLocale());
    }

    /**
     * Evaluates english message.
     */
    public String getEnMessage() {
        return getMessage(Locale.ENGLISH);
    }

    /**
     * Evaluates message in default sys locale.
     * @deprecated 2017-11: ambiguous; use appropriate method above instead ({@link #getDefaultSystemLocaleMessage}, {@link #getLogMessage}, etc.).
     */
    @Deprecated
    public String getMessage() {
        return getMessage(Locale.getDefault());
    }

    /**
     * Gets English message for debugging purposes - AVOID this method - use {@link #getMessage} or others above.
     */
    @Override
    public String toString() {
        return getEnMessage();
    }

    /**
     * Returns a list of evaluated message using given locale, one for each supplied property message.
     */
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

    /**
     * Evaluates and combines the given messages using specified delimiter.
     * The message can be PropertyMessage instances, simple strings or any class
     * for which it toString() makes sense here.
     */
    public static String combineMessages(Locale locale, String delim, Collection<?> messages) {
        StringBuilder sb = new StringBuilder();
        if (delim == null) delim = "";
        for(Object msgObj : messages) {
            String msg = null;
            if (msgObj instanceof PropertyMessage) {
                PropertyMessage propMsg = (PropertyMessage) msgObj;
                msg = propMsg.getMessage(locale);
            } else if (msgObj instanceof String) {
                msg = (String) msgObj;
            } else if (msgObj != null) {
                msg = msgObj.toString();
            }
            if (msg != null && msg.length() > 0) { // msg.trim().length()?
                if (sb.length() > 0) sb.append(delim);
                sb.append(msg);
            }
        }
        return sb.toString();
    }

    public static String combineMessages(Locale locale, String delim, Object... messages) {
        return combineMessages(locale, delim, Arrays.asList(messages));
    }

    public static String combineLogMessages(String delim, Collection<?> messages) {
        return combineMessages(getLogLocale(), delim, messages);
    }

    public static String combineLogMessages(String delim, Object... messages) {
        return combineMessages(getLogLocale(), delim, Arrays.asList(messages));
    }

    public static String combineDefPropLocaleMessages(String delim, Collection<?> messages) {
        return combineMessages(getDefPropLocale(), delim, messages);
    }

    public static String combineDefPropLocaleMessages(String delim, Object... messages) {
        return combineMessages(getDefPropLocale(), delim, Arrays.asList(messages));
    }

    public static String combineDefSysLocaleMessages(String delim, Collection<?> messages) {
        return combineMessages(getDefSysLocale(), delim, messages);
    }

    public static String combineDefSysLocaleMessages(String delim, Object... messages) {
        return combineMessages(getDefSysLocale(), delim, Arrays.asList(messages));
    }

    public static String combineDefExLocaleMessages(String delim, Collection<?> messages) {
        return combineMessages(getDefExLocale(), delim, messages);
    }

    public static String combineDefExLocaleMessages(String delim, Object... messages) {
        return combineMessages(getDefExLocale(), delim, Arrays.asList(messages));
    }

    /*
     * ****************************************
     * Factory methods
     * ****************************************
     */

    /**
     * Gets the null-returning instance, can be used to both return null values and as a special
     * token value.
     */
    public static PropertyMessage getNull() {
        return NullPropertyMessage.INSTANCE;
    }

    /**
     * Gets the empty-string-returning instance, can be used to return empty values,
     * but shouldn't be relied on as a token value.
     */
    public static PropertyMessage getEmpty() {
        return EmptyPropertyMessage.INSTANCE;
    }

    /**
     * Makes a localized property message, with delayed evaluation.
     * Similar to calling {@link UtilProperties#getMessage}, except the parameters are stored
     * and the evaluation is delayed to {#getMessage} or equivalent call.
     */
    public static PropertyMessage make(String resource, String propertyName, String prefix, String suffix, Map<String, ?> msgVars) {
        return new DynamicPropertyMessage(resource, propertyName, prefix, suffix, msgVars);
    }

    public static PropertyMessage make(String resource, String propertyName, String prefix, Map<String, ?> msgVars) {
        return new DynamicPropertyMessage(resource, propertyName, prefix, null, msgVars);
    }

    public static PropertyMessage make(String resource, String propertyName, Map<String, ?> msgVars) {
        return new DynamicPropertyMessage(resource, propertyName, null, null, msgVars);
    }

    public static PropertyMessage make(String resource, String propertyName, String prefix, String suffix) {
        return new DynamicPropertyMessage(resource, propertyName, prefix, suffix, null);
    }

    public static PropertyMessage make(String resource, String propertyName, String prefix) {
        return new DynamicPropertyMessage(resource, propertyName, prefix, null, null);
    }

    public static PropertyMessage make(String resource, String propertyName) {
        return new DynamicPropertyMessage(resource, propertyName, null, null, null);
    }

    /**
     * Makes a localized property message, with delayed evaluation.
     * Same as {@link #make(String, String, String, String, Map)} except the message variables can be passed
     * as inline vararg pairs to the method.
     */
    public static PropertyMessage makeWithVarsFull(String resource, String propertyName, String prefix, String suffix, Object... msgVars) {
        return new DynamicPropertyMessage(resource, propertyName, prefix, suffix, UtilMisc.toMap(msgVars));
    }

    public static PropertyMessage makeWithVars(String resource, String propertyName, Object... msgVars) {
        return new DynamicPropertyMessage(resource, propertyName, null, null, UtilMisc.toMap(msgVars));
    }

    /**
     * Makes a static message, which is non-localizable (only localized if caller localized it beforehand).
     */
    public static PropertyMessage makeFromStatic(String message) {
        return new StaticPropertyMessage(message);
    }

    /**
     * Wraps the whole exception in a special PropertyMessage instance, for passing around.
     */
    public static PropertyMessage makeFromException(Throwable t) {
        if (t instanceof PropertyMessageEx) {
            return new PropertyExceptionPropertyMessage((PropertyMessageEx) t, true);
        } else {
            return new StandardExceptionPropertyMessage(t);
        }
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
        else if (message instanceof String) return makeFromStatic((String) message);
        else if (message instanceof Throwable) return makeFromException((Throwable) message);
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
     * Makes an appropriate PropertyMessage instance for the object instance type.
     * Does not throw exceptions.
     * @param message the source message object
     */
    public static PropertyMessage makeAutoSafe(Object message) {
        return makeAuto(message, false);
    }

    /**
     * SCIPIO: Returns a new list where each given message is converted to a PropertyMessage instance
     * if it is not already one.
     * @param messages the source messages objects
     * @param strict if true and not a String, Throwable or PropertyMessage, throws IllegalArgumentException;
     *               if false, safe mode, unknown type gets toString() and wrapped in StaticPropertyMessage, and no exceptions are thrown (logged instead)
     */
    public static List<PropertyMessage> makeListAuto(Collection<?> messages, boolean strict) {
        if (messages == null) return null;
        List<PropertyMessage> propMsgs = new ArrayList<>(messages.size());
        for(Object msg : messages) {
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

    /**
     * SCIPIO: Returns a new list where each given message is converted to a PropertyMessage instance
     * if it is not already one.
     * Does not throw exceptions.
     * @param messages the source messages objects
     */
    public static List<PropertyMessage> makeListAutoSafe(Collection<?> messages) {
        return makeListAuto(messages, false);
    }

    /**
     * Makes a property message list from either a single message or list.
     * If messageOrList is null, this will return null (not an empty list).
     */
    public static List<PropertyMessage> makeListAuto(Object messageOrList, boolean strict) {
        if (messageOrList instanceof Collection) {
            return makeListAuto((Collection<?>) messageOrList, strict);
        } else {
            PropertyMessage msg = makeAuto(messageOrList, strict);
            if (msg == null) return null;
            List<PropertyMessage> list = new ArrayList<>();
            list.add(msg);
            return list;
        }
    }

    /**
     * Makes a property message list from either a single message or list.
     * If messageOrList is null, this will return null (not an empty list).
     * Does not throw exceptions.
     */
    public static List<PropertyMessage> makeListAutoSafe(Object messageOrList) {
        return makeListAuto(messageOrList, false);
    }


    /*
     * ****************************************
     * Instance testing
     * ****************************************
     */

    /**
     * Tests if this is the special null instance, {@link NullPropertyMessage}.
     */
    public final boolean isNullInst() {
        return this == NullPropertyMessage.INSTANCE;
    }

    /**
     * Tests if this is specifically the empty instance, {@link EmptyPropertyMessage}.
     */
    public final boolean isEmptyInst() {
        return this == EmptyPropertyMessage.INSTANCE;
    }

    /**
     * Tests if this is an always-null returning instance, best-effort.
     */
    public boolean isAlwaysNull() {
        return false;
    }

    /**
     * Tests if this is an always-empty or always-null returning instance, best-effort.
     */
    public boolean isAlwaysEmpty() {
        return false;
    }


    /*
     * ****************************************
     * Implementations & sub-classes
     * ****************************************
     */

    public static class NullPropertyMessage extends PropertyMessage {
        private static final NullPropertyMessage INSTANCE = new NullPropertyMessage();
        private NullPropertyMessage() {}

        @Override
        protected String getMessageImpl(Locale locale) {
            return null;
        }

        @Override
        public boolean isAlwaysNull() {
            return true;
        }

        @Override
        public boolean isAlwaysEmpty() {
            return true;
        }
    }

    public static class EmptyPropertyMessage extends PropertyMessage {
        private static final String EMPTY_STRING = "";
        private static final EmptyPropertyMessage INSTANCE = new EmptyPropertyMessage();

        private EmptyPropertyMessage() {}

        @Override
        protected String getMessageImpl(Locale locale) {
            return EMPTY_STRING;
        }

        @Override
        public boolean isAlwaysNull() {
            return false;
        }

        @Override
        public boolean isAlwaysEmpty() {
            return true;
        }
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

        @Override
        public boolean isAlwaysNull() {
            return (message == null);
        }

        @Override
        public boolean isAlwaysEmpty() {
            return (message == null || message.isEmpty());
        }
    }

    public abstract static class ExceptionPropertyMessage extends PropertyMessage {
        public abstract Throwable getException();
    }

    /**
     * Wraps an exception and returns its non-localized {@link Exception#getMessage()} value.
     */
    public static class StandardExceptionPropertyMessage extends ExceptionPropertyMessage {
        private final Throwable t;

        protected StandardExceptionPropertyMessage(Throwable t) {
            this.t = t;
        }

        @Override
        protected String getMessageImpl(Locale locale) {
            return getException().getMessage();
        }

        @Override
        public Throwable getException() {
            return t;
        }
    }

    /**
     * Wraps a {@link PropertyMessageEx}-implementing exception and delegates to its
     * localized PropertyMessage message via {@link PropertyMessageEx#getPropertyMessage()}.
     * NOTE: if useFallback true, it will use {@link Exception#getMessage()} if property message
     * gives null.
     */
    public static class PropertyExceptionPropertyMessage extends ExceptionPropertyMessage {
        private final PropertyMessageEx propMsgEx;
        private final boolean useFallback;

        protected PropertyExceptionPropertyMessage(PropertyMessageEx propMsgEx, boolean useFallback) {
            this.propMsgEx = propMsgEx;
            this.useFallback = useFallback;
        }

        @Override
        protected String getMessageImpl(Locale locale) {
            String msg = propMsgEx.getPropertyMessage().getMessage(locale);
            if (msg == null && useFallback) {
                msg = getException().getMessage();
            }
            return msg;
        }

        @Override
        public Throwable getException() {
            return (Throwable) propMsgEx;
        }
    }

    public static class DynamicPropertyMessage extends PropertyMessage {
        private final String resource;
        private final String propertyName;
        private final Map<String, ?> msgVars;

        private final String prefix;
        private final String suffix;

        protected DynamicPropertyMessage(String resource, String propertyName, String prefix, String suffix, Map<String, ?> msgVars) {
            this.resource = resource;
            this.propertyName = propertyName;
            this.prefix = prefix;
            this.suffix = suffix;
            this.msgVars = msgVars;
        }

        @Override
        protected String getMessageImpl(Locale locale) {
            if (msgVars != null) {
                return (prefix != null ? prefix : "") +
                        UtilProperties.getMessage(resource, propertyName, msgVars, locale) + (suffix != null ? suffix : "");
            } else {
                return (prefix != null ? prefix : "") +
                        UtilProperties.getMessage(resource, propertyName, locale) + (suffix != null ? suffix : "");
            }
        }
    }
}
