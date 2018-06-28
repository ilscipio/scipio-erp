package com.ilscipio.scipio.cms;

import java.io.Serializable;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.PropertyMessage;
import org.ofbiz.base.util.PropertyMessageEx;
import org.ofbiz.service.ServiceUtil;

/**
 * Small class to format and localized the exceptions caught in services, mainly
 * to exploit {@link org.ofbiz.base.util.PropertyMessageEx}.
 * <p>
 * The outer instance is meant to be in static class variables.
 * <p>
 * NOTE: this is not done in a single static method because calling Debug.log(...)
 * from a central location has messed up call stack details before, so it must be
 * left to the caller to do the actual Debug.log call using {@link #getLogMsg()} as
 * message below. See services for examples.
 */
@SuppressWarnings("serial")
public class ServiceErrorFormatter implements Serializable {

    private final String globalLogPrefix;
    private final Precision defaultPrecision;
    private final String msgPartDelim;
    private final String defaultLogMsgGeneral;
    private final PropertyMessage defaultServMsgGeneral;

    protected ServiceErrorFormatter(String globalLogPrefix, Precision defaultPrecision, String msgPartDelim,
            String defaultLogMsgGeneral, PropertyMessage defaultServMsgGeneral) {
        this.globalLogPrefix = (globalLogPrefix != null) ? globalLogPrefix : "";
        this.defaultPrecision = (defaultPrecision != null) ? defaultPrecision : Precision.DEFAULT;
        this.msgPartDelim = (msgPartDelim != null) ? msgPartDelim : ": ";
        this.defaultLogMsgGeneral = defaultLogMsgGeneral;
        this.defaultServMsgGeneral = (defaultServMsgGeneral != null) ? defaultServMsgGeneral : PropertyMessage.getNull();
    }
    
    public ServiceErrorFormatter(String globalLogPrefix, Precision defaultPrecision) {
        this(globalLogPrefix, defaultPrecision, null, null, null);
    }
    
    public static class FormatterBuilder {
        private String globalLogPrefix;
        private Precision defaultPrecision;
        private String msgPartDelim;
        private String defaultLogMsgGeneral;
        private PropertyMessage defaultServMsgGeneral;
        
        public FormatterBuilder() {}
        public FormatterBuilder(ServiceErrorFormatter other) {
            this.globalLogPrefix = other.globalLogPrefix;
            this.defaultPrecision = other.defaultPrecision;
            this.msgPartDelim = other.msgPartDelim;
            this.defaultLogMsgGeneral = other.defaultLogMsgGeneral;
            this.defaultServMsgGeneral = other.defaultServMsgGeneral;
        }
        public FormatterBuilder(String globalLogPrefix, Precision defaultPrecision) { this.globalLogPrefix = globalLogPrefix; this.defaultPrecision = defaultPrecision; }
        
        public String getGlobalLogPrefix() { return globalLogPrefix; }
        public FormatterBuilder setGlobalLogPrefix(String globalLogPrefix) { this.globalLogPrefix = globalLogPrefix; return this; }
        public Precision getDefaultPrecision() { return defaultPrecision; }
        public FormatterBuilder setDefaultPrecision(Precision defaultPrecision) { this.defaultPrecision = defaultPrecision; return this; }
        public String getMsgPartDelim() { return msgPartDelim; }
        public FormatterBuilder setMsgPartDelim(String msgPartDelim) { this.msgPartDelim = msgPartDelim; return this; }
        public String getDefaultLogMsgGeneral() { return defaultLogMsgGeneral; }
        public FormatterBuilder setDefaultLogMsgGeneral(String defaultLogMsgGeneral) { this.defaultLogMsgGeneral = defaultLogMsgGeneral; return this; }
        public PropertyMessage getDefaultServMsgGeneral() { return defaultServMsgGeneral; }
        public FormatterBuilder setDefaultServMsgGeneral(PropertyMessage defaultServMsgGeneral) { this.defaultServMsgGeneral = defaultServMsgGeneral; return this; }

        public ServiceErrorFormatter build() { return new ServiceErrorFormatter(globalLogPrefix, defaultPrecision, 
                msgPartDelim, defaultLogMsgGeneral, defaultServMsgGeneral); }
    }
    
    public FormatterBuilder specialize() {
        return new FormatterBuilder(this);
    }
    
    public String getGlobalLogPrefix() {
        return globalLogPrefix;
    }

    public Precision getDefaultPrecision() {
        return defaultPrecision;
    }
    
    public String getMsgPartDelim() {
        return msgPartDelim;
    }

    public String getDefaultLogMsgGeneral() {
        return defaultLogMsgGeneral;
    }

    public PropertyMessage getDefaultServMsgGeneral() {
        return defaultServMsgGeneral;
    }
    
    public String getDefaultServMsgGeneral(Locale locale) {
        return (defaultServMsgGeneral != null) ? defaultServMsgGeneral.getMessage(locale) : null;
    }

    /**
     * Checks and formats error messages from the exception.
     */
    public FormattedError format(Throwable ex, Map<String, ?> context) {
        return format(getDefaultPrecision(), ex, null, null, (Locale) context.get("locale")); 
    }
    
    /**
     * Checks and formats error messages from the exception.
     * NOTE: Here the msgGeneral is used as both log and service message prefix.
     */
    public FormattedError format(Throwable ex, String msgGeneral, Map<String, ?> context) {
        return format(getDefaultPrecision(), ex, msgGeneral, PropertyMessage.makeFromStatic(msgGeneral), (Locale) context.get("locale")); 
    }
    
    /**
     * Checks and formats error messages from the exception.
     * NOTE: Here the msgGeneral is used as both log and service message prefix.
     */
    public FormattedError format(Throwable ex, PropertyMessage msgGeneral, Map<String, ?> context) {
        return format(getDefaultPrecision(), ex, null, msgGeneral, (Locale) context.get("locale")); 
    }
    
    /**
     * Checks and formats error messages from the exception.
     * NOTE: Here the log will fallback on servMsgGeneral, but the service errors
     * will NOT fallback on logMsgGeneral.
     */
    public FormattedError format(Throwable ex, String logMsgGeneral, PropertyMessage servMsgGeneral, Map<String, ?> context) {
        return format(getDefaultPrecision(), ex, logMsgGeneral, servMsgGeneral, (Locale) context.get("locale")); 
    }
    
    /**
     * Checks and formats error messages from the exception.
     * NOTE: Here the log will fallback on servMsgGeneral, but the service errors
     * will NOT fallback on logMsgGeneral.
     */
    public FormattedError format(Precision precision, Throwable ex, String logMsgGeneral, PropertyMessage servMsgGeneral, Map<String, ?> context) {
        return format(precision, ex, logMsgGeneral, servMsgGeneral, (Locale) context.get("locale"));
    } 
    
    /**
     * Checks and formats error messages from the exception.
     */
    public FormattedError format(Throwable ex, Locale locale) {
        return format(getDefaultPrecision(), ex, null, null, locale); 
    }
    
    /**
     * Checks and formats error messages from the exception.
     * NOTE: Here the msgGeneral is used as both log and service message prefix.
     */
    public FormattedError format(Throwable ex, String msgGeneral, Locale locale) {
        return format(getDefaultPrecision(), ex, msgGeneral, PropertyMessage.makeFromStatic(msgGeneral), locale); 
    }
    
    /**
     * Checks and formats error messages from the exception.
     * NOTE: Here the msgGeneral is used as both log and service message prefix.
     */
    public FormattedError format(Throwable ex, PropertyMessage msgGeneral, Locale locale) {
        return format(getDefaultPrecision(), ex, null, msgGeneral, locale); 
    }
    
    /**
     * Checks and formats error messages from the exception.
     * NOTE: Here the log will fallback on servMsgGeneral, but the service errors
     * will NOT fallback on logMsgGeneral.
     */
    public FormattedError format(Throwable ex, String logMsgGeneral, PropertyMessage servMsgGeneral, Locale locale) {
        return format(getDefaultPrecision(), ex, logMsgGeneral, servMsgGeneral, locale); 
    }
    
    /**
     * Checks and formats error messages from the exception.
     * NOTE: Here the log will fallback on servMsgGeneral, but the service errors
     * will NOT fallback on logMsgGeneral.
     */
    public FormattedError format(Precision precision, Throwable ex, String logMsgGeneral, PropertyMessage servMsgGeneral, Locale locale) {
        String logMsg = null;
        String servMsg = null;
        List<String> servMsgList = null;

        // extract PropertyMessages from exception
        if (precision.isMinimum(Precision.PROPERTY)) {
            // prop message will usually be cleaner than ex.getMessage(), enough to include in general, 
            // at least we'll assume...            
            if (ex instanceof PropertyMessageEx) {
                PropertyMessageEx propMsgEx = (PropertyMessageEx) ex;
                if (propMsgEx.getPropertyMessage() != null) {
                    servMsg = propMsgEx.getPropertyMessage().getMessage(locale);
                    logMsg = propMsgEx.getPropertyMessage().getLogMessage();
                }
                // NOTE: automatically covers GeneralException's messageList
                servMsgList = PropertyMessage.getMessages(propMsgEx.getPropertyMessageList(), locale);
            }
        }
        
        // extract standard english exception message if no ex PropertyMessage
        if (precision.isMinimum(Precision.DETAILED)) {
            if (empty(logMsg)) logMsg = ex.getMessage();
            if (empty(servMsg)) servMsg = ex.getMessage();
        }
        
        // prepend the general messages, used as prefix for more detailed ones
        logMsg = concatMsgParts((logMsgGeneral != null) ? logMsgGeneral : ((servMsgGeneral != null) ? servMsgGeneral.getLogMessage() : getDefaultLogMsgGeneral()), logMsg);
        servMsg = concatMsgParts((servMsgGeneral != null) ? servMsgGeneral.getMessage(locale) : getDefaultServMsgGeneral(locale), servMsg);
        
        // append global prefix (log msg only) and prevent empty strings
        if (empty(logMsg)) logMsg = null;
        else logMsg = getGlobalLogPrefix() + logMsg;
        if (empty(servMsg)) servMsg = null;
        
        return makeFormattedError(ex, logMsg, servMsg, servMsgList);
    }
    
    public FormattedError makeFormattedError(Throwable ex, String logMsg, String servMsg, List<String> servMsgList) {
        return new FormattedError(ex, logMsg, servMsg, servMsgList);
    }
    
    public String concatMsgParts(String first, String second) {
        return concatMsgParts(first, second, getMsgPartDelim());
    }
    
    private String concatMsgParts(String first, String second, String delim) {
        if (empty(first)) {
            if (empty(second)) return null;
            else return second;
        } else {
            if (empty(second)) return first;
            else return first + delim + second;
        }
    }
    
    private static boolean empty(String str) {
        return (str == null || str.length() == 0);
    }
    
    public static class FormattedError implements Serializable {
        private final Throwable ex;
        private final String logMsg;
        private final String servMsg;
        private final List<String> servMsgList;
        
        protected FormattedError(Throwable ex, String logMsg, String servMsg, List<String> servMsgList) {
            this.ex = ex;
            this.logMsg = logMsg;
            this.servMsg = servMsg;
            this.servMsgList = servMsgList;
        }
        
        /**
         * Pass this to {@link org.ofbiz.base.util.Debug#logError} as-is.
         */
        public Throwable getEx() {
            return ex;
        }

        /**
         * Pass this to {@link org.ofbiz.base.util.Debug#logError} as-is.
         */
        public String getLogMsg() {
            return logMsg;
        }

        public String getServMsg() {
            return servMsg;
        }

        public List<String> getServMsgList() {
            return servMsgList;
        }

        public Map<String, Object> returnError() {
            return ServiceUtil.returnError(servMsg, servMsgList);
        }
        
        public Map<String, Object> returnError(Map<String, ?> outParams) {
            Map<String, Object> result = returnError();
            if (outParams != null) result.putAll(outParams);
            return result;
        }
        
        public Map<String, Object> returnFailure() {
            return ServiceUtil.returnFailure(servMsg, servMsgList);
        }
        
        public Map<String, Object> returnFailure(Map<String, ?> outParams) {
            Map<String, Object> result = returnFailure();
            if (outParams != null) result.putAll(outParams);
            return result;
        }
        
        @Override
        public String toString() {
            return logMsg;
        }
    }
    
    
    /**
     * Controls the level of detail in the service message (not so much log message).
     */
    public enum Precision {
        /**
         * Shows only the most generic messages.
         */
        BASIC(0),
        /**
         * Shows general + localized PropertyMessage, which is usually cleaner than other exception msg.
         */
        PROPERTY(1),
        /**
         * Shows general + exception getMessage().
         */
        DETAILED(2),
        /**
         * Maybe extras.
         */
        VERBOSE(3);
        
        public static final Precision DEFAULT = DETAILED;
        
        private final int level;
        private Precision(int level) {
            this.level = level;
        }
        public int getLevel() { return level; }
        public boolean isMinimum(Precision minLevelPrecision) {
            return this.level >= minLevelPrecision.level;
        }
    }
}
