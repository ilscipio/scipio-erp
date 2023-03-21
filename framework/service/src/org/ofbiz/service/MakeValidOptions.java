package org.ofbiz.service;

import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

/**
 * Options for {@link ModelService#makeValid} methods.
 *
 * <p>SCIPIO: 3.0.0: Added options to simplify interface and delegation.</p>
 */
public class MakeValidOptions {

    public static final MakeValidOptions DEFAULT = new MakeValidOptions();

    protected Map<String, Object> targetContext;
    protected Boolean includeInternal;
    protected List<Object> errorMessages;
    protected TimeZone timeZone;
    protected Locale locale;
    protected String namePrefix;
    protected String toNamePrefix;

    public MakeValidOptions() {
    }

    public MakeValidOptions(MakeValidOptions other) {
        this.targetContext = other.targetContext;
        this.includeInternal = other.includeInternal;
        this.errorMessages = other.errorMessages;
        this.timeZone = other.timeZone;
        this.locale = other.locale;
        this.namePrefix = other.namePrefix;
        this.toNamePrefix = other.toNamePrefix;
    }

    protected MakeValidOptions(Map<String, Object> targetContext, Boolean includeInternal, List<Object> errorMessages, TimeZone timeZone, Locale locale, String namePrefix, String toNamePrefix) {
        this.targetContext = targetContext;
        this.includeInternal = includeInternal;
        this.errorMessages = errorMessages;
        this.timeZone = timeZone;
        this.locale = locale;
        this.namePrefix = namePrefix;
        this.toNamePrefix = toNamePrefix;
    }

    protected MakeValidOptions(Boolean includeInternal, List<Object> errorMessages, TimeZone timeZone, Locale locale) {
        this(null, includeInternal, errorMessages, timeZone, locale, null, null);
    }

    public Map<String, Object> targetContext() {
        return targetContext;
    }

    public MakeValidOptions targetContext(Map<String, Object> targetContext) {
        this.targetContext = targetContext;
        return this;
    }

    /**
     * When false will exclude internal fields.
     */
    public Boolean includeInternal() {
        return includeInternal;
    }

    /**
     * When false will exclude internal fields; default true.
     */
    public MakeValidOptions includeInternal(Boolean includeInternal) {
        this.includeInternal = includeInternal;
        return this;
    }

    public List<Object> errorMessages() {
        return errorMessages;
    }

    public MakeValidOptions errorMessages(List<Object> errorMessages) {
        this.errorMessages = errorMessages;
        return this;
    }

    public TimeZone timeZone() {
        return timeZone;
    }

    public MakeValidOptions timeZone(TimeZone timeZone) {
        this.timeZone = timeZone;
        return this;
    }

    public Locale locale() {
        return locale;
    }

    public MakeValidOptions locale(Locale locale) {
        this.locale = locale;
        return this;
    }

    public String namePrefix() {
        return namePrefix;
    }

    public MakeValidOptions namePrefix(String namePrefix) {
        this.namePrefix = namePrefix;
        return this;
    }

    public String toNamePrefix() {
        return toNamePrefix;
    }

    public MakeValidOptions toNamePrefix(String toNamePrefix) {
        this.toNamePrefix = toNamePrefix;
        return this;
    }
}
