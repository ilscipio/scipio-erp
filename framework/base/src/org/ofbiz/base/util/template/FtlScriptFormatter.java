package org.ofbiz.base.util.template;

import org.apache.commons.lang.StringUtils;

/**
 * SCIPIO: Freemarker script value formatter.
 * <p>
 * Used to format Java values to Freemarker.
 */
public class FtlScriptFormatter {

    public static final String module = FtlScriptFormatter.class.getName();
    
    protected String literalDQStringDefault;
    protected String literalDQGeneralDefault;
    
    protected String literalSQStringDefault;
    protected String literalSQGeneralDefault;
    
    public FtlScriptFormatter() {
        this.literalDQStringDefault = "\"\"";
        this.literalDQGeneralDefault = "\"\"";
        
        this.literalSQStringDefault = "''";
        this.literalSQGeneralDefault = "''";
    }
    
    public static FtlScriptFormatter getInstance() {
        return new FtlScriptFormatter();
    }

    /**
     * SCIPIO: Makes an escaped Freemarker string literal including enclosing double-quotes.
     * <p>
     * Intended to prevent all execution/interpretation/interpolation within the string literal.
     * May or may not produce a Freemarker raw string, as appropriate.
     */
    public String makeStringLiteral(String value) {
        if (value == null) {
            return this.literalDQStringDefault;
        } else if (value.isEmpty()) {
            return "\"\"";
        } else if (!value.contains("\"")) { // optimization (most non-empty cases fall here)
            return "r\"" + value + "\""; // use Freemarker raw string to prevent interpretation
        } else { // strings containing double quotes must be handled using a regular string literal in order to preserve them
            return "\"" + makeStringValue(value) + "\"";
        }
    }

    /**
     * SCIPIO: Makes an escaped Freemarker string value without enclosing double-quotes,
     * as intended for a regular (non-raw) string.
     * <p>
     * Intended to prevent all execution/interpretation/interpolation within the string value
     * if it is enclosed within string literal.
     * Must be non-null.
     * <p>
     * NOTE: Non-optimized (not optimizable).
     */
    public String makeStringValue(String value) {
        value = StringUtils.replace(value, "\\", "\\\\"); // prevent interpreting backslashes
        value = StringUtils.replace(value, "${", "$\\{"); // prevent executing FTL code
        value = StringUtils.replace(value, "\"", "\\\""); // escape double-quotes and prevent early string termination
        return value;
    }

    /**
     * SCIPIO: Makes an escaped Freemarker string literal including enclosing double-quotes.
     * <p>
     * Trivial implementation for non-String types (Integer, Boolean, ...), which get returned as strings. 
     */
    public <T> String makeStringLiteral(T value) {
        return (value != null) ? ("\"" + makeStringValue(value) + "\"") : this.literalDQGeneralDefault;
    }

    /**
     * SCIPIO: Makes an escaped Freemarker string value without enclosing double-quotes,
     * as intended for a regular (non-raw) string.
     * <p>
     * Trivial implementation for non-String types (Integer, Boolean, ...), which get returned as strings. 
     * Must be non-null.
     */
    public <T> String makeStringValue(T value) {
        return value.toString();
    }

    /**
     * SCIPIO: Makes an escaped Freemarker string literal including enclosing single-quotes.
     * <p>
     * Intended to prevent all execution/interpretation/interpolation within the string literal.
     * May or may not produce a Freemarker raw string, as appropriate.
     */
    public String makeStringLiteralSQ(String value) {
        if (value == null) {
            return this.literalSQStringDefault;
        } else if (value.isEmpty()) {
            return "''";
        } else if (!value.contains("'")) { // optimization (most non-empty cases fall here)
            return "r'" + value + "'"; // use Freemarker raw string to prevent interpretation
        } else { // strings containing single quotes must be handled using a regular string literal in order to preserve them
            return "'" + makeStringValueSQ(value) + "'";
        }
    }

    /**
     * SCIPIO: Makes an escaped Freemarker string value without enclosing single-quotes,
     * as intended for a regular (non-raw) string.
     * <p>
     * Intended to prevent all execution/interpretation/interpolation within the string value
     * if it is enclosed within string literal.
     * Must be non-null.
     * <p>
     * NOTE: Non-optimized (not optimizable).
     */
    public String makeStringValueSQ(String value) {
        value = StringUtils.replace(value, "\\", "\\\\"); // prevent interpreting backslashes
        value = StringUtils.replace(value, "${", "$\\{"); // prevent executing FTL code
        value = StringUtils.replace(value, "'", "\\'"); // escape single-quotes and prevent early string termination
        return value;
    }

    /**
     * SCIPIO: Makes an escaped Freemarker string literal including enclosing single-quotes.
     * <p>
     * Trivial implementation for non-String types (Integer, Boolean, ...), which get returned as strings. 
     */
    public <T> String makeStringLiteralSQ(T value) {
        return (value != null) ? ("'" + makeStringValueSQ(value) + "'") : this.literalSQGeneralDefault;
    }

    /**
     * SCIPIO: Makes an escaped Freemarker string value without enclosing single-quotes,
     * as intended for a regular (non-raw) string.
     * <p>
     * Trivial implementation for non-String types (Integer, Boolean, ...), which get returned as strings. 
     * Must be non-null.
     */
    public <T> String makeStringValueSQ(T value) {
        return value.toString();
    }


    public String getLiteralDQStringDefault() {
        return literalDQStringDefault;
    }

    public void setLiteralDQStringDefault(String literalDQStringDefault) {
        this.literalDQStringDefault = literalDQStringDefault;
    }

    public String getLiteralDQGeneralDefault() {
        return literalDQGeneralDefault;
    }

    public void setLiteralDQGeneralDefault(String literalDQGeneralDefault) {
        this.literalDQGeneralDefault = literalDQGeneralDefault;
    }

    public String getLiteralSQStringDefault() {
        return literalSQStringDefault;
    }

    public void setLiteralSQStringDefault(String literalSQStringDefault) {
        this.literalSQStringDefault = literalSQStringDefault;
    }

    public String getLiteralSQGeneralDefault() {
        return literalSQGeneralDefault;
    }

    public void setLiteralSQGeneralDefault(String literalSQGeneralDefault) {
        this.literalSQGeneralDefault = literalSQGeneralDefault;
    }

}
