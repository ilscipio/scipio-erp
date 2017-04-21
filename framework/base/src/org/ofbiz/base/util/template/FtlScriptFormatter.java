package org.ofbiz.base.util.template;

import java.util.Map;

import org.apache.commons.lang.StringUtils;

/**
 * SCIPIO: Freemarker script value formatter.
 * <p>
 * Used to format Java values to Freemarker.
 */
public class FtlScriptFormatter {

    public static final String module = FtlScriptFormatter.class.getName();
    
    
    /******************************************************/
    /* Properties */
    /******************************************************/
    
    protected String literalDQStringDefault;
    protected String literalDQGeneralDefault;
    
    protected String literalSQStringDefault;
    protected String literalSQGeneralDefault;
    

    /******************************************************/
    /* Constructors */
    /******************************************************/
    
    public FtlScriptFormatter() {
        this.literalDQStringDefault = "\"\"";
        this.literalDQGeneralDefault = "\"\"";
        
        this.literalSQStringDefault = "''";
        this.literalSQGeneralDefault = "''";
    }
    
    public static FtlScriptFormatter getInstance() {
        return new FtlScriptFormatter();
    }


    /******************************************************/
    /* Setters and configuration */
    /******************************************************/
    
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
    
    
    /******************************************************/
    /* String literal and value building methods */
    /******************************************************/
    
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
    
    
    /******************************************************/
    /* Boolean literal and value building methods */
    /******************************************************/
    
    public String makeBooleanLiteral(boolean value) {
        return Boolean.toString(value);
    }
    
    public String makeTernaryBooleanLiteral(Boolean value) {
        return value != null ? value.toString() : this.literalDQGeneralDefault;
    }
    
    public String makeBooleanLiteralSQ(boolean value) {
        return Boolean.toString(value);
    }
    
    public String makeTernaryBooleanLiteralSQ(Boolean value) {
        return value != null ? value.toString() : this.literalSQGeneralDefault;
    }

    
    /******************************************************/
    /* Versatile literal and value building methods */
    /******************************************************/

    /**
     * Makes a Freemarker literal from any supported object type.
     */
    public String makeLiteral(Object object, boolean singleQuote) {
        StringBuilder sb = new StringBuilder();
        makeLiteral(sb, object, singleQuote);
        return sb.toString();
    }
    
    /**
     * Makes a Freemarker literal from any supported object type.
     */
    public String makeLiteral(Object object) {
        StringBuilder sb = new StringBuilder();
        makeLiteral(sb, object, false);
        return sb.toString();
    }
    
    /**
     * Makes a Freemarker literal from any supported object type.
     */
    public String makeLiteralSQ(Object object) {
        StringBuilder sb = new StringBuilder();
        makeLiteral(sb, object, true);
        return sb.toString();
    }
    
    /**
     * Makes a Freemarker literal from any supported object type, automatically producing the most
     * appropriate type.
     * <p>
     * TODO: May not yet support all types.
     * <p>
     * <strong>WARN:</strong> LISTS WITH NULL VALUES CANNOT BE EXPRESSED IN FREEMARKER LITERALS AND WILL
     * BE TRUNCATED!
     */
    public void makeLiteral(StringBuilder sb, Object object, boolean singleQuote) {
        if (object == null) {
            return;
        }
        else if (object instanceof Map) {
            sb.append("{");
            int i = 0;
            for(Map.Entry<?, ?> entry : ((Map<?, ?>) object).entrySet()) {
                if (entry.getValue() != null) {
                    if (i > 0) {
                        sb.append(",");
                    }
                    makeLiteral(sb, entry.getKey(), singleQuote);
                    sb.append(":");
                    makeLiteral(sb, entry.getValue(), singleQuote);
                    i++;
                }
            }
            sb.append("}");
        } else if (object instanceof Iterable) {
            sb.append("[");
            int i = 0;
            for(Object entry : (Iterable<?>) object) {
                // FIXME?: can't add nulls! changes array size!
                if (entry != null) {
                    if (i > 0) {
                        sb.append(",");
                    }
                    makeLiteral(sb, entry, singleQuote);
                    i++;
                }
            }
            sb.append("]");
        } else if (object instanceof Object[]) {
            sb.append("[");
            int i = 0;
            for(Object entry : (Object[]) object) {
                // FIXME?: can't add nulls! changes array size!
                if (entry != null) {
                    if (i > 0) {
                        sb.append(",");
                    }
                    makeLiteral(sb, entry, singleQuote);
                    i++;
                }
            }
            sb.append("]");
        } else if (object instanceof String) {
            sb.append(singleQuote ? makeStringLiteralSQ((String) object) : makeStringLiteral((String) object)); 
        } else if (object instanceof Number) {
            sb.append(object.toString());
        } else if (object instanceof Boolean) {
            sb.append(((Boolean) object) ? "true" : "false");
        } else {
            sb.append(singleQuote ? makeStringLiteralSQ(object.toString()) : makeStringLiteral(object.toString())); 
        }
    }
    
}
