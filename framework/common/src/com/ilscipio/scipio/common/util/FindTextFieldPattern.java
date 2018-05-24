package com.ilscipio.scipio.common.util;

import java.io.Serializable;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.FindServices;
import org.ofbiz.entity.condition.EntityComparisonOperator;
import org.ofbiz.entity.condition.EntityOperator;

/**
 * SCIPIO: Small utility for matching texts using the parameters from a text-find field or equivalent,
 * to emulate the otherwise DB-only find filter.
 * Adjunct to and based on {@link org.ofbiz.common.FindServices} and {@link org.ofbiz.common.FindServices#createSingleCondition).
 * Default operator is equals, case-sensitive (NOTE: this intentionally might not match UI default).
 * Added 2017-12-15.
 */
@SuppressWarnings("serial")
public class FindTextFieldPattern implements Serializable {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    @SuppressWarnings("unchecked")
    public static final EntityComparisonOperator<Comparable<Object>, Object> EQUALS_ENTITY_OPERATOR = (EntityComparisonOperator<Comparable<Object>, Object>) FindServices.getEntityOperatorsMap().get("equals");
    @SuppressWarnings("unchecked")
    public static final EntityComparisonOperator<Comparable<Object>, Object> LIKE_ENTITY_OPERATOR = (EntityComparisonOperator<Comparable<Object>, Object>) FindServices.getEntityOperatorsMap().get("like");

    public static final String DEFAULT_FIND_OPERATOR = "equals";
    public static final EntityComparisonOperator<?, ?> DEFAULT_ENTITY_OPERATOR = EQUALS_ENTITY_OPERATOR;

    protected final String pattern;
    protected final String operatorName;
    // NOTE: String doesn't implement Comparable<Object> so we have to make workarounds...
    protected final EntityComparisonOperator<?, ?> entityOperator;
    protected final boolean ignoreCase;

    public FindTextFieldPattern(String pattern, String operatorName, EntityComparisonOperator<?, ?> entityOperator,
            boolean ignoreCase) {
        this.pattern = (pattern != null) ? (ignoreCase ? pattern.toLowerCase() : pattern) : "";
        this.operatorName = operatorName;
        this.entityOperator = entityOperator;
        this.ignoreCase = ignoreCase;
    }

    public static FindTextFieldPattern fromFindOperator(String pattern, String operatorName, Boolean ignoreCase) throws IllegalArgumentException {
        if (UtilValidate.isEmpty(pattern)) return null;
        EntityComparisonOperator<?, ?> entityOperator;
        if (UtilValidate.isNotEmpty(operatorName)) {
            // based on org.ofbiz.common.FindServices.createSingleCondition(ModelField, String, Object, boolean, Delegator, Map<String, ?>)
            if (operatorName.equals("contains")) {
                entityOperator = EntityOperator.LIKE;
                pattern = "%" + pattern + "%";
            } else if ("not-contains".equals(operatorName) || "notContains".equals(operatorName)) {
                entityOperator = EntityOperator.NOT_LIKE;
                pattern = "%" + pattern + "%";
            } else if (operatorName.equals("empty")) {
                return EmptyCheckFindTextFieldPattern.INSTANCE;
            } else if (operatorName.equals("like")) {
                entityOperator = EntityOperator.LIKE;
                pattern = pattern + "%";
            } else if ("not-like".equals(operatorName) || "notLike".equals(operatorName)) {
                entityOperator = EntityOperator.NOT_LIKE;
                pattern = pattern + "%";
            } else {
                entityOperator = FindServices.getEntityOperatorsMap().get(operatorName);
            }
            if (entityOperator == null) throw new IllegalArgumentException("Invalid find operator name: " + operatorName);
        } else {
            operatorName = DEFAULT_FIND_OPERATOR;
            entityOperator = DEFAULT_ENTITY_OPERATOR;
        }
        return new FindTextFieldPattern(pattern, operatorName, entityOperator, Boolean.TRUE.equals(ignoreCase));
    }
    
    public static FindTextFieldPattern fromFindOperatorSafe(String pattern, String operatorName, Boolean ignoreCase) {
        try {
            return fromFindOperator(pattern, operatorName, ignoreCase);
        } catch(Exception e) {
            Debug.logError("Error creating find text field pattern '" + pattern 
                    + "' for operator '" + operatorName + "': " + e.getMessage(), module);
            return null;
        }
    }
    
    public static FindTextFieldPattern fromFindOperatorSafe(String pattern, String operatorName, Object ignoreCase) throws IllegalArgumentException {
        return fromFindOperatorSafe(pattern, operatorName, UtilMisc.booleanValueVersatile(ignoreCase));
    }
    
    public static FindTextFieldPattern fromStrictEqualsOperator(String pattern) {
        return new FindTextFieldPattern(pattern, "equals", EQUALS_ENTITY_OPERATOR, false);
    }
    
    public static FindTextFieldPattern fromPermissiveContainsOperator(String pattern) {
        return new FindTextFieldPattern(pattern, "like", LIKE_ENTITY_OPERATOR, true);
    }

    public String getPattern() {
        return pattern;
    }

    public String getOperatorName() {
        return operatorName;
    }

    public EntityComparisonOperator<?, ?> getEntityOperator() {
        return entityOperator;
    }

    public boolean isIgnoreCase() {
        return ignoreCase;
    }
    
    @SuppressWarnings("unchecked")
    public boolean matches(String text) {
        if (ignoreCase) text = text.toLowerCase();
        // NOTE: ridiculous workarounds due to questionable use of interfaces in EntityComparisonOperator
        EntityComparisonOperator<Comparable<Object>, Object> op = (EntityComparisonOperator<Comparable<Object>, Object>) entityOperator;
        // WARN: String does NOT implement Comparable<Object>, but we have to make this work anyway through erasure
        final Object targetTextObj = text;
        return op.compare((Comparable<Object>) targetTextObj, pattern);
//        return op.compare(new Comparable<Object>() {
//            @Override
//            public int compareTo(Object o) {
//                return targetText.compareTo((String) o);
//            }
//        }, pattern);
    }
    
    public boolean matchesSafe(String text) {
        try {
            return matches(text);
        } catch(Exception e) {
            Debug.logError("Error matching text '" + text + "' to find text field pattern '" + pattern 
                    + "' using operator '" + operatorName + "': " + e.getMessage(), module);
            return false;
        }
    }
    
    // TODO: in, not-in operators (rhs is Collection)
    
    private static class EmptyCheckFindTextFieldPattern extends FindTextFieldPattern {
        private static final EmptyCheckFindTextFieldPattern INSTANCE = new EmptyCheckFindTextFieldPattern();
        
        private EmptyCheckFindTextFieldPattern() {
            super("", "empty", null, false);
        }
        @Override
        public boolean matches(String text) {
            return UtilValidate.isEmpty(text);
        }
    }
}