package com.ilscipio.scipio.entity;

import org.ofbiz.entity.GenericValue;

/**
 * Identifies objects that wrap a main {@link GenericValue} (SCIPIO).
 * <p></p>
 * <p>SCIPIO: 2.1.0: Added.</p>
 */
public interface GenericValueBean {

    GenericValue getEntityValue();

    default <T> T get(String fieldName) {
        @SuppressWarnings("unchecked")
        T value = (T) getEntityValue().get(fieldName);
        return value;
    }

}
