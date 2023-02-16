package com.ilscipio.scipio.util.collections;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Map and set key ordering scheme.
 *
 * <p>SCIPIO: 3.0.0: Added.</p>
 */
public enum KeyOrder {

    NONE,
    LINKED,
    SORTED;

    public static KeyOrder from(String name, KeyOrder defaultValue) throws IllegalArgumentException {
        return (name != null && !name.isEmpty()) ? KeyOrder.valueOf(name.toUpperCase()) : defaultValue;
    }

    public static KeyOrder from(String name) throws IllegalArgumentException {
        return from(name, null);
    }

}
