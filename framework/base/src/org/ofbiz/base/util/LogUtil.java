package org.ofbiz.base.util;

import java.util.Map;

/**
 * Higher-level common logging utilities for use with {@link Debug} (SCIPIO).
 */
public abstract class LogUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Formats key-value pairs for logging purposes, following the framework map format: <code>key1=value1, key2=value2, ...</code>.
     * Used to describe/identify classes or entity values in logs. NOTE: By convention, the result is usually inserted between brackets.
     */
    public static <K> StringBuilder toLog(Map<K, ?> map, K... keys) {
        StringBuilder sb = new StringBuilder();
        for(K key : keys) {
            if (sb.length() > 0) {
                sb.append(", ");
            }
            sb.append(key);
            sb.append('=');
            sb.append(map.get(key));
        }
        return sb;
    }

    /**
     * Formats key-value pairs with non-null values for logging purposes, following the framework map format: <code>key1=value1, key2=value2, ...</code>.
     * Used to describe/identify classes or entity values in logs. NOTE: By convention, the result is usually inserted between brackets.
     */
    public static <K> StringBuilder toLogNonNull(Map<K, ?> map, K... keys) {
        StringBuilder sb = new StringBuilder();
        for(K key : keys) {
            Object value = map.get(key);
            if (value == null) {
                continue;
            }
            if (sb.length() > 0) {
                sb.append(", ");
            }
            sb.append(key);
            sb.append('=');
            sb.append(value);
        }
        return sb;
    }

    /**
     * SCIPIO: Formats key-value pairs for logging purposes, following the framework map format: <code>key1=value1, key2=value2, ...</code>.
     * Used to describe/identify classes or entity values in logs. NOTE: By convention, the result is usually inserted between brackets.
     */
    public static StringBuilder toLogProps(Object... keyValuePairs) {
        StringBuilder sb = new StringBuilder();
        for(int i=0; i < keyValuePairs.length; i += 2) {
            if (sb.length() > 0) {
                sb.append(", ");
            }
            sb.append(keyValuePairs[i]);
            sb.append('=');
            sb.append(keyValuePairs[i+1]);
        }
        return sb;
    }

    /**
     * SCIPIO: Appends key-value pairs with non-null values for logging purposes, following the framework map format: <code>key1=value1, key2=value2, ...</code>.
     * Used to describe/identify classes or entity values in logs. NOTE: By convention, the result is usually inserted between brackets.
     */
    public static StringBuilder toLogPropsNonNull(Object... keyValuePairs) {
        StringBuilder sb = new StringBuilder();
        for(int i=0; i < keyValuePairs.length; i += 2) {
            if (keyValuePairs[i+1] != null) {
                if (sb.length() > 0) {
                    sb.append(", ");
                }
                sb.append(keyValuePairs[i]);
                sb.append('=');
                sb.append(keyValuePairs[i+1]);
            }
        }
        return sb;
    }

}
