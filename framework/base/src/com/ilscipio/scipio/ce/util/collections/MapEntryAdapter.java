package com.ilscipio.scipio.ce.util.collections;

import java.util.Map;

/**
 * SCIPIO: A simple key/value pair that extends {@link Map.Entry} to provide infinite reuse.
 */
public interface MapEntryAdapter<K, V> extends Map.Entry<K, V> {

    /**
     * Sets the value, without returning the old one.
     * <p>
     * NOTE: The reason why this matters is that not all adapted collections
     * can efficiently return the old value (e.g. servlet attributes).
     */
    public default void setValueOnly(V value) {
        setValue(value);
    }

    /**
     * Optional operation: tries to remove the entry from the containing collection;
     * by default, only sets the value to null; returns the old/previous value.
     */
    public default V removeValue() {
        return setValue(null);
    }

    /**
     * Optional operation: tries to remove the entry from the containing collection;
     * by default, only sets the value to null.
     * <p>
     * NOTE: The reason why this matters is that not all adapted collections
     * can efficiently return the old value (e.g. servlet attributes).
     */
    public default void removeValueOnly() {
        removeValue(null);
    }

    /**
     * Returns the result of call to entry's {@link #getKey()} method, or null if entry is null.
     */
    public static <K, V> K getKey(Map.Entry<K, V> entry) {
        return (entry != null) ? entry.getKey() : null;
    }

    /**
     * Returns the result of call to entry's {@link #getValue()} method, or null if entry is null.
     */
    public static <K, V> V getValue(Map.Entry<K, V> entry) {
        return (entry != null) ? entry.getValue() : null;
    }

    /**
     * Returns the result of call to entry's {@link #setValue} method, or null if entry is null.
     */
    public static <K, V> V setValue(Map.Entry<K, V> entry, V value) {
        return (entry != null) ? entry.setValue(value) : null;
    }

    /**
     * Calls entry's {@link #setValueOnly} method IF it is an instance of MapEntryAdapter.
     */
    public static <K, V> void setValueOnly(Map.Entry<K, V> entry, V value) {
        if (entry instanceof MapEntryAdapter) {
            ((MapEntryAdapter<K, V>) entry).setValueOnly(value);
        } else if (entry != null) {
            entry.setValue(value);
        }
    }

    /**
     * Calls entry's {@link #setValueOnly} method.
     */
    public static <K, V> void setValueOnly(MapEntryAdapter<K, V> entry, V value) {
        if (entry != null) {
            entry.setValueOnly(value);
        }
    }

    /**
     * Calls entry's {@link #removeValue()} method IF it is an instance of MapEntryAdapter;
     * otherwise, sets the value to <code>null</code>.
     */
    public static <K, V> V removeValue(Map.Entry<K, V> entry) {
        if (entry instanceof MapEntryAdapter) {
            return ((MapEntryAdapter<K, V>) entry).removeValue();
        } else if (entry != null) {
            return entry.setValue(null);
        }
        return null;
    }

    /**
     * Calls entry's {@link #removeValue()} method.
     */
    public static <K, V> V removeValue(MapEntryAdapter<K, V> entry) {
        return (entry != null) ? entry.removeValue() : null;
    }

    /**
     * Calls entry's {@link #removeValueOnly()} method IF it is an instance of MapEntryAdapter;
     * otherwise, sets the value to <code>null</code>.
     */
    public static <K, V> void removeValueOnly(Map.Entry<K, V> entry) {
        if (entry instanceof MapEntryAdapter) {
            ((MapEntryAdapter<K, V>) entry).removeValueOnly();
        } else if (entry != null) {
            entry.setValue(null);
        }
    }
}