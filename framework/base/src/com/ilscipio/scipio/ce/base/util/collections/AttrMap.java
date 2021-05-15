package com.ilscipio.scipio.ce.base.util.collections;

import java.util.Map;
import java.util.function.Supplier;

/**
 * Extensions to Map interface for service contexts and any Map implementation needing helper getters.
 * <p>SCIPIO: 2.1.0: Added for ServiceContext integration.</p>
 */
public interface AttrMap<K, V> extends Map<K, V> {

    /**
     * If map is an AttrMap, returns it as-is, otherwise returns a new WrapperAttrMap around the map.
     */
    @SuppressWarnings("unchecked")
    static <K, V> AttrMap<K, V> from(Map<K, V> map) {
        return (map instanceof AttrMap) ? (AttrMap<K, V>) map : wrap(map);
    }

    /**
     * Returns a new WrapperAttrMap around the map (even if it already an AttrMap).
     */
    static <K, V> AttrMap<K, V> wrap(Map<K, V> map) {
        return WrapperAttrMap.wrap(map);
    }

    /**
     * Returns an attribute value from the context/map, or null.
     */
    @SuppressWarnings("unchecked")
    default <T> T attr(Object key) {
        return (T) get(key);
    }

    /**
     * Returns an attribute value from the context/map, or if null, the given default value.
     */
    default <T> T attr(Object key, T defaultValue) {
        T value = attr(key);
        return (value != null) ? value : defaultValue;
    }

    /**
     * Returns an attribute value from the context/map, or if null, the given default value supplied
     * by the given supplier callback or lambda function.
     */
    default <T> T attr(Object key, Supplier<? extends T> defaultValueSupplier) {
        T value = attr(key);
        return (value != null) ? value : defaultValueSupplier.get();
    }

    /**
     * Returns an attribute value from the context/map and invokes toString() on it, or null.
     */
    @SuppressWarnings("unchecked")
    default String getString(Object key) {
        Object value = get(key);
        return (value != null) ? value.toString() : null;
    }

    /**
     * Returns an attribute value from the context/map and invokes toString() on it, or if null, the given default value.
     */
    default String getString(Object key, String defaultValue) {
        String value = getString(key);
        return (value != null) ? value : defaultValue;
    }

    /**
     * Returns an attribute value from the context/map and invokes toString() on it, or if null, the given default value supplied
     * by the given supplier callback or lambda function.
     */
    default String getString(Object key, Supplier<String> defaultValueSupplier) {
        String value = getString(key);
        return (value != null) ? value : defaultValueSupplier.get();
    }

}
