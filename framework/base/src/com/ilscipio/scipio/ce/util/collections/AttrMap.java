package com.ilscipio.scipio.ce.util.collections;

import org.ofbiz.base.util.UtilValidate;

import java.io.Serializable;
import java.util.Collections;
import java.util.Map;
import java.util.function.Supplier;

/**
 * Extensions to {@link Map} interface for service contexts and any implementation needing field- and parameter-related
 * accessors and helpers.
 * <p>This actually extends {@link ScipioMap}, which contains generic Map language extensions; AttrMap
 * relates specifically to service attributes, fields and contexts in Scipio entity and service frameworks. ScipioMap is
 * formally parametrized whereas AttrMap has String keys and Object values though casting is allowed.</p>
 * <p>SCIPIO: 2.1.0: Added for ServiceContext integration; named after the {@link #attr} method for obviousness.</p>
 */
public interface AttrMap extends ScipioMap<String, Object> {

    /**
     * If map is an AttrMap, returns it as-is, otherwise returns a new {@link Wrapper} around the map, null, or
     * IllegalArgumentException if another type.
     */
    static AttrMap as(Object map) {
        if (map instanceof AttrMap) {
            return (AttrMap) map;
        } else if (map instanceof Map) {
            @SuppressWarnings("unchecked")
            Map<String, ?> simpleMap = (Map<String, ?>) map;
            return wrap(simpleMap);
        } else if (map == null) {
            return null;
        } else {
            throw new IllegalArgumentException("Not a map, got type [" + map.getClass().getName() + "]");
        }
    }

    /**
     * Returns a new {@link Wrapper} around the map, even if it already one.
     * @see #as(Object)
     */
    static AttrMap wrap(Map<String, ?> map) {
        return new Wrapper(map);
    }

    /**
     * Creates a new empty unmodifiable map; currently {@link Collections#emptyMap()}.
     */
    static AttrMap emptyMap() {
        return AttrMap.Wrapper.EMPTY;
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
     * Returns an attribute value from the context/map, or if null, the given default value.
     */
    default <T> T attr(Object key, T defaultValue) {
        return attr(key, () -> defaultValue);
    }

    /**
     * Returns an attribute value from the context/map, or null.
     */
    default <T> T attr(Object key) {
        @SuppressWarnings("unchecked")
        T value = (T) get(key);
        return value;
    }

    /**
     * Returns an attribute value from the context/map only if {@link #containsKey(Object)} returns true, or otherwise the given default value supplied
     * by the given supplier callback or lambda function.
     */
    default <T> T attrIfSet(Object key, Supplier<? extends T> defaultValueSupplier) {
        return containsKey(key) ? attr(key) : defaultValueSupplier.get();
    }

    /**
     * Returns an attribute value from the context/map only if {@link #containsKey(Object)} returns true, or otherwise the given default value.
     */
    default <T> T attrIfSet(Object key, T defaultValue) {
        return attrIfSet(key, () -> defaultValue);
    }

    /**
     * Returns an attribute value from the context/map only if {@link UtilValidate#isNotEmpty(Object)} returns true, or otherwise the given default value supplied
     * by the given supplier callback or lambda function.
     */
    default <T> T attrNonEmpty(Object key, Supplier<? extends T> defaultValueSupplier) {
        T value = attr(key);
        return UtilValidate.isNotEmpty(value) ? value : defaultValueSupplier.get();
    }

    /**
     * Returns an attribute value from the context/map only if {@link UtilValidate#isNotEmpty(Object)} returns true, or otherwise the given default value.
     */
    default <T> T attrNonEmpty(Object key, T defaultValue) {
        return attrNonEmpty(key, () -> defaultValue);
    }

    /**
     * Returns an attribute value from the context/map only if {@link UtilValidate#isNotEmpty(Object)} returns true, or otherwise null.
     */
    default <T> T attrNonEmpty(Object key) {
        return attrNonEmpty(key, () -> null);
    }

    /*
     * Abstract classes and reference implementations
     */

    class Wrapper extends MapWrapper.Single<String, Object> implements AttrMap, Serializable {
        private static final AttrMap.Wrapper EMPTY = new AttrMap.Wrapper(Collections.emptyMap());

        public Wrapper(Map<String, ?> wrappedMap) {
            super(wrappedMap);
        }
    }
}
