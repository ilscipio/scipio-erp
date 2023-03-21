package com.ilscipio.scipio.ce.util.collections;

import org.ofbiz.base.util.UtilMisc;

import java.io.Serializable;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Supplier;

/**
 * Enhanced {@link Map} interface for Scipio ERP for generic map utilities, intended for both framework reuse of common
 * Map methods for implementations like ServiceContext and GenericValue; as a facade for client code to the numerous common utilities
 * it is not scheduled to replace {@link UtilMisc} for now (this class suffer from static method ambiguity for now).
 *
 * <p>NOTE: The ScipioMap instance methods are mostly intended as a reusable mixin (abstract class) for our
 * implementations, such that some static methods on this class return a Map instead of a ScipioMap to client code.</p>
 *
 * <p>This class's methods default to insert-order-preserving collections (LinkedHashMap).
 *
 * <p>NOTE: For service attribute and context-related interfaces, use {@link AttrMap}; ScipioMap is a generic
 * {@link Map} extension scoped to Scipio ERP and related projects and does not deal with contexts or attributes but rather
 * language-level Java Map- and primitive-level Map interface additions.</p>
 *
 * <p>SCIPIO: 3.0.0: Added {@link #of}/{@link #copyOf}, insert-order-preserving versions of {@link Map#of}/{@link Map#copyOf}.</p>
 * <p>SCIPIO: 2.1.0: Added to replace ExtendedMap and {@link UtilMisc}.</p>
 */
public interface ScipioMap<K, V> extends Map<K, V> {

    class Wrapper<K, V> extends MapWrapper.Single<K, V> implements ScipioMap<K, V>, MapWrapper<K, V>, Serializable {
        private static final Wrapper<?, ?> EMPTY = new Wrapper<>(Collections.emptyMap());

        public Wrapper(Map<? extends K, ? extends V> wrapped) {
            super(wrapped);
        }
    }

    /**
     * If map is an ScipioMap, returns it as-is, otherwise returns a new {@link Wrapper} delegating wrapper, null,
     * or IllegalArgumentException if another type.
     */
    static <K, V> ScipioMap<K, V> as(Object map) {
        if (map instanceof ScipioMap) {
            @SuppressWarnings("unchecked")
            ScipioMap<K, V> scipioMap = (ScipioMap<K, V>) map;
            return scipioMap;
        } else if (map instanceof Map) {
            @SuppressWarnings("unchecked")
            Map<? extends K, ? extends V> simpleMap = (Map<? extends K, ? extends V>) map;
            return new Wrapper<>(simpleMap);
        } else if (map == null) {
            return null;
        } else {
            throw new IllegalArgumentException("Unrecognized map type: [" + map.getClass().getName() + "]");
        }
    }

    /**
     * Returns the Map wrapped by the given wrapper, null if null, otherwise IllegalArgumentException.
     * <p>This may be needed because {@link ScipioMap} does not extend {@link MapWrapper} itself.</p>
     * @see MapWrapper#wrapped(Object)
     */
    static <K, V> Map<K, V> wrapped(Object mapWrapper) {
        return MapWrapper.wrapped(mapWrapper);
    }

    /**
     * Returns the empty unmodifiable map wrapper.
     */
    static <K, V> ScipioMap<K, V> emptyWrapper() {
        @SuppressWarnings("unchecked")
        ScipioMap<K, V> emptyWrapper = (ScipioMap<K, V>) Wrapper.EMPTY;
        return emptyWrapper;
    }

    /**
     * Create a map from passed nameX, valueX parameters, as umodifiable/read-only linked map (currently LinkedHashMap).
     *
     * <p>NOTE: This is intended as a guaranteed-safe replacement for {@link Map#of} with respect
     * insertion order guarantees.</p>
     *
     * @return The resulting Map
     */
    @SuppressWarnings("unchecked")
    static <K, V> Map<K, V> of(Object... data) {
        return Collections.unmodifiableMap(putPairs(new LinkedHashMap<>(), data));
    }

    /**
     * Create a map from passed nameX, valueX parameters, as umodifiable/read-only linked map (currently LinkedHashMap).
     *
     * <p>NOTE: This is intended as a guaranteed-safe replacement for {@link Map#copyOf(Map)} with respect
     * insertion order guarantees.</p>
     *
     * @return The resulting Map
     */
    @SuppressWarnings("unchecked")
    static <K, V> Map<K, V> ofCopy(Map<? extends K, ? extends V> map) {
        return Collections.unmodifiableMap(new LinkedHashMap<>(map));
    }



    /*
     * Map extension instance methods and helper public static methods
     */

    static <K, V, E extends Entry<K, V>, C extends Collection<E>> C entries(C out, BiFunction<K, V, E> entryProducer,
                                                                            Object... keyValuePairs) {
        if (keyValuePairs.length % 2 != 0) {
            throw new IllegalArgumentException("Uneven number of key-value pair arguments");
        } else if (keyValuePairs.length > 0) {
            List<? extends Entry<? extends K, ? extends V>> entries =
                    new ArrayList<>(keyValuePairs.length / 2);
            for (int i = 0; i < keyValuePairs.length; i += 2) {
                @SuppressWarnings("unchecked")
                K key = (K) keyValuePairs[i];
                @SuppressWarnings("unchecked")
                V value = (V) keyValuePairs[i + 1];
                out.add(entryProducer.apply(key, value));
            }
        }
        return out;
    }

    static <K, V> Set<Map.Entry<K, V>> entrySet(Object... keyValuePairs) {
        return entries(new LinkedHashSet<>(), AbstractMap.SimpleEntry::new, keyValuePairs);
    }

    static <K, V> List<Map.Entry<K, V>> entryList(Object... keyValuePairs) {
        return entries(new ArrayList<>(keyValuePairs.length / 2), AbstractMap.SimpleEntry::new, keyValuePairs);
    }

    /**
     * Puts each key-value pair in the form of {@link Map.Entry} into the map.
     * <p>Not atomic for concurrent operations unless overridden.</p>
     * @see #putAll(Map)
     * @see #putPairs(Object...)
     */
    default ScipioMap<K, V> putEntries(Iterable<? extends Map.Entry<? extends K, ? extends V>> entries) {
        for (Map.Entry<? extends K, ? extends V> entry : entries) {
            put(entry.getKey(), entry.getValue());
        }
        return this;
    }

    /**
     * Helper putAll implementation with key filter and (on caller's discretion).
     */
    static <K, V, M extends Map<K, V>> M putKeys(M out, Map<? extends K, ? extends V> in, Collection<? extends K> keys, boolean preserveNull) {
        return UtilMisc.putKeys(out, in, keys, preserveNull);
    }

    /**
     * Helper putAll implementation with key filter and (on caller's discretion).
     */
    default ScipioMap<K, V> putKeys(Map<? extends K, ? extends V> in, Collection<K> keys, boolean preserveNull) {
        return putKeys(this, in, keys, preserveNull);
    }

    /**
     * Puts each key-value pair into the map.
     * <p>Not atomic for concurrent operations.</p>
     */
    static <K, V, M extends Map<K, V>> M putPairs(M map, Object... keyValuePairs) {
        if (keyValuePairs.length % 2 != 0) {
            throw new IllegalArgumentException("Uneven number of key-value pair arguments");
        }
        for (int i = 0; i < keyValuePairs.length; i += 2) {
            @SuppressWarnings("unchecked")
            K key = (K) keyValuePairs[i];
            @SuppressWarnings("unchecked")
            V value = (V) keyValuePairs[i + 1];
            map.put(key, value);
        }
        return map;
    }

    /**
     * Puts each key-value pair into the map.
     * <p>Not atomic for concurrent operations unless overridden.</p>
     * @see #putAll(Map)
     * @see #putEntries(Iterable)
     */
    default ScipioMap<K, V> putPairs(Object... keyValuePairs) {
        return putPairs(this, keyValuePairs);
    }

//    /**
//     * Puts each key-value pair into the map.
//     * <p>Not atomic for concurrent operations unless overridden.</p>
//     * @see #putAll(Map)
//     * @see #putEntries(Iterable)
//     */
//    default ScipioMap<K, V> put(K key1, V value1, K key2, V value2, Object... keyValuePairs) {
//        put(key1, value1);
//        put(key2, value2);
//        if (keyValuePairs.length > 2) {
//            putPairs(this, keyValuePairs);
//        }
//        return this;
//    }

    /**
     * Puts each key-value pair into the map from a map, key-value pairs or both.
     *
     * <p>Not atomic for concurrent operations unless overridden.</p>
     *
     * @see #putAll(Map)
     * @see #putEntries(Iterable)
     * @see #putPairs(Object...)
     */
    default ScipioMap<K, V> putAll(Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        if (map != null) {
            putAll(map);
        }
        if (keyValuePairs.length > 0) {
            putPairs(keyValuePairs);
        }
        return this;
    }

    /**
     * Puts each key-value pair into the map from a map, key-value pairs or both.
     *
     * @see #putAll(Map)
     * @see #putEntries(Iterable)
     * @see #putPairs(Object...)
     */
    static <K, V, M extends Map<K, V>> M putAll(M out, Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        if (map != null) {
            out.putAll(map);
        }
        if (keyValuePairs.length > 0) {
            putPairs(out, keyValuePairs);
        }
        return out;
    }

    /**
     * Fetches the value for the given key using {@link #get(Object)} or the default value if
     * {@link #containsKey(Object)} is false.
     */
    default V getIfSet(Object key, Supplier<V> defaultValueSupplier) {
        V value = get(key);
        return (value != null) ? value : (containsKey(key) ? null : defaultValueSupplier.get());
    }

    /**
     * Fetches the value for the given key using {@link #get(Object)} or the default value if
     * {@link #containsKey(Object)} is false.
     */
    default V getIfSet(Object key, V defaultValue) {
        return getIfSet(key, () -> defaultValue);
    }

    /**
     * Fetches the value for the given key using {@link #get(Object)} and invokes {@link Object#toString()} on it; if
     * any value is missing or null returns the supplied default value.
     */
    static String getString(Map<?, ?> map, Object key, Supplier<String> defaultValueSupplier) {
        Object value = map.get(key);
        if (value != null) {
            value = value.toString();
            return (value != null) ? (String) value : defaultValueSupplier.get();
        } else {
            return defaultValueSupplier.get();
        }
    }

    /**
     * Fetches the value for the given key using {@link #get(Object)} and invokes {@link Object#toString()} on it; if
     * any value is missing or null returns null.
     */
    static String getString(Map<?, ?> map, Object key) {
        return getString(map, key, () -> null);
    }

    /**
     * Fetches the value for the given key using {@link #get(Object)} and invokes {@link Object#toString()} on it; if
     * any value is missing or null returns the supplied default value.
     */
    default String getString(Object key, Supplier<String> defaultValueSupplier) {
        return getString(this, key, defaultValueSupplier);
    }

    /**
     * Fetches the value for the given key using {@link #get(Object)} and invokes {@link Object#toString()} on it; if
     * any value is missing or null returns the supplied default value.
     */
    default String getString(Object key, String defaultValue) {
        return getString(key, () -> defaultValue);
    }

    /**
     * Fetches the value for the given key using {@link #get(Object)} and invokes {@link Object#toString()} on it; if
     * any value is missing or null returns null.
     */
    default String getString(Object key) {
        return getString(key, () -> null);
    }

    /**
     * Fetches the value for the given key using {@link #get(Object)} and invokes {@link Object#toString()} on it; if
     * any value is missing or null returns empty string.
     * <p>Use when you don't want null.</p>
     * <p>Can be used as safe input trim: <code>getStringOrEmpty(key).trim()</code>; trim() also works with
     * {@link #getString(Object, Supplier)} if the supplier never returns null.</p>
     */
    default String getStringOrEmpty(Object key) {
        return getString(key, () -> "");
    }

    /**
     * Fetches the value for the given key using {@link #get(Object)} and invokes {@link Object#toString()} on it; if
     * any value is missing, null or empty returns the supplied default value.
     * <p>Use when you don't want to have to test for empty string (including model optimization).</p>
     */
    default String getStringNonEmpty(Object key, Supplier<String> defaultValueSupplier) {
        String value = getString(key);
        return (value != null && !value.isEmpty()) ? value : defaultValueSupplier.get();
    }

    /**
     * Fetches the value for the given key using {@link #get(Object)} and invokes {@link Object#toString()} on it; if
     * any value is missing, null or empty returns the supplied default value.
     * <p>Use when you don't want to have to test for empty string (including model optimization).</p>
     */
    default String getStringNonEmpty(Object key, String defaultValue) {
        return getStringNonEmpty(key, () -> defaultValue);
    }

    /**
     * Fetches the value for the given key using {@link #get(Object)} and invokes {@link Object#toString()} on it; if
     * any value is missing, null or empty returns null.
     * <p>Use when you don't want to have to test for empty string (including model optimization).</p>
     */
    default String getStringNonEmpty(Object key) {
        return getStringNonEmpty(key, () -> null);
    }

    /**
     * Fetches the value for the given key using {@link #get(Object)} and invokes {@link Object#toString()} on it; if
     * any value is missing, null or empty after {@link String#trim()} returns the supplied default value.
     * <p>Use when you don't want to have to test for empty string (including model optimization) and you need
     * user-friendliness, such as accepting extra whitespace in properties.</p>
     */
    default String getStringNonEmptyTrim(Object key, Supplier<String> defaultValueSupplier) {
        String value = getString(key);
        if (value != null) {
            value = value.trim();
            return !value.isEmpty() ? value : defaultValueSupplier.get();
        } else {
            return defaultValueSupplier.get();
        }
    }

    /**
     * Fetches the value for the given key using {@link #get(Object)} and invokes {@link Object#toString()} on it; if
     * any value is missing, null or empty after {@link String#trim()} returns the supplied default value.
     * <p>Use when you don't want to have to test for empty string (including model optimization) and you need
     * user-friendliness, such as accepting extra whitespace in properties.</p>
     */
    default String getStringNonEmptyTrim(Object key, String defaultValue) {
        return getStringNonEmptyTrim(key, () -> defaultValue);
    }

    /**
     * Fetches the value for the given key using {@link #get(Object)} and invokes {@link Object#toString()} on it; if
     * any value is missing, null or empty after {@link String#trim()} returns null.
     * <p>Use when you don't want to have to test for empty string (including model optimization) and you need
     * user-friendliness, such as accepting extra whitespace in properties.</p>
     */
    default String getStringNonEmptyTrim(Object key) {
        return getStringNonEmptyTrim(key, () -> null);
    }

}
