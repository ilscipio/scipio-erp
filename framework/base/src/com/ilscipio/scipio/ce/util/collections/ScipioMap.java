package com.ilscipio.scipio.ce.util.collections;

import java.io.Serializable;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiFunction;
import java.util.function.Supplier;

/**
 * Enhanced {@link Map} interface for Scipio ERP for generic map utilities.
 * <p>Note: For service attribute and context-related interfaces, use {@link AttrMap}; ScipioMap is a generic
 * Map extension scoped to Scipio ERP and related projects.</p>
 * <p>SCIPIO: 2.1.0: Added to replace ExtendedMap and UtilMisc.</p>
 */
public interface ScipioMap<K, V> extends Map<K, V> {

    /*
     * Factory methods
     */

    /**
     * If map is an ScipioMap, returns it as-is, otherwise returns a new {@link Wrapper} delegating wrapper, null,
     * or IllegalArgumentException if another type.
     */
    static <K, V> ScipioMap<K, V> from(Object map) {
        if (map instanceof ScipioMap) {
            @SuppressWarnings("unchecked")
            ScipioMap<K, V> scipioMap = (ScipioMap<K, V>) map;
            return scipioMap;
        } else if (map instanceof Map) {
            @SuppressWarnings("unchecked")
            Map<? extends K, ? extends V> simpleMap = (Map<? extends K, ? extends V>) map;
            return wrap(simpleMap);
        } else if (map == null) {
            return null;
        } else {
            throw new IllegalArgumentException("Unrecognized map type: [" + map.getClass().getName() + "]");
        }
    }

    /**
     * Returns a new {@link Wrapper} delegating wrapper around the map, even if it is already wrapped.
     * @see #from(Object)
     */
    static <K, V> ScipioMap<K, V> wrap(Map<? extends K, ? extends V> map) {
        return new Wrapper<K, V>(map);
    }

    /**
     * Returns the Map wrapped by the given wrapper, null if null, otherwise IllegalArgumentException.
     * <p>This may be needed because {@link ScipioMap} does not extend {@link MapWrapper}.</p>
     * @see MapWrapper#wrappedMap(Object)
     */
    static <K, V> Map<K, V> wrappedMap(Object mapWrapper) {
        return MapWrapper.wrappedMap(mapWrapper);
    }

    /**
     * Creates a new map initialized from the given key-value pairs; currently {@link LinkedHashMap}.
     * <p>Note: When the code critically requires insertion order, explicit {@link #newOrderedMap} is preferred.</p>
     * @see #newOrderedMap(Map)
     */
    static <K, V> ScipioMap<K, V> toMap(Object... keyValuePairs) {
        return toOrderedMap(keyValuePairs);
    }

    /**
     * Creates a new empty map; currently {@link LinkedHashMap}.
     * <p>Note: When the code critically requires insertion order, explicit {@link #newOrderedMap} is preferred.</p>
     * @see #newOrderedMap() 
     */
    static <K, V> ScipioMap<K, V> newMap() {
        return newOrderedMap();
    }

    /**
     * Creates a new map initialized from the given map; currently {@link LinkedHashMap}.
     * <p>Note: When the code critically requires insertion order, explicit {@link #newOrderedMap} is preferred.</p>
     * @see #newOrderedMap(Map) 
     */
    static <K, V> ScipioMap<K, V> newMap(Map<? extends K, ? extends V> map) {
        return newOrderedMap(map);
    }

    /**
     * Creates a new map initialized from the given map and key-value pairs; currently {@link LinkedHashMap}.
     * <p>Note: When the code critically requires insertion order, explicit {@link #newOrderedMap} is preferred.</p>
     * @see #newOrderedMap(Map)
     */
    static <K, V> ScipioMap<K, V> newMap(Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return newOrderedMap(map, keyValuePairs);
    }

    /**
     * Creates a new empty map from capacity options; currently {@link LinkedHashMap}.
     * <p>Note: When the code critically requires insertion order, explicit {@link #newOrderedMap} is preferred.</p>
     * @see #newOrderedMap(int)
     */
    static <K, V> ScipioMap<K, V> newMap(int initialCapacity) {
        return newOrderedMap(initialCapacity);
    }

    /**
     * Creates a new empty map from capacity options; currently {@link LinkedHashMap}.
     * <p>Note: When the code critically requires insertion order, explicit {@link #newOrderedMap} is preferred.</p>
     * @see #newOrderedMap(int, float) 
     */
    static <K, V> ScipioMap<K, V> newMap(int initialCapacity, float loadFactor) {
        return newOrderedMap(initialCapacity, loadFactor);
    }

    /**
     * Creates a new empty map from capacity options; currently {@link LinkedHashMap}.
     * <p>Note: When the code critically requires insertion order, explicit {@link #newOrderedMap} is preferred.</p>
     * @see #newOrderedMap(int, float)
     */
    static <K, V> ScipioMap<K, V> newMap(int initialCapacity, Map<? extends K, ? extends V> map,
                                         Object... keyValuePairs) {
        return newOrderedMap(initialCapacity, map, keyValuePairs);
    }

    /**
     * Creates a new empty map from capacity options; currently {@link LinkedHashMap}.
     * <p>Note: When the code critically requires insertion order, explicit {@link #newOrderedMap} is preferred.</p>
     * @see #newOrderedMap(int, float)
     */
    static <K, V> ScipioMap<K, V> newMap(int initialCapacity, float loadFactor, Map<? extends K, ? extends V> map,
                                         Object... keyValuePairs) {
        return newOrderedMap(initialCapacity, loadFactor, map, keyValuePairs);
    }

    /**
     * Creates a new, insert-order-preserving map initialized from the given key-value pairs; currently
     * {@link LinkedHashMap}.
     */
    static <K, V> ScipioMap<K, V> toOrderedMap(Object... keyValuePairs) {
        return new ScipioLinkedHashMap<>(null, keyValuePairs);
    }

    /**
     * Creates a new, insert-order-preserving empty map; currently {@link LinkedHashMap}.
     */
    static <K, V> ScipioMap<K, V> newOrderedMap() {
        return new ScipioLinkedHashMap<>();
    }

    /**
     * Creates a new, insert-order-preserving map initialized from the given map; currently {@link LinkedHashMap}.
     */
    static <K, V> ScipioMap<K, V> newOrderedMap(Map<? extends K, ? extends V> map) {
        return new ScipioLinkedHashMap<>(map);
    }

    /**
     * Creates a new, insert-order-preserving map initialized from the given map and key-value pairs; currently
     * {@link LinkedHashMap}.
     */
    static <K, V> ScipioMap<K, V> newOrderedMap(Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return new ScipioLinkedHashMap<>(map, keyValuePairs);
    }

    /**
     * Creates a new, insert-order-preserving empty map from capacity options; currently {@link LinkedHashMap}.
     */
    static <K, V> ScipioMap<K, V> newOrderedMap(int initialCapacity) {
        return new ScipioLinkedHashMap<>(initialCapacity);
    }

    /**
     * Creates a new, insert-order-preserving empty map from capacity options; currently {@link LinkedHashMap}.
     */
    static <K, V> ScipioMap<K, V> newOrderedMap(int initialCapacity, float loadFactor) {
        return new ScipioLinkedHashMap<>(initialCapacity, loadFactor);
    }

    /**
     * Creates a new, insert-order-preserving empty map from capacity options; currently {@link LinkedHashMap}.
     */
    static <K, V> ScipioMap<K, V> newOrderedMap(int initialCapacity, float loadFactor, boolean accessOrder) {
        return new ScipioLinkedHashMap<>(initialCapacity, loadFactor, accessOrder);
    }

    /**
     * Creates a new, insert-order-preserving empty map from capacity options; currently {@link LinkedHashMap}.
     */
    static <K, V> ScipioMap<K, V> newOrderedMap(int initialCapacity, Map<? extends K, ? extends V> map,
                                                Object... keyValuePairs) {
        return new ScipioLinkedHashMap<>(initialCapacity, map, keyValuePairs);
    }

    /**
     * Creates a new, insert-order-preserving empty map from capacity options; currently {@link LinkedHashMap}.
     */
    static <K, V> ScipioMap<K, V> newOrderedMap(int initialCapacity, float loadFactor,
                                                Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return new ScipioLinkedHashMap<>(initialCapacity, loadFactor, map, keyValuePairs);
    }

    /**
     * Creates a new, insert-order-preserving empty map from capacity options; currently {@link LinkedHashMap}.
     */
    static <K, V> ScipioMap<K, V> newOrderedMap(int initialCapacity, float loadFactor, boolean accessOrder,
                                                Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return new ScipioLinkedHashMap<>(initialCapacity, loadFactor, accessOrder, map, keyValuePairs);
    }

    /**
     * Creates a new unordered fast map initialized from the given key-value pairs; currently {@link HashMap}.
     */
    static <K, V> ScipioMap<K, V> toFastMap(Object... keyValuePairs) {
        return new ScipioHashMap<>(null, keyValuePairs);
    }

    /**
     * Creates a new unordered empty fast map; currently {@link HashMap}.
     */
    static <K, V> ScipioMap<K, V> newFastMap() {
        return new ScipioHashMap<>();
    }

    /**
     * Creates a new unordered fast map initialized from the given map; currently {@link HashMap}.
     */
    static <K, V> ScipioMap<K, V> newFastMap(Map<? extends K, ? extends V> map) {
        return new ScipioHashMap<>(map);
    }

    /**
     * Creates a new unordered fast map initialized from the given map and key-value pairs; currently {@link HashMap}.
     */
    static <K, V> ScipioMap<K, V> newFastMap(Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return new ScipioHashMap<>(map, keyValuePairs);
    }

    /**
     * Creates a new unordered fast map from capacity options; currently {@link HashMap}.
     */
    static <K, V> ScipioMap<K, V> newFastMap(int initialCapacity) {
        return new ScipioHashMap<>(initialCapacity);
    }

    /**
     * Creates a new unordered fast map from capacity options; currently {@link HashMap}.
     */
    static <K, V> ScipioMap<K, V> newFastMap(int initialCapacity, float loadFactor) {
        return new ScipioHashMap<>(initialCapacity, loadFactor);
    }

    /**
     * Creates a new unordered fast map from capacity options; currently {@link HashMap}.
     */
    static <K, V> ScipioMap<K, V> newFastMap(int initialCapacity, Map<? extends K, ? extends V> map,
                                             Object... keyValuePairs) {
        return new ScipioHashMap<>(initialCapacity, map, keyValuePairs);
    }

    /**
     * Creates a new unordered fast map from capacity options; currently {@link HashMap}.
     */
    static <K, V> ScipioMap<K, V> newFastMap(int initialCapacity, float loadFactor, Map<? extends K, ? extends V> map,
                                             Object... keyValuePairs) {
        return new ScipioHashMap<>(initialCapacity, loadFactor, map, keyValuePairs);
    }

    /**
     * Creates a new sorted map initialized from the given key-value pairs; currently {@link TreeMap}.
     */
    static <K, V> ScipioMap<K, V> toSortedMap(Object... keyValuePairs) {
        return new ScipioTreeMap<>(null, keyValuePairs);
    }

    /**
     * Creates a new empty sorted map; currently {@link TreeMap}.
     */
    static <K, V> ScipioMap<K, V> newSortedMap() {
        return new ScipioTreeMap<>();
    }

    /**
     * Creates a new sorted map initialized from the given map; currently {@link TreeMap}.
     */
    static <K, V> ScipioMap<K, V> newSortedMap(Map<? extends K, ? extends V> map) {
        return new ScipioTreeMap<>(map);
    }

    /**
     * Creates a new sorted map initialized from the given sorted map; currently {@link TreeMap}.
     */
    static <K, V> ScipioMap<K, V> newSortedMap(SortedMap<K, ? extends V> map) {
        return new ScipioTreeMap<>(map);
    }

    /**
     * Creates a new sorted map initialized from the given map and key-value pairs; currently {@link TreeMap}.
     */
    static <K, V> ScipioMap<K, V> newSortedMap(Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return new ScipioTreeMap<>(map, keyValuePairs);
    }

    /**
     * Creates a new sorted map initialized from the given map and key-value pairs; currently {@link TreeMap}.
     */
    static <K, V> ScipioMap<K, V> newSortedMap(SortedMap<K, ? extends V> map, Object... keyValuePairs) {
        return new ScipioTreeMap<>(map, keyValuePairs);
    }

    /**
     * Creates a new empty sorted map; currently {@link TreeMap}.
     */
    static <K, V> ScipioMap<K, V> newSortedMap(Comparator<? super K> comparator) {
        return new ScipioTreeMap<>(comparator);
    }

    /**
     * Creates a new sorted map; currently {@link TreeMap}.
     */
    static <K, V> ScipioMap<K, V> newSortedMap(Comparator<? super K> comparator, Map<? extends K, ? extends V> map,
                                               Object... keyValuePairs) {
        return new ScipioTreeMap<>(comparator, map, keyValuePairs);
    }

    /**
     * Creates a new thread-safe unordered map initialized from the given key-value pairs; currently
     * {@link ConcurrentHashMap}.
     */
    static <K, V> ScipioMap<K, V> toConcurrentMap(Object... keyValuePairs) {
        return new ScipioConcurrentHashMap<>(null, keyValuePairs);
    }

    /**
     * Creates a new empty thread-safe unordered map; currently {@link ConcurrentHashMap}.
     * <p>Note: {@link #putAll} operations are not atomic.</p>
     */
    static <K, V> ScipioMap<K, V> newConcurrentMap() {
        return new ScipioConcurrentHashMap<>();
    }

    /**
     * Creates a new thread-safe unordered map initialized from the given map; currently {@link ConcurrentHashMap}.
     */
    static <K, V> ScipioMap<K, V> newConcurrentMap(Map<? extends K, ? extends V> map) {
        return new ScipioConcurrentHashMap<>(map);
    }

    /**
     * Creates a new thread-safe unordered map initialized from the given map and key-value pairs; currently
     * {@link ConcurrentHashMap}.
     */
    static <K, V> ScipioMap<K, V> newConcurrentMap(Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return new ScipioConcurrentHashMap<>(map, keyValuePairs);
    }

    /**
     * Creates a new empty thread-safe unordered map from capacity options; currently {@link ConcurrentHashMap}.
     * <p>Note: {@link #putAll} operations are not atomic.</p>
     */
    static <K, V> ScipioMap<K, V> newConcurrentMap(int initialCapacity, float loadFactor) {
        return new ScipioConcurrentHashMap<>(initialCapacity, loadFactor);
    }

    /**
     * Creates a new empty thread-safe unordered map from capacity options; currently {@link ConcurrentHashMap}.
     * <p>Note: {@link #putAll} operations are not atomic.</p>
     */
    static <K, V> ScipioMap<K, V> newConcurrentMap(int initialCapacity, float loadFactor, int concurrencyLevel) {
        return new ScipioConcurrentHashMap<>(initialCapacity, loadFactor, concurrencyLevel);
    }

    /**
     * Creates a new empty thread-safe unordered map from capacity options; currently {@link ConcurrentHashMap}.
     * <p>Note: {@link #putAll} operations are not atomic.</p>
     */
    static <K, V> ScipioMap<K, V> newConcurrentMap(int initialCapacity, Map<? extends K, ? extends V> map,
                                                   Object... keyValuePairs) {
        return new ScipioConcurrentHashMap<>(initialCapacity, map, keyValuePairs);
    }

    /**
     * Creates a new empty thread-safe unordered map from capacity options; currently {@link ConcurrentHashMap}.
     * <p>Note: {@link #putAll} operations are not atomic.</p>
     */
    static <K, V> ScipioMap<K, V> newConcurrentMap(int initialCapacity, float loadFactor,
                                                   Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return new ScipioConcurrentHashMap<>(initialCapacity, loadFactor, map, keyValuePairs);
    }

    /**
     * Creates a new empty thread-safe unordered map from capacity options; currently {@link ConcurrentHashMap}.
     * <p>Note: {@link #putAll} operations are not atomic.</p>
     */
    static <K, V> ScipioMap<K, V> newConcurrentMap(int initialCapacity, float loadFactor, int concurrencyLevel,
                                                   Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return new ScipioConcurrentHashMap<>(initialCapacity, loadFactor, concurrencyLevel, map, keyValuePairs);
    }

    /**
     * Creates a new empty unmodifiable map; currently {@link Collections#emptyMap()}.
     */
    static <K, V> ScipioMap<K, V> emptyMap() {
        @SuppressWarnings("unchecked")
        ScipioMap<K, V> emptyMap = (ScipioMap<K, V>) Wrapper.EMPTY;
        return emptyMap;
    }

    /**
     * Creates a new unmodifiable map from given key-value pairs; currently {@link LinkedHashMap} or
     * {@link Collections#emptyMap()}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> toReadOnlyMap(Object... keyValuePairs) {
        return toReadOnlyOrderedMap(keyValuePairs);
    }

    /**
     * Creates a new unmodifiable map from given map; currently {@link LinkedHashMap} or {@link Collections#emptyMap()}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> newReadOnlyMap(Map<? extends K, ? extends V> map) {
        return newReadOnlyOrderedMap(map);
    }

    /**
     * Creates a new unmodifiable map from given map and key-value pairs; currently {@link LinkedHashMap} or
     * {@link Collections#emptyMap()}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> newReadOnlyMap(Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return newReadOnlyOrderedMap(map, keyValuePairs);
    }

    /**
     * Creates a new empty map from capacity options; currently {@link LinkedHashMap}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     * <p>Note: When the code critically requires insertion order, explicit {@link #newReadOnlyOrderedMap} is
     * preferred.</p>
     */
    static <K, V> ScipioMap<K, V> newReadOnlyMap(int initialCapacity, Map<? extends K, ? extends V> map,
                                                 Object... keyValuePairs) {
        return newReadOnlyOrderedMap(initialCapacity, map, keyValuePairs);
    }

    /**
     * Creates a new empty map from capacity options; currently {@link LinkedHashMap}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     * <p>Note: When the code critically requires insertion order, explicit {@link #newOrderedMap} is preferred.</p>
     * @see #newOrderedMap(int, float)
     */
    static <K, V> ScipioMap<K, V> newReadOnlyMap(int initialCapacity, float loadFactor, Map<? extends K, ? extends V> map,
                                                 Object... keyValuePairs) {
        return newReadOnlyOrderedMap(initialCapacity, loadFactor, map, keyValuePairs);
    }

    /**
     * Creates a new unmodifiable insertion-order-presering map from given key-value pairs; currently
     * {@link LinkedHashMap} or {@link Collections#emptyMap()}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> toReadOnlyOrderedMap(Object... keyValuePairs) {
        return new ScipioLinkedHashMap<>(null, keyValuePairs);
        //return wrap(Collections.unmodifiableMap(putAll(new LinkedHashMap<>(), firstKey, firstValue, keyValuePairs)));
    }

    /**
     * Creates a new unmodifiable insertion-order-presering map from given map; currently {@link LinkedHashMap} or
     * {@link Collections#emptyMap()}.
     * FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> newReadOnlyOrderedMap(Map<? extends K, ? extends V> map) {
        return new ScipioLinkedHashMap<>(map);
        //return !map.isEmpty() ? Collections.unmodifiableMap(new ScipioLinkedHashMap<K, V>(map)) : Collections.emptyMap();
    }

    /**
     * Creates a new unmodifiable insertion-order-presering map from given key-value pairs; currently
     * {@link LinkedHashMap} or {@link Collections#emptyMap()}.
     * FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> newReadOnlyOrderedMap(Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return new ScipioLinkedHashMap<>(map, keyValuePairs);
        //return wrap(Collections.unmodifiableMap(putAll(new LinkedHashMap<>(map), firstKey, firstValue, keyValuePairs)));
    }

    /**
     * Creates a new, insert-order-preserving empty map from capacity options; currently {@link LinkedHashMap}.
     * FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> newReadOnlyOrderedMap(int initialCapacity, Map<? extends K, ? extends V> map,
                                                        Object... keyValuePairs) {
        return new ScipioLinkedHashMap<>(initialCapacity, map, keyValuePairs);
    }

    /**
     * Creates a new, insert-order-preserving empty map from capacity options; currently {@link LinkedHashMap}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> newReadOnlyOrderedMap(int initialCapacity, float loadFactor,
                                                        Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return new ScipioLinkedHashMap<>(initialCapacity, loadFactor, map, keyValuePairs);
    }

    /**
     * Creates a new, insert-order-preserving empty map from capacity options; currently {@link LinkedHashMap}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> newReadOnlyOrderedMap(int initialCapacity, float loadFactor, boolean accessOrder,
                                                        Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return new ScipioLinkedHashMap<>(initialCapacity, loadFactor, accessOrder, map, keyValuePairs);
    }

    /**
     * Creates a new unmodifiable fast unordered map from given key-value pairs; currently {@link HashMap} or
     * {@link Collections#emptyMap()}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> toReadOnlyFastMap(Object... keyValuePairs) {
        return new ScipioHashMap<>(null, keyValuePairs);
        //return wrap(Collections.unmodifiableMap(putAll(new HashMap<>(), firstKey, firstValue, keyValuePairs)));
    }

    /**
     * Creates a new unmodifiable fast unordered map from given map; currently {@link HashMap} or
     * {@link Collections#emptyMap()}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> newReadOnlyFastMap(Map<? extends K, ? extends V> map) {
        return new ScipioHashMap<>(map);
        //return wrap(!map.isEmpty() ? Collections.unmodifiableMap(new HashMap<>(map)) : Collections.emptyMap());
    }

    /**
     * Creates a new unmodifiable fast unordered map from given map and key-value pairs; currently {@link HashMap} or
     * {@link Collections#emptyMap()}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> newReadOnlyFastMap(Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return new ScipioHashMap<>(map, keyValuePairs);
        //return wrap(Collections.unmodifiableMap(putAll(new HashMap<>(map), firstKey, firstValue, keyValuePairs)));
    }

    /**
     * Creates a new unordered fast map from capacity options; currently {@link HashMap}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> newReadOnlyFastMap(int initialCapacity, Map<? extends K, ? extends V> map,
                                                     Object... keyValuePairs) {
        return new ScipioHashMap<>(initialCapacity, map, keyValuePairs);
    }

    /**
     * Creates a new unordered fast map from capacity options; currently {@link HashMap}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> newReadOnlyFastMap(int initialCapacity, float loadFactor,
                                                     Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return new ScipioHashMap<>(initialCapacity, loadFactor, map, keyValuePairs);
    }

    /**
     * Creates a new unmodifiable sorted map from given key-value pairs; currently {@link TreeMap} or
     * {@link Collections#emptyMap()}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> toReadOnlySortedMap(Object... keyValuePairs) {
        return new ScipioTreeMap<>(null, keyValuePairs);
    }

    /**
     * Creates a new unmodifiable sorted map from given map; currently {@link TreeMap} or
     * {@link Collections#emptyMap()}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> newReadOnlySortedMap(Map<? extends K, ? extends V> map) {
        return new ScipioTreeMap<>(map);
        //return wrap(!map.isEmpty() ? Collections.unmodifiableSortedMap(new TreeMap<>(map)) : Collections.emptyMap());
    }

    /**
     * Creates a new unmodifiable sorted map from given map and key-value pairs; currently {@link TreeMap} or
     * {@link Collections#emptyMap()}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> newReadOnlySortedMap(Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return new ScipioTreeMap<>(map, keyValuePairs);
    }

    /**
     * Creates a new unmodifiable sorted map from given key-value pairs; currently {@link TreeMap} or
     * {@link Collections#emptyMap()}.
     * <p>FIXME: Not correctly using unmodifiable classes at this time; make sure to use final/volatile fields for
     * the instance or safe publishing.</p>
     */
    static <K, V> ScipioMap<K, V> newReadOnlySortedMap(Comparator<? super K> comparator,
                                                       Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        return new ScipioTreeMap<>(comparator, map, keyValuePairs);
    }

    /*
     * Map extension instance methods
     */

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
     * Puts each key-value pair into the map from a map, key-value pairs or both.
     * <p>Not atomic for concurrent operations unless overridden.</p>
     * @see #putAll(Map)
     * @see #putEntries(Iterable)
     * @see #putPairs(Object...)
     */
    default ScipioMap<K, V> putAllOrPairs(Map<? extends K, ? extends V> map, Object... keyValuePairs) {
        if (map != null) {
            putAll(map);
        }
        if (keyValuePairs.length > 0) {
            putPairs(keyValuePairs);
        }
        return this;
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

    /*
     * Abstract classes and reference implementations
     */

    class Wrapper<K, V> extends MapWrapper.Single<K, V> implements ScipioMap<K, V>, MapWrapper<K, V>, Serializable {
        private static final Wrapper<?, ?> EMPTY = new Wrapper<>(Collections.emptyMap());

        public Wrapper(Map<? extends K, ? extends V> wrappedMap) {
            super(wrappedMap);
        }
    }

    /*
     * Generic map and entry helpers
     */

    static <K, V, E extends Entry<K, V>, C extends Collection<E>> C toEntries(C out, BiFunction<K, V, E> entryProducer,
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

    static <K, V> Set<Map.Entry<K, V>> toEntrySet(Object... keyValuePairs) {
        return toEntries(new LinkedHashSet<>(), AbstractMap.SimpleEntry::new, keyValuePairs);
    }

    static <K, V> List<Map.Entry<K, V>> toEntryList(Object... keyValuePairs) {
        return toEntries(new ArrayList<>(keyValuePairs.length / 2), AbstractMap.SimpleEntry::new,
                keyValuePairs);
    }
}
