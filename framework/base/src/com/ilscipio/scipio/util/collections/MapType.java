package com.ilscipio.scipio.util.collections;

import org.ofbiz.base.util.UtilProperties;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Map type enum.
 *
 * <p>SCIPIO: 3.0.0: Added.</p>
 */
public enum MapType {

    GENERIC(KeyOrder.NONE, false,
            HashMap::new, m -> (m != null) ? new HashMap<>(m) : new HashMap<>(), HashMap::new),
    LINKED(KeyOrder.LINKED, false,
            LinkedHashMap::new, m -> (m != null) ? new LinkedHashMap<>(m) : new LinkedHashMap<>(), LinkedHashMap::new),
    SORTED(KeyOrder.SORTED, false,
            TreeMap::new, m -> (m != null) ? new TreeMap<>(m) : new TreeMap<>(), c -> new TreeMap<>()),
    GENERIC_CONCURRENT(KeyOrder.NONE, true,
            ConcurrentHashMap::new, m -> (m != null) ? new ConcurrentHashMap<>(m) : new ConcurrentHashMap<>(), ConcurrentHashMap::new),
    ;

    private final KeyOrder keyOrder;
    private final boolean concurrent;
    private final Supplier<Map<?, ?>> defaultFactory;
    private final Function<Map<?, ?>, Map<?, ?>> copyFactory;
    private final Function<Integer, Map<?, ?>> capacityFactory;

    MapType(KeyOrder keyOrder, boolean concurrent, Supplier<Map<? ,?>> defaultFactory,
            Function<Map<?, ?>, Map<?, ?>> copyFactory, Function<Integer, Map<?, ?>> capacityFactory) {
        this.keyOrder = keyOrder;
        this.concurrent = concurrent;
        this.defaultFactory = defaultFactory;
        this.copyFactory = copyFactory;
        this.capacityFactory = capacityFactory;
    }

    public static final MapType DEFAULT = from(UtilProperties.getPropertyValue("codelang",
            "util.collections.map.defaultType", MapType.GENERIC.toString().toLowerCase()));

    public static MapType from(String name, MapType defaultValue) throws IllegalArgumentException {
        return (name != null && !name.isEmpty()) ? MapType.valueOf(name.toUpperCase()) : defaultValue;
    }

    public static MapType from(String name) throws IllegalArgumentException {
        return from(name, null);
    }

    public static MapType from(KeyOrder keyOrder, boolean concurrent) throws IllegalArgumentException {
        if (keyOrder == null) {
            return null;
        }
        for (MapType type : values()) {
            if (concurrent == type.concurrent && keyOrder == type.keyOrder) {
                return type;
            }
        }
        throw new IllegalArgumentException("No MapType for keyOrder [" + keyOrder + "] concurrent [" + concurrent + "]");
    }

    public static MapType from(KeyOrder keyOrder) throws IllegalArgumentException {
        return from(keyOrder, false);
    }

    public KeyOrder getKeyOrder() {
        return keyOrder;
    }

    public boolean isConcurrent() {
        return concurrent;
    }

    public <K, V> Supplier<Map<K, V>> getDefaultFactory() {
        return cast(defaultFactory);
    }

    public <K, V> Function<Map<? extends K, ? extends V>, Map<K, V>> getCopyFactory() {
        return cast(copyFactory);
    }

    public <K, V> Function<Integer, Map<K, V>> getCapacityFactory() {
        return cast(capacityFactory);
    }

    @SuppressWarnings("unchecked")
    public <K, V> Map<K, V> newMap() {
        return (Map<K, V>) defaultFactory.get();
    }

    @SuppressWarnings("unchecked")
    public <K, V> Map<K, V> newMap(Map<? extends K, ? extends V> other) {
        return (Map<K, V>) copyFactory.apply(other);
    }

    @SuppressWarnings("unchecked")
    public <K, V> Map<K, V> newMap(int initialCapacity) {
        return (Map<K, V>) capacityFactory.apply(initialCapacity);
    }

    @SuppressWarnings("unchecked")
    private static <T> T cast(Object o) {
        return (T) o;
    }

}
