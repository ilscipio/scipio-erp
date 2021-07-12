package com.ilscipio.scipio.ce.util.collections;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class ScipioConcurrentHashMap<K, V> extends ConcurrentHashMap<K, V> implements ScipioMap<K, V> {
    public ScipioConcurrentHashMap() {
    }

    public ScipioConcurrentHashMap(Map<? extends K, ? extends V> m) {
        super(m);
    }

    public ScipioConcurrentHashMap(Map<? extends K, ? extends V> m, Object... keyValuePairs) {
        this(m);
        putPairs(keyValuePairs);
    }

    public ScipioConcurrentHashMap(int initialCapacity) {
        super(initialCapacity);
    }

    public ScipioConcurrentHashMap(int initialCapacity, float loadFactor) {
        super(initialCapacity, loadFactor);
    }

    public ScipioConcurrentHashMap(int initialCapacity, float loadFactor, int concurrencyLevel) {
        super(initialCapacity, loadFactor, concurrencyLevel);
    }

    public ScipioConcurrentHashMap(int initialCapacity, Map<? extends K, ? extends V> m, Object... keyValuePairs) {
        super(initialCapacity);
        putAllOrPairs(m, keyValuePairs);
    }

    public ScipioConcurrentHashMap(int initialCapacity, float loadFactor, Map<? extends K, ? extends V> m,
                                   Object... keyValuePairs) {
        super(initialCapacity, loadFactor);
        putAllOrPairs(m, keyValuePairs);
    }

    public ScipioConcurrentHashMap(int initialCapacity, float loadFactor, int concurrencyLevel,
                                   Map<? extends K, ? extends V> m, Object... keyValuePairs) {
        super(initialCapacity, loadFactor, concurrencyLevel);
        putAllOrPairs(m, keyValuePairs);
    }
}
