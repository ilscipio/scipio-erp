package com.ilscipio.scipio.ce.util.collections;

import java.util.HashMap;
import java.util.Map;

public class ScipioHashMap<K, V> extends HashMap<K, V> implements ScipioMap<K, V> {
    public ScipioHashMap() {
    }

    public ScipioHashMap(Map<? extends K, ? extends V> m) {
        super(m);
    }

    public ScipioHashMap(Map<? extends K, ? extends V> m, Object... keyValuePairs) {
        this(m);
        putPairs(keyValuePairs);
    }

    public ScipioHashMap(int initialCapacity) {
        super(initialCapacity);
    }

    public ScipioHashMap(int initialCapacity, float loadFactor) {
        super(initialCapacity, loadFactor);
    }

    public ScipioHashMap(int initialCapacity, Map<? extends K, ? extends V> m, Object... keyValuePairs) {
        super(initialCapacity);
        putAllOrPairs(m, keyValuePairs);
    }

    public ScipioHashMap(int initialCapacity, float loadFactor, Map<? extends K, ? extends V> m,
                         Object... keyValuePairs) {
        super(initialCapacity, loadFactor);
        putAllOrPairs(m, keyValuePairs);
    }
}
