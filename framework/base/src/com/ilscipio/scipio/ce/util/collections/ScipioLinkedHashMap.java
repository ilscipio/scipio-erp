package com.ilscipio.scipio.ce.util.collections;

import java.util.LinkedHashMap;
import java.util.Map;

public class ScipioLinkedHashMap<K, V> extends LinkedHashMap<K, V> implements ScipioMap<K, V> {
    public ScipioLinkedHashMap() {
    }

    public ScipioLinkedHashMap(Map<? extends K, ? extends V> m) {
        super(m);
    }

    public ScipioLinkedHashMap(Map<? extends K, ? extends V> m, Object... keyValuePairs) {
        this(m);
        putPairs(keyValuePairs);
    }

    public ScipioLinkedHashMap(int initialCapacity) {
        super(initialCapacity);
    }

    public ScipioLinkedHashMap(int initialCapacity, float loadFactor) {
        super(initialCapacity, loadFactor);
    }

    public ScipioLinkedHashMap(int initialCapacity, float loadFactor, boolean accessOrder) {
        super(initialCapacity, loadFactor, accessOrder);
    }

    public ScipioLinkedHashMap(int initialCapacity, Map<? extends K, ? extends V> m, Object... keyValuePairs) {
        super(initialCapacity);
        putAllOrPairs(m, keyValuePairs);
    }

    public ScipioLinkedHashMap(int initialCapacity, float loadFactor, Map<? extends K, ? extends V> m,
                               Object... keyValuePairs) {
        super(initialCapacity, loadFactor);
        putAllOrPairs(m, keyValuePairs);
    }

    public ScipioLinkedHashMap(int initialCapacity, float loadFactor, boolean accessOrder,
                               Map<? extends K, ? extends V> m, Object... keyValuePairs) {
        super(initialCapacity, loadFactor, accessOrder);
        putAllOrPairs(m, keyValuePairs);
    }
}
