package com.ilscipio.scipio.ce.util.collections;

import java.util.Comparator;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

public class ScipioTreeMap<K, V> extends TreeMap<K, V> implements ScipioMap<K, V> {
    public ScipioTreeMap() {
    }

    public ScipioTreeMap(Map<? extends K, ? extends V> m) {
        super(m);
    }

    public ScipioTreeMap(SortedMap<K, ? extends V> m) {
        super(m);
    }

    public ScipioTreeMap(Map<? extends K, ? extends V> m, Object... keyValuePairs) {
        this(m);
        putPairs(keyValuePairs);
    }

    public ScipioTreeMap(SortedMap<K, ? extends V> m, Object... keyValuePairs) {
        this(m);
        putPairs(keyValuePairs);
    }

    public ScipioTreeMap(Comparator<? super K> comparator) {
        super(comparator);
    }

    public ScipioTreeMap(Comparator<? super K> comparator, Map<? extends K, ? extends V> m, Object... keyValuePairs) {
        super(comparator);
        putAllOrPairs(m, keyValuePairs);
    }
}
