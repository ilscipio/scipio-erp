package com.ilscipio.scipio.ce.util.collections;

import java.util.Map;

/**
 * SCIPIO: Common collections API(s) adapters for the {@link com.ilscipio.scipio.ce.util.collections.MapEntryAdapter}
 * ({@link Map.Entry}) interface.
 * <p>
 * NOTE: See also {@link com.ilscipio.scipio.ce.util.servlet.ServletMapEntryAdapters}.
 */
public final class MapEntryAdapters {

    private MapEntryAdapters() {
    }

    public static class MapMapEntryAdapter<K, V> implements MapEntryAdapter<K, V> {
        private final Map<K, V> map;
        private final K key;

        MapMapEntryAdapter(Map<K, V> map, K key) {
            this.map = map;
            this.key = key;
        }

        @Override
        public K getKey() {
            return key;
        }

        @Override
        public V getValue() {
            return map.get(getKey());
        }

        @Override
        public V setValue(V value) {
            return map.put(key, value);
        }
    }

    public static <K, V> MapEntryAdapter<K, V> getAdapter(Map<K, V> map, K key) {
        return new MapMapEntryAdapter<>(map, key);
    }

}
