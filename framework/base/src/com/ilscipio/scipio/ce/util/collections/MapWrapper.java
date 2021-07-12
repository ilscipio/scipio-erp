package com.ilscipio.scipio.ce.util.collections;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Reference {@link Map} public wrapper with explicit public accessor {@link #wrappedMap()}.
 */
public interface MapWrapper<K, V> {

    /**
     * Returns the raw Map delegated to by this map wrapper.
     */
    Map<K, V> wrappedMap();

    /**
     * Returns the Map wrapped by the given wrapper, null if null, otherwise IllegalArgumentException.
     */
    static <K, V> Map<K, V> wrappedMap(Object wrapper) {
        if (wrapper instanceof MapWrapper) {
            @SuppressWarnings("unchecked")
            MapWrapper<K, V> wrappedMap = (MapWrapper<K, V>) wrapper;
            return wrappedMap.wrappedMap();
        } else if (wrapper == null) {
            return null;
        } else {
            throw new IllegalArgumentException("Not a map wrapper: [" + wrapper.getClass().getName() + "]");
        }
    }

    abstract class Abstract<K, V> implements Map<K, V>, MapWrapper<K, V> {
        @Override
        public int size() {
            return wrappedMap().size();
        }

        @Override
        public boolean isEmpty() {
            return wrappedMap().isEmpty();
        }

        @Override
        public boolean containsKey(Object key) {
            return wrappedMap().containsKey(key);
        }

        @Override
        public boolean containsValue(Object value) {
            return wrappedMap().containsValue(value);
        }

        @Override
        public V get(Object key) {
            return wrappedMap().get(key);
        }

        @Override
        public V put(K key, V value) {
            return wrappedMap().put(key, value);
        }

        @Override
        public V remove(Object key) {
            return wrappedMap().remove(key);
        }

        @Override
        public void putAll(Map<? extends K, ? extends V> m) {
            wrappedMap().putAll(m);
        }

        @Override
        public void clear() {
            wrappedMap().clear();
        }

        @Override
        public Set<K> keySet() {
            return wrappedMap().keySet();
        }

        @Override
        public Collection<V> values() {
            return wrappedMap().values();
        }

        @Override
        public Set<Entry<K, V>> entrySet() {
            return wrappedMap().entrySet();
        }

        @Override
        public boolean equals(Object o) {
            return wrappedMap().equals(o);
        }

        @Override
        public int hashCode() {
            return wrappedMap().hashCode();
        }

        @Override
        public V getOrDefault(Object key, V defaultValue) {
            return wrappedMap().getOrDefault(key, defaultValue);
        }

        @Override
        public void forEach(BiConsumer<? super K, ? super V> action) {
            wrappedMap().forEach(action);
        }

        @Override
        public void replaceAll(BiFunction<? super K, ? super V, ? extends V> function) {
            wrappedMap().replaceAll(function);
        }

        @Override
        public V putIfAbsent(K key, V value) {
            return wrappedMap().putIfAbsent(key, value);
        }

        @Override
        public boolean remove(Object key, Object value) {
            return wrappedMap().remove(key, value);
        }

        @Override
        public boolean replace(K key, V oldValue, V newValue) {
            return wrappedMap().replace(key, oldValue, newValue);
        }

        @Override
        public V replace(K key, V value) {
            return wrappedMap().replace(key, value);
        }

        @Override
        public V computeIfAbsent(K key, Function<? super K, ? extends V> mappingFunction) {
            return wrappedMap().computeIfAbsent(key, mappingFunction);
        }

        @Override
        public V computeIfPresent(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
            return wrappedMap().computeIfPresent(key, remappingFunction);
        }

        @Override
        public V compute(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
            return wrappedMap().compute(key, remappingFunction);
        }

        @Override
        public V merge(K key, V value, BiFunction<? super V, ? super V, ? extends V> remappingFunction) {
            return wrappedMap().merge(key, value, remappingFunction);
        }
    }

    abstract class Single<K, V> extends Abstract<K, V> {
        protected final Map<K, V> wrappedMap;

        public Single(Map<? extends K, ? extends V> wrappedMap) {
            @SuppressWarnings("unchecked")
            Map<K, V> wrappedMapCast = (Map<K, V>) wrappedMap;
            this.wrappedMap = wrappedMapCast;
        }

        @Override
        public Map<K, V> wrappedMap() {
            return wrappedMap;
        }
    }
}
