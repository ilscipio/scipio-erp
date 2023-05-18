package com.ilscipio.scipio.util.collections;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Alternative to AbstractMap where a mutable wrapped map is instantiated on first access (convenience class).
 *
 * <p>Useful when a bean/other class needs to implement Map interface and avoid recreating map representations (typically read-only).</p>
 *
 * <p>SCIPIO: 3.0.0: Added.</p>
 */
public abstract class LazyMap<K, V> implements Map<K, V> {

    protected Map<K, V> wrappedMap;

    protected abstract Map<K, V> makeMap();

    protected Map<K, V> getWrappedMap() {
        Map<K, V> wrappedMap = this.wrappedMap;
        if (wrappedMap == null) {
            wrappedMap = makeMap();
            this.wrappedMap = wrappedMap;
        }
        return wrappedMap;
    }

    @Override
    public int size() {
        return getWrappedMap().size();
    }

    @Override
    public boolean isEmpty() {
        return getWrappedMap().isEmpty();
    }

    @Override
    public boolean containsKey(Object key) {
        return getWrappedMap().containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        return getWrappedMap().containsValue(value);
    }

    @Override
    public V get(Object key) {
        return getWrappedMap().get(key);
    }

    @Override
    public V put(K key, V value) {
        return getWrappedMap().put(key, value);
    }

    @Override
    public V remove(Object key) {
        return getWrappedMap().remove(key);
    }

    @Override
    public void putAll(Map<? extends K, ? extends V> m) {
        getWrappedMap().putAll(m);
    }

    @Override
    public void clear() {
        getWrappedMap().clear();
    }

    @Override
    public Set<K> keySet() {
        return getWrappedMap().keySet();
    }

    @Override
    public Collection<V> values() {
        return getWrappedMap().values();
    }

    @Override
    public Set<Entry<K, V>> entrySet() {
        return getWrappedMap().entrySet();
    }

    @Override
    public boolean equals(Object o) {
        return getWrappedMap().equals(o);
    }

    @Override
    public int hashCode() {
        return getWrappedMap().hashCode();
    }

    @Override
    public V getOrDefault(Object key, V defaultValue) {
        return getWrappedMap().getOrDefault(key, defaultValue);
    }

    @Override
    public void forEach(BiConsumer<? super K, ? super V> action) {
        getWrappedMap().forEach(action);
    }

    @Override
    public void replaceAll(BiFunction<? super K, ? super V, ? extends V> function) {
        getWrappedMap().replaceAll(function);
    }

    @Override
    public V putIfAbsent(K key, V value) {
        return getWrappedMap().putIfAbsent(key, value);
    }

    @Override
    public boolean remove(Object key, Object value) {
        return getWrappedMap().remove(key, value);
    }

    @Override
    public boolean replace(K key, V oldValue, V newValue) {
        return getWrappedMap().replace(key, oldValue, newValue);
    }

    @Override
    public V replace(K key, V value) {
        return getWrappedMap().replace(key, value);
    }

    @Override
    public V computeIfAbsent(K key, Function<? super K, ? extends V> mappingFunction) {
        return getWrappedMap().computeIfAbsent(key, mappingFunction);
    }

    @Override
    public V computeIfPresent(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
        return getWrappedMap().computeIfPresent(key, remappingFunction);
    }

    @Override
    public V compute(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
        return getWrappedMap().compute(key, remappingFunction);
    }

    @Override
    public V merge(K key, V value, BiFunction<? super V, ? super V, ? extends V> remappingFunction) {
        return getWrappedMap().merge(key, value, remappingFunction);
    }
}
