/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package com.ilscipio.scipio.ce.util.collections;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Reference {@link Map} public wrapper with explicit public accessor {@link #wrapped()}.
 *
 * <p>SCIPIO: 3.0.0</p>
 */
public interface MapWrapper<K, V> {

    /**
     * Returns the raw Map delegated to by this map wrapper.
     */
    Map<K, V> wrapped();

    @Deprecated
    default Map<K, V> wrappedMap() {
        return wrapped();
    }

    /**
     * Returns the Map wrapped by the given wrapper, null if null, otherwise IllegalArgumentException.
     */
    static <K, V> Map<K, V> wrapped(Object wrapper) {
        if (wrapper instanceof MapWrapper) {
            @SuppressWarnings("unchecked")
            MapWrapper<K, V> wrappedMap = (MapWrapper<K, V>) wrapper;
            return wrappedMap.wrapped();
        } else if (wrapper == null) {
            return null;
        } else {
            throw new IllegalArgumentException("Not a map wrapper: [" + wrapper.getClass().getName() + "]");
        }
    }

    abstract class Abstract<K, V> implements Map<K, V>, MapWrapper<K, V> {
        @Override
        public int size() {
            return wrapped().size();
        }

        @Override
        public boolean isEmpty() {
            return wrapped().isEmpty();
        }

        @Override
        public boolean containsKey(Object key) {
            return wrapped().containsKey(key);
        }

        @Override
        public boolean containsValue(Object value) {
            return wrapped().containsValue(value);
        }

        @Override
        public V get(Object key) {
            return wrapped().get(key);
        }

        @Override
        public V put(K key, V value) {
            return wrapped().put(key, value);
        }

        @Override
        public V remove(Object key) {
            return wrapped().remove(key);
        }

        @Override
        public void putAll(Map<? extends K, ? extends V> m) {
            wrapped().putAll(m);
        }

        @Override
        public void clear() {
            wrapped().clear();
        }

        @Override
        public Set<K> keySet() {
            return wrapped().keySet();
        }

        @Override
        public Collection<V> values() {
            return wrapped().values();
        }

        @Override
        public Set<Entry<K, V>> entrySet() {
            return wrapped().entrySet();
        }

        @Override
        public boolean equals(Object o) {
            return wrapped().equals(o);
        }

        @Override
        public int hashCode() {
            return wrapped().hashCode();
        }

        @Override
        public V getOrDefault(Object key, V defaultValue) {
            return wrapped().getOrDefault(key, defaultValue);
        }

        @Override
        public void forEach(BiConsumer<? super K, ? super V> action) {
            wrapped().forEach(action);
        }

        @Override
        public void replaceAll(BiFunction<? super K, ? super V, ? extends V> function) {
            wrapped().replaceAll(function);
        }

        @Override
        public V putIfAbsent(K key, V value) {
            return wrapped().putIfAbsent(key, value);
        }

        @Override
        public boolean remove(Object key, Object value) {
            return wrapped().remove(key, value);
        }

        @Override
        public boolean replace(K key, V oldValue, V newValue) {
            return wrapped().replace(key, oldValue, newValue);
        }

        @Override
        public V replace(K key, V value) {
            return wrapped().replace(key, value);
        }

        @Override
        public V computeIfAbsent(K key, Function<? super K, ? extends V> mappingFunction) {
            return wrapped().computeIfAbsent(key, mappingFunction);
        }

        @Override
        public V computeIfPresent(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
            return wrapped().computeIfPresent(key, remappingFunction);
        }

        @Override
        public V compute(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
            return wrapped().compute(key, remappingFunction);
        }

        @Override
        public V merge(K key, V value, BiFunction<? super V, ? super V, ? extends V> remappingFunction) {
            return wrapped().merge(key, value, remappingFunction);
        }
    }

    abstract class Single<K, V> extends Abstract<K, V> {
        protected final Map<K, V> wrapped;

        public Single(Map<? extends K, ? extends V> wrapped) {
            @SuppressWarnings("unchecked")
            Map<K, V> wrappedCast = (Map<K, V>) wrapped;
            this.wrapped = wrappedCast;
        }

        @Override
        public Map<K, V> wrapped() {
            return wrapped;
        }
    }
}
