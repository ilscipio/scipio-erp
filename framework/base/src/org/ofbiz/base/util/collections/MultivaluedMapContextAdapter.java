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
package org.ofbiz.base.util.collections;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

// Adapter which allows viewing a multi-value map as a single-value map.
public class MultivaluedMapContextAdapter<K, V> implements Map<K, V> {
    private MultivaluedMapContext<K, V> adaptee;

    public MultivaluedMapContextAdapter(MultivaluedMapContext<K, V> adaptee) {
        this.adaptee = adaptee;
    }

    @Override
    public int size() {
        return adaptee.size();
    }

    @Override
    public boolean isEmpty() {
        return adaptee.isEmpty();
    }

    @Override
    public boolean containsKey(Object key) {
        return adaptee.containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        return adaptee.values().stream()
                .map(l -> l.get(0))
                .anyMatch(value::equals);
    }

    @Override
    public V get(Object key) {
        return adaptee.getFirst(key);
    }

    @Override
    public V put(K key, V value) {
        V prev = get(key);
        adaptee.putSingle(key, value);
        return prev;
    }

    @Override
    public V remove(Object key) {
        V prev = get(key);
        adaptee.remove(key);
        return prev;
    }

    @Override
    public void putAll(Map<? extends K, ? extends V> m) {
        m.forEach(adaptee::putSingle);
    }

    @Override
    public void clear() {
        adaptee.clear();
    }

    @Override
    public Set<K> keySet() {
        return adaptee.keySet();
    }

    @Override
    public Collection<V> values() {
        return adaptee.values().stream()
                .map(l -> l.get(0))
                .collect(Collectors.toList());
    }

    @Override
    public Set<Entry<K, V>> entrySet() {
        return adaptee.keySet().stream()
                .collect(Collectors.toMap(k -> k, k -> get(k)))
                .entrySet();
    }
}
