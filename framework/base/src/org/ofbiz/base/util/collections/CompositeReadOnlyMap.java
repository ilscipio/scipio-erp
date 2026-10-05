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
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * SCIPIO: A helper composite read-only map that classes can safely extend and override behaviors.
 * <p>
 * NOTE: has no relation to ReadOnlyMapEntry class.
 */
public abstract class CompositeReadOnlyMap<K, V> implements Map<K, V> {

    protected final Map<K, V> internalMap;
    protected final Map<K, V> readOnlyMap;

    protected CompositeReadOnlyMap() {
        this.internalMap = new HashMap<K, V>();
        this.readOnlyMap = Collections.unmodifiableMap(this.internalMap);
    }

    protected CompositeReadOnlyMap(Map<K, V> internalMap) {
        this.internalMap = internalMap;
        this.readOnlyMap = Collections.unmodifiableMap(this.internalMap);
    }

    protected Map<K, V> getInternalMap() {
        return internalMap;
    }

    protected Map<K, V> getReadOnlyMap() {
        return readOnlyMap;
    }

    @Override
    public int size() {
        return internalMap.size();
    }

    @Override
    public boolean isEmpty() {
        return internalMap.isEmpty();
    }

    @Override
    public boolean containsKey(Object key) {
        return internalMap.containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        return internalMap.containsValue(value);
    }

    @Override
    public V get(Object key) {
        return internalMap.get(key);
    }

    @Override
    public V put(K key, V value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public V remove(Object key) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void putAll(Map<? extends K, ? extends V> m) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Set<K> keySet() {
        return readOnlyMap.keySet();
    }

    @Override
    public Collection<V> values() {
        return readOnlyMap.values();
    }

    @Override
    public Set<java.util.Map.Entry<K, V>> entrySet() {
        return readOnlyMap.entrySet();
    }

}
