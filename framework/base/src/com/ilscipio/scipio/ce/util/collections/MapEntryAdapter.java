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

import java.util.Map;

/**
 * SCIPIO: A simple key/value pair that extends {@link Map.Entry} to provide infinite reuse.
 */
public interface MapEntryAdapter<K, V> extends Map.Entry<K, V> {

    /**
     * Sets the value, without returning the old one.
     * <p>
     * NOTE: The reason why this matters is that not all adapted collections
     * can efficiently return the old value (e.g. servlet attributes).
     */
    default void setValueOnly(V value) {
        setValue(value);
    }

    /**
     * Optional operation: tries to remove the entry from the containing collection;
     * by default, only sets the value to null; returns the old/previous value.
     */
    default V removeValue() {
        return setValue(null);
    }

    /**
     * Optional operation: tries to remove the entry from the containing collection;
     * by default, only sets the value to null.
     * <p>
     * NOTE: The reason why this matters is that not all adapted collections
     * can efficiently return the old value (e.g. servlet attributes).
     */
    default void removeValueOnly() {
        removeValue(null);
    }

    /**
     * Returns the result of call to entry's {@link #getKey()} method, or null if entry is null.
     */
    static <K, V> K getKey(Map.Entry<K, V> entry) {
        return (entry != null) ? entry.getKey() : null;
    }

    /**
     * Returns the result of call to entry's {@link #getValue()} method, or null if entry is null.
     */
    static <K, V> V getValue(Map.Entry<K, V> entry) {
        return (entry != null) ? entry.getValue() : null;
    }

    /**
     * Returns the result of call to entry's {@link #setValue} method, or null if entry is null.
     */
    static <K, V> V setValue(Map.Entry<K, V> entry, V value) {
        return (entry != null) ? entry.setValue(value) : null;
    }

    /**
     * Calls entry's {@link #setValueOnly} method IF it is an instance of MapEntryAdapter.
     */
    static <K, V> void setValueOnly(Map.Entry<K, V> entry, V value) {
        if (entry instanceof MapEntryAdapter) {
            ((MapEntryAdapter<K, V>) entry).setValueOnly(value);
        } else if (entry != null) {
            entry.setValue(value);
        }
    }

    /**
     * Calls entry's {@link #setValueOnly} method.
     */
    static <K, V> void setValueOnly(MapEntryAdapter<K, V> entry, V value) {
        if (entry != null) {
            entry.setValueOnly(value);
        }
    }

    /**
     * Calls entry's {@link #removeValue()} method IF it is an instance of MapEntryAdapter;
     * otherwise, sets the value to <code>null</code>.
     */
    static <K, V> V removeValue(Map.Entry<K, V> entry) {
        if (entry instanceof MapEntryAdapter) {
            return ((MapEntryAdapter<K, V>) entry).removeValue();
        } else if (entry != null) {
            return entry.setValue(null);
        }
        return null;
    }

    /**
     * Calls entry's {@link #removeValue()} method.
     */
    static <K, V> V removeValue(MapEntryAdapter<K, V> entry) {
        return (entry != null) ? entry.removeValue() : null;
    }

    /**
     * Calls entry's {@link #removeValueOnly()} method IF it is an instance of MapEntryAdapter;
     * otherwise, sets the value to <code>null</code>.
     */
    static <K, V> void removeValueOnly(Map.Entry<K, V> entry) {
        if (entry instanceof MapEntryAdapter) {
            ((MapEntryAdapter<K, V>) entry).removeValueOnly();
        } else if (entry != null) {
            entry.setValue(null);
        }
    }

    static <K, V> MapEntryAdapter<K, V> getAdapter(Map<K, V> map, K key) {
        return new MapKey<>(map, key);
    }

    class MapKey<K, V> implements MapEntryAdapter<K, V> {
        private final Map<K, V> map;
        private final K key;

        MapKey(Map<K, V> map, K key) {
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

        @Override
        public V removeValue() {
            return map.remove(getKey());
        }
    }
}