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

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Extended Map utilities.
 * @deprecated SCIPIO: 2.1.0: For framework and client code, please use {@link ScipioMap}.
 */
@Deprecated
public interface ExtendedMap<K, V> extends Map<K, V> {

    default void putOnly(K key, V value) { put(key, value); }

    default void removeOnly(Object key) { remove(key); };

    default Set<K> keySetCopy() { return new LinkedHashSet<>(keySet()); }

    default List<K> keyListCopy() { return new ArrayList<>(keySet()); }

    default Collection<V> valuesCopy() { return valuesListCopy(); }

    default List<V> valuesListCopy() { return new ArrayList<>(values()); }

    default Set<Entry<K, V>> entrySetCopy() { return new LinkedHashSet<>(entrySet()); }

    default List<Map.Entry<K, V>> entryListCopy() { return new ArrayList<>(entrySet()); }

    Map<K, V> mapCopy();

}
