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
package com.ilscipio.scipio.util.collections;

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * Simplification of AbstractMap for read-only implementations (mostly).
 *
 * <p>Useful when a bean/other class needs to implement Map interface and avoid recreating map representations (typically read-only).</p>
 *
 * <p>SCIPIO: 3.0.0: Added.</p>
 */
public abstract class LazyListAbstractMap<K, V> extends AbstractMap<K, V> {

    protected Set<Entry<K, V>> entrySet;
    protected List<Entry<K, V>> entryList;

    protected abstract List<Entry<K, V>> makeEntryList();

    public List<Entry<K, V>> entryList() {
        List<Entry<K, V>> entryList = this.entryList;
        if (entryList == null) {
            entryList = makeEntryList();
            this.entryList = entryList;
        }
        return entryList;
    }

    @Override
    public Set<Entry<K, V>> entrySet() {
        Set<Entry<K, V>> entrySet = this.entrySet;
        if (entrySet == null) {
            entrySet = new AbstractSet<Entry<K, V>>() {
                private final List<Entry<K, V>> entryList = entryList();
                @Override
                public Iterator<Entry<K, V>> iterator() {
                    return entryList.iterator();
                }
                @Override
                public int size() {
                    return entryList.size();
                }
            };
            this.entrySet = entrySet;
        }
        return entrySet;
    }

}
