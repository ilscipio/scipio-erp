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

import java.util.ArrayList;
import java.util.List;

/**
 * MultivaluedMap Context
 *
 * A MapContext which handles multiple values for the same key.
 */
public class MultivaluedMapContext<K, V> extends MapContext<K, List<V>> {

    public static final String module = MultivaluedMapContext.class.getName();

    /**
     * Create a multi-value map initialized with one context
     */
    public MultivaluedMapContext() {
        push();
    }

    /**
     * Associate {@code key} with the single value {@code value}.
     * If other values are already associated with {@code key} then override them.
     *
     * @param key the key to associate {@code value} with
     * @param value the value to add to the context
     */
    public void putSingle(K key, V value) {
        List<V> box = new ArrayList<>(); // SCIPIO: 2018-08-30: switched to ArrayList
        box.add(value);
        put(key, box);
    }

    /**
     * Associate {@code key} with the single value {@code value}.
     * If other values are already associated with {@code key},
     * then add {@code value} to them.
     *
     * @param key the key to associate {@code value} with
     * @param value the value to add to the context
     */
    public void add(K key, V value) {
        // SCIPIO: 2018-08-30: we have an ArrayList
        //List<V> cur = contexts.getFirst().get(key);
        List<V> cur = getFromTopOnly(key);
        if (cur == null) {
            //cur = new ArrayList<>(); // SCIPIO: 2018-08-30: switched to ArrayList
            /* if this method is called after a context switch, copy the previous values
               in current context to not mask them. */
            List<V> old = get(key);
            if (old != null) {
                //cur.addAll(old);
                cur = new ArrayList<>(old);
            } else {
                cur = new ArrayList<>(); // SCIPIO
            }
        }
        cur.add(value);
        put(key, cur);
    }

    /**
     * Get the first value contained in the list of values associated with {@code key}.
     *
     * @param key a candidate key
     * @return the first value associated with {@code key} or null if no value
     * is associated with it.
     */
    public V getFirst(Object key) {
        List<V> res = get(key);
        return res == null ? null : res.get(0);
    }
}
