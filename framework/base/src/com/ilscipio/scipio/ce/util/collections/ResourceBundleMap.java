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

import java.io.Serializable;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Iterator;
import java.util.ResourceBundle;
import java.util.Set;

/**
 * Immutable simple map wrapper around java ResourceBundle.
 * <p>SCIPIO: 2.1.0: Added for UtilProperties/UtilCache.</p>
 */
public class ResourceBundleMap extends AbstractMap<String, Object> implements Serializable {
    private final ResourceBundle rb;

    public ResourceBundleMap(ResourceBundle rb) {
        this.rb = rb;
    }

    @Override
    public Object get(Object key) { // optimization
        return rb.getObject(key != null ? key.toString() : null);
    }

    @Override
    public Set<Entry<String, Object>> entrySet() {
        return new AbstractSet<Entry<String, Object>>() {
            private final Set<String> keySet = rb.keySet();
            @Override
            public Iterator<Entry<String, Object>> iterator() {
                final Iterator<String> keySetIt = keySet.iterator();
                return new Iterator<Entry<String, Object>>() {
                    @Override
                    public boolean hasNext() {
                        return keySetIt.hasNext();
                    }

                    @Override
                    public Entry<String, Object> next() {
                        String key = keySetIt.next();
                        return new SimpleEntry<>(key, rb.getObject(key));
                    }
                };
            }

            @Override
            public int size() {
                return keySet.size();
            }
        };
    }
}
