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
package org.ofbiz.base.util;

import org.apache.tomcat.jni.Proc;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Used to stop rebuildSolrIndex (SCIPIO).
 */
public class ProcessSignals implements Map<String, Object> {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private final Map<String, Object> signals = new ConcurrentHashMap<>();
    private final String process;
    private final boolean verbose;

    protected ProcessSignals(String process, boolean verbose) {
        this.process = process;
        this.verbose = verbose;
    }

    public static ProcessSignals make(String process) {
        return make(process, false);
    }

    public static ProcessSignals make(String process, boolean verbose) {
        // TODO: REVIEW: this method may want to hold a central reference later...
        return new ProcessSignals(process, verbose);
    }

    public String getProcess() {
        return process;
    }

    @Override
    public Object get(Object name) {
        return signals.get(name);
    }

    public static Object get(ProcessSignals processSignals, String name) {
        return (processSignals != null) ? processSignals.get(name) : null;
    }

    public boolean isSet(String name) {
        return signals.containsKey(name);
    }

    public static boolean isSet(ProcessSignals processSignals, String name) {
        return (processSignals != null) && processSignals.isSet(name);
    }

    @Override
    public Object put(String name, Object value) {
        if (verbose) {
            Debug.logInfo("Sent signal [" + name + "] to process [" + process + "]", module);
        }
        return signals.put(name, value);
    }

    public void put(String name) {
        put(name, true);
    }

    @Override
    public Object remove(Object name) {
        return signals.remove(name);
    }

    @Override
    public void clear() {
        signals.clear();
    }

    @Override
    public int size() {
        return signals.size();
    }

    @Override
    public boolean isEmpty() {
        return signals.isEmpty();
    }

    @Override
    public boolean containsKey(Object key) {
        return signals.containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        return signals.containsValue(value);
    }

    @Override
    public void putAll(Map<? extends String, ?> m) {
        signals.putAll(m);
    }

    @Override
    public Set<String> keySet() {
        return signals.keySet();
    }

    @Override
    public Collection<Object> values() {
        return signals.values();
    }

    @Override
    public Set<Entry<String, Object>> entrySet() {
        return signals.entrySet();
    }
}
