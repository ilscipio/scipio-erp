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
package org.ofbiz.entity.util;

import java.util.ArrayDeque;
import java.util.Deque;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;

/**
 * SCIPIO: 4.0.0: Pooled runtime: the store (tenant) that the current thread works for, also outside a web request.
 *
 * <p>The service dispatcher pushes the store of its delegator around every service body, the web request
 * resolver pushes the store of the Host header. Code without a delegator argument (for example the Solr client
 * factory, or a fallback that used the {@code default} delegator by name) reads the store here. A stack, because a
 * service of one store never calls a service of another store on the same thread, but nested calls of the same store
 * are common.</p>
 */
public final class TenantScope {

    private static final ThreadLocal<Deque<String>> STACK = ThreadLocal.withInitial(ArrayDeque::new);

    private TenantScope() {}

    /** Enters the store of the delegator (the base delegator counts as no store). Always pair with {@link #exit()}. */
    public static void enter(Delegator delegator) {
        STACK.get().push(delegator != null ? delegator.getDelegatorName() : "");
    }

    public static void exit() {
        Deque<String> stack = STACK.get();
        if (!stack.isEmpty()) {
            stack.pop();
        }
        if (stack.isEmpty()) {
            STACK.remove();
        }
    }

    /** The store of the current thread, or null for the base delegator or when no store is set. */
    public static String current() {
        String name = STACK.get().peek();
        int hash = (name != null) ? name.indexOf('#') : -1;
        return (hash >= 0 && hash < name.length() - 1) ? name.substring(hash + 1) : null;
    }

    /** The delegator name of the current scope, or null when no scope is set. */
    public static String currentDelegatorName() {
        String name = STACK.get().peek();
        return (name == null || name.isEmpty()) ? null : name;
    }

    /**
     * The delegator of the current scope, or the delegator named {@code fallbackName} when no scope is set. Use this
     * instead of {@code DelegatorFactory.getDelegator("default")} where code has no delegator argument (G13): in a
     * store thread it returns the store delegator, never the base delegator.
     */
    public static Delegator currentDelegator(String fallbackName) {
        String name = currentDelegatorName();
        return DelegatorFactory.getDelegator(name != null ? name : fallbackName);
    }
}
