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
package org.ofbiz.service;

import java.io.Serializable;

/**
 * Factory/facade for {@link LocalDispatcher} API service options for client code (SCIPIO).
 * Setters support chaining for one-liners and fewer imports.
 * TODO: no SyncOptions yet, only {@link AsyncOptions}.
 * Example: <code>dispatcher.runAsync("rebuildSolrIndex", ..., ServiceOptions.async(false).priority(30))</code>
 */
public abstract class ServiceOptions implements Serializable {
    protected ServiceOptions() {}

    /** Returns new {@link PersistAsyncOptions} or {@link MemoryAsyncOptions}. */
    @SuppressWarnings("unchecked")
    public static <T extends AsyncOptions> T async(boolean persist) {
        return persist ? (T) new PersistAsyncOptions() : (T) new MemoryAsyncOptions();
    }

    /** Returns default read-only {@link PersistAsyncOptions} or {@link MemoryAsyncOptions}. */
    @SuppressWarnings("unchecked")
    public static <T extends AsyncOptions> T asyncDefault(boolean persist) {
        return persist ? (T) PersistAsyncOptions.DEFAULT : (T) MemoryAsyncOptions.DEFAULT;
    }

    /** Returns new {@link MemoryAsyncOptions}. */
    public static MemoryAsyncOptions asyncMemory() {
        return new MemoryAsyncOptions();
    }

    /** Returns default read-only {@link MemoryAsyncOptions}. */
    public static MemoryAsyncOptions asyncMemoryDefault() {
        return MemoryAsyncOptions.DEFAULT;
    }

    /** Returns new {@link PersistAsyncOptions}. */
    public static PersistAsyncOptions asyncPersist() {
        return new PersistAsyncOptions();
    }

    /** Returns default read-only {@link PersistAsyncOptions}. */
    public static PersistAsyncOptions asyncPersistDefault() {
        return PersistAsyncOptions.DEFAULT;
    }
}
