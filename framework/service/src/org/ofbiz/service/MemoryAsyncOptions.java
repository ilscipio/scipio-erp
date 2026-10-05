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

/**
 * Non-persisted async service options for non-persisted {@link LocalDispatcher#runAsync} calls.
 * NOTE: Use {@link AsyncOptions#persist()} to determine if {@link PersistAsyncOptions} because class hierarchy could change. */
public class MemoryAsyncOptions extends AsyncOptions {
    public static final MemoryAsyncOptions DEFAULT = new MemoryAsyncOptions() {
        @Override
        public PersistAsyncOptions jobPool(String jobPool) { throw new UnsupportedOperationException(); }
        @Override
        public PersistAsyncOptions priority(Long priority) { throw new UnsupportedOperationException(); }
        @Override
        public MemoryAsyncOptions transactionTimeout(Integer transactionTimeout) { throw new UnsupportedOperationException(); }
        @Override
        public MemoryAsyncOptions requireNewTransaction(Boolean requireNewTransaction) { throw new UnsupportedOperationException(); }
    };

    protected Integer transactionTimeout;
    protected Boolean requireNewTransaction;

    /** Default/main constructor, not recommended for client code - use {@link ServiceOptions#async(boolean)} and setters. */
    protected MemoryAsyncOptions() {
    }

    /** Legacy parameter constructor, not recommended for client code - use {@link ServiceOptions#async(boolean)} and setters. */
    protected MemoryAsyncOptions(String jobPool, Long priority, Integer transactionTimeout, Boolean requireNewTransaction) {
        super(jobPool, priority);
        this.transactionTimeout = transactionTimeout;
        this.requireNewTransaction = requireNewTransaction;
    }

    @Override
    public boolean persist() {
        return false;
    }

    /** The overriding timeout for the transaction (if we started it). */
    public Integer transactionTimeout() {
        return transactionTimeout;
    }

    /** The overriding timeout for the transaction (if we started it). */
    public MemoryAsyncOptions transactionTimeout(Integer transactionTimeout) {
        this.transactionTimeout = transactionTimeout;
        return this;
    }

    /** If true we will suspend and create a new transaction so we are sure to start. */
    public Boolean requireNewTransaction() {
        return requireNewTransaction;
    }

    /** If true we will suspend and create a new transaction so we are sure to start. */
    public MemoryAsyncOptions requireNewTransaction(Boolean requireNewTransaction) {
        this.requireNewTransaction = requireNewTransaction;
        return this;
    }

    @Override
    public String toString() {
        return "{" +
                "jobPool='" + jobPool + '\'' +
                ", priority=" + priority +
                ", transactionTimeout=" + transactionTimeout +
                ", requireNewTransaction=" + requireNewTransaction +
                '}';
    }
}
