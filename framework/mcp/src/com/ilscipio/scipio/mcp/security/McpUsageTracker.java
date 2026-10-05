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
package com.ilscipio.scipio.mcp.security;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityQuery;

/**
 * SCIPIO: 4.0.0: Counts tool calls in memory and flushes deltas to McpToolUsage on a timer.
 *
 * <p>Pooled runtime: one set of counters per delegator (store), loaded from and flushed to that store's own
 * McpToolUsage table (G4). Counts of one store never reach another store.</p>
 */
public final class McpUsageTracker {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final McpUsageTracker INSTANCE = new McpUsageTracker();

    private static final class Counter {
        final AtomicLong calls = new AtomicLong();
        final AtomicLong errors = new AtomicLong();
        final AtomicLong pendingCalls = new AtomicLong();
        final AtomicLong pendingErrors = new AtomicLong();
    }

    /** The counters of one store. */
    private static final class StoreCounters {
        final Map<String, Counter> counters = new ConcurrentHashMap<>();
        volatile boolean loaded;
    }

    private final Map<String, StoreCounters> stores = new ConcurrentHashMap<>();
    private final AtomicBoolean started = new AtomicBoolean();

    private McpUsageTracker() {}

    public static McpUsageTracker get() {
        return INSTANCE;
    }

    public void record(Delegator delegator, String server, String tool, boolean error) {
        Counter c = storeOf(delegator).counters.computeIfAbsent(key(server, tool), k -> new Counter());
        c.calls.incrementAndGet();
        c.pendingCalls.incrementAndGet();
        if (error) {
            c.errors.incrementAndGet();
            c.pendingErrors.incrementAndGet();
        }
    }

    /** Total calls for a tool across all servers of the store (in memory plus loaded DB totals). */
    public long getCallCount(Delegator delegator, String tool) {
        long total = 0;
        for (Map.Entry<String, Counter> e : storeOf(delegator).counters.entrySet()) {
            if (e.getKey().endsWith("|" + tool)) total += e.getValue().calls.get();
        }
        return total;
    }

    public long getCallCount(Delegator delegator, String server, String tool) {
        Counter c = storeOf(delegator).counters.get(key(server, tool));
        return c != null ? c.calls.get() : 0;
    }

    private static String key(String server, String tool) {
        return server + "|" + tool;
    }

    private StoreCounters storeOf(Delegator delegator) {
        StoreCounters store = stores.computeIfAbsent(delegator.getDelegatorName(), k -> new StoreCounters());
        if (!store.loaded) {
            synchronized (store) {
                if (!store.loaded) {
                    try {
                        for (GenericValue gv : EntityQuery.use(delegator).from("McpToolUsage").queryList()) {
                            Counter c = store.counters.computeIfAbsent(key(gv.getString("serverName"), gv.getString("toolName")), k -> new Counter());
                            Long calls = gv.getLong("callCount");
                            Long errors = gv.getLong("errorCount");
                            c.calls.set(calls != null ? calls : 0);
                            c.errors.set(errors != null ? errors : 0);
                        }
                    } catch (GenericEntityException e) {
                        Debug.logWarning(e, "MCP: could not load McpToolUsage counters for " + delegator.getDelegatorName(), module);
                    }
                    store.loaded = true;
                }
            }
            startFlusher();
        }
        return store;
    }

    private void startFlusher() {
        if (!started.compareAndSet(false, true)) return;
        int seconds = Math.max(10, McpConfig.getUsageFlushSeconds());
        ScheduledExecutorService exec = Executors.newSingleThreadScheduledExecutor(r -> {
            Thread t = new Thread(r, "scipio-mcp-usage-flush");
            t.setDaemon(true);
            return t;
        });
        exec.scheduleWithFixedDelay(this::flushSafe, seconds, seconds, TimeUnit.SECONDS);
    }

    private void flushSafe() {
        for (String delegatorName : stores.keySet()) {
            try {
                flush(DelegatorFactory.getDelegator(delegatorName));
            } catch (Throwable t) {
                Debug.logWarning(t, "MCP: usage flush failed for " + delegatorName, module);
            }
        }
    }

    /** Writes the pending deltas of the store of this delegator to its database in one transaction. */
    public void flush(Delegator delegator) throws GenericEntityException {
        StoreCounters store = stores.get(delegator.getDelegatorName());
        if (store == null) return;
        List<Object[]> pending = new ArrayList<>();
        for (Map.Entry<String, Counter> e : store.counters.entrySet()) {
            long calls = e.getValue().pendingCalls.getAndSet(0);
            long errors = e.getValue().pendingErrors.getAndSet(0);
            if (calls == 0 && errors == 0) continue;
            pending.add(new Object[] { e.getKey(), calls, errors });
        }
        if (pending.isEmpty()) return;
        boolean began = false;
        try {
            began = TransactionUtil.begin();
            for (Object[] p : pending) {
                String[] parts = ((String) p[0]).split("\\|", 2);
                GenericValue gv = EntityQuery.use(delegator).from("McpToolUsage")
                        .where("serverName", parts[0], "toolName", parts[1]).queryOne();
                if (gv == null) {
                    gv = delegator.makeValue("McpToolUsage");
                    gv.set("serverName", parts[0]);
                    gv.set("toolName", parts[1]);
                    gv.set("callCount", 0L);
                    gv.set("errorCount", 0L);
                    gv.create();
                }
                Long calls = gv.getLong("callCount");
                Long errors = gv.getLong("errorCount");
                gv.set("callCount", (calls != null ? calls : 0L) + (Long) p[1]);
                gv.set("errorCount", (errors != null ? errors : 0L) + (Long) p[2]);
                gv.set("lastCallDate", UtilDateTime.nowTimestamp());
                gv.store();
            }
            TransactionUtil.commit(began);
        } catch (GenericEntityException e) {
            TransactionUtil.rollback(began, "MCP usage flush failed", e);
            throw e;
        }
    }
}
