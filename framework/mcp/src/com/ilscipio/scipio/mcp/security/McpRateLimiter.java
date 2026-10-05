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

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicLong;

/**
 * SCIPIO: 4.0.0: In-memory token buckets and concurrency guards keyed by token id or client address.
 * Per-JVM only; cluster-wide limits are out of scope for 4.0. Both maps are swept of idle entries.
 */
public final class McpRateLimiter {

    private static final McpRateLimiter INSTANCE = new McpRateLimiter();

    private static final long SWEEP_INTERVAL_MS = 300_000L;
    private static final long IDLE_MS = 600_000L;

    private static final class Bucket {
        double tokens;
        long lastRefillNanos;
        volatile long lastUsedMillis;
    }

    private static final class Slot {
        final Semaphore semaphore;
        final int max;
        volatile long lastUsedMillis;

        Slot(int max) {
            this.semaphore = new Semaphore(max, true);
            this.max = max;
        }

        boolean idle(long now) {
            return semaphore.availablePermits() >= max && now - lastUsedMillis > IDLE_MS;
        }
    }

    private final Map<String, Bucket> buckets = new ConcurrentHashMap<>();
    private final Map<String, Slot> semaphores = new ConcurrentHashMap<>();
    private final AtomicLong lastSweep = new AtomicLong(System.currentTimeMillis());

    private McpRateLimiter() {}

    public static McpRateLimiter get() {
        return INSTANCE;
    }

    /** Returns true when one unit is available for the key at the given per-minute rate. */
    public boolean tryAcquire(String key, int perMinute) {
        if (perMinute <= 0) return true;
        sweepIfDue();
        Bucket b = buckets.computeIfAbsent(key, k -> {
            Bucket nb = new Bucket();
            nb.tokens = perMinute;
            nb.lastRefillNanos = System.nanoTime();
            return nb;
        });
        synchronized (b) {
            long now = System.nanoTime();
            double refill = (now - b.lastRefillNanos) / 60_000_000_000d * perMinute;
            b.tokens = Math.min(perMinute, b.tokens + refill);
            b.lastRefillNanos = now;
            b.lastUsedMillis = System.currentTimeMillis();
            if (b.tokens >= 1d) {
                b.tokens -= 1d;
                return true;
            }
            return false;
        }
    }

    /** Acquires a concurrency slot; the caller must call {@link #release(String)} in a finally block. */
    public boolean tryAcquireSlot(String key, int max) {
        if (max <= 0) return true;
        sweepIfDue();
        Slot s = semaphores.computeIfAbsent(key, k -> new Slot(max));
        s.lastUsedMillis = System.currentTimeMillis();
        return s.semaphore.tryAcquire();
    }

    public void release(String key) {
        Slot s = semaphores.get(key);
        if (s != null) {
            s.lastUsedMillis = System.currentTimeMillis();
            s.semaphore.release();
        }
    }

    private void sweepIfDue() {
        long now = System.currentTimeMillis();
        long last = lastSweep.get();
        if (now - last < SWEEP_INTERVAL_MS) return;
        if (!lastSweep.compareAndSet(last, now)) return;
        sweep(now);
    }

    /** Removes idle buckets and idle, fully released slots. Package-private for tests. */
    void sweep(long now) {
        buckets.entrySet().removeIf(e -> now - e.getValue().lastUsedMillis > IDLE_MS);
        semaphores.entrySet().removeIf(e -> e.getValue().idle(now));
    }

    /** For tests. */
    int trackedKeys() {
        return buckets.size() + semaphores.size();
    }

    /** For tests. */
    void reset() {
        buckets.clear();
        semaphores.clear();
    }
}
