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
package com.ilscipio.scipio.mcp.protocol;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import com.ilscipio.scipio.mcp.security.McpConfig;
import com.ilscipio.scipio.mcp.security.McpTokenUtil;

/**
 * SCIPIO: 4.0.0: In-memory registry of MCP sessions with idle expiry.
 */
public final class McpSessionRegistry {

    private static final McpSessionRegistry INSTANCE = new McpSessionRegistry();

    private final Map<String, McpSession> sessions = new ConcurrentHashMap<>();
    /** SCIPIO: 4.0.0: live sessions per delegator (store) */
    private final Map<String, AtomicInteger> perStore = new ConcurrentHashMap<>();
    private final AtomicLong lastSweep = new AtomicLong(System.currentTimeMillis());

    private McpSessionRegistry() {}

    public static McpSessionRegistry get() {
        return INSTANCE;
    }

    public McpSession create(String tokenId, String webappName, String protocolVersion) {
        return create(tokenId, webappName, protocolVersion, "");
    }

    /**
     * SCIPIO: 4.0.0: Pooled runtime: creates a session that belongs to one delegator (store) (G6). Returns null when
     * the store has mcp.session.maxPerStore live sessions in this JVM, so that one store cannot fill the registry.
     */
    public McpSession create(String tokenId, String webappName, String protocolVersion, String delegatorName) {
        sweepIfDue();
        String store = delegatorName != null ? delegatorName : "";
        AtomicInteger count = perStore.computeIfAbsent(store, k -> new AtomicInteger());
        if (count.incrementAndGet() > McpConfig.getSessionMaxPerStore() && org.ofbiz.entity.util.Tenants.isPooled()) {
            count.decrementAndGet();
            return null;
        }
        McpSession s = new McpSession(McpTokenUtil.randomSessionId(), tokenId, webappName, protocolVersion, store);
        sessions.put(s.getId(), s);
        return s;
    }

    private void forget(McpSession s) {
        AtomicInteger count = perStore.get(s.getDelegatorName());
        if (count != null) {
            count.decrementAndGet();
        }
    }

    /** Returns the live session or null when unknown or expired. */
    public McpSession find(String id) {
        if (id == null || id.isEmpty()) return null;
        McpSession s = sessions.get(id);
        if (s == null) return null;
        if (isExpired(s)) {
            if (sessions.remove(id, s)) {
                forget(s);
            }
            return null;
        }
        s.touch();
        return s;
    }

    public boolean remove(String id) {
        McpSession s = (id != null) ? sessions.remove(id) : null;
        if (s != null) {
            forget(s);
        }
        return s != null;
    }

    public int size() {
        return sessions.size();
    }

    private boolean isExpired(McpSession s) {
        long idleMillis = McpConfig.getSessionIdleMinutes() * 60_000L;
        return System.currentTimeMillis() - s.getLastAccessMillis() > idleMillis;
    }

    private void sweepIfDue() {
        long now = System.currentTimeMillis();
        long last = lastSweep.get();
        if (now - last < 60_000L) return;
        if (!lastSweep.compareAndSet(last, now)) return;
        for (McpSession s : sessions.values()) {
            if (isExpired(s) && sessions.remove(s.getId(), s)) {
                forget(s);
            }
        }
    }
}
