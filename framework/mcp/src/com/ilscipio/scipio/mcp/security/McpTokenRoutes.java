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

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityQuery;

/**
 * SCIPIO: 4.0.0: Pooled runtime: the master table {@code McpTokenRoute(tokenId -> tenantId)} (G2).
 *
 * <p>TenantResolver asks for the store of a bearer token before MCP authenticates it; MCP then authenticates the
 * token on the delegator of that store. Lookups are cached for {@link #CACHE_TTL_MS}, unknown ids included.</p>
 */
public final class McpTokenRoutes {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    static final long CACHE_TTL_MS = 30000;
    private static final int MAX_ENTRIES = 50000;
    private static final String NONE = "";

    private static final class Entry {
        final String tenantId;
        final long expires;

        Entry(String tenantId, long expires) {
            this.tenantId = tenantId;
            this.expires = expires;
        }
    }

    private static final Map<String, Entry> cache = new ConcurrentHashMap<>();

    private McpTokenRoutes() {}

    /** The store of the bearer token of the request, or null (no bearer token, bad format, or no route). */
    public static String getTenantId(HttpServletRequest request, Delegator baseDelegator) {
        String header = request.getHeader("Authorization");
        if (header == null || !header.regionMatches(true, 0, "Bearer ", 0, 7)) {
            return null;
        }
        String tokenId = McpTokenUtil.parseTokenId(header.substring(7).trim());
        if (tokenId == null) {
            return null;
        }
        long now = System.currentTimeMillis();
        Entry entry = cache.get(tokenId);
        if (entry == null || entry.expires <= now) {
            String tenantId = NONE;
            try {
                GenericValue route = EntityQuery.use(baseDelegator).from("McpTokenRoute").where("tokenId", tokenId).queryOne();
                if (route != null) {
                    tenantId = route.getString("tenantId");
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, "MCP: token route lookup failed", module);
            }
            if (cache.size() >= MAX_ENTRIES) {
                cache.clear();
            }
            entry = new Entry(tenantId, now + CACHE_TTL_MS);
            cache.put(tokenId, entry);
        }
        return NONE.equals(entry.tenantId) ? null : entry.tenantId;
    }

    /** Writes the route of a new token when the delegator belongs to a store; no-op on the base delegator. */
    public static void register(Delegator delegator, String tokenId) throws GenericEntityException {
        String tenantId = delegator.getDelegatorTenantId();
        if (UtilValidate.isEmpty(tenantId)) {
            return;
        }
        Delegator baseDelegator = DelegatorFactory.getDelegator(delegator.getDelegatorBaseName());
        // the master database is another datasource: write the route in its own transaction
        TransactionUtil.doNewTransaction(() -> {
            GenericValue route = baseDelegator.makeValue("McpTokenRoute");
            route.set("tokenId", tokenId);
            route.set("tenantId", tenantId);
            route.set("createdDate", UtilDateTime.nowTimestamp());
            baseDelegator.createOrStore(route);
            return null;
        }, "MCP: could not write the token route", 0, true);
        cache.remove(tokenId);
    }
}
