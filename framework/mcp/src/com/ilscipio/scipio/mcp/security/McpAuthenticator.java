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

import java.sql.Timestamp;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.mcp.web.McpRequest;

/**
 * SCIPIO: 4.0.0: Bearer token authentication. Never reads the HttpSession.
 */
public final class McpAuthenticator {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private McpAuthenticator() {}

    /**
     * Returns the principal for the Authorization header, null when no header is present, or throws for a bad token.
     */
    public static McpPrincipal authenticate(McpRequest req) throws McpAuthException {
        String header = req.getHttpRequest().getHeader("Authorization");
        if (UtilValidate.isEmpty(header)) {
            return null;
        }
        if (!header.regionMatches(true, 0, "Bearer ", 0, 7)) {
            throw McpAuthException.unauthorized("Bearer token required");
        }
        String raw = header.substring(7).trim();
        String tokenId = McpTokenUtil.parseTokenId(raw);
        if (tokenId == null) {
            throw McpAuthException.unauthorized("Invalid token");
        }
        Delegator delegator = req.getDelegator();
        GenericValue token;
        GenericValue userLogin;
        try {
            token = EntityQuery.use(delegator).from("McpAccessToken").where("tokenId", tokenId).queryOne();
            if (token == null || !McpTokenUtil.verify(raw, token.getString("tokenHash"))) {
                throw McpAuthException.unauthorized("Invalid token");
            }
            if ("Y".equals(token.getString("disabled"))) {
                throw McpAuthException.unauthorized("Token revoked");
            }
            Timestamp expires = token.getTimestamp("expiresDate");
            if (expires == null && !McpConfig.isTokenAllowNoExpiry()) {
                throw McpAuthException.unauthorized("Token has no expiry date; reissue it or set mcp.token.allowNoExpiry");
            }
            if (expires != null && expires.before(UtilDateTime.nowTimestamp())) {
                throw McpAuthException.unauthorized("Token expired");
            }
            if (!isRemoteAddrAllowed(token.getString("remoteAddrAllow"), req.getRemoteAddr())) {
                throw McpAuthException.forbidden("Token not allowed from this address");
            }
            userLogin = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", token.getString("userLoginId")).queryOne();
        } catch (GenericEntityException e) {
            Debug.logError(e, "MCP: token lookup failed", module);
            throw McpAuthException.unauthorized("Authentication unavailable");
        }
        if (userLogin == null) {
            throw McpAuthException.unauthorized("Token user missing");
        }
        if ("N".equals(userLogin.getString("enabled"))) {
            throw McpAuthException.unauthorized("Token user disabled");
        }
        if (McpConfig.getTokenDenyUsers().contains(userLogin.getString("userLoginId"))) {
            throw McpAuthException.forbidden("Token user " + userLogin.getString("userLoginId") + " may not use MCP");
        }
        touchLastUsed(delegator, token);
        return new McpPrincipal(token, userLogin);
    }

    /** Allows exact addresses, dotted prefixes (10.0.), and IPv4 CIDR blocks; empty list = any address. */
    public static boolean isRemoteAddrAllowed(String allowList, String remoteAddr) {
        if (UtilValidate.isEmpty(allowList)) return true;
        if (remoteAddr == null) return false;
        for (String rule : allowList.split(",")) {
            rule = rule.trim();
            if (rule.isEmpty()) continue;
            if (rule.equals(remoteAddr)) return true;
            if (rule.endsWith(".") && remoteAddr.startsWith(rule)) return true;
            if (rule.contains("/") && cidrMatches(rule, remoteAddr)) return true;
        }
        return false;
    }

    static boolean cidrMatches(String cidr, String addr) {
        try {
            String[] parts = cidr.split("/");
            long net = ipv4(parts[0]);
            long ip = ipv4(addr);
            int bits = Integer.parseInt(parts[1]);
            if (net < 0 || ip < 0 || bits < 0 || bits > 32) return false;
            long mask = bits == 0 ? 0 : (0xFFFFFFFFL << (32 - bits)) & 0xFFFFFFFFL;
            return (net & mask) == (ip & mask);
        } catch (RuntimeException e) {
            return false;
        }
    }

    private static long ipv4(String s) {
        String[] o = s.split("\\.");
        if (o.length != 4) return -1;
        long v = 0;
        for (String part : o) {
            int n = Integer.parseInt(part);
            if (n < 0 || n > 255) return -1;
            v = (v << 8) | n;
        }
        return v;
    }

    private static void touchLastUsed(Delegator delegator, GenericValue token) {
        Timestamp last = token.getTimestamp("lastUsedDate");
        long now = System.currentTimeMillis();
        if (last != null && now - last.getTime() < 60_000L) return;
        boolean began = false;
        try {
            began = TransactionUtil.begin();
            token.set("lastUsedDate", new Timestamp(now));
            token.store();
            TransactionUtil.commit(began);
        } catch (GenericEntityException e) {
            try {
                TransactionUtil.rollback(began, "MCP lastUsedDate update failed", e);
            } catch (GenericEntityException ignored) {
                // ignore
            }
            Debug.logWarning(e, "MCP: could not update token lastUsedDate", module);
        }
    }
}
