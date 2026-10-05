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
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

import org.ofbiz.base.util.UtilProperties;

/**
 * SCIPIO: 4.0.0: Typed access to {@code mcp.properties}. Values are read on each call (cheap, cached by UtilProperties).
 */
public final class McpConfig {

    public static final String RESOURCE = "mcp";

    private McpConfig() {}

    public static boolean isEnabled() { return bool("mcp.enabled", true); }
    public static String getPathSegment() { return str("mcp.path.segment", "mcp"); }
    public static List<String> getExcludedWebapps() { return list("mcp.webapp.exclude"); }
    public static boolean isAllowInsecure() { return bool("mcp.allowInsecure", false); }
    public static List<String> getAllowedOrigins() { return list("mcp.origin.allow"); }
    public static List<String> getAllowedHosts() { return list("mcp.host.allow"); }
    /** Reverse proxy addresses (exact, dotted prefix or CIDR) whose {@code X-Forwarded-Proto} header is trusted. */
    public static List<String> getTrustedProxies() { return list("mcp.trustedProxies"); }

    public static int getRequestMaxBytes() { return integer("mcp.request.maxBytes", 1048576); }
    public static int getJsonMaxDepth() { return integer("mcp.request.maxDepth", 64); }
    public static int getBatchMaxSize() { return integer("mcp.batch.maxSize", 20); }
    public static int getStringMaxLength() { return integer("mcp.string.maxLength", 65536); }
    public static int getArrayMaxLength() { return integer("mcp.array.maxLength", 1000); }
    public static int getListDefaultLimit() { return integer("mcp.list.defaultLimit", 50); }
    public static int getListMaxLimit() { return integer("mcp.list.maxLimit", 500); }
    public static int getResultMaxChars() { return integer("mcp.result.maxChars", 200000); }
    public static int getSessionIdleMinutes() { return integer("mcp.session.idleMinutes", 30); }
    /** SCIPIO: 4.0.0: pooled runtime: live MCP sessions per store and JVM (G6). */
    public static int getSessionMaxPerStore() { return integer("mcp.session.maxPerStore", 1000); }
    public static int getRateLimitPerMinute() { return integer("mcp.rateLimit.perMinute", 120); }
    public static int getAnonymousRateLimitPerMinute() { return integer("mcp.rateLimit.anonymousPerMinute", 60); }
    public static int getFailedAuthPerMinute() { return integer("mcp.rateLimit.failedAuthPerMinute", 10); }
    public static int getMaxConcurrent() { return integer("mcp.rateLimit.maxConcurrent", 4); }
    /**
     * SCIPIO: 4.0.0: the per-minute limit of one login: {@code mcp.rateLimit.perMinute.user.<userLoginId>}, else the default.
     * A platform login (the desk of a hosted store, login "desk") makes the calls of the seller's app and of the store jobs, so
     * it needs more than the default of an agent token.
     */
    public static int getRateLimitPerMinute(String userLoginId) {
        return userLoginId == null ? getRateLimitPerMinute() : integer("mcp.rateLimit.perMinute.user." + userLoginId, getRateLimitPerMinute());
    }
    /** SCIPIO: 4.0.0: the concurrent calls of one login: {@code mcp.rateLimit.maxConcurrent.user.<userLoginId>}, else the default. */
    public static int getMaxConcurrent(String userLoginId) {
        return userLoginId == null ? getMaxConcurrent() : integer("mcp.rateLimit.maxConcurrent.user." + userLoginId, getMaxConcurrent());
    }
    public static int getServiceTimeoutSeconds() { return integer("mcp.service.timeoutSeconds", 120); }

    /** User logins that may never own a token. */
    public static List<String> getTokenDenyUsers() { return listOr("mcp.token.denyUsers", "system,anonymous"); }
    public static int getTokenDefaultExpiryDays() { return integer("mcp.token.defaultExpiryDays", 90); }
    public static int getTokenMaxExpiryDays() { return integer("mcp.token.maxExpiryDays", 365); }
    /** When false, a token row without an expiry date is treated as expired. */
    public static boolean isTokenAllowNoExpiry() { return bool("mcp.token.allowNoExpiry", false); }

    public static List<String> getServiceDenyPatterns() { return list("mcp.service.deny"); }
    /** Service name patterns callable only by users with MCP_ADMIN (user login and password services). */
    public static List<String> getServiceAdminOnlyPatterns() { return list("mcp.service.adminOnly"); }
    public static List<String> getEntityDenyPatterns() { return list("mcp.entity.deny"); }
    public static boolean isReadOnlyHeuristic() { return bool("mcp.gateway.readOnlyHeuristic", true); }
    public static boolean isAllowUnguarded() { return bool("mcp.gateway.allowUnguarded", true); }
    /** Components without a webapp whose services fall back to the endpoint's base permission. */
    public static List<String> getOpenComponents() { return listOr("mcp.gateway.openComponents", "common"); }
    /** Tool names ({@code tool} or {@code server.tool}) removed from every server at registry build. */
    public static List<String> getDisabledTools() { return list("mcp.tool.disable"); }
    /** EmailTemplateSetting ids that mail_send_template may use. */
    public static List<String> getMailTemplates() { return listOr("mcp.mail.templates", "MCP_PURCHASE_ORDER,MCP_STATEMENT,MCP_CUSTOMER_NOTICE"); }
    public static List<String> getRedactFields() { return list("mcp.redact.fields"); }
    public static List<String> getRedactPatterns() { return list("mcp.redact.patterns"); }

    public static boolean isAuditEnabled() { return bool("mcp.audit.enabled", true); }
    public static int getAuditArgsMaxChars() { return integer("mcp.audit.argsMaxChars", 4000); }
    /** Largest stored result for idempotent replay; a longer result is not stored and cannot be replayed. */
    public static int getAuditResultMaxChars() { return integer("mcp.audit.resultMaxChars", 65536); }
    public static int getUsageFlushSeconds() { return integer("mcp.usage.flushSeconds", 60); }

    private static String str(String key, String def) {
        String v = UtilProperties.getPropertyValue(RESOURCE, key, def);
        return v != null ? v.trim() : def;
    }

    private static boolean bool(String key, boolean def) {
        Boolean v = UtilProperties.getPropertyAsBoolean(RESOURCE, key, def);
        return v != null ? v : def;
    }

    private static int integer(String key, int def) {
        Integer v = UtilProperties.getPropertyAsInteger(RESOURCE, key, def);
        return v != null ? v : def;
    }

    private static List<String> list(String key) {
        return split(str(key, ""));
    }

    private static List<String> listOr(String key, String def) {
        return split(str(key, def));
    }

    private static List<String> split(String v) {
        if (v == null || v.isEmpty()) return Collections.emptyList();
        List<String> out = new ArrayList<>();
        for (String s : v.split(",")) {
            s = s.trim();
            if (!s.isEmpty()) out.add(s);
        }
        return out;
    }

    /** Simple glob match: {@code *} matches any run of characters; case-insensitive. */
    public static boolean globMatches(String pattern, String value) {
        if (pattern == null || value == null) return false;
        if (!pattern.contains("*")) return pattern.equalsIgnoreCase(value);
        StringBuilder regex = new StringBuilder("^");
        for (String part : pattern.split("\\*", -1)) {
            regex.append(Pattern.quote(part)).append(".*");
        }
        regex.setLength(regex.length() - 2);
        regex.append("$");
        return Pattern.compile(regex.toString(), Pattern.CASE_INSENSITIVE).matcher(value).matches();
    }

    public static boolean anyGlobMatches(List<String> patterns, String value) {
        if (patterns == null) return false;
        for (String p : patterns) {
            if (globMatches(p, value)) return true;
        }
        return false;
    }
}
