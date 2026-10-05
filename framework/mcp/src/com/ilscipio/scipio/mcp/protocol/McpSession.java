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

/**
 * SCIPIO: 4.0.0: Server-side MCP session (not an HttpSession). Bound to the token that created it.
 */
public final class McpSession {

    private final String id;
    private final String tokenId;
    private final String webappName;
    private final String protocolVersion;
    private final long createdMillis;
    private volatile long lastAccessMillis;
    private final Map<String, Object> attributes = new ConcurrentHashMap<>();

    /** SCIPIO: 4.0.0: pooled runtime: delegator (store) that created the session; a session is valid only there (G6). */
    private final String delegatorName;

    public McpSession(String id, String tokenId, String webappName, String protocolVersion) {
        this(id, tokenId, webappName, protocolVersion, "");
    }

    public McpSession(String id, String tokenId, String webappName, String protocolVersion, String delegatorName) {
        this.delegatorName = delegatorName != null ? delegatorName : "";
        this.id = id;
        this.tokenId = tokenId != null ? tokenId : "";
        this.webappName = webappName;
        this.protocolVersion = protocolVersion;
        this.createdMillis = System.currentTimeMillis();
        this.lastAccessMillis = createdMillis;
    }

    public String getId() { return id; }
    /** SCIPIO: 4.0.0: the delegator (store) of the session. */
    public String getDelegatorName() { return delegatorName; }
    /** Token id that created the session; empty for anonymous sessions. */
    public String getTokenId() { return tokenId; }
    public String getWebappName() { return webappName; }
    public String getProtocolVersion() { return protocolVersion; }
    public long getCreatedMillis() { return createdMillis; }
    public long getLastAccessMillis() { return lastAccessMillis; }

    public void touch() {
        this.lastAccessMillis = System.currentTimeMillis();
    }

    public Object getAttribute(String name) {
        return attributes.get(name);
    }

    @SuppressWarnings("unchecked")
    public <T> T getAttribute(String name, Class<T> type) {
        Object v = attributes.get(name);
        return type.isInstance(v) ? (T) v : null;
    }

    public void setAttribute(String name, Object value) {
        if (value == null) attributes.remove(name);
        else attributes.put(name, value);
    }

    public void removeAttribute(String name) {
        attributes.remove(name);
    }
}
