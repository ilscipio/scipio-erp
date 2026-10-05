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

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

import org.ofbiz.entity.GenericValue;

/**
 * SCIPIO: 4.0.0: Authenticated token plus its UserLogin.
 */
public final class McpPrincipal {

    private final GenericValue token;
    private final GenericValue userLogin;
    private final Set<String> webapps;
    private final boolean readOnly;

    public McpPrincipal(GenericValue token, GenericValue userLogin) {
        this.token = token;
        this.userLogin = userLogin;
        Set<String> w = new LinkedHashSet<>();
        String raw = token.getString("webapps");
        if (raw != null) {
            for (String s : raw.split(",")) {
                s = s.trim();
                if (!s.isEmpty()) w.add(s);
            }
        }
        this.webapps = Collections.unmodifiableSet(w);
        this.readOnly = "Y".equals(token.getString("readOnly"));
    }

    public GenericValue getToken() { return token; }
    public GenericValue getUserLogin() { return userLogin; }
    public String getTokenId() { return token.getString("tokenId"); }
    public String getUserLoginId() { return userLogin.getString("userLoginId"); }
    public String getPartyId() { return userLogin.getString("partyId"); }
    public boolean isReadOnly() { return readOnly; }
    /** Spend cap for orders placed through this token, or null when unlimited. */
    public java.math.BigDecimal getMaxOrderAmount() { return token.getBigDecimal("maxOrderAmount"); }

    /** True when the token may be used in the given webapp ({@code *} or empty list = all). */
    public boolean isWebappAllowed(String webappName) {
        if (webapps.isEmpty() || webapps.contains("*")) return true;
        return webapps.contains(webappName);
    }
}
