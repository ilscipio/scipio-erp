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
package com.ilscipio.scipio.mcp.registry;

import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceAuthException;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.ServiceValidationException;

import com.ilscipio.scipio.mcp.protocol.McpSession;
import com.ilscipio.scipio.mcp.security.McpConfig;
import com.ilscipio.scipio.mcp.security.McpPrincipal;
import com.ilscipio.scipio.mcp.web.McpRequest;

/**
 * SCIPIO: 4.0.0: What a tool implementation receives: framework objects, the acting user and helpers.
 */
public final class McpCallContext {

    private final McpRequest request;

    public McpCallContext(McpRequest request) {
        this.request = request;
    }

    public McpRequest getRequest() { return request; }
    public Delegator getDelegator() { return request.getDelegator(); }
    public LocalDispatcher getDispatcher() { return request.getDispatcher(); }
    public Security getSecurity() { return request.getSecurity(); }
    /** The acting UserLogin; null for anonymous calls to PUBLIC tools. */
    public GenericValue getUserLogin() { return request.getUserLogin(); }
    public String getUserLoginId() { return request.getUserLoginId(); }
    public String getPartyId() { return request.getPrincipal() != null ? request.getPrincipal().getPartyId() : null; }
    public McpPrincipal getPrincipal() { return request.getPrincipal(); }
    public boolean isAnonymous() { return request.isAnonymous(); }
    public McpServerDef getServer() { return request.getServer(); }
    public McpSession getSession() { return request.getSession(); }
    public Locale getLocale() { return request.getLocale(); }
    public TimeZone getTimeZone() { return request.getTimeZone(); }
    public String getWebappName() { return request.getWebappName(); }
    public String getWebSiteId() { return request.getWebSiteId(); }
    public String getProductStoreId() { return request.getProductStoreId(); }
    public String getCurrencyUomId() { return request.getCurrencyUomId(); }

    /** Builds a service context with userLogin, locale and timeZone added. */
    public Map<String, Object> serviceContext(Map<String, ?> params) {
        Map<String, Object> ctx = new LinkedHashMap<>();
        if (params != null) ctx.putAll(params);
        if (getUserLogin() != null) ctx.put("userLogin", getUserLogin());
        ctx.put("locale", getLocale());
        ctx.put("timeZone", getTimeZone());
        return ctx;
    }

    /**
     * Runs a service as the acting user in its own transaction. Service error results and auth failures become
     * {@link McpToolException}; the returned map is the raw (not yet JSON-converted) result.
     */
    public Map<String, Object> runService(String serviceName, Map<String, ?> params) throws McpToolException {
        try {
            Map<String, Object> result = getDispatcher().runSync(serviceName, serviceContext(params),
                    McpConfig.getServiceTimeoutSeconds(), true);
            if (ServiceUtil.isError(result)) {
                throw new McpToolException("Service " + serviceName + " failed: " + ServiceUtil.getErrorMessage(result));
            }
            return result;
        } catch (ServiceAuthException e) {
            throw McpToolException.denied("Service " + serviceName + " denied: " + e.getMessage());
        } catch (ServiceValidationException e) {
            throw new McpToolException("Service " + serviceName + " rejected the parameters: " + e.getMessage());
        } catch (GenericServiceException e) {
            throw new McpToolException("Service " + serviceName + " failed: " + safeMessage(e));
        }
    }

    public boolean hasPermission(String permission) {
        GenericValue ul = getUserLogin();
        if (ul == null || permission == null || permission.isEmpty()) return false;
        int idx = permission.lastIndexOf('_');
        if (idx > 0) {
            String base = permission.substring(0, idx);
            String action = permission.substring(idx);
            if (getSecurity().hasEntityPermission(base, action, ul)) return true;
        }
        return getSecurity().hasPermission(permission, ul);
    }

    public void requirePermission(String permission) throws McpToolException {
        if (!hasPermission(permission)) {
            throw McpToolException.denied("Permission " + permission + " required");
        }
    }

    /** Clamps a requested list limit to the configured default and maximum. */
    public int limit(Integer requested) {
        int def = McpConfig.getListDefaultLimit();
        int max = McpConfig.getListMaxLimit();
        if (requested == null || requested <= 0) return def;
        return Math.min(requested, max);
    }

    static String safeMessage(Throwable t) {
        String m = t.getMessage();
        if (m == null || m.isEmpty()) return t.getClass().getSimpleName();
        return m.length() > 500 ? m.substring(0, 500) : m;
    }
}
