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
package org.ofbiz.webapp.control;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.logging.log4j.ThreadContext;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.tenant.TenantLoad;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.entity.util.TenantScope;
import org.ofbiz.entity.util.Tenants;
import org.ofbiz.security.Security;
import org.ofbiz.security.SecurityConfigurationException;
import org.ofbiz.security.SecurityFactory;
import org.ofbiz.service.LocalDispatcher;

/**
 * SCIPIO: 4.0.0: Pooled runtime: resolves the store (tenant) of a request from the Host header only.
 *
 * <p>The delegator, dispatcher and security of a store live on the request and in a ThreadLocal, never in the
 * ServletContext, which all stores share (G1). No request parameter or login field selects a store (G3). A bearer
 * token can only select a store through {@link WebappPathHandler#getTenantId} (the MCP token route, G2); on a store host the token
 * and the host must name the same store. An HttpSession belongs to one store: a session from another store is
 * invalidated before the request can read it.</p>
 *
 * <p>Hosts: a {@code TenantDomainName} row maps a host to a store; the hosts in {@code tenant.resolver.baseHosts}
 * (general.properties) serve the base delegator; any other host gets 404. A disabled Tenant gets 503. The host map is
 * cached for {@code tenant.resolver.cacheTtlMs}.</p>
 *
 * <p>Only active when multitenant=Y; otherwise every method is a no-op.</p>
 */
public final class TenantResolver {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /** Request attribute that holds the resolved {@link TenantContext}. */
    public static final String RESOLVED_ATTR = "_SCP_TENANT_CTX_";
    /** Session attribute that binds a session to one store ("_base_" for the base delegator). */
    public static final String SESSION_TENANT_ATTR = "_SCP_TENANT_ID_";
    /** Log MDC key. */
    public static final String MDC_KEY = "tenantId";
    private static final String BASE_KEY = "_base_";
    private static final int MAX_HOST_ENTRIES = 10000;

    private static final boolean MULTITENANT = EntityUtil.isMultiTenantEnabled();
    private static final ThreadLocal<TenantContext> CURRENT = new ThreadLocal<>();
    private static final Map<String, HostEntry> hostCache = new ConcurrentHashMap<>();
    private static final Map<String, LocalDispatcher> dispatcherCache = new ConcurrentHashMap<>();
    /** G17: request slots per store; a store with slow requests (reports) cannot take all threads of the JVM.
     * -1 (default): the plan of the store sets the slots (Tenants.Plan); 0: no limit. */
    private static final int MAX_CONCURRENT = UtilProperties.getPropertyAsInteger("general", "tenant.resolver.maxConcurrentRequests", -1);
    private static final int MAX_CONCURRENT_HEAVY = UtilProperties.getPropertyAsInteger("general", "tenant.resolver.maxConcurrentHeavyRequests", -1);
    private static final Pattern HEAVY_URI = Pattern.compile(UtilProperties.getPropertyValue("general", "tenant.resolver.heavyRequestPattern",
            ".*\\.(pdf|csv|xls|xlsx|xml)$|^/(ordermgr|accounting|webtools|admin|facility|catalog)/.*"));
    private static final long MAX_WAIT_MS = UtilProperties.getPropertyAsLong("general", "tenant.resolver.maxWaitMs", 250L);
    public static final String SLOT_ATTR = "_SCP_TENANT_SLOT_";
    private static final Map<String, Semaphore> slotsByStore = new ConcurrentHashMap<>();
    /** G11: the live sessions of each store in this JVM, so that a suspend can end them at once */
    private static final Map<String, Set<HttpSession>> sessionsByStore = new ConcurrentHashMap<>();

    private TenantResolver() {}

    public static boolean isMultitenant() {
        return MULTITENANT;
    }

    /** The store objects of one request. tenantId is null for the base delegator. */
    public static final class TenantContext {
        private final String tenantId;
        private final Delegator delegator;
        private final LocalDispatcher dispatcher;
        private final Security security;

        TenantContext(String tenantId, Delegator delegator, LocalDispatcher dispatcher, Security security) {
            this.tenantId = tenantId;
            this.delegator = delegator;
            this.dispatcher = dispatcher;
            this.security = security;
        }

        public String getTenantId() { return tenantId; }
        public Delegator getDelegator() { return delegator; }
        public LocalDispatcher getDispatcher() { return dispatcher; }
        public Security getSecurity() { return security; }
        String getSessionKey() { return tenantId != null ? tenantId : BASE_KEY; }
    }

    private static final class HostEntry {
        final String tenantId; // null = base host or unknown host
        final boolean base;
        final boolean disabled;
        final long expires;

        HostEntry(String tenantId, boolean base, boolean disabled, long expires) {
            this.tenantId = tenantId;
            this.base = base;
            this.disabled = disabled;
            this.expires = expires;
        }
    }

    /** The store of the current thread, or null outside a resolved request. */
    public static TenantContext current() {
        return CURRENT.get();
    }

    /** The store of this request, or null when the request was not resolved (single-tenant mode). */
    public static TenantContext fromRequest(HttpServletRequest request) {
        return (TenantContext) request.getAttribute(RESOLVED_ATTR);
    }

    /**
     * Resolves the store of the request once and puts its objects on the request. Returns false when the response
     * is already sent (unknown host 404, suspended store 503, token of another store 403); the caller must stop.
     */
    public static boolean resolve(HttpServletRequest request, HttpServletResponse response, ServletContext servletContext) throws IOException {
        if (!MULTITENANT) {
            return true;
        }
        TenantContext ctx = fromRequest(request);
        if (ctx == null) {
            Delegator baseDelegator = ContextFilter.getDelegator(servletContext);
            String host = request.getServerName() != null ? request.getServerName().toLowerCase(Locale.ROOT) : "";
            HostEntry entry = getHostEntry(host, baseDelegator);
            // G2: a path handler (MCP) may name the store of its token; on a store host it must be the same store
            WebappPathHandler pathHandler = WebappPathHandlerRegistry.findHandler(
                    RequestLinkUtil.getFirstPathElem(RequestLinkUtil.getServletAndPathInfo(request)));
            String tokenTenantId = pathHandler != null ? pathHandler.getTenantId(request, baseDelegator) : null;
            String tenantId;
            if (entry.tenantId != null) {
                if (tokenTenantId != null && !tokenTenantId.equals(entry.tenantId)) {
                    Debug.logWarning("Tenant: token of store [" + tokenTenantId + "] on host [" + host + "] of store ["
                            + entry.tenantId + "]; refused", module);
                    response.sendError(HttpServletResponse.SC_FORBIDDEN, "Token and host name different stores");
                    return false;
                }
                tenantId = entry.tenantId;
            } else if (entry.base) {
                tenantId = tokenTenantId;
            } else {
                response.sendError(HttpServletResponse.SC_NOT_FOUND);
                return false;
            }
            if (tenantId != null && (entry.disabled || (tokenTenantId != null && isDisabled(tokenTenantId, baseDelegator)))) {
                response.setHeader("Retry-After", "300");
                response.sendError(HttpServletResponse.SC_SERVICE_UNAVAILABLE, "Store suspended");
                return false;
            }
            ctx = makeContext(tenantId, baseDelegator, servletContext);
            if (ctx == null) {
                response.sendError(HttpServletResponse.SC_NOT_FOUND);
                return false;
            }
            request.setAttribute(RESOLVED_ATTR, ctx);
        }
        if (CURRENT.get() != ctx) {
            if (CURRENT.get() != null) {
                TenantScope.exit();
            }
            TenantScope.enter(ctx.getDelegator());
        }
        CURRENT.set(ctx);
        ThreadContext.put(MDC_KEY, ctx.getSessionKey());
        request.setAttribute("delegator", ctx.getDelegator());
        request.setAttribute("dispatcher", ctx.getDispatcher());
        request.setAttribute("security", ctx.getSecurity());
        if (ctx.getTenantId() != null) {
            request.setAttribute("userTenantId", ctx.getTenantId());
        }
        checkSession(request, ctx);
        return true;
    }

    /**
     * Binds the session of the request to its store. Call after the request creates its session. A session that
     * belongs to another store is invalidated (a cookie of store A sent to store B).
     */
    public static void bindSession(HttpServletRequest request) {
        TenantContext ctx = MULTITENANT ? fromRequest(request) : null;
        if (ctx == null) {
            return;
        }
        HttpSession session = request.getSession(false);
        if (session != null && session.getAttribute(SESSION_TENANT_ATTR) == null) {
            session.setAttribute(SESSION_TENANT_ATTR, ctx.getSessionKey());
            session.setAttribute("delegatorName", ctx.getDelegator().getDelegatorName());
            trackSession(ctx, session);
        }
    }

    private static void trackSession(TenantContext ctx, HttpSession session) {
        if (ctx.getTenantId() != null) {
            Set<HttpSession> sessions = sessionsByStore.computeIfAbsent(ctx.getTenantId(),
                    k -> Collections.newSetFromMap(new java.util.WeakHashMap<>()));
            synchronized (sessions) {
                sessions.add(session);
            }
        }
    }

    /**
     * G11: suspends a store in this JVM at once: drops the host cache (the store host answers 503 from the next
     * request), ends the store's sessions and frees its request slots. Other JVMs see the suspend within
     * tenant.resolver.cacheTtlMs. Returns the number of ended sessions.
     */
    public static int suspendLocal(String tenantId) {
        clearHostCache();
        Tenants.invalidate(tenantId);
        int ended = 0;
        Set<HttpSession> sessions = sessionsByStore.remove(tenantId);
        if (sessions != null) {
            List<HttpSession> copy;
            synchronized (sessions) {
                copy = new ArrayList<>(sessions);
            }
            for (HttpSession session : copy) {
                try {
                    session.invalidate();
                    ended++;
                } catch (IllegalStateException e) {
                    // already invalidated
                }
            }
        }
        slotsByStore.keySet().removeIf(k -> k.startsWith(tenantId + "#"));
        return ended;
    }

    /** G11: resumes a store in this JVM: drops the cached host map and store state. */
    public static void resumeLocal(String tenantId) {
        clearHostCache();
        Tenants.invalidate(tenantId);
    }

    private static void checkSession(HttpServletRequest request, TenantContext ctx) {
        HttpSession session = request.getSession(false);
        if (session == null) {
            return;
        }
        Object bound;
        try {
            bound = session.getAttribute(SESSION_TENANT_ATTR);
        } catch (IllegalStateException e) {
            return; // already invalidated
        }
        if (bound != null && !bound.equals(ctx.getSessionKey())) {
            Debug.logWarning("Tenant: session of store [" + bound + "] sent to store [" + ctx.getSessionKey()
                    + "]; session invalidated", module);
            session.invalidate();
        } else if (bound == null) {
            session.setAttribute(SESSION_TENANT_ATTR, ctx.getSessionKey());
            session.setAttribute("delegatorName", ctx.getDelegator().getDelegatorName());
            trackSession(ctx, session);
        }
    }

    /**
     * G17: takes one of the store's request slots for this request: tenant.resolver.maxConcurrentRequests (503 when
     * no slot is free within tenant.resolver.maxWaitMs). A URI that matches tenant.resolver.heavyRequestPattern is heavy
     * work (W1-01c, {@link TenantLoad}): the store's heavy slots, queue and time budget, then a heavy slot of the JVM;
     * a store over its own limit gets 429, a JVM without a free heavy slot 503, both with Retry-After.
     * Returns false when the request got no slot; the response is then sent.
     * Returns true without a slot for the base host, when the request already holds one, or when there is no limit.
     * The caller that got a slot calls {@link #releaseSlot} in finally.
     */
    public static boolean acquireSlot(HttpServletRequest request, HttpServletResponse response) throws IOException {
        TenantContext ctx = MULTITENANT ? fromRequest(request) : null;
        if (ctx == null || ctx.getTenantId() == null || request.getAttribute(SLOT_ATTR) != null) {
            return true;
        }
        // heavy requests (reports, exports, back-office apps) have their own, smaller pool of slots per store
        boolean heavy = HEAVY_URI.matcher(request.getRequestURI()).matches();
        if (heavy) {
            if (MAX_CONCURRENT_HEAVY == 0) {
                return true;
            }
            TenantLoad.Admission admission = TenantLoad.enterRequest(ctx.getTenantId());
            if (!admission.isAdmitted()) {
                if (admission.getStatus() == 200) {
                    return true; // no limit (the thread already does heavy work)
                }
                response.setHeader("Retry-After", String.valueOf(admission.getRetryAfterSeconds()));
                response.sendError(admission.getStatus(), admission.getStatus() == 429 ? "Store over its limit for reports and exports"
                        : "Server busy");
                return false;
            }
            request.setAttribute(SLOT_ATTR, admission.getTicket());
            return true;
        }
        int max = (MAX_CONCURRENT >= 0) ? MAX_CONCURRENT : Tenants.getPlan(ctx.getTenantId()).getRequestSlots();
        if (max <= 0) {
            return true;
        }
        // the key holds the size: a plan change gets a new semaphore (the requests on the old one release to it)
        int size = max;
        Semaphore slots = slotsByStore.computeIfAbsent(ctx.getTenantId() + "#" + size, k -> new Semaphore(size, true));
        boolean got;
        try {
            got = slots.tryAcquire(MAX_WAIT_MS, TimeUnit.MILLISECONDS);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            got = false;
        }
        if (!got) {
            response.setHeader("Retry-After", "5");
            response.sendError(HttpServletResponse.SC_SERVICE_UNAVAILABLE, "Store busy");
            return false;
        }
        request.setAttribute(SLOT_ATTR, slots);
        return true;
    }

    /** Gives back the request slot that {@link #acquireSlot} took for this request (no-op when it took none). */
    public static void releaseSlot(HttpServletRequest request) {
        Object slots = request.getAttribute(SLOT_ATTR);
        if (slots instanceof Semaphore) {
            request.removeAttribute(SLOT_ATTR);
            ((Semaphore) slots).release();
        } else if (slots instanceof TenantLoad.Ticket) {
            request.removeAttribute(SLOT_ATTR);
            TenantLoad.exit((TenantLoad.Ticket) slots);
        }
    }

    /** Clears the ThreadLocal and the MDC key; the filter that resolved the request calls this in finally. */
    public static void clear() {
        if (CURRENT.get() != null) {
            TenantScope.exit();
        }
        CURRENT.remove();
        ThreadContext.remove(MDC_KEY);
    }

    /** Drops the cached host map, so that a new or suspended store takes effect at once. */
    public static void clearHostCache() {
        hostCache.clear();
    }

    private static TenantContext makeContext(String tenantId, Delegator baseDelegator, ServletContext servletContext) {
        if (tenantId == null) {
            return new TenantContext(null, baseDelegator, ContextFilter.getDispatcher(servletContext),
                    (Security) servletContext.getAttribute("security"));
        }
        Delegator delegator = DelegatorFactory.getDelegator(baseDelegator.getDelegatorBaseName() + "#" + tenantId);
        if (delegator == null) {
            Debug.logError("Tenant: no delegator for store [" + tenantId + "]", module);
            return null;
        }
        String dispatcherKey = servletContext.getContextPath() + "#" + tenantId;
        LocalDispatcher dispatcher = dispatcherCache.computeIfAbsent(dispatcherKey,
                k -> ContextFilter.makeWebappDispatcher(servletContext, delegator));
        Security security;
        try {
            security = SecurityFactory.getInstance(delegator);
        } catch (SecurityConfigurationException e) {
            Debug.logError(e, "Tenant: no security object for store [" + tenantId + "]", module);
            return null;
        }
        return new TenantContext(tenantId, delegator, dispatcher, security);
    }

    private static HostEntry getHostEntry(String host, Delegator baseDelegator) {
        long now = System.currentTimeMillis();
        HostEntry entry = hostCache.get(host);
        if (entry != null && entry.expires > now) {
            return entry;
        }
        long expires = now + UtilProperties.getPropertyAsLong("general", "tenant.resolver.cacheTtlMs", 5000L);
        if (getBaseHosts().contains(host)) {
            entry = new HostEntry(null, true, false, expires);
        } else {
            String tenantId = null;
            boolean disabled = false;
            try {
                GenericValue domain = EntityQuery.use(baseDelegator).from("TenantDomainName").where("domainName", host).queryOne();
                if (domain != null) {
                    tenantId = domain.getString("tenantId");
                    disabled = isDisabled(tenantId, baseDelegator);
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, "Tenant: host lookup failed for [" + host + "]", module);
            }
            entry = new HostEntry(tenantId, false, disabled, expires);
        }
        if (hostCache.size() >= MAX_HOST_ENTRIES) {
            hostCache.clear(); // random Host headers must not grow the map without limit
        }
        hostCache.put(host, entry);
        return entry;
    }

    private static boolean isDisabled(String tenantId, Delegator baseDelegator) {
        try {
            GenericValue tenant = EntityQuery.use(baseDelegator).from("Tenant").where("tenantId", tenantId).queryOne();
            return tenant == null || "Y".equals(tenant.getString("disabled"));
        } catch (GenericEntityException e) {
            Debug.logError(e, "Tenant: lookup failed for store [" + tenantId + "]", module);
            return true;
        }
    }

    /** The store of a store host; null for a base host and for an unknown host (see {@link #isBaseHost}). */
    public static String getStoreOfHost(String host, Delegator baseDelegator) {
        return (host != null) ? getHostEntry(host.toLowerCase(Locale.ROOT), baseDelegator).tenantId : null;
    }

    /** True when a Tenant with this id exists (entity cache). */
    public static boolean isStoreId(String id, Delegator baseDelegator) {
        if (UtilValidate.isEmpty(id)) {
            return false;
        }
        try {
            return EntityQuery.use(baseDelegator).from("Tenant").where("tenantId", id).cache(true).queryOne() != null;
        } catch (GenericEntityException e) {
            Debug.logError(e, "Tenant: lookup failed for [" + id + "]", module);
            return true; // fail closed: a path that might belong to a store is refused
        }
    }

    /** True when the host serves the base delegator (tenant.resolver.baseHosts), not a store. */
    public static boolean isBaseHost(String host) {
        return host != null && getBaseHosts().contains(host.toLowerCase(Locale.ROOT));
    }

    private static Set<String> getBaseHosts() {
        String value = UtilProperties.getPropertyValue("general", "tenant.resolver.baseHosts", "localhost,127.0.0.1");
        if (UtilValidate.isEmpty(value)) {
            return Collections.emptySet();
        }
        Set<String> hosts = new HashSet<>();
        for (String h : value.split(",")) {
            if (!h.trim().isEmpty()) {
                hosts.add(h.trim().toLowerCase(Locale.ROOT));
            }
        }
        return hosts;
    }
}
