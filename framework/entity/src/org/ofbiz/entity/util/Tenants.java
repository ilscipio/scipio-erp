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
package org.ofbiz.entity.util;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

/**
 * SCIPIO: 4.0.0: Pooled runtime: the state and the plan limits of a store (tenant), read from the master table
 * {@code Tenant} through the base delegator and cached for {@code tenant.info.cacheTtlMs} (general.properties).
 *
 * <p>Plan limits (G17) come from general.properties: {@code tenant.plan.<planId>.<limit>}, then
 * {@code tenant.plan.default.<limit>}, then the built-in default of the pooled profile. A store without a planId
 * uses {@code tenant.plan.defaultPlanId}. The limits: request slots and heavy request slots per JVM, jobs per poll
 * cycle, the maximum size of the store's connection pool, and mails per day.</p>
 *
 * <p>Runtime only: never call this during the build (the static initializers of Debug load DelegatorFactory, which is not on the build class path).</p>
 */
public final class Tenants {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final String RES = "general";
    /** The base delegator that holds the master tables (Tenant, TenantDataSource, TenantDomainName). */
    public static final String BASE_DELEGATOR = UtilProperties.getPropertyValue(RES, "tenant.baseDelegator", "default");

    /** Built-in defaults of the pooled profile (W0-03 values). */
    public static final int DEFAULT_REQUEST_SLOTS = 4;
    public static final int DEFAULT_HEAVY_REQUEST_SLOTS = 1;
    public static final int DEFAULT_JOBS_PER_POLL = 3;
    public static final int DEFAULT_DB_POOL_MAX = 12;
    public static final int DEFAULT_MAIL_PER_DAY = 500;
    public static final int DEFAULT_HEAVY_QUEUE = 2;
    public static final int DEFAULT_HEAVY_SHARE = 25;
    public static final int DEFAULT_HEAVY_BURST_SECONDS = 20;
    public static final int DEFAULT_JOB_THREADS = 2;

    private static final Map<String, Info> infoCache = new ConcurrentHashMap<>();
    private static final Map<String, Plan> planCache = new ConcurrentHashMap<>();

    private Tenants() {}

    /** The limits of one plan. */
    public static final class Plan {
        private final String planId;
        private final int requestSlots;
        private final int heavyRequestSlots;
        private final int jobsPerPoll;
        private final int dbPoolMax;
        private final int mailPerDay;
        private final int heavyQueue;
        private final int heavyShare;
        private final int heavyBurstSeconds;
        private final int jobThreads;

        Plan(String planId) {
            this.planId = planId;
            this.requestSlots = limit(planId, "requestSlots", DEFAULT_REQUEST_SLOTS);
            this.heavyRequestSlots = limit(planId, "heavyRequestSlots", DEFAULT_HEAVY_REQUEST_SLOTS);
            this.jobsPerPoll = limit(planId, "jobsPerPoll", DEFAULT_JOBS_PER_POLL);
            this.dbPoolMax = limit(planId, "dbPoolMax", DEFAULT_DB_POOL_MAX);
            this.mailPerDay = limit(planId, "mailPerDay", DEFAULT_MAIL_PER_DAY);
            this.heavyQueue = limit(planId, "heavyQueue", DEFAULT_HEAVY_QUEUE);
            this.heavyShare = limit(planId, "heavyShare", DEFAULT_HEAVY_SHARE);
            this.heavyBurstSeconds = limit(planId, "heavyBurstSeconds", DEFAULT_HEAVY_BURST_SECONDS);
            this.jobThreads = limit(planId, "jobThreads", DEFAULT_JOB_THREADS);
        }

        public String getPlanId() { return planId; }
        /** Request slots of the store in one JVM (0 = no limit). */
        public int getRequestSlots() { return requestSlots; }
        /** Slots for heavy requests (reports, exports, back-office apps) of the store in one JVM (0 = no limit). */
        public int getHeavyRequestSlots() { return heavyRequestSlots; }
        /** Jobs of the store per poll cycle of the shared job queue (0 or less = no cap). */
        public int getJobsPerPoll() { return jobsPerPoll; }
        /** Maximum connections of the store's connection pool per entity group datasource (0 = datasource setting). */
        public int getDbPoolMax() { return dbPoolMax; }
        /** Mails the store may send per day and JVM (0 = no limit). */
        public int getMailPerDay() { return mailPerDay; }
        /** W1-01c: heavy requests of the store that may wait for a heavy slot (TenantLoad). */
        public int getHeavyQueue() { return heavyQueue; }
        /** W1-01c: percent of the time of one thread that the store's heavy work may use on average (0 = no budget). */
        public int getHeavyShare() { return heavyShare; }
        /** W1-01c: seconds of heavy work at full speed that a full budget gives. */
        public int getHeavyBurstSeconds() { return heavyBurstSeconds; }
        /** W1-01c: jobs of the store that are queued or running at once in one JVM (0 or less = no cap). */
        public int getJobThreads() { return jobThreads; }

        @Override
        public String toString() {
            return "Plan[" + planId + ": requestSlots=" + requestSlots + ", heavyRequestSlots=" + heavyRequestSlots + ", jobsPerPoll="
                    + jobsPerPoll + ", dbPoolMax=" + dbPoolMax + ", mailPerDay=" + mailPerDay + ", heavyQueue=" + heavyQueue + ", heavyShare=" + heavyShare
                    + ", heavyBurstSeconds=" + heavyBurstSeconds + ", jobThreads=" + jobThreads + "]";
        }
    }

    private static final class Info {
        final boolean exists;
        final boolean disabled;
        final String planId;
        final long expires;

        Info(boolean exists, boolean disabled, String planId, long expires) {
            this.exists = exists;
            this.disabled = disabled;
            this.planId = planId;
            this.expires = expires;
        }
    }

    /** True in the pooled runtime (general.properties multitenant=Y). */
    public static boolean isPooled() {
        return EntityUtil.isMultiTenantEnabled();
    }

    /** The limits of the store's plan; the default plan for an unknown store or null (the base delegator). */
    public static Plan getPlan(String tenantId) {
        String planId = null;
        if (UtilValidate.isNotEmpty(tenantId)) {
            planId = getInfo(tenantId).planId;
        }
        if (UtilValidate.isEmpty(planId)) {
            planId = UtilProperties.getPropertyValue(RES, "tenant.plan.defaultPlanId", "default");
        }
        return planCache.computeIfAbsent(planId, Plan::new);
    }

    /** True when the Tenant row exists and is not disabled (suspended). Cached for tenant.info.cacheTtlMs. */
    public static boolean isActive(String tenantId) {
        Info info = getInfo(tenantId);
        return info.exists && !info.disabled;
    }

    /** Drops the cached state of one store (null: of all stores) and the cached plans; call after a change of Tenant. */
    public static void invalidate(String tenantId) {
        if (tenantId == null) {
            infoCache.clear();
        } else {
            infoCache.remove(tenantId);
        }
        planCache.clear();
    }

    /** True when a Tenant row exists (suspended or not). Cached like {@link #isActive}. */
    public static boolean exists(String tenantId) {
        return tenantId != null && getInfo(tenantId).exists;
    }

    private static Info getInfo(String tenantId) {
        Info info = infoCache.get(tenantId);
        if (info != null && info.expires > System.currentTimeMillis()) {
            return info;
        }
        synchronized (Tenants.class) {
            info = infoCache.get(tenantId);
            if (info != null && info.expires > System.currentTimeMillis()) {
                return info; // another thread reloaded the table
            }
            reloadAll();
            info = infoCache.get(tenantId);
            if (info == null) {
                info = new Info(false, true, null, System.currentTimeMillis() + UtilProperties.getPropertyAsLong(RES, "tenant.info.cacheTtlMs", 5000L));
                infoCache.put(tenantId, info);
            }
            return info;
        }
    }

    /**
     * Reads the state of all stores in one query (the job poller asks for every store in each cycle). A store that is
     * no longer in the table drops out of the cache.
     */
    private static void reloadAll() {
        long now = System.currentTimeMillis();
        long expires = now + UtilProperties.getPropertyAsLong(RES, "tenant.info.cacheTtlMs", 5000L);
        Delegator base = DelegatorFactory.getDelegator(BASE_DELEGATOR);
        try {
            Map<String, Info> fresh = new java.util.HashMap<>();
            if (base != null) {
                for (GenericValue tenant : EntityQuery.use(base).from("Tenant").select("tenantId", "disabled", "planId").queryList()) {
                    fresh.put(tenant.getString("tenantId"), new Info(true, "Y".equals(tenant.getString("disabled")), tenant.getString("planId"), expires));
                }
            }
            infoCache.keySet().retainAll(fresh.keySet());
            infoCache.putAll(fresh);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Tenant: could not read the Tenant table; every store is treated as suspended for 1 s", module);
            for (String tenantId : new java.util.ArrayList<>(infoCache.keySet())) {
                infoCache.put(tenantId, new Info(false, true, null, now + 1000L)); // fail closed, retry soon
            }
        }
    }

    private static int limit(String planId, String name, int builtIn) {
        String value = UtilProperties.getPropertyValue(RES, "tenant.plan." + planId + "." + name);
        if (UtilValidate.isEmpty(value)) {
            value = UtilProperties.getPropertyValue(RES, "tenant.plan.default." + name);
        }
        if (UtilValidate.isEmpty(value)) {
            return builtIn;
        }
        try {
            return Integer.parseInt(value.trim());
        } catch (NumberFormatException e) {
            Debug.logWarning("Tenant: bad value [" + value + "] for tenant.plan." + planId + "." + name + "; using " + builtIn, module);
            return builtIn;
        }
    }
}
