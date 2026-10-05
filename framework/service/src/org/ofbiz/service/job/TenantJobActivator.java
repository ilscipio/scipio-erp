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
package org.ofbiz.service.job;

import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.ofbiz.base.container.Container;
import org.ofbiz.base.container.ContainerException;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.Tenants;
import org.ofbiz.service.ServiceContainer;

/**
 * SCIPIO: 4.0.0: Pooled runtime: the worker JVM (G10). With multitenant=Y and general.properties
 * {@code tenant.worker.activateAll=Y}, this container creates a service dispatcher for every active store at start and
 * every {@code tenant.worker.scanSeconds}, so that each store's JobManager takes part in the poll cycle. Without it,
 * a store's jobs run only after a web request of that store created its dispatcher (an unvisited store's jobs never
 * start). Suspended stores stay in the registry; the job poller skips them ({@link Tenants#isActive}).
 *
 * <p>A web JVM keeps {@code tenant.worker.activateAll=N} and polls no jobs (serviceengine.xml thread-pool
 * poll-enabled="false"); the worker JVM runs the jobs of all stores.</p>
 */
public class TenantJobActivator implements Container {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final String DISPATCHER_PREFIX = "tenant-worker#";

    private String name;
    private ScheduledExecutorService scanner;

    @Override
    public void init(String[] args, String name, String configFile) throws ContainerException {
        this.name = name;
    }

    @Override
    public boolean start() throws ContainerException {
        String activateAll = UtilProperties.getPropertyValue("general", "tenant.worker.activateAll", "N");
        if (!Tenants.isPooled() || !("Y".equalsIgnoreCase(activateAll) || "true".equalsIgnoreCase(activateAll))) {
            return true;
        }
        long scanSeconds = Math.max(5L, UtilProperties.getPropertyAsLong("general", "tenant.worker.scanSeconds", 30L));
        scanner = Executors.newSingleThreadScheduledExecutor(r -> {
            Thread t = new Thread(r, "Scipio-TenantJobActivator");
            t.setDaemon(true);
            return t;
        });
        // the first scan runs in the background: store activation must not delay the JVM start
        scanner.scheduleWithFixedDelay(TenantJobActivator::scanSafe, 5, scanSeconds, TimeUnit.SECONDS);
        Debug.logInfo("Tenant worker: activates the jobs of all active stores every " + scanSeconds + " s", module);
        return true;
    }

    private static void scanSafe() {
        try {
            int activated = activateAll();
            if (activated > 0) {
                Debug.logInfo("Tenant worker: activated the jobs of " + activated + " store(s)", module);
            }
        } catch (Throwable t) {
            Debug.logError(t, "Tenant worker: scan failed", module);
        }
    }

    /** Creates the dispatcher (and so the JobManager) of every active store that has none. Returns the new ones. */
    public static int activateAll() throws GenericEntityException {
        Delegator base = DelegatorFactory.getDelegator(Tenants.BASE_DELEGATOR);
        List<GenericValue> tenants = EntityQuery.use(base).from("Tenant").select("tenantId", "disabled").queryList();
        int activated = 0;
        for (GenericValue tenant : tenants) {
            String tenantId = tenant.getString("tenantId");
            if ("Y".equals(tenant.getString("disabled")) || JobManager.isRegistered(base.getDelegatorName() + "#" + tenantId)) {
                continue;
            }
            if (activate(tenantId)) {
                activated++;
            }
        }
        return activated;
    }

    /** Creates the dispatcher of one store in this JVM, which registers its JobManager with the job poller. */
    public static boolean activate(String tenantId) {
        Delegator base = DelegatorFactory.getDelegator(Tenants.BASE_DELEGATOR);
        String delegatorName = base.getDelegatorName() + "#" + tenantId;
        try {
            Delegator delegator = DelegatorFactory.getDelegator(delegatorName);
            if (delegator == null) {
                Debug.logWarning("Tenant worker: no delegator for store [" + tenantId + "]", module);
                return false;
            }
            ServiceContainer.getLocalDispatcher(DISPATCHER_PREFIX + tenantId, delegator);
            // the dispatcher stays cached after deleteTenant unregistered the JobManager: register a new one
            JobManager.getInstance(delegator, true);
            return true;
        } catch (RuntimeException e) {
            Debug.logError(e, "Tenant worker: could not activate store [" + tenantId + "]", module);
            return false;
        }
    }

    @Override
    public void stop() throws ContainerException {
        if (scanner != null) {
            scanner.shutdownNow();
        }
    }

    @Override
    public String getName() {
        return name;
    }
}
