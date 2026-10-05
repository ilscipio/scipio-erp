/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
/*
 * Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
 * under the GNU Affero General Public License, version 3, or a commercial
 * license from Ilscipio GmbH (file LICENSE). The original code stays under
 * the Apache License, version 2.0, as stated above.
 */
package org.ofbiz.entity.connection;

import java.sql.Connection;
import java.sql.Driver;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import javax.transaction.TransactionManager;

import org.apache.commons.dbcp2.DriverConnectionFactory;
import org.apache.commons.dbcp2.PoolableConnection;
import org.apache.commons.dbcp2.PoolableConnectionFactory;
import org.apache.commons.dbcp2.managed.LocalXAConnectionFactory;
import org.apache.commons.dbcp2.managed.PoolableManagedConnectionFactory;
import org.apache.commons.dbcp2.managed.XAConnectionFactory;
import org.apache.commons.pool2.impl.GenericObjectPool;
import org.apache.commons.pool2.impl.GenericObjectPoolConfig;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.GenericEntityConfException;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.config.model.EntityConfig;
import org.ofbiz.entity.config.model.InlineJdbc;
import org.ofbiz.entity.config.model.JdbcElement;
import org.ofbiz.entity.datasource.GenericHelperInfo;
import org.ofbiz.entity.transaction.TransactionFactoryLoader;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.Tenants;

/**
 * Apache Commons DBCP connection factory.
 *
 * @see <a href="http://commons.apache.org/proper/commons-dbcp/">Apache Commons DBCP</a>
 */
public class DBCPConnectionFactory implements ConnectionFactory {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    // ManagedDataSource is useful to debug the usage of connections in the pool (must be verbose)
    // In case you don't want to be disturbed in the log (focusing on something else), it's still easy to comment out the line from DebugManagedDataSource
    protected static final ConcurrentHashMap<String, DebugManagedDataSource<? extends Connection>> dsCache =
            new ConcurrentHashMap<>();

    /** SCIPIO: 4.0.0: Pooled runtime: last borrow time per store pool (key: helper full name with "#tenantId") (G12). */
    private static final ConcurrentHashMap<String, AtomicLong> tenantPoolLastUse = new ConcurrentHashMap<>();
    /** SCIPIO: 4.0.0: Pooled runtime: close the pool of a store after this many seconds without a borrow (0 = never). */
    private static final long TENANT_POOL_IDLE_CLOSE_MS = UtilProperties.getPropertyAsLong("general", "tenant.pool.idleCloseSeconds", 900L) * 1000L;
    private static final long TENANT_POOL_CLOSE_GRACE_MS = 30000L;
    private static volatile ScheduledExecutorService tenantPoolReaper;

    public Connection getConnection(GenericHelperInfo helperInfo, JdbcElement abstractJdbc) throws SQLException, GenericEntityException {
        String cacheKey = helperInfo.getHelperFullName();
        boolean tenantPool = !helperInfo.getTenantId().isEmpty();
        if (tenantPool) {
            markTenantPoolUse(cacheKey);
        }
        DebugManagedDataSource<? extends Connection> mds = dsCache.get(cacheKey);
        if (mds != null) {
            return TransactionUtil.getCursorConnection(helperInfo, mds.getConnection());
        }
        if (!(abstractJdbc instanceof InlineJdbc)) {
            throw new GenericEntityConfException("DBCP requires an <inline-jdbc> child element in the <datasource> element");
        }
        InlineJdbc jdbcElement = (InlineJdbc) abstractJdbc;
        // connection properties
        TransactionManager txMgr = TransactionFactoryLoader.getInstance().getTransactionManager();
        String driverName = jdbcElement.getJdbcDriver();

        String jdbcUri = helperInfo.getOverrideJdbcUri(jdbcElement.getJdbcUri());
        String jdbcUsername = helperInfo.getOverrideUsername(jdbcElement.getJdbcUsername());
        String jdbcPassword = helperInfo.getOverridePassword(EntityConfig.getJdbcPassword(jdbcElement));

        // pool settings
        int maxSize = jdbcElement.getPoolMaxsize();
        if (tenantPool && Tenants.isPooled()) {
            // SCIPIO: 4.0.0: pooled runtime: the plan of the store sets the size of its pool (G12, G17)
            int planMax = Tenants.getPlan(helperInfo.getTenantId()).getDbPoolMax();
            if (planMax > 0) {
                maxSize = planMax;
            }
        }
        int minSize = jdbcElement.getPoolMinsize();
        int maxIdle = jdbcElement.getIdleMaxsize();
        // maxIdle must be greater than pool-minsize
        maxIdle = maxIdle > minSize ? maxIdle : minSize;
        // load the driver
        Driver jdbcDriver;
        synchronized (DBCPConnectionFactory.class) {
            // Sync needed for MS SQL JDBC driver. See OFBIZ-5216.
            try {
                jdbcDriver = (Driver) Class.forName(driverName, true, Thread.currentThread().getContextClassLoader()).getConstructor().newInstance();
            } catch (Exception e) {
                Debug.logError(e, module);
                throw new GenericEntityException(e.getMessage(), e);
            }
        }

        // connection factory properties
        Properties cfProps = new Properties();
        cfProps.put("user", jdbcUsername);
        cfProps.put("password", jdbcPassword);

        // create the connection factory
        org.apache.commons.dbcp2.ConnectionFactory cf = new DriverConnectionFactory(jdbcDriver, jdbcUri, cfProps);

        // wrap it with a LocalXAConnectionFactory
        XAConnectionFactory xacf = new LocalXAConnectionFactory(txMgr, cf);

        // create the pool object factory
        PoolableConnectionFactory factory = new PoolableManagedConnectionFactory(xacf, null);
        factory.setValidationQuery(jdbcElement.getPoolJdbcTestStmt());
        factory.setDefaultReadOnly(false);
        factory.setRollbackOnReturn(false);
        factory.setEnableAutoCommitOnReturn(false);
        String transIso = jdbcElement.getIsolationLevel();
        if (!transIso.isEmpty()) {
            if ("Serializable".equals(transIso)) {
                factory.setDefaultTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE);
            } else if ("RepeatableRead".equals(transIso)) {
                factory.setDefaultTransactionIsolation(Connection.TRANSACTION_REPEATABLE_READ);
            } else if ("ReadUncommitted".equals(transIso)) {
                factory.setDefaultTransactionIsolation(Connection.TRANSACTION_READ_UNCOMMITTED);
            } else if ("ReadCommitted".equals(transIso)) {
                factory.setDefaultTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED);
            } else if ("None".equals(transIso)) {
                factory.setDefaultTransactionIsolation(Connection.TRANSACTION_NONE);
            }
        }

        // configure the pool settings
        GenericObjectPoolConfig<PoolableConnection> poolConfig = new GenericObjectPoolConfig<>();
        poolConfig.setMaxTotal(maxSize);
        // settings for idle connections
        poolConfig.setMaxIdle(maxIdle);
        poolConfig.setMinIdle(minSize);
        poolConfig.setTimeBetweenEvictionRunsMillis(jdbcElement.getTimeBetweenEvictionRunsMillis());
        poolConfig.setMinEvictableIdleTimeMillis(-1); // disabled in favour of setSoftMinEvictableIdleTimeMillis(...)
        poolConfig.setSoftMinEvictableIdleTimeMillis(jdbcElement.getSoftMinEvictableIdleTimeMillis());
        poolConfig.setNumTestsPerEvictionRun(maxSize); // test all the idle connections
        // settings for when the pool is exhausted
        poolConfig.setBlockWhenExhausted(true); // the thread requesting the connection waits if no connection is available
        poolConfig.setMaxWaitMillis(jdbcElement.getPoolSleeptime()); // throw an exception if, after getPoolSleeptime() ms, no connection is available for the requesting thread
        // settings for the execution of the validation query
        poolConfig.setTestOnCreate(jdbcElement.getTestOnCreate());
        poolConfig.setTestOnBorrow(jdbcElement.getTestOnBorrow());
        poolConfig.setTestOnReturn(jdbcElement.getTestOnReturn());
        poolConfig.setTestWhileIdle(jdbcElement.getTestWhileIdle());

        GenericObjectPool<PoolableConnection> pool = new GenericObjectPool<PoolableConnection>(factory, poolConfig);
        factory.setPool(pool);

        mds = new DebugManagedDataSource<>(pool, xacf.getTransactionRegistry());
        mds.setAccessToUnderlyingConnectionAllowed(true);

        // cache the pool
        DebugManagedDataSource<? extends Connection> prev = dsCache.putIfAbsent(cacheKey, mds);
        if (prev != null) {
            closeQuietly(cacheKey, mds); // SCIPIO: 4.0.0: another thread created the pool first; do not leak this one
            mds = prev;
        } else if (tenantPool) {
            startTenantPoolReaper();
        }

        return TransactionUtil.getCursorConnection(helperInfo, mds.getConnection());
    }

    private static void markTenantPoolUse(String cacheKey) {
        AtomicLong last = tenantPoolLastUse.get(cacheKey);
        if (last == null) {
            last = tenantPoolLastUse.computeIfAbsent(cacheKey, k -> new AtomicLong());
        }
        last.set(System.currentTimeMillis());
    }

    /**
     * SCIPIO: 4.0.0: Pooled runtime: closes the connection pools of one store (all its datasources), for example when
     * the store is suspended. The next connection request of the store creates a new pool. Returns the number of pools.
     */
    public static int closeTenantPools(String tenantId) {
        int closed = 0;
        String suffix = "#" + tenantId;
        for (String key : new ArrayList<>(dsCache.keySet())) {
            if (key.endsWith(suffix)) {
                DebugManagedDataSource<? extends Connection> mds = dsCache.remove(key);
                tenantPoolLastUse.remove(key);
                if (mds != null) {
                    scheduleClose(key, mds);
                    closed++;
                }
            }
        }
        return closed;
    }

    /** SCIPIO: 4.0.0: Pooled runtime: the number of open store pools and of all pools (for the samplers). */
    public static Map<String, Object> getPoolCounts() {
        int tenantPools = 0;
        for (String key : dsCache.keySet()) {
            if (key.indexOf('#') >= 0) {
                tenantPools++;
            }
        }
        Map<String, Object> counts = new HashMap<>();
        counts.put("pools", dsCache.size());
        counts.put("tenantPools", tenantPools);
        return counts;
    }

    private static void startTenantPoolReaper() {
        if (tenantPoolReaper != null) {
            return;
        }
        synchronized (DBCPConnectionFactory.class) {
            if (tenantPoolReaper != null) {
                return;
            }
            ScheduledExecutorService reaper = Executors.newSingleThreadScheduledExecutor(r -> {
                Thread t = new Thread(r, "Scipio-TenantPoolReaper");
                t.setDaemon(true);
                return t;
            });
            if (TENANT_POOL_IDLE_CLOSE_MS > 0) {
                long period = Math.max(10000L, Math.min(60000L, TENANT_POOL_IDLE_CLOSE_MS / 4));
                reaper.scheduleWithFixedDelay(DBCPConnectionFactory::closeIdleTenantPools, period, period, TimeUnit.MILLISECONDS);
            }
            tenantPoolReaper = reaper;
        }
    }

    /**
     * Closes the pools of stores without a borrow for tenant.pool.idleCloseSeconds, so that the number of pools follows
     * the active stores, not all stores (G12). A pool with a connection in use stays open.
     */
    private static void closeIdleTenantPools() {
        try {
            long limit = System.currentTimeMillis() - TENANT_POOL_IDLE_CLOSE_MS;
            for (Map.Entry<String, AtomicLong> entry : tenantPoolLastUse.entrySet()) {
                String key = entry.getKey();
                if (entry.getValue().get() >= limit) {
                    continue;
                }
                DebugManagedDataSource<? extends Connection> mds = dsCache.get(key);
                if (mds == null) {
                    tenantPoolLastUse.remove(key, entry.getValue());
                    continue;
                }
                if (mds.getNumActive() > 0) {
                    continue;
                }
                if (dsCache.remove(key, mds)) {
                    tenantPoolLastUse.remove(key, entry.getValue());
                    scheduleClose(key, mds);
                }
            }
        } catch (RuntimeException e) {
            Debug.logWarning(e, "Could not close idle store connection pools", module);
        }
    }

    /**
     * Closes a pool that is no longer in dsCache after a grace time: a thread that read the pool from dsCache just
     * before the removal still gets its connection. Connections returned after the close are destroyed.
     */
    private static void scheduleClose(String key, DebugManagedDataSource<? extends Connection> mds) {
        startTenantPoolReaper();
        tenantPoolReaper.schedule(() -> closeQuietly(key, mds), TENANT_POOL_CLOSE_GRACE_MS, TimeUnit.MILLISECONDS);
    }

    private static void closeQuietly(String key, DebugManagedDataSource<? extends Connection> mds) {
        try {
            mds.close();
            if (Debug.verboseOn()) {
                Debug.logVerbose("Closed connection pool " + key, module);
            }
        } catch (Exception e) {
            Debug.logWarning("Could not close connection pool " + key + ": " + e.getMessage(), module);
        }
    }

    public void closeAll() {
        // no methods on the pool to shutdown; so just clearing for GC
        dsCache.clear();
    }

    public static Map<String, Object> getDataSourceInfo(String helperName) {
        Map<String, Object> dataSourceInfo = new HashMap<String, Object>();
        DebugManagedDataSource<? extends Connection> mds = dsCache.get(helperName);
        if (mds != null) {
            dataSourceInfo = mds.getInfo();
        }
        return dataSourceInfo;
    }

}
