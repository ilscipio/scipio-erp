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
package org.ofbiz.common.tenant;

import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.regex.Pattern;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilIO;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.connection.DBCPConnectionFactory;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.Tenants;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.job.JobManager;
import org.ofbiz.service.job.TenantJobActivator;
import org.ofbiz.webapp.control.TenantResolver;

import com.ilscipio.scipio.service.def.Attribute;
import com.ilscipio.scipio.service.def.Service;

/**
 * SCIPIO: 4.0.0: Pooled runtime: the life cycle of a store (tenant): provision, suspend, resume, delete (G11, G18).
 *
 * <p>The services run on the base delegator only and need TENANT_ADMIN. A store database is a copy of a template
 * database ({@code CREATE DATABASE t_<id> TEMPLATE <template>}, PostgreSQL); the master rows (Tenant,
 * TenantDataSource, TenantDomainName) connect it. Settings: general.properties {@code tenant.provision.*}.</p>
 *
 * <p>Suspend and resume act at once in the JVM that runs the service (host cache, sessions, request slots, connection
 * pools). Other JVMs see the new state within {@code tenant.resolver.cacheTtlMs} (web requests) and
 * {@code tenant.info.cacheTtlMs} (jobs).</p>
 */
public class TenantServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final String LOCATION = "org.ofbiz.common.tenant.TenantServices";
    private static final String RES = "general";
    private static final Pattern TENANT_ID = Pattern.compile("[a-z][a-z0-9]{0,29}");
    private static final Pattern DB_NAME = Pattern.compile("[a-z][a-z0-9_]{0,62}");
    private static final Pattern HOST = Pattern.compile("(?=.{1,253}$)([a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?)(\\.[a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?)*");

    @Service(
        name = "provisionTenant",
        engine = "java", location = LOCATION, invoke = "provisionTenant",
        description = "Pooled runtime: creates a store: its database as a copy of the template database, then its master rows "
                + "(Tenant, TenantDataSource, TenantDomainName). Base delegator only; needs TENANT_ADMIN.",
        auth = "true", useTransaction = "false",
        attributes = {
            @Attribute(name = "tenantId", type = "String", mode = "IN", description = "Store id: a-z, 0-9, starts with a letter, at most 30 characters"),
            @Attribute(name = "tenantName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "domainNames", type = "String", mode = "IN", description = "Host names of the store, comma-separated"),
            @Attribute(name = "planId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateDatabase", type = "String", mode = "IN", optional = "true", description = "Default: tenant.provision.templateDatabase"),
            @Attribute(name = "activate", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "Create the store's delegator (and its jobs in a worker JVM) before the service returns"),
            @Attribute(name = "databaseName", type = "String", mode = "OUT"),
            @Attribute(name = "copyMs", type = "Long", mode = "OUT"),
            @Attribute(name = "durationMs", type = "Long", mode = "OUT")
        }
    )
    public interface ProvisionTenant {}

    @Service(
        name = "suspendTenant",
        engine = "java", location = LOCATION, invoke = "suspendTenant",
        description = "Pooled runtime: suspends a store: its hosts answer 503, its sessions end, its jobs stop, its connection pools close. "
                + "Base delegator only; needs TENANT_ADMIN.",
        auth = "true", useTransaction = "false",
        attributes = {
            @Attribute(name = "tenantId", type = "String", mode = "IN"),
            @Attribute(name = "reason", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sessionsEnded", type = "Integer", mode = "OUT"),
            @Attribute(name = "poolsClosed", type = "Integer", mode = "OUT"),
            @Attribute(name = "durationMs", type = "Long", mode = "OUT")
        }
    )
    public interface SuspendTenant {}

    @Service(
        name = "resumeTenant",
        engine = "java", location = LOCATION, invoke = "resumeTenant",
        description = "Pooled runtime: resumes a suspended store. Base delegator only; needs TENANT_ADMIN.",
        auth = "true", useTransaction = "false",
        attributes = {
            @Attribute(name = "tenantId", type = "String", mode = "IN"),
            @Attribute(name = "durationMs", type = "Long", mode = "OUT")
        }
    )
    public interface ResumeTenant {}

    @Service(
        name = "deleteTenant",
        engine = "java", location = LOCATION, invoke = "deleteTenant",
        description = "Pooled runtime: deletes a suspended store: its master rows and its database. Cannot be undone. "
                + "Base delegator only; needs TENANT_ADMIN.",
        auth = "true", useTransaction = "false",
        attributes = {
            @Attribute(name = "tenantId", type = "String", mode = "IN"),
            @Attribute(name = "dropDatabase", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "durationMs", type = "Long", mode = "OUT")
        }
    )
    public interface DeleteTenant {}

    public static Map<String, Object> provisionTenant(DispatchContext dctx, Map<String, ? extends Object> context) {
        long t0 = System.currentTimeMillis();
        Delegator delegator = dctx.getDelegator();
        String error = checkCaller(dctx, context);
        if (error != null) {
            return ServiceUtil.returnError(error);
        }
        String tenantId = (String) context.get("tenantId");
        if (tenantId == null || !TENANT_ID.matcher(tenantId).matches()) {
            return ServiceUtil.returnError("Bad store id [" + tenantId + "]: a-z and 0-9, starts with a letter, at most 30 characters");
        }
        // the store's files live in images/<tenantId>/: the id must not name a shared folder (images/products, ...)
        if (new java.io.File(org.ofbiz.entity.tenant.TenantFiles.getImagesRoot().toFile(), tenantId).exists()) {
            return ServiceUtil.returnError("Store id [" + tenantId + "] is the name of a folder of the images webapp");
        }
        Set<String> hosts = new LinkedHashSet<>();
        for (String host : StringUtil.split((String) context.get("domainNames"), ",")) {
            String h = host.trim().toLowerCase(Locale.ROOT);
            if (!HOST.matcher(h).matches() || TenantResolver.isBaseHost(h)) {
                return ServiceUtil.returnError("Bad host name [" + host + "]");
            }
            hosts.add(h);
        }
        if (hosts.isEmpty()) {
            return ServiceUtil.returnError("A store needs at least one host name");
        }
        String template = (String) context.get("templateDatabase");
        if (UtilValidate.isEmpty(template)) {
            template = UtilProperties.getPropertyValue(RES, "tenant.provision.templateDatabase", "commerce_seed");
        }
        String dbName = UtilProperties.getPropertyValue(RES, "tenant.provision.databaseName", "t_${tenantId}").replace("${tenantId}", tenantId);
        if (!DB_NAME.matcher(template).matches() || !DB_NAME.matcher(dbName).matches()) {
            return ServiceUtil.returnError("Bad database name [" + template + "] or [" + dbName + "]");
        }
        try {
            if (EntityQuery.use(delegator).from("Tenant").where("tenantId", tenantId).queryOne() != null) {
                return ServiceUtil.returnError("Store [" + tenantId + "] exists");
            }
            for (String host : hosts) {
                GenericValue domain = EntityQuery.use(delegator).from("TenantDomainName").where("domainName", host).queryOne();
                if (domain != null) {
                    return ServiceUtil.returnError("Host [" + host + "] belongs to store [" + domain.getString("tenantId") + "]");
                }
            }
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError("Could not check the master tables: " + e.getMessage());
        }

        // 1. the database: a copy of the template
        long copyMs;
        try (Connection admin = adminConnection(null); Statement st = admin.createStatement()) {
            long c0 = System.currentTimeMillis();
            String options = UtilProperties.getPropertyValue(RES, "tenant.provision.createOptions", "STRATEGY FILE_COPY");
            st.execute("CREATE DATABASE \"" + dbName + "\" TEMPLATE \"" + template + "\"" + (UtilValidate.isNotEmpty(options) ? " " + options : ""));
            copyMs = System.currentTimeMillis() - c0;
        } catch (SQLException e) {
            Debug.logError(e, "Tenant: could not create database " + dbName, module);
            return ServiceUtil.returnError("Could not create the database " + dbName + ": " + e.getMessage());
        }
        try {
            // 2. first state of the store database (job hygiene, own Solr core)
            runInitSql(dbName);
            // 3. the master rows, in one transaction
            String jdbcUri = UtilProperties.getPropertyValue(RES, "tenant.provision.jdbcUri",
                    "jdbc:postgresql://127.0.0.1:5432/${databaseName}").replace("${databaseName}", dbName);
            String tenantName = (String) context.get("tenantName");
            String planId = (String) context.get("planId");
            TransactionUtil.doNewTransaction(() -> {
                GenericValue tenant = delegator.makeValue("Tenant", UtilMisc.toMap("tenantId", tenantId,
                        "tenantName", UtilValidate.isNotEmpty(tenantName) ? tenantName : tenantId, "disabled", "N", "planId", planId));
                tenant.create();
                for (String group : StringUtil.split(UtilProperties.getPropertyValue(RES, "tenant.provision.entityGroups", "org.ofbiz,org.ofbiz.olap"), ",")) {
                    delegator.create("TenantDataSource", UtilMisc.toMap("tenantId", tenantId, "entityGroupName", group.trim(),
                            "jdbcUri", jdbcUri, "jdbcUsername", UtilProperties.getPropertyValue(RES, "tenant.provision.jdbcUsername", ""),
                            "jdbcPassword", UtilProperties.getPropertyValue(RES, "tenant.provision.jdbcPassword", "")));
                }
                for (String host : hosts) {
                    delegator.create("TenantDomainName", UtilMisc.toMap("tenantId", tenantId, "domainName", host));
                }
                return null;
            }, "Tenant: could not write the master rows of " + tenantId, 0, true);
        } catch (Exception e) {
            Debug.logError(e, "Tenant: provisioning of " + tenantId + " failed; dropping database " + dbName, module);
            dropDatabase(dbName);
            return ServiceUtil.returnError("Provisioning of store " + tenantId + " failed: " + e.getMessage());
        }
        TenantResolver.clearHostCache();
        Tenants.invalidate(tenantId);
        if (!Boolean.FALSE.equals(context.get("activate"))) {
            // the first request of the store finds its delegator ready; a worker JVM starts the store's jobs now
            TenantJobActivator.activate(tenantId);
        }
        long durationMs = System.currentTimeMillis() - t0;
        Debug.logInfo("Tenant: provisioned store " + tenantId + " (" + hosts + ", database " + dbName + ") in " + durationMs + " ms (copy " + copyMs + " ms)", module);
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("databaseName", dbName);
        result.put("copyMs", copyMs);
        result.put("durationMs", durationMs);
        return result;
    }

    public static Map<String, Object> suspendTenant(DispatchContext dctx, Map<String, ? extends Object> context) {
        long t0 = System.currentTimeMillis();
        String error = checkCaller(dctx, context);
        if (error != null) {
            return ServiceUtil.returnError(error);
        }
        String tenantId = (String) context.get("tenantId");
        String reason = (String) context.get("reason");
        error = setState(dctx.getDelegator(), tenantId, true, reason);
        if (error != null) {
            return ServiceUtil.returnError(error);
        }
        // at once in this JVM: 503 on the store hosts, sessions end, request slots free; the poller skips its jobs
        int sessions = TenantResolver.suspendLocal(tenantId);
        int pools = DBCPConnectionFactory.closeTenantPools(tenantId);
        long durationMs = System.currentTimeMillis() - t0;
        Debug.logInfo("Tenant: suspended store " + tenantId + " (" + reason + "): " + sessions + " sessions ended, " + pools
                + " pools closed, " + durationMs + " ms", module);
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("sessionsEnded", sessions);
        result.put("poolsClosed", pools);
        result.put("durationMs", durationMs);
        return result;
    }

    public static Map<String, Object> resumeTenant(DispatchContext dctx, Map<String, ? extends Object> context) {
        long t0 = System.currentTimeMillis();
        String error = checkCaller(dctx, context);
        if (error != null) {
            return ServiceUtil.returnError(error);
        }
        String tenantId = (String) context.get("tenantId");
        error = setState(dctx.getDelegator(), tenantId, false, null);
        if (error != null) {
            return ServiceUtil.returnError(error);
        }
        TenantResolver.resumeLocal(tenantId);
        TenantJobActivator.activate(tenantId);
        long durationMs = System.currentTimeMillis() - t0;
        Debug.logInfo("Tenant: resumed store " + tenantId + " in " + durationMs + " ms", module);
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("durationMs", durationMs);
        return result;
    }

    public static Map<String, Object> deleteTenant(DispatchContext dctx, Map<String, ? extends Object> context) {
        long t0 = System.currentTimeMillis();
        Delegator delegator = dctx.getDelegator();
        String error = checkCaller(dctx, context);
        if (error != null) {
            return ServiceUtil.returnError(error);
        }
        String tenantId = (String) context.get("tenantId");
        String dbName;
        try {
            GenericValue tenant = EntityQuery.use(delegator).from("Tenant").where("tenantId", tenantId).queryOne();
            if (tenant == null) {
                return ServiceUtil.returnError("No store [" + tenantId + "]");
            }
            if (!"Y".equals(tenant.getString("disabled"))) {
                return ServiceUtil.returnError("Suspend store [" + tenantId + "] before you delete it");
            }
            GenericValue ds = EntityQuery.use(delegator).from("TenantDataSource").where("tenantId", tenantId).queryFirst();
            dbName = (ds != null) ? databaseOf(ds.getString("jdbcUri")) : null;
            TransactionUtil.doNewTransaction(() -> {
                for (String entity : new String[] {"McpTokenRoute", "TenantDomainName", "TenantDataSource", "TenantComponent", "TenantKeyEncryptingKey"}) {
                    delegator.removeByAnd(entity, UtilMisc.toMap("tenantId", tenantId));
                }
                delegator.removeByAnd("Tenant", UtilMisc.toMap("tenantId", tenantId));
                return null;
            }, "Tenant: could not delete the master rows of " + tenantId, 0, true);
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError("Could not delete store " + tenantId + ": " + e.getMessage());
        }
        TenantResolver.suspendLocal(tenantId);
        DBCPConnectionFactory.closeTenantPools(tenantId);
        JobManager.unregister(delegator.getDelegatorBaseName() + "#" + tenantId);
        if (!Boolean.FALSE.equals(context.get("dropDatabase")) && dbName != null) {
            String dropError = dropDatabase(dbName);
            if (dropError != null) {
                return ServiceUtil.returnError("Master rows of store " + tenantId + " deleted, but not its database: " + dropError);
            }
        }
        long durationMs = System.currentTimeMillis() - t0;
        Debug.logInfo("Tenant: deleted store " + tenantId + " (database " + dbName + ") in " + durationMs + " ms", module);
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("durationMs", durationMs);
        return result;
    }

    /** Null when the caller may run a store life cycle service: base delegator and TENANT_ADMIN. */
    private static String checkCaller(DispatchContext dctx, Map<String, ? extends Object> context) {
        if (!Tenants.isPooled()) {
            return "The store life cycle services need multitenant=Y";
        }
        if (dctx.getDelegator().getDelegatorTenantId() != null) {
            return "The store life cycle services run on the base delegator only";
        }
        Security security = dctx.getSecurity();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (userLogin == null || !security.hasPermission("TENANT_ADMIN", userLogin)) {
            return "TENANT_ADMIN is required";
        }
        return null;
    }

    private static String setState(Delegator delegator, String tenantId, boolean suspend, String reason) {
        try {
            GenericValue tenant = EntityQuery.use(delegator).from("Tenant").where("tenantId", tenantId).queryOne();
            if (tenant == null) {
                return "No store [" + tenantId + "]";
            }
            tenant.set("disabled", suspend ? "Y" : "N");
            tenant.set("suspendedDate", suspend ? UtilDateTime.nowTimestamp() : null);
            if (suspend) {
                tenant.set("suspendReason", reason);
            }
            TransactionUtil.doNewTransaction(() -> {
                tenant.store();
                return null;
            }, "Tenant: could not change the state of " + tenantId, 0, true);
            return null;
        } catch (GenericEntityException e) {
            return "Could not change the state of store " + tenantId + ": " + e.getMessage();
        }
    }

    private static Connection adminConnection(String database) throws SQLException {
        String uri = UtilProperties.getPropertyValue(RES, "tenant.provision.adminJdbcUri", "jdbc:postgresql://127.0.0.1:5432/postgres");
        if (database != null) {
            uri = withDatabase(uri, database);
        }
        Properties props = new Properties();
        props.put("user", UtilProperties.getPropertyValue(RES, "tenant.provision.adminJdbcUsername", ""));
        props.put("password", UtilProperties.getPropertyValue(RES, "tenant.provision.adminJdbcPassword", ""));
        Connection connection = DriverManager.getConnection(uri, props);
        connection.setAutoCommit(true);
        return connection;
    }

    /** jdbc:postgresql://host:port/db?x -> jdbc:postgresql://host:port/database?x */
    static String withDatabase(String uri, String database) {
        int query = uri.indexOf('?');
        String base = (query >= 0) ? uri.substring(0, query) : uri;
        String params = (query >= 0) ? uri.substring(query) : "";
        int slash = base.lastIndexOf('/');
        return base.substring(0, slash + 1) + database + params;
    }

    /** The database name of a JDBC URI (the last path segment), or null. */
    static String databaseOf(String uri) {
        if (uri == null) {
            return null;
        }
        int query = uri.indexOf('?');
        String base = (query >= 0) ? uri.substring(0, query) : uri;
        String db = base.substring(base.lastIndexOf('/') + 1);
        return DB_NAME.matcher(db).matches() ? db : null;
    }

    private static void runInitSql(String dbName) throws Exception {
        String location = UtilProperties.getPropertyValue(RES, "tenant.provision.initSql", "component://common/config/tenant-init.sql");
        if (UtilValidate.isEmpty(location)) {
            return;
        }
        URL url = FlexibleLocation.resolveLocation(location);
        if (url == null) {
            throw new IllegalStateException("No file " + location);
        }
        String sql = UtilIO.readString(url.openStream(), StandardCharsets.UTF_8);
        try (Connection c = adminConnection(dbName); Statement st = c.createStatement()) {
            for (String statement : splitSql(sql)) {
                st.execute(statement);
            }
        }
    }

    /** Splits a script at the semicolons that end a line; drops comment lines and empty statements. */
    static List<String> splitSql(String sql) {
        List<String> statements = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        for (String line : sql.split("\r?\n")) {
            String trimmed = line.trim();
            if (trimmed.isEmpty() || trimmed.startsWith("--")) {
                continue;
            }
            current.append(line).append('\n');
            if (trimmed.endsWith(";")) {
                String statement = current.toString().trim();
                statements.add(statement.substring(0, statement.length() - 1));
                current.setLength(0);
            }
        }
        if (current.toString().trim().length() > 0) {
            statements.add(current.toString().trim());
        }
        return statements;
    }

    private static String dropDatabase(String dbName) {
        try (Connection admin = adminConnection(null); Statement st = admin.createStatement()) {
            st.execute("DROP DATABASE IF EXISTS \"" + dbName + "\" WITH (FORCE)");
            return null;
        } catch (SQLException e) {
            Debug.logError(e, "Tenant: could not drop database " + dbName, module);
            return e.getMessage();
        }
    }
}
