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
package com.ilscipio.scipio.entityext.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Services {

    @Service(
        name = "watchEntity",
        location = "org.ofbiz.entityext.EntityWatchServices",
        invoke = "watchEntity",
        attributes = {
            @Attribute(name = "newValue", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "fieldName", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface WatchEntity {}

    /**
     * Clear All Entity Engine Caches for all Servers listening to the topic
     */
    @Service(
        name = "distributedClearAllEntityCaches",
        engine = "jms",
        location = "serviceMessenger",
        invoke = "clearAllEntityCaches",
        description = "Clear All Entity Engine Caches for all Servers listening to the topic",
        auth = "true",
        useTransaction = "false"
    )
    public interface DistributedClearAllEntityCaches {}

    /**
     * Clears all values from all Entity Engine caches. By default does not distribute.
     */
    @Service(
        name = "clearAllEntityCaches",
        location = "org.ofbiz.entityext.cache.EntityCacheServices",
        invoke = "clearAllEntityCaches",
        description = "Clears all values from all Entity Engine caches. By default does not distribute.",
        auth = "true",
        export = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "distribute", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface ClearAllEntityCaches {}

    /**
     * Clear Cache Line by value for all Servers listening to the topic
     */
    @Service(
        name = "distributedClearCacheLineByValue",
        engine = "jms",
        location = "serviceMessenger",
        invoke = "clearCacheLineByValue",
        description = "Clear Cache Line by value for all Servers listening to the topic",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "value", type = "org.ofbiz.entity.GenericValue", mode = "IN")
        }
    )
    public interface DistributedClearCacheLineByValue {}

    /**
     * Clear Cache Line with a value (GenericValue); this is the preferred method since the all, by primary key and by and caches will be cleared. By default does not distribute.
     */
    @Service(
        name = "clearCacheLineByValue",
        location = "org.ofbiz.entityext.cache.EntityCacheServices",
        invoke = "clearCacheLine",
        description = "Clear Cache Line with a value (GenericValue); this is the preferred method since the all, by primary key and by and caches will be cleared. By default does not distribute.",
        auth = "true",
        export = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "value", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "distribute", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface ClearCacheLineByValue {}

    /**
     * Clear Cache Line by dummyPK for all Servers listening to the topic
     */
    @Service(
        name = "distributedClearCacheLineByDummyPK",
        engine = "jms",
        location = "serviceMessenger",
        invoke = "clearCacheLineByDummyPK",
        description = "Clear Cache Line by dummyPK for all Servers listening to the topic",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "dummyPK", type = "GenericEntity", mode = "IN")
        }
    )
    public interface DistributedClearCacheLineByDummyPK {}

    /**
     * Clear Cache Line with a dummyPK (GenericEntity); clears that all cache entry and depending on whether the dummyPK is a primaryKey or not this clears the by primary key cache or the specified entry in the by and cache. By default does not distribute.
     */
    @Service(
        name = "clearCacheLineByDummyPK",
        location = "org.ofbiz.entityext.cache.EntityCacheServices",
        invoke = "clearCacheLine",
        description = "Clear Cache Line with a dummyPK (GenericEntity); clears that all cache entry and depending on whether the dummyPK is a primaryKey or not this clears the by primary key cache or the specified entry in the by and cache. By default does not distribute.",
        auth = "true",
        export = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "dummyPK", type = "GenericEntity", mode = "IN"),
            @Attribute(name = "distribute", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface ClearCacheLineByDummyPK {}

    /**
     * Clear Cache Line by primaryKey for all Servers listening to the topic
     */
    @Service(
        name = "distributedClearCacheLineByPrimaryKey",
        engine = "jms",
        location = "serviceMessenger",
        invoke = "clearCacheLineByPrimaryKey",
        description = "Clear Cache Line by primaryKey for all Servers listening to the topic",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "primaryKey", type = "GenericPK", mode = "IN"),
            @Attribute(name = "distribute", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface DistributedClearCacheLineByPrimaryKey {}

    /**
     * Clear Cache Line with a primaryKey (GenericPK); clears the all and by primary key caches. By default does not distribute.
     */
    @Service(
        name = "clearCacheLineByPrimaryKey",
        location = "org.ofbiz.entityext.cache.EntityCacheServices",
        invoke = "clearCacheLine",
        description = "Clear Cache Line with a primaryKey (GenericPK); clears the all and by primary key caches. By default does not distribute.",
        auth = "true",
        export = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "primaryKey", type = "GenericPK", mode = "IN"),
            @Attribute(name = "distribute", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface ClearCacheLineByPrimaryKey {}

    /**
     * Clear Cache Line by condition for all Servers listening to the topic
     */
    @Service(
        name = "distributedClearCacheLineByCondition",
        engine = "jms",
        location = "serviceMessenger",
        invoke = "clearCacheLineByCondition",
        description = "Clear Cache Line by condition for all Servers listening to the topic",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "entityName", type = "String", mode = "IN"),
            @Attribute(name = "condition", type = "org.ofbiz.entity.condition.EntityCondition", mode = "IN"),
            @Attribute(name = "distribute", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface DistributedClearCacheLineByCondition {}

    /**
     * Clear Cache Line with a condition; By default does not distribute.
     */
    @Service(
        name = "clearCacheLineByCondition",
        location = "org.ofbiz.entityext.cache.EntityCacheServices",
        invoke = "clearCacheLine",
        description = "Clear Cache Line with a condition; By default does not distribute.",
        auth = "true",
        export = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "entityName", type = "String", mode = "IN"),
            @Attribute(name = "condition", type = "org.ofbiz.entity.condition.EntityCondition", mode = "IN"),
            @Attribute(name = "distribute", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface ClearCacheLineByCondition {}

    /**
     * Clear all util caches for all Servers listening to the topic (SCIPIO)
     */
    @Service(
        name = "distributedClearAllUtilCaches",
        engine = "jms",
        location = "serviceMessenger",
        invoke = "clearAllUtilCaches",
        description = "Clear all util caches for all Servers listening to the topic (SCIPIO)",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "excludeNames", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "excludePatterns", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "excludeTypes", type = "Object", mode = "IN", optional = "true", defaultValue = "system-essential", description = "Supported values:\n                none: force clear all;\n                system-essential (default): excludes service models and problematic caches")
        }
    )
    public interface DistributedClearAllUtilCaches {}

    /**
     * Clears all util caches, automatically includes entity caches (SCIPIO)
     */
    @Service(
        name = "clearAllUtilCaches",
        location = "org.ofbiz.entityext.cache.EntityCacheServices",
        invoke = "clearAllUtilCaches",
        description = "Clears all util caches, automatically includes entity caches (SCIPIO)",
        auth = "true",
        export = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "distribute", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "excludeNames", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "excludePatterns", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "excludeTypes", type = "Object", mode = "IN", optional = "true", defaultValue = "system-essential", description = "Supported values:\n                none: force clear all;\n                system-essential (default): excludes service models and problematic caches")
        }
    )
    public interface ClearAllUtilCaches {}

    /**
     * Counts key/value UtilCache entries by key pattern filter (slow operation)
     */
    @Service(
        name = "utilCacheFilterOpCommon",
        engine = "interface",
        description = "Counts key/value UtilCache entries by key pattern filter (slow operation)",
        attributes = {
            @Attribute(name = "cacheName", type = "String", mode = "IN", description = "The cache name"),
            @Attribute(name = "entryFilter", type = "org.ofbiz.base.util.cache.UtilCache$CacheEntryFilter", mode = "IN", optional = "true", description = "Filters keys by the provided filter matched against each entry (key, value)"),
            @Attribute(name = "keyPat", type = "Object", mode = "IN", optional = "true", description = "Filters keys by the provided regular expression (regex) pattern matched against each key,\n                as string or java.util.regex.Pattern. Uses java.util.regex.Matcher#matches()"),
            @Attribute(name = "keyPrefix", type = "String", mode = "IN", optional = "true", description = "Filters keys by the provided key prefix")
        }
    )
    public interface UtilCacheFilterOpCommon {}

    /**
     * Counts key/value UtilCache entries by key pattern filter (slow operation)
     */
    @Service(
        name = "utilCacheCountBy",
        location = "org.ofbiz.entityext.cache.EntityCacheServices$UtilCacheCountBy",
        invoke = "exec",
        description = "Counts key/value UtilCache entries by key pattern filter (slow operation)",
        auth = "true",
        export = "true",
        useTransaction = "false",
        implemented = {@Implements(service = "utilCacheFilterOpCommon")},
        attributes = {
            @Attribute(name = "count", type = "Long", mode = "OUT", optional = "true")
        }
    )
    public interface UtilCacheCountBy {}

    /**
     * Removes key/value UtilCache entries by key pattern filter (slow operation)
     */
    @Service(
        name = "utilCacheRemoveBy",
        location = "org.ofbiz.entityext.cache.EntityCacheServices$UtilCacheRemoveBy",
        invoke = "exec",
        description = "Removes key/value UtilCache entries by key pattern filter (slow operation)",
        auth = "true",
        export = "true",
        useTransaction = "false",
        implemented = {@Implements(service = "utilCacheFilterOpCommon")},
        attributes = {
            @Attribute(name = "allEntries", type = "Boolean", mode = "IN", optional = "true", description = "Removes all entries and resets counters"),
            @Attribute(name = "removed", type = "Long", mode = "OUT", optional = "true")
        }
    )
    public interface UtilCacheRemoveBy {}

    @Service(
        name = "localhostClearAllEntityCaches",
        engine = "http",
        location = "eedcc-test",
        invoke = "clearAllEntityCaches",
        implemented = {@Implements(service = "clearAllEntityCaches")}
    )
    public interface LocalhostClearAllEntityCaches {}

    @Service(
        name = "localhostClearCacheLineByValue",
        engine = "http",
        location = "eedcc-test",
        invoke = "clearCacheLineByValue",
        implemented = {@Implements(service = "clearCacheLineByValue")}
    )
    public interface LocalhostClearCacheLineByValue {}

    @Service(
        name = "localhostClearCacheLineByDummyPK",
        engine = "http",
        location = "eedcc-test",
        invoke = "clearCacheLineByDummyPK",
        implemented = {@Implements(service = "clearCacheLineByDummyPK")}
    )
    public interface LocalhostClearCacheLineByDummyPK {}

    @Service(
        name = "localhostClearCacheLineByPrimaryKey",
        engine = "http",
        location = "eedcc-test",
        invoke = "clearCacheLineByPrimaryKey",
        implemented = {@Implements(service = "clearCacheLineByPrimaryKey")}
    )
    public interface LocalhostClearCacheLineByPrimaryKey {}

    /**
     * Rebuilds all indexes/keys
     */
    @Service(
        name = "rebuildEntityIndexesAndKeys",
        location = "org.ofbiz.entityext.data.EntityDataServices",
        invoke = "rebuildAllIndexesAndKeys",
        description = "Rebuilds all indexes/keys",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "groupName", type = "String", mode = "IN"),
            @Attribute(name = "fixColSizes", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "messages", type = "List", mode = "OUT")
        },
        permissions = {@Permissions(joinType = "AND", permissions = {@Permission(permission = "ENTITY_MAINT")})}
    )
    public interface RebuildEntityIndexesAndKeys {}

    /**
     * Read a directory for .txt files with entity names; read each line as a record
     */
    @Service(
        name = "importEntityFileDirectory",
        location = "org.ofbiz.entityext.data.EntityDataServices",
        invoke = "importDelimitedFromDirectory",
        description = "Read a directory for .txt files with entity names; read each line as a record",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "rootDirectory", type = "String", mode = "IN"),
            @Attribute(name = "delimiter", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ImportEntityFileDirectory {}

    /**
     * Import delimited file
     */
    @Service(
        name = "importDelimitedEntityFile",
        location = "org.ofbiz.entityext.data.EntityDataServices",
        invoke = "importDelimitedFile",
        description = "Import delimited file",
        auth = "true",
        requireNewTransaction = "true",
        transactionTimeout = "14400",
        attributes = {
            @Attribute(name = "file", type = "java.io.File", mode = "IN"),
            @Attribute(name = "delimiter", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "records", type = "Integer", mode = "OUT")
        }
    )
    public interface ImportDelimitedEntityFile {}

    /**
     * Unwrap ByteWrapper Fields for the given entity and field
     */
    @Service(
        name = "unwrapByteWrappers",
        location = "org.ofbiz.entityext.data.EntityDataServices",
        invoke = "unwrapByteWrappers",
        description = "Unwrap ByteWrapper Fields for the given entity and field",
        auth = "true",
        requireNewTransaction = "true",
        transactionTimeout = "14400",
        attributes = {
            @Attribute(name = "entityName", type = "String", mode = "IN"),
            @Attribute(name = "fieldName", type = "String", mode = "IN")
        }
    )
    public interface UnwrapByteWrappers {}

    /**
     * Re-encrypt the private keys, encrypted in EntityKeyStore with oldKey, using the newKey.
     */
    @Service(
        name = "reencryptPrivateKeys",
        location = "org.ofbiz.entityext.data.EntityDataServices",
        invoke = "reencryptPrivateKeys",
        description = "Re-encrypt the private keys, encrypted in EntityKeyStore with oldKey, using the newKey.",
        auth = "true",
        transactionTimeout = "14400",
        attributes = {
            @Attribute(name = "oldKey", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "newKey", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ReencryptPrivateKeys {}

    /**
     * Re-encrypt all the encrypted fields in the data model.
     */
    @Service(
        name = "reencryptFields",
        location = "org.ofbiz.entityext.data.EntityDataServices",
        invoke = "reencryptFields",
        description = "Re-encrypt all the encrypted fields in the data model.",
        auth = "true",
        transactionTimeout = "14400",
        attributes = {
            @Attribute(name = "groupName", type = "String", mode = "IN", optional = "true", defaultValue = "org.ofbiz")
        }
    )
    public interface ReencryptFields {}

    /**
     * Create EntitySync
     */
    @Service(
        name = "createEntitySync",
        engine = "simple",
        location = "component://entityext/script/org/ofbiz/entityext/synchronization/EntitySyncServices.xml",
        invoke = "createEntitySync",
        description = "Create EntitySync",
        defaultEntityName = "EntitySync",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "entitySyncPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateEntitySync {}

    /**
     * Update EntitySync
     */
    @Service(
        name = "updateEntitySync",
        engine = "entity-auto",
        invoke = "update",
        description = "Update EntitySync",
        defaultEntityName = "EntitySync",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "entitySyncPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateEntitySync {}

    /**
     * Create EntitySyncInclude
     */
    @Service(
        name = "createEntitySyncInclude",
        engine = "simple",
        location = "component://entityext/script/org/ofbiz/entityext/synchronization/EntitySyncServices.xml",
        invoke = "createEntitySyncInclude",
        description = "Create EntitySyncInclude",
        defaultEntityName = "EntitySyncInclude",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "entitySyncPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "applEnumId", optional = "false")
        }
    )
    public interface CreateEntitySyncInclude {}

    /**
     * Update EntitySyncInclude
     */
    @Service(
        name = "updateEntitySyncInclude",
        engine = "entity-auto",
        invoke = "update",
        description = "Update EntitySyncInclude",
        defaultEntityName = "EntitySyncInclude",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "entitySyncPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateEntitySyncInclude {}

    /**
     * Delete EntitySyncInclude
     */
    @Service(
        name = "deleteEntitySyncInclude",
        engine = "simple",
        location = "component://entityext/script/org/ofbiz/entityext/synchronization/EntitySyncServices.xml",
        invoke = "deleteEntitySyncInclude",
        description = "Delete EntitySyncInclude",
        defaultEntityName = "EntitySyncInclude",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "entitySyncPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteEntitySyncInclude {}

    /**
     * Update EntitySync while Running
     */
    @Service(
        name = "updateEntitySyncRunning",
        engine = "entity-auto",
        invoke = "update",
        description = "Update EntitySync while Running",
        defaultEntityName = "EntitySync",
        auth = "true",
        requireNewTransaction = "true",
        implemented = {@Implements(service = "updateEntitySync")},
        permissionService = @PermissionService(service = "entitySyncPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateEntitySyncRunning {}

    /**
     * Create EntitySyncHistory
     */
    @Service(
        name = "createEntitySyncHistory",
        engine = "simple",
        location = "component://entityext/script/org/ofbiz/entityext/synchronization/EntitySyncServices.xml",
        invoke = "createEntitySyncHistory",
        description = "Create EntitySyncHistory",
        defaultEntityName = "EntitySyncHistory",
        auth = "true",
        requireNewTransaction = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "entitySyncPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "startDate", mode = "OUT")
        }
    )
    public interface CreateEntitySyncHistory {}

    /**
     * Update EntitySyncHistory
     */
    @Service(
        name = "updateEntitySyncHistory",
        engine = "entity-auto",
        invoke = "update",
        description = "Update EntitySyncHistory",
        defaultEntityName = "EntitySyncHistory",
        auth = "true",
        requireNewTransaction = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "entitySyncPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateEntitySyncHistory {}

    /**
     * Delete EntitySyncHistory
     */
    @Service(
        name = "deleteEntitySyncHistory",
        engine = "simple",
        location = "component://entityext/script/org/ofbiz/entityext/synchronization/EntitySyncServices.xml",
        invoke = "deleteEntitySyncHistory",
        description = "Delete EntitySyncHistory",
        defaultEntityName = "EntitySyncHistory",
        auth = "true",
        requireNewTransaction = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "entitySyncPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteEntitySyncHistory {}

    /**
     * Not implemented.
     */
    @Service(
        name = "updateOfflineEntitySync",
        location = "org.ofbiz.entityext.synchronization.EntitySyncServices",
        invoke = "updateOfflineEntitySync",
        description = "Not implemented.",
        auth = "true"
    )
    public interface UpdateOfflineEntitySync {}

    /**
     * Clean EntitySyncRemove Info - Generally should be run asynchronously after each sync run, or periodically run on a schedule
     */
    @Service(
        name = "cleanSyncRemoveInfo",
        location = "org.ofbiz.entityext.synchronization.EntitySyncServices",
        invoke = "cleanSyncRemoveInfo",
        description = "Clean EntitySyncRemove Info - Generally should be run asynchronously after each sync run, or periodically run on a schedule",
        auth = "true",
        transactionTimeout = "600"
    )
    public interface CleanSyncRemoveInfo {}

    /**
     * Generally run manually to reset the status of an EntitySync when it has "crashed". Update a EntitySync, set the Status to ESR_NOT_STARTED, but ONLY if running (ie in ESR_RUNNING)
     */
    @Service(
        name = "resetEntitySyncStatusToNotStarted",
        engine = "simple",
        location = "component://entityext/script/org/ofbiz/entityext/synchronization/EntitySyncServices.xml",
        invoke = "resetEntitySyncStatusToNotStarted",
        description = "Generally run manually to reset the status of an EntitySync when it has \"crashed\". Update a EntitySync, set the Status to ESR_NOT_STARTED, but ONLY if running (ie in ESR_RUNNING)",
        auth = "true",
        transactionTimeout = "600",
        attributes = {
            @Attribute(name = "entitySyncId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "entitySyncPermissionCheck", mainAction = "UPDATE")
    )
    public interface ResetEntitySyncStatusToNotStarted {}

    @Service(
        name = "runOfflineEntitySync",
        location = "org.ofbiz.entityext.synchronization.EntitySyncServices",
        invoke = "runOfflineEntitySync",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "entitySyncId", type = "String", mode = "IN"),
            @Attribute(name = "fileName", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface RunOfflineEntitySync {}

    @Service(
        name = "loadOfflineEntitySyncData",
        location = "org.ofbiz.entityext.synchronization.EntitySyncServices",
        invoke = "loadOfflineSyncData",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "xmlFileName", type = "String", mode = "IN")
        }
    )
    public interface LoadOfflineEntitySyncData {}

    /**
     * Run Entity Sync
     */
    @Service(
        name = "runEntitySync",
        location = "org.ofbiz.entityext.synchronization.EntitySyncServices",
        invoke = "runEntitySync",
        description = "Run Entity Sync",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "entitySyncId", type = "String", mode = "IN")
        }
    )
    public interface RunEntitySync {}

    /**
     * Run Entity Sync
     */
    @Service(
        name = "storeEntitySyncData",
        location = "org.ofbiz.entityext.synchronization.EntitySyncServices",
        invoke = "storeEntitySyncData",
        description = "Run Entity Sync",
        auth = "true",
        export = "true",
        requireNewTransaction = "true",
        transactionTimeout = "900",
        attributes = {
            @Attribute(name = "entitySyncId", type = "String", mode = "IN"),
            @Attribute(name = "valuesToCreate", type = "List", mode = "IN"),
            @Attribute(name = "valuesToStore", type = "List", mode = "IN"),
            @Attribute(name = "keysToRemove", type = "List", mode = "IN"),
            @Attribute(name = "delegatorName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "toCreateInserted", type = "Long", mode = "OUT"),
            @Attribute(name = "toCreateUpdated", type = "Long", mode = "OUT"),
            @Attribute(name = "toCreateNotUpdated", type = "Long", mode = "OUT"),
            @Attribute(name = "toStoreInserted", type = "Long", mode = "OUT"),
            @Attribute(name = "toStoreUpdated", type = "Long", mode = "OUT"),
            @Attribute(name = "toStoreNotUpdated", type = "Long", mode = "OUT"),
            @Attribute(name = "toRemoveDeleted", type = "Long", mode = "OUT"),
            @Attribute(name = "toRemoveAlreadyDeleted", type = "Long", mode = "OUT")
        }
    )
    public interface StoreEntitySyncData {}

    /**
     * Run Entity Sync Pulling Data From a Remote Server
     */
    @Service(
        name = "runPullEntitySync",
        location = "org.ofbiz.entityext.synchronization.EntitySyncServices",
        invoke = "runPullEntitySync",
        description = "Run Entity Sync Pulling Data From a Remote Server",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "entitySyncId", type = "String", mode = "IN"),
            @Attribute(name = "remotePullAndReportEntitySyncDataName", type = "String", mode = "IN"),
            @Attribute(name = "localDelegatorName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "remoteDelegatorName", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface RunPullEntitySync {}

    /**
     * Pull And Report Entity Sync Data
     */
    @Service(
        name = "pullAndReportEntitySyncData",
        location = "org.ofbiz.entityext.synchronization.EntitySyncServices",
        invoke = "pullAndReportEntitySyncData",
        description = "Pull And Report Entity Sync Data",
        auth = "true",
        export = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "entitySyncId", type = "String", mode = "IN"),
            @Attribute(name = "delegatorName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "valuesToCreate", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "valuesToStore", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "keysToRemove", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "startDate", type = "Timestamp", mode = "INOUT", optional = "true"),
            @Attribute(name = "toCreateInserted", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "toCreateUpdated", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "toCreateNotUpdated", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "toStoreInserted", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "toStoreUpdated", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "toStoreNotUpdated", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "toRemoveDeleted", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "toRemoveAlreadyDeleted", type = "Long", mode = "IN", optional = "true")
        }
    )
    public interface PullAndReportEntitySyncData {}

    /**
     * Remotely Store Entity Sync Date
     */
    @Service(
        name = "remoteStoreEntitySyncDataHttp",
        engine = "http",
        location = "entity-sync-http",
        invoke = "storeEntitySyncData",
        description = "Remotely Store Entity Sync Date",
        auth = "true",
        useTransaction = "false",
        implemented = {@Implements(service = "storeEntitySyncData")}
    )
    public interface RemoteStoreEntitySyncDataHttp {}

    /**
     * Remotely Store Entity Sync Data
     */
    @Service(
        name = "remoteStoreEntitySyncDataRmi",
        engine = "rmi",
        location = "entity-sync-rmi",
        invoke = "storeEntitySyncData",
        description = "Remotely Store Entity Sync Data",
        auth = "true",
        useTransaction = "false",
        implemented = {@Implements(service = "storeEntitySyncData")}
    )
    public interface RemoteStoreEntitySyncDataRmi {}

    /**
     * Remotely Pull And Report Entity Sync Data
     */
    @Service(
        name = "remotePullAndReportEntitySyncDataHttp",
        engine = "http",
        location = "entity-sync-http",
        invoke = "pullAndReportEntitySyncData",
        description = "Remotely Pull And Report Entity Sync Data",
        auth = "true",
        useTransaction = "false",
        implemented = {@Implements(service = "pullAndReportEntitySyncData")}
    )
    public interface RemotePullAndReportEntitySyncDataHttp {}

    /**
     * Remotely Pull And Report Entity Sync Data
     */
    @Service(
        name = "remotePullAndReportEntitySyncDataRmi",
        engine = "rmi",
        location = "entity-sync-rmi",
        invoke = "pullAndReportEntitySyncData",
        description = "Remotely Pull And Report Entity Sync Data",
        auth = "true",
        useTransaction = "false",
        implemented = {@Implements(service = "pullAndReportEntitySyncData")}
    )
    public interface RemotePullAndReportEntitySyncDataRmi {}

    /**
     * Create a TestingSubtype record
     */
    @Service(
        name = "createTestingSubtype",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a TestingSubtype record",
        defaultEntityName = "TestingSubtype",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateTestingSubtype {}

    /**
     * Update a TestingSubtype record
     */
    @Service(
        name = "updateTestingSubtype",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a TestingSubtype record",
        defaultEntityName = "TestingSubtype",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateTestingSubtype {}

    /**
     * Delete a TestingSubtype record
     */
    @Service(
        name = "deleteTestingSubtype",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a TestingSubtype record",
        defaultEntityName = "TestingSubtype",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteTestingSubtype {}

    /**
     * Create a TestingType record
     */
    @Service(
        name = "createTestingType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a TestingType record",
        defaultEntityName = "TestingType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateTestingType {}

    /**
     * Update a TestingType record
     */
    @Service(
        name = "updateTestingType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a TestingType record",
        defaultEntityName = "TestingType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateTestingType {}

    /**
     * Delete a TestingType record
     */
    @Service(
        name = "deleteTestingType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a TestingType record",
        defaultEntityName = "TestingType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteTestingType {}

    /**
     * Create a UserAgentType record
     */
    @Service(
        name = "createUserAgentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a UserAgentType record",
        defaultEntityName = "UserAgentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateUserAgentType {}

    /**
     * Update a UserAgentType record
     */
    @Service(
        name = "updateUserAgentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a UserAgentType record",
        defaultEntityName = "UserAgentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateUserAgentType {}

    /**
     * Delete a UserAgentType record
     */
    @Service(
        name = "deleteUserAgentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a UserAgentType record",
        defaultEntityName = "UserAgentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteUserAgentType {}

    /**
     * Create a UserAgentMethodType record
     */
    @Service(
        name = "createUserAgentMethodType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a UserAgentMethodType record",
        defaultEntityName = "UserAgentMethodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateUserAgentMethodType {}

    /**
     * Update a UserAgentMethodType record
     */
    @Service(
        name = "updateUserAgentMethodType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a UserAgentMethodType record",
        defaultEntityName = "UserAgentMethodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateUserAgentMethodType {}

    /**
     * Delete a UserAgentMethodType record
     */
    @Service(
        name = "deleteUserAgentMethodType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a UserAgentMethodType record",
        defaultEntityName = "UserAgentMethodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteUserAgentMethodType {}

    /**
     * Create a BrowserType
     */
    @Service(
        name = "createBrowserType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a BrowserType",
        defaultEntityName = "BrowserType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateBrowserType {}

    /**
     * Update a BrowserType
     */
    @Service(
        name = "updateBrowserType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a BrowserType",
        defaultEntityName = "BrowserType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBrowserType {}

    /**
     * Delete a BrowserType
     */
    @Service(
        name = "deleteBrowserType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a BrowserType",
        defaultEntityName = "BrowserType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteBrowserType {}

    /**
     * Create a PlatformType
     */
    @Service(
        name = "createPlatformType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PlatformType",
        defaultEntityName = "PlatformType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreatePlatformType {}

    /**
     * Update a PlatformType
     */
    @Service(
        name = "updatePlatformType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PlatformType",
        defaultEntityName = "PlatformType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePlatformType {}

    /**
     * Delete a PlatformType
     */
    @Service(
        name = "deletePlatformType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PlatformType",
        defaultEntityName = "PlatformType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePlatformType {}

    /**
     * Create a ProtocolType
     */
    @Service(
        name = "createProtocolType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProtocolType",
        defaultEntityName = "ProtocolType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateProtocolType {}

    /**
     * Update a ProtocolType
     */
    @Service(
        name = "updateProtocolType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProtocolType",
        defaultEntityName = "ProtocolType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProtocolType {}

    /**
     * Delete a ProtocolType
     */
    @Service(
        name = "deleteProtocolType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProtocolType",
        defaultEntityName = "ProtocolType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProtocolType {}

    /**
     * Create a ServerHitType
     */
    @Service(
        name = "createServerHitType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ServerHitType",
        defaultEntityName = "ServerHitType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateServerHitType {}

    /**
     * Update a ServerHitType
     */
    @Service(
        name = "updateServerHitType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ServerHitType",
        defaultEntityName = "ServerHitType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateServerHitType {}

    /**
     * Delete a ServerHitType
     */
    @Service(
        name = "deleteServerHitType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ServerHitType",
        defaultEntityName = "ServerHitType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteServerHitType {}

    /**
     * Entity sync permission Checking Logic
     */
    @Service(
        name = "entitySyncPermissionCheck",
        engine = "simple",
        location = "component://entityext/script/org/ofbiz/entityext/synchronization/EntitySyncServices.xml",
        invoke = "entitySyncPermissionCheck",
        description = "Entity sync permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface EntitySyncPermissionCheck {}

    /**
     * mysql timestamp Field migration service,             it will generate sql file with alter query statement to update the datatype of timestamp field to support Fractional Seconds in Time Values             mySql 5.6.4 added support for Fractional Seconds in Time Values. 
     */
    @Service(
        name = "generateMySqlFileWithAlterTableForTimestamps",
        location = "org.ofbiz.entityext.data.UpgradeServices",
        invoke = "generateMySqlFileWithAlterTableForTimestamps",
        description = "mysql timestamp Field migration service,\n            it will generate sql file with alter query statement to update the datatype of timestamp field to support Fractional Seconds in Time Values\n            mySql 5.6.4 added support for Fractional Seconds in Time Values. ",
        auth = "true",
        transactionTimeout = "14400",
        attributes = {
            @Attribute(name = "groupName", type = "String", mode = "IN", optional = "true", defaultValue = "org.ofbiz")
        }
    )
    public interface GenerateMySqlFileWithAlterTableForTimestamps {}

}
