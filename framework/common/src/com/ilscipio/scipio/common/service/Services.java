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
package com.ilscipio.scipio.common.service;

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
        name = "commonGenericPermission",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "commonGenericPermission",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface CommonGenericPermission {}

    /**
     * Returns all CRUD and View Permissions
     */
    @Service(
        name = "commonGetAllCrudPermissions",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml",
        invoke = "getAllCrudPermissions",
        description = "Returns all CRUD and View Permissions",
        attributes = {
            @Attribute(name = "primaryPermission", type = "String", mode = "IN"),
            @Attribute(name = "altPermission", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "hasCreatePermission", type = "Boolean", mode = "OUT"),
            @Attribute(name = "hasUpdatePermission", type = "Boolean", mode = "OUT"),
            @Attribute(name = "hasDeletePermission", type = "Boolean", mode = "OUT"),
            @Attribute(name = "hasViewPermission", type = "Boolean", mode = "OUT")
        }
    )
    public interface CommonGetAllCrudPermissions {}

    /**
     * Echos back all passed parameters
     */
    @Service(
        name = "echoService",
        location = "org.ofbiz.common.CommonServices$EchoService",
        invoke = "exec",
        description = "Echos back all passed parameters",
        validate = "false"
    )
    public interface EchoService {}

    /**
     * Always returns error
     */
    @Service(
        name = "returnErrorService",
        location = "org.ofbiz.common.CommonServices",
        invoke = "returnErrorService",
        description = "Always returns error",
        validate = "false"
    )
    public interface ReturnErrorService {}

    /**
     * Logs all passed parameters
     */
    @Service(
        name = "logAllService",
        location = "org.ofbiz.common.CommonServices$LogAllService",
        invoke = "exec",
        description = "Logs all passed parameters",
        validate = "false"
    )
    public interface LogAllService {}

    /**
     * Sleeps for specified number of milliseconds (SCIPIO)
     */
    @Service(
        name = "sleepService",
        location = "org.ofbiz.common.CommonServices$SleepService",
        invoke = "exec",
        description = "Sleeps for specified number of milliseconds (SCIPIO)",
        validate = "false",
        attributes = {
            @Attribute(name = "timeMs", type = "Object", mode = "IN")
        }
    )
    public interface SleepService {}

    /**
     * Force the JVM to run the GC
     */
    @Service(
        name = "forceGarbageCollection",
        location = "org.ofbiz.common.CommonServices",
        invoke = "forceGc",
        description = "Force the JVM to run the GC",
        permissions = {@Permissions(joinType = "AND", permissions = {@Permission(permission = "UTIL_CACHE_EDIT")})}
    )
    public interface ForceGarbageCollection {}

    /**
     * Create a new note record
     */
    @Service(
        name = "createNote",
        location = "org.ofbiz.common.CommonServices",
        invoke = "createNote",
        description = "Create a new note record",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "noteName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "note", type = "String", mode = "IN", allowHtml = "any"),
            @Attribute(name = "noteId", type = "String", mode = "OUT")
        }
    )
    public interface CreateNote {}

    /**
     * Update a note record
     */
    @Service(
        name = "updateNote",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "updateNote",
        description = "Update a note record",
        defaultEntityName = "NoteData",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "noteInfo", allowHtml = "any")
        }
    )
    public interface UpdateNote {}

    /**
     * Sets/Updates cached debugging levels
     */
    @Service(
        name = "adjustDebugLevels",
        location = "org.ofbiz.common.CommonServices",
        invoke = "adjustDebugLevels",
        description = "Sets/Updates cached debugging levels",
        auth = "true",
        attributes = {
            @Attribute(name = "fatal", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "error", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "warning", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "important", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "info", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "timing", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "verbose", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface AdjustDebugLevels {}

    @Service(
        name = "displayXaDebugInfo",
        location = "org.ofbiz.common.CommonServices",
        invoke = "displayXaDebugInfo",
        auth = "true",
        permissions = {@Permissions(joinType = "AND", permissions = {@Permission(permission = "SERVICE_INVOKE_ANY")})}
    )
    public interface DisplayXaDebugInfo {}

    /**
     * Create a Enumeration
     */
    @Service(
        name = "createEnumeration",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/EnumerationServices.xml",
        invoke = "createEnumeration",
        description = "Create a Enumeration",
        defaultEntityName = "Enumeration",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "enumTypeId", optional = "false"),
            @OverrideAttribute(name = "description", optional = "false", allowHtml = "any")
        }
    )
    public interface CreateEnumeration {}

    /**
     * Update a Enumeration
     */
    @Service(
        name = "updateEnumeration",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/EnumerationServices.xml",
        invoke = "updateEnumeration",
        description = "Update a Enumeration",
        defaultEntityName = "Enumeration",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "enumTypeId", optional = "false"),
            @OverrideAttribute(name = "description", optional = "false", allowHtml = "any")
        }
    )
    public interface UpdateEnumeration {}

    /**
     * Delete a Enumeration
     */
    @Service(
        name = "deleteEnumeration",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Enumeration",
        defaultEntityName = "Enumeration",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteEnumeration {}

    @Service(
        name = "interfaceDataSource",
        engine = "interface",
        attributes = {
            @Attribute(name = "dataSourceId", type = "String", mode = "IN"),
            @Attribute(name = "dataSourceTypeId", type = "String", mode = "IN"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface InterfaceDataSource {}

    /**
     * Create a DataSource record
     */
    @Service(
        name = "createDataSource",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/datasource/DataSourceServices.xml",
        invoke = "createDataSource",
        description = "Create a DataSource record",
        auth = "true",
        implemented = {@Implements(service = "interfaceDataSource")},
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "CREATE")
    )
    public interface CreateDataSource {}

    /**
     * Update a DataSource record
     */
    @Service(
        name = "updateDataSource",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/datasource/DataSourceServices.xml",
        invoke = "updateDataSource",
        description = "Update a DataSource record",
        auth = "true",
        implemented = {@Implements(service = "interfaceDataSource")},
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateDataSource {}

    /**
     * Delete a DataSource record
     */
    @Service(
        name = "deleteDataSource",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/datasource/DataSourceServices.xml",
        invoke = "deleteDataSource",
        description = "Delete a DataSource record",
        auth = "true",
        attributes = {
            @Attribute(name = "dataSourceId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteDataSource {}

    @Service(
        name = "interfaceDataSourceType",
        engine = "interface",
        attributes = {
            @Attribute(name = "dataSourceTypeId", type = "String", mode = "IN"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface InterfaceDataSourceType {}

    /**
     * Create a DataSourceType record
     */
    @Service(
        name = "createDataSourceType",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/datasource/DataSourceTypeServices.xml",
        invoke = "createDataSourceType",
        description = "Create a DataSourceType record",
        auth = "true",
        implemented = {@Implements(service = "interfaceDataSourceType")},
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "CREATE")
    )
    public interface CreateDataSourceType {}

    /**
     * Update a DataSourceType record
     */
    @Service(
        name = "updateDataSourceType",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/datasource/DataSourceTypeServices.xml",
        invoke = "updateDataSourceType",
        description = "Update a DataSourceType record",
        auth = "true",
        implemented = {@Implements(service = "interfaceDataSourceType")},
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateDataSourceType {}

    /**
     * Delete a DataSourceType record
     */
    @Service(
        name = "deleteDataSourceType",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/datasource/DataSourceTypeServices.xml",
        invoke = "deleteDataSourceType",
        description = "Delete a DataSourceType record",
        auth = "true",
        attributes = {
            @Attribute(name = "dataSourceTypeId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteDataSourceType {}

    /**
     * Create a CustomTimePeriod record
     */
    @Service(
        name = "createCustomTimePeriod",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/period/PeriodServices.xml",
        invoke = "createCustomTimePeriod",
        description = "Create a CustomTimePeriod record",
        defaultEntityName = "CustomTimePeriod",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "false"),
            @OverrideAttribute(name = "thruDate", optional = "false"),
            @OverrideAttribute(name = "periodTypeId", optional = "false")
        }
    )
    public interface CreateCustomTimePeriod {}

    /**
     * Update a CustomTimePeriod record
     */
    @Service(
        name = "updateCustomTimePeriod",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/period/PeriodServices.xml",
        invoke = "updateCustomTimePeriod",
        description = "Update a CustomTimePeriod record",
        defaultEntityName = "CustomTimePeriod",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCustomTimePeriod {}

    /**
     * Delete a CustomTimePeriod record
     */
    @Service(
        name = "deleteCustomTimePeriod",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/period/PeriodServices.xml",
        invoke = "deleteCustomTimePeriod",
        description = "Delete a CustomTimePeriod record",
        defaultEntityName = "CustomTimePeriod",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCustomTimePeriod {}

    /**
     * Gets all StatusItem entries for the supplied StatusTypeId's
     */
    @Service(
        name = "getStatusItems",
        location = "org.ofbiz.common.status.StatusServices",
        invoke = "getStatusItems",
        description = "Gets all StatusItem entries for the supplied StatusTypeId's",
        attributes = {
            @Attribute(name = "statusTypeIds", type = "List", mode = "IN"),
            @Attribute(name = "statusItems", type = "List", mode = "OUT")
        }
    )
    public interface GetStatusItems {}

    /**
     * Gets all StatusValidChangeToDetails entries for the supplied statusId
     */
    @Service(
        name = "getStatusValidChangeToDetails",
        location = "org.ofbiz.common.status.StatusServices",
        invoke = "getStatusValidChangeToDetails",
        description = "Gets all StatusValidChangeToDetails entries for the supplied statusId",
        attributes = {
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "statusValidChangeToDetails", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface GetStatusValidChangeToDetails {}

    /**
     * Generic service to return a entity conditions
     */
    @Service(
        name = "prepareFind",
        location = "org.ofbiz.common.FindServices",
        invoke = "prepareFind",
        description = "Generic service to return a entity conditions",
        attributes = {
            @Attribute(name = "entityName", type = "String", mode = "IN"),
            @Attribute(name = "inputFields", type = "java.util.Map", mode = "IN"),
            @Attribute(name = "orderBy", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "noConditionFind", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filterByDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filterByDateValue", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "fromDateName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "thruDateName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "queryString", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "queryStringMap", type = "java.util.Map", mode = "OUT", optional = "true"),
            @Attribute(name = "orderByList", type = "java.util.List", mode = "OUT", optional = "true"),
            @Attribute(name = "entityConditionList", type = "org.ofbiz.entity.condition.EntityConditionList", mode = "OUT", optional = "true")
        }
    )
    public interface PrepareFind {}

    /**
     * Generic service to return an entity iterator
     */
    @Service(
        name = "executeFind",
        location = "org.ofbiz.common.FindServices",
        invoke = "executeFind",
        description = "Generic service to return an entity iterator",
        attributes = {
            @Attribute(name = "entityName", type = "String", mode = "IN"),
            @Attribute(name = "fieldList", type = "java.util.List", mode = "IN", optional = "true"),
            @Attribute(name = "orderByList", type = "java.util.List", mode = "IN", optional = "true"),
            @Attribute(name = "maxRows", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "entityConditionList", type = "org.ofbiz.entity.condition.EntityConditionList", mode = "IN", optional = "true"),
            @Attribute(name = "noConditionFind", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "distinct", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "listIt", type = "org.ofbiz.entity.util.EntityListIterator", mode = "OUT", optional = "true"),
            @Attribute(name = "listSize", type = "Integer", mode = "OUT", optional = "true")
        }
    )
    public interface ExecuteFind {}

    /**
     * Generic service to return an entity iterator.  set filterByDate to Y to exclude expired records.            set noConditionFind to Y to find without conditions.  
     */
    @Service(
        name = "performFind",
        location = "org.ofbiz.common.FindServices",
        invoke = "performFind",
        description = "Generic service to return an entity iterator.  set filterByDate to Y to exclude expired records.\n           set noConditionFind to Y to find without conditions.  ",
        attributes = {
            @Attribute(name = "entityName", type = "String", mode = "IN"),
            @Attribute(name = "inputFields", type = "java.util.Map", mode = "IN"),
            @Attribute(name = "fieldList", type = "java.util.List", mode = "IN", optional = "true"),
            @Attribute(name = "orderBy", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "noConditionFind", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "distinct", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filterByDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filterByDateValue", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "fromDateName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "thruDateName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "viewIndex", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "viewSize", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "listIt", type = "org.ofbiz.entity.util.EntityListIterator", mode = "OUT", optional = "true"),
            @Attribute(name = "listSize", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "queryString", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "queryStringMap", type = "java.util.Map", mode = "OUT", optional = "true")
        }
    )
    public interface PerformFind {}

    /**
     * Generic service to return an partial list.  set filterByDate to Y to exclude expired records.             set noConditionFind to Y to find without conditions.              SCIPIO: WARN: 2018-09-10: Some paging issues exist with this service, recommend             using performFind until resolved properly.
     */
    @Service(
        name = "performFindList",
        location = "org.ofbiz.common.FindServices",
        invoke = "performFindList",
        description = "Generic service to return an partial list.  set filterByDate to Y to exclude expired records.\n            set noConditionFind to Y to find without conditions. \n            SCIPIO: WARN: 2018-09-10: Some paging issues exist with this service, recommend\n            using performFind until resolved properly.",
        attributes = {
            @Attribute(name = "entityName", type = "String", mode = "IN"),
            @Attribute(name = "inputFields", type = "java.util.Map", mode = "IN"),
            @Attribute(name = "orderBy", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "noConditionFind", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filterByDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filterByDateValue", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "viewIndex", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "viewSize", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "list", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "listSize", type = "Integer", mode = "OUT"),
            @Attribute(name = "queryString", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "queryStringMap", type = "java.util.Map", mode = "OUT", optional = "true")
        }
    )
    public interface PerformFindList {}

    /**
     * Generic service to return an single GenericValue.  set filterByDate to Y to exclude expired records.
     */
    @Service(
        name = "performFindItem",
        location = "org.ofbiz.common.FindServices",
        invoke = "performFindItem",
        description = "Generic service to return an single GenericValue.  set filterByDate to Y to exclude expired records.",
        attributes = {
            @Attribute(name = "entityName", type = "String", mode = "IN"),
            @Attribute(name = "inputFields", type = "java.util.Map", mode = "IN"),
            @Attribute(name = "orderBy", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filterByDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filterByDateValue", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "item", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "queryString", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "queryStringMap", type = "java.util.Map", mode = "OUT", optional = "true")
        }
    )
    public interface PerformFindItem {}

    /**
     * Create a Keyword Thesaurus
     */
    @Service(
        name = "createKeywordThesaurus",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "createKeywordThesaurus",
        description = "Create a Keyword Thesaurus",
        defaultEntityName = "KeywordThesaurus",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateKeywordThesaurus {}

    /**
     * Update a Keyword Thesaurus
     */
    @Service(
        name = "updateKeywordThesaurus",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "updateKeywordThesaurus",
        description = "Update a Keyword Thesaurus",
        defaultEntityName = "KeywordThesaurus",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateKeywordThesaurus {}

    /**
     * Delete a Keyword Thesaurus
     */
    @Service(
        name = "deleteKeywordThesaurus",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "deleteKeywordThesaurus",
        description = "Delete a Keyword Thesaurus",
        defaultEntityName = "KeywordThesaurus",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "alternateKeyword", optional = "true")
        }
    )
    public interface DeleteKeywordThesaurus {}

    /**
     * Create a new dated UOM converesion entity
     */
    @Service(
        name = "createUomConversionDated",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "createUomConversionDated",
        description = "Create a new dated UOM converesion entity",
        defaultEntityName = "UomConversionDated",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateUomConversionDated {}

    /**
     * Make a unit of measure conversion, first using UomConversion, then with UomConversionDated
     */
    @Service(
        name = "convertUom",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "convertUom",
        description = "Make a unit of measure conversion, first using UomConversion, then with UomConversionDated",
        defaultEntityName = "UomConversion",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "asOfDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "originalValue", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "conversionParameters", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "purposeEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "convertedValue", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "defaultDecimalScale", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "defaultRoundingMode", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ConvertUom {}

    /**
     * Make a unit of measure conversion, using CustomMethod entity
     */
    @Service(
        name = "convertUomCustom",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "convertUomCustom",
        description = "Make a unit of measure conversion, using CustomMethod entity",
        defaultEntityName = "UomConversion",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "originalValue", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "uomConversion", type = "Map", mode = "IN"),
            @Attribute(name = "conversionParameters", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "convertedValue", type = "BigDecimal", mode = "OUT", optional = "true")
        }
    )
    public interface ConvertUomCustom {}

    /**
     * Returns true if an UomConversion record exists
     */
    @Service(
        name = "checkUomConversion",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "checkUomConversion",
        description = "Returns true if an UomConversion record exists",
        defaultEntityName = "UomConversion",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "exist", type = "Boolean", mode = "OUT")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "VIEW")
    )
    public interface CheckUomConversion {}

    /**
     * Returns true if an UomConversionDated record exists
     */
    @Service(
        name = "checkUomConversionDated",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "checkUomConversionDated",
        description = "Returns true if an UomConversionDated record exists",
        defaultEntityName = "UomConversionDated",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "exist", type = "Boolean", mode = "OUT")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "VIEW")
    )
    public interface CheckUomConversionDated {}

    /**
     * Look up progress made in File Upload process
     */
    @Service(
        name = "getFileUploadProgressStatus",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "getFileUploadProgressStatus",
        description = "Look up progress made in File Upload process",
        attributes = {
            @Attribute(name = "uploadProgressListener", type = "org.ofbiz.webapp.event.FileUploadProgressListener", mode = "IN", optional = "true"),
            @Attribute(name = "contentLength", type = "Long", mode = "OUT", optional = "true"),
            @Attribute(name = "bytesRead", type = "Long", mode = "OUT", optional = "true"),
            @Attribute(name = "readPercent", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "hasStarted", type = "Boolean", mode = "OUT", optional = "true")
        }
    )
    public interface GetFileUploadProgressStatus {}

    @Service(
        name = "ftpInterface",
        engine = "interface",
        attributes = {
            @Attribute(name = "hostname", type = "String", mode = "IN"),
            @Attribute(name = "username", type = "String", mode = "IN"),
            @Attribute(name = "password", type = "String", mode = "IN"),
            @Attribute(name = "localFilename", type = "String", mode = "IN"),
            @Attribute(name = "remoteFilename", type = "String", mode = "IN"),
            @Attribute(name = "binaryTransfer", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "passiveMode", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "defaultTimeout", type = "Integer", mode = "IN", optional = "true")
        }
    )
    public interface FtpInterface {}

    @Service(
        name = "ftpPutFile",
        location = "org.ofbiz.common.FtpServices",
        invoke = "putFile",
        useTransaction = "false",
        implemented = {@Implements(service = "ftpInterface")},
        attributes = {
            @Attribute(name = "siteCommands", type = "List", mode = "IN", optional = "true")
        }
    )
    public interface FtpPutFile {}

    @Service(
        name = "ftpGetFile",
        location = "org.ofbiz.common.FtpServices",
        invoke = "getFile",
        useTransaction = "false",
        implemented = {@Implements(service = "ftpInterface")}
    )
    public interface FtpGetFile {}

    /**
     * Used to Automatically Authenticate a username/password; create a UserLogin object
     */
    @Service(
        name = "userLogin",
        location = "org.ofbiz.common.login.LoginServices",
        invoke = "userLogin",
        description = "Used to Automatically Authenticate a username/password; create a UserLogin object",
        implemented = {@Implements(service = "authenticationInterface")},
        attributes = {
            @Attribute(name = "request", type = "javax.servlet.http.HttpServletRequest", mode = "IN", optional = "true")
        }
    )
    public interface UserLogin {}

    /**
     * Create a UserLogin
     */
    @Service(
        name = "createUserLogin",
        location = "org.ofbiz.common.login.LoginServices",
        invoke = "createUserLogin",
        description = "Create a UserLogin",
        attributes = {
            @Attribute(name = "userLoginId", type = "String", mode = "IN"),
            @Attribute(name = "enabled", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "currentPassword", type = "String", mode = "IN"),
            @Attribute(name = "currentPasswordVerify", type = "String", mode = "IN"),
            @Attribute(name = "passwordHint", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "requirePasswordChange", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "externalAuthId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateUserLogin {}

    /**
     * Update a UserLoginId by creating a new one and expiring the old one
     */
    @Service(
        name = "updateUserLoginId",
        location = "org.ofbiz.common.login.LoginServices",
        invoke = "updateUserLoginId",
        description = "Update a UserLoginId by creating a new one and expiring the old one",
        auth = "true",
        attributes = {
            @Attribute(name = "userLoginId", type = "String", mode = "IN"),
            @Attribute(name = "newUserLogin", type = "org.ofbiz.entity.GenericValue", mode = "OUT")
        }
    )
    public interface UpdateUserLoginId {}

    /**
     * Update a UserLogin Password
     */
    @Service(
        name = "updatePassword",
        location = "org.ofbiz.common.login.LoginServices",
        invoke = "updatePassword",
        description = "Update a UserLogin Password",
        defaultEntityName = "UserLogin",
        auth = "true",
        attributes = {
            @Attribute(name = "userLoginId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "currentPassword", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "newPassword", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "newPasswordVerify", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "passwordHint", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "updatedUserLogin", type = "org.ofbiz.entity.GenericValue", mode = "OUT")
        }
    )
    public interface UpdatePassword {}

    /**
     * Update UserLogin Security Settings
     */
    @Service(
        name = "updateUserLoginSecurity",
        location = "org.ofbiz.common.login.LoginServices",
        invoke = "updateUserLoginSecurity",
        description = "Update UserLogin Security Settings",
        defaultEntityName = "UserLogin",
        auth = "true",
        attributes = {
            @Attribute(name = "userLoginId", type = "String", mode = "IN"),
            @Attribute(name = "enabled", type = "String", mode = "IN"),
            @Attribute(name = "disabledDateTime", type = "java.sql.Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "successiveFailedLogins", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "externalAuthId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userLdapDn", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "requirePasswordChange", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "disabledBy", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateUserLoginSecurity {}

    @Service(
        name = "genericBasePermissionCheck",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml",
        invoke = "genericBasePermissionCheck",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "primaryPermission", type = "String", mode = "IN"),
            @Attribute(name = "altPermission", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface GenericBasePermissionCheck {}

    /**
     * Interface for ROME RSS feed services; should return the WireFeed object (serializable)
     */
    @Service(
        name = "rssFeedInterface",
        engine = "interface",
        description = "Interface for ROME RSS feed services; should return the WireFeed object (serializable)",
        attributes = {
            @Attribute(name = "feedType", type = "String", mode = "IN"),
            @Attribute(name = "mainLink", type = "String", mode = "IN"),
            @Attribute(name = "entryLink", type = "String", mode = "IN"),
            @Attribute(name = "wireFeed", type = "com.sun.syndication.feed.WireFeed", mode = "OUT")
        }
    )
    public interface RssFeedInterface {}

    /**
     * Copies the preferences from one userLoginId and preference group to another.             If no userPrefLoginId is specified, preferences are copied to current user's preferences.
     */
    @Service(
        name = "copyUserPrefGroup",
        location = "org.ofbiz.common.preferences.PreferenceServices",
        invoke = "copyUserPreferenceGroup",
        description = "Copies the preferences from one userLoginId and preference group to another.\n            If no userPrefLoginId is specified, preferences are copied to current user's preferences.",
        auth = "true",
        attributes = {
            @Attribute(name = "fromUserLoginId", type = "String", mode = "IN"),
            @Attribute(name = "userPrefGroupTypeId", type = "String", mode = "IN"),
            @Attribute(name = "userPrefLoginId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "preferenceCopyPermission")
    )
    public interface CopyUserPrefGroup {}

    /**
     *              Gets a single user preference.             If not found for the specific userLogin, find it for the _NA_ userlogin.             If the value is DEFAULT, find the value in general.properties file.         
     */
    @Service(
        name = "getUserPreference",
        location = "org.ofbiz.common.preferences.PreferenceServices",
        invoke = "getUserPreference",
        description = "\n            Gets a single user preference.\n            If not found for the specific userLogin, find it for the _NA_ userlogin.\n            If the value is DEFAULT, find the value in general.properties file.\n        ",
        attributes = {
            @Attribute(name = "userPrefTypeId", type = "String", mode = "IN"),
            @Attribute(name = "userPrefLoginId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userPrefGroupTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userPrefMap", type = "Map", mode = "OUT", optional = "true"),
            @Attribute(name = "userPrefValue", type = "Object", mode = "OUT", optional = "true")
        }
    )
    public interface GetUserPreference {}

    /**
     * Gets a group of user preferences.
     */
    @Service(
        name = "getUserPreferenceGroup",
        location = "org.ofbiz.common.preferences.PreferenceServices",
        invoke = "getUserPreferenceGroup",
        description = "Gets a group of user preferences.",
        attributes = {
            @Attribute(name = "userPrefGroupTypeId", type = "String", mode = "IN"),
            @Attribute(name = "userPrefLoginId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userPrefMap", type = "Map", mode = "OUT", optional = "true")
        }
    )
    public interface GetUserPreferenceGroup {}

    /**
     * Sets a single user preference.
     */
    @Service(
        name = "setUserPreference",
        location = "org.ofbiz.common.preferences.PreferenceServices",
        invoke = "setUserPreference",
        description = "Sets a single user preference.",
        auth = "true",
        attributes = {
            @Attribute(name = "userPrefTypeId", type = "String", mode = "IN"),
            @Attribute(name = "userPrefValue", type = "String", mode = "IN"),
            @Attribute(name = "userPrefGroupTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userPrefLoginId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "preferenceGetSetPermission", mainAction = "CREATE")
    )
    public interface SetUserPreference {}

    /**
     * Sets a single user preference.
     */
    @Service(
        name = "removeUserPreference",
        location = "org.ofbiz.common.preferences.PreferenceServices",
        invoke = "removeUserPreference",
        description = "Sets a single user preference.",
        auth = "true",
        attributes = {
            @Attribute(name = "userPrefTypeId", type = "String", mode = "IN"),
            @Attribute(name = "userPrefLoginId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "preferenceGetSetPermission", mainAction = "CREATE")
    )
    public interface RemoveUserPreference {}

    /**
     * Sets a group of user preferences.
     */
    @Service(
        name = "setUserPreferenceGroup",
        location = "org.ofbiz.common.preferences.PreferenceServices",
        invoke = "setUserPreferenceGroup",
        description = "Sets a group of user preferences.",
        auth = "true",
        attributes = {
            @Attribute(name = "userPrefMap", type = "Map", mode = "IN"),
            @Attribute(name = "userPrefGroupTypeId", type = "String", mode = "IN"),
            @Attribute(name = "userPrefLoginId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "preferenceGetSetPermission", mainAction = "CREATE")
    )
    public interface SetUserPreferenceGroup {}

    /**
     * User preference get/set permission checking.
     */
    @Service(
        name = "preferenceGetSetPermission",
        location = "org.ofbiz.common.preferences.PreferenceWorker",
        invoke = "checkPermission",
        description = "User preference get/set permission checking.",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "userPrefLoginId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface PreferenceGetSetPermission {}

    /**
     * User preference copy permission checking.
     */
    @Service(
        name = "preferenceCopyPermission",
        location = "org.ofbiz.common.preferences.PreferenceWorker",
        invoke = "checkCopyPermission",
        description = "User preference copy permission checking.",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface PreferenceCopyPermission {}

    /**
     * Get a visual theme resources Map. Call with visualThemeId String,             and optional themeResources Map. Returns themeResources Map - a             Map of Lists, where the resourceTypeEnumId is the key and the value             is a List of resourceValue Strings for that resourceTypeEnumId.         
     */
    @Service(
        name = "getVisualThemeResources",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "getVisualThemeResources",
        description = "Get a visual theme resources Map. Call with visualThemeId String,\n            and optional themeResources Map. Returns themeResources Map - a\n            Map of Lists, where the resourceTypeEnumId is the key and the value\n            is a List of resourceValue Strings for that resourceTypeEnumId.\n        ",
        attributes = {
            @Attribute(name = "visualThemeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "themeResources", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "themeResources", type = "Map", mode = "OUT"),
            @Attribute(name = "visualThemeId", type = "String", mode = "OUT")
        }
    )
    public interface GetVisualThemeResources {}

    /**
     * Create a Visual Theme
     */
    @Service(
        name = "createVisualTheme",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Visual Theme",
        defaultEntityName = "VisualTheme",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "OUT", include = "pk")
        },
        permissionService = @PermissionService(service = "visualThemePermissionCheck", mainAction = "CREATE")
    )
    public interface CreateVisualTheme {}

    /**
     * Update a Visual Theme
     */
    @Service(
        name = "updateVisualTheme",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Visual Theme",
        defaultEntityName = "VisualTheme",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "visualThemePermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateVisualTheme {}

    /**
     * Delete a Visual Theme
     */
    @Service(
        name = "deleteVisualTheme",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Visual Theme",
        defaultEntityName = "VisualTheme",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "visualThemePermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteVisualTheme {}

    /**
     * Create a Visual Theme Resource
     */
    @Service(
        name = "createVisualThemeResource",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Visual Theme Resource",
        defaultEntityName = "VisualThemeResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "OUT", include = "pk")
        },
        attributes = {
            @Attribute(name = "visualThemeId", type = "String", mode = "IN"),
            @Attribute(name = "resourceTypeEnumId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "visualThemePermissionCheck", mainAction = "CREATE")
    )
    public interface CreateVisualThemeResource {}

    /**
     * Update a Visual Theme Resource
     */
    @Service(
        name = "updateVisualThemeResource",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Visual Theme Resource",
        defaultEntityName = "VisualThemeResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "visualThemePermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateVisualThemeResource {}

    /**
     * Delete a Visual Theme Resource
     */
    @Service(
        name = "deleteVisualThemeResource",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Visual Theme Resource",
        defaultEntityName = "VisualThemeResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "visualThemePermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteVisualThemeResource {}

    /**
     * Visual Theme Permission Checking Logic
     */
    @Service(
        name = "visualThemePermissionCheck",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml",
        invoke = "visualThemePermissionCheck",
        description = "Visual Theme Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface VisualThemePermissionCheck {}

    @Service(
        name = "tempExprPermissionCheck",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml",
        invoke = "genericBasePermissionCheck",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "primaryPermission", type = "String", mode = "IN", defaultValue = "TEMPEXPR"),
            @Attribute(name = "altPermission", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface TempExprPermissionCheck {}

    /**
     * Create a Temporal Expression
     */
    @Service(
        name = "createTemporalExpression",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Temporal Expression",
        defaultEntityName = "TemporalExpression",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "tempExprPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateTemporalExpression {}

    /**
     * Update a Temporal Expression
     */
    @Service(
        name = "updateTemporalExpression",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Temporal Expression",
        defaultEntityName = "TemporalExpression",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "tempExprPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateTemporalExpression {}

    /**
     * Create a Temporal Expression Association
     */
    @Service(
        name = "createTemporalExpressionAssoc",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Temporal Expression Association",
        defaultEntityName = "TemporalExpressionAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "tempExprPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateTemporalExpressionAssoc {}

    /**
     * Delete a Temporal Expression Association
     */
    @Service(
        name = "deleteTemporalExpressionAssoc",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Temporal Expression Association",
        defaultEntityName = "TemporalExpressionAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "tempExprPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteTemporalExpressionAssoc {}

    /**
     * Add a registered PortalPortlet to a PortalPage
     */
    @Service(
        name = "createPortalPagePortlet",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/PortalPageServices.xml",
        invoke = "createPortalPagePortlet",
        description = "Add a registered PortalPortlet to a PortalPage",
        defaultEntityName = "PortalPagePortlet",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "portletSeqId", mode = "OUT", optional = "true")
        }
    )
    public interface CreatePortalPagePortlet {}

    /**
     * Update a PortalPortlet
     */
    @Service(
        name = "updatePortalPagePortlet",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PortalPortlet",
        defaultEntityName = "PortalPagePortlet",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePortalPagePortlet {}

    /**
     * Delete a PortalPortlet from a PortalPage
     */
    @Service(
        name = "deletePortalPagePortlet",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/PortalPageServices.xml",
        invoke = "deletePortalPagePortlet",
        description = "Delete a PortalPortlet from a PortalPage",
        defaultEntityName = "PortalPagePortlet",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePortalPagePortlet {}

    /**
     * Move a PortalPortlet from the actual portalPage to a different one
     */
    @Service(
        name = "movePortletToPortalPage",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/PortalPageServices.xml",
        invoke = "movePortletToPortalPage",
        description = "Move a PortalPortlet from the actual portalPage to a different one",
        defaultEntityName = "PortalPagePortlet",
        auth = "true",
        attributes = {
            @Attribute(name = "portalPageId", type = "String", mode = "IN"),
            @Attribute(name = "portalPortletId", type = "String", mode = "IN"),
            @Attribute(name = "portletSeqId", type = "String", mode = "IN"),
            @Attribute(name = "newPortalPageId", type = "String", mode = "IN")
        }
    )
    public interface MovePortletToPortalPage {}

    /**
     * Create a new Portal Page
     */
    @Service(
        name = "createPortalPage",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/PortalPageServices.xml",
        invoke = "createPortalPage",
        description = "Create a new Portal Page",
        defaultEntityName = "PortalPage",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePortalPage {}

    /**
     * Update a Portal Page
     */
    @Service(
        name = "updatePortalPage",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Portal Page",
        defaultEntityName = "PortalPage",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePortalPage {}

    /**
     * Delete a Portal Page, related colums and used portlets
     */
    @Service(
        name = "deletePortalPage",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/PortalPageServices.xml",
        invoke = "deletePortalPage",
        description = "Delete a Portal Page, related colums and used portlets",
        defaultEntityName = "PortalPage",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface DeletePortalPage {}

    @Service(
        name = "updatePortalPageSeq",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/PortalPageServices.xml",
        invoke = "updatePortalPageSeq",
        defaultEntityName = "PortalPage",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "mode", type = "String", mode = "IN")
        }
    )
    public interface UpdatePortalPageSeq {}

    /**
     * Add a new Column to a PortalPage
     */
    @Service(
        name = "addPortalPageColumn",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/PortalPageServices.xml",
        invoke = "addPortalPageColumn",
        description = "Add a new Column to a PortalPage",
        defaultEntityName = "PortalPageColumn",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "columnSeqId", mode = "INOUT", optional = "true")
        }
    )
    public interface AddPortalPageColumn {}

    /**
     * Update a Portal Page Column
     */
    @Service(
        name = "updatePortalPageColumn",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Portal Page Column",
        defaultEntityName = "PortalPageColumn",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePortalPageColumn {}

    /**
     * Delete a Column from a PortalPage
     */
    @Service(
        name = "deletePortalPageColumn",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/PortalPageServices.xml",
        invoke = "deletePortalPageColumn",
        description = "Delete a Column from a PortalPage",
        defaultEntityName = "PortalPageColumn",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePortalPageColumn {}

    @Service(
        name = "updatePortletSeqDragDrop",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/PortalPageServices.xml",
        invoke = "updatePortletSeqDragDrop",
        auth = "true",
        attributes = {
            @Attribute(name = "o_portalPageId", type = "String", mode = "IN"),
            @Attribute(name = "o_portalPortletId", type = "String", mode = "IN"),
            @Attribute(name = "o_portletSeqId", type = "String", mode = "IN"),
            @Attribute(name = "d_portalPageId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "d_portalPortletId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "d_portletSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "destinationColumn", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mode", type = "String", mode = "IN")
        }
    )
    public interface UpdatePortletSeqDragDrop {}

    /**
     * Create a new Portlet Attribute
     */
    @Service(
        name = "createPortletAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new Portlet Attribute",
        defaultEntityName = "PortletAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePortletAttribute {}

    /**
     * Get all attributes of a Portlet
     */
    @Service(
        name = "getPortletAttributes",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/PortalPageServices.xml",
        invoke = "getPortletAttributes",
        description = "Get all attributes of a Portlet",
        auth = "true",
        attributes = {
            @Attribute(name = "portalPageId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "ownerUserLoginId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "portalPortletId", type = "String", mode = "IN"),
            @Attribute(name = "portletSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "attributeMap", type = "Map", mode = "OUT", optional = "true")
        }
    )
    public interface GetPortletAttributes {}

    /**
     * Create a Geo
     */
    @Service(
        name = "createGeo",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Geo",
        defaultEntityName = "Geo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "geoName", optional = "false"),
            @OverrideAttribute(name = "geoTypeId", optional = "false")
        }
    )
    public interface CreateGeo {}

    /**
     * Update a Geo
     */
    @Service(
        name = "updateGeo",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Geo",
        defaultEntityName = "Geo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateGeo {}

    /**
     * Delete a Geo
     */
    @Service(
        name = "deleteGeo",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Geo",
        defaultEntityName = "Geo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteGeo {}

    /**
     * Delete a GeoAssoc
     */
    @Service(
        name = "deleteGeoAssoc",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a GeoAssoc",
        defaultEntityName = "GeoAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteGeoAssoc {}

    /**
     * Link Geos to another Geo
     */
    @Service(
        name = "linkGeos",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "linkGeos",
        description = "Link Geos to another Geo",
        auth = "true",
        attributes = {
            @Attribute(name = "geoIds", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "geoId", type = "String", mode = "IN"),
            @Attribute(name = "geoAssocTypeId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "CREATE")
    )
    public interface LinkGeos {}

    @Service(
        name = "getRelatedGeos",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "getRelatedGeos",
        attributes = {
            @Attribute(name = "geoId", type = "String", mode = "IN"),
            @Attribute(name = "geoAssocTypeId", type = "String", mode = "IN"),
            @Attribute(name = "geoList", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetRelatedGeos {}

    /**
     * Get a list of country and associated states from Geo
     */
    @Service(
        name = "getCountryList",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "getCountryList",
        description = "Get a list of country and associated states from Geo",
        attributes = {
            @Attribute(name = "countryList", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetCountryList {}

    @Service(
        name = "getAssociatedStateList",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "getAssociatedStateList",
        attributes = {
            @Attribute(name = "countryGeoId", type = "String", mode = "IN"),
            @Attribute(name = "listOrderBy", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "stateList", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetAssociatedStateList {}

    /**
     * Create a GeoPoint
     */
    @Service(
        name = "createGeoPoint",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a GeoPoint",
        defaultEntityName = "GeoPoint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "dataSourceId", optional = "false"),
            @OverrideAttribute(name = "latitude", optional = "false"),
            @OverrideAttribute(name = "longitude", optional = "false")
        }
    )
    public interface CreateGeoPoint {}

    /**
     * Update a GeoPoint
     */
    @Service(
        name = "updateGeoPoint",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a GeoPoint",
        defaultEntityName = "GeoPoint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "dataSourceId", optional = "false"),
            @OverrideAttribute(name = "latitude", optional = "false"),
            @OverrideAttribute(name = "longitude", optional = "false")
        }
    )
    public interface UpdateGeoPoint {}

    /**
     * Delete a GeoPoint
     */
    @Service(
        name = "deleteGeoPoint",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a GeoPoint",
        defaultEntityName = "GeoPoint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteGeoPoint {}

    @Service(
        name = "getServerTimestamp",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "getServerTimestamp",
        attributes = {
            @Attribute(name = "serverTimestamp", type = "Timestamp", mode = "OUT")
        }
    )
    public interface GetServerTimestamp {}

    @Service(
        name = "getServerTimeZone",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "getServerTimeZone",
        attributes = {
            @Attribute(name = "serverTimeZone", type = "String", mode = "OUT")
        }
    )
    public interface GetServerTimeZone {}

    @Service(
        name = "getServerTimestampAsLong",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "getServerTimestampAsLong",
        attributes = {
            @Attribute(name = "serverTimestamp", type = "Long", mode = "OUT")
        }
    )
    public interface GetServerTimestampAsLong {}

    @Service(
        name = "getServerTimestampAsString",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CommonServices.xml",
        invoke = "getServerTimestampAsString",
        attributes = {
            @Attribute(name = "dateTimeFormat", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "useServerTz", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "serverTimestamp", type = "String", mode = "OUT")
        }
    )
    public interface GetServerTimestampAsString {}

    /**
     * Create or update the JsLanguageFilesMapping.java. You still need to compile thereafter
     */
    @Service(
        name = "createJsLanguageFileMapping",
        location = "org.ofbiz.common.JsLanguageFileMappingCreator",
        invoke = "createJsLanguageFileMapping",
        description = "Create or update the JsLanguageFilesMapping.java. You still need to compile thereafter",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "encoding", type = "String", mode = "IN", optional = "true", defaultValue = "UTF-8")
        }
    )
    public interface CreateJsLanguageFileMapping {}

    /**
     *              Get all metrics. Returns a List of Maps - one Map per metric. Each Map includes the following keys:             name, serviceRate, threshold, totalEvents. See org.ofbiz.base.metrics.Metrics.         
     */
    @Service(
        name = "getAllMetrics",
        location = "org.ofbiz.common.CommonServices",
        invoke = "getAllMetrics",
        description = "\n            Get all metrics. Returns a List of Maps - one Map per metric. Each Map includes the following keys:\n            name, serviceRate, threshold, totalEvents. See org.ofbiz.base.metrics.Metrics.\n        ",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "metricsList", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetAllMetrics {}

    /**
     * Resets a metric. See org.ofbiz.base.metrics.Metrics.
     */
    @Service(
        name = "resetMetric",
        location = "org.ofbiz.common.CommonServices",
        invoke = "resetMetric",
        description = "Resets a metric. See org.ofbiz.base.metrics.Metrics.",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "name", type = "String", mode = "IN")
        }
    )
    public interface ResetMetric {}

    /**
     * Create GeoAssocType
     */
    @Service(
        name = "createGeoAssocType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create GeoAssocType",
        defaultEntityName = "GeoAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateGeoAssocType {}

    /**
     * Update GeoAssocType
     */
    @Service(
        name = "updateGeoAssocType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update GeoAssocType",
        defaultEntityName = "GeoAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateGeoAssocType {}

    /**
     * Delete GeoAssocType
     */
    @Service(
        name = "deleteGeoAssocType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete GeoAssocType",
        defaultEntityName = "GeoAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteGeoAssocType {}

    /**
     * Create GeoType
     */
    @Service(
        name = "createGeoType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create GeoType",
        defaultEntityName = "GeoType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateGeoType {}

    /**
     * Update GeoType
     */
    @Service(
        name = "updateGeoType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update GeoType",
        defaultEntityName = "GeoType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateGeoType {}

    /**
     * Delete GeoType
     */
    @Service(
        name = "deleteGeoType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete GeoType",
        defaultEntityName = "GeoType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteGeoType {}

    /**
     * Create a PeriodType
     */
    @Service(
        name = "createPeriodType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PeriodType",
        defaultEntityName = "PeriodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePeriodType {}

    /**
     * Update a PeriodType
     */
    @Service(
        name = "updatePeriodType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PeriodType",
        defaultEntityName = "PeriodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePeriodType {}

    /**
     * Delete a PeriodType
     */
    @Service(
        name = "deletePeriodType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PeriodType",
        defaultEntityName = "PeriodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePeriodType {}

    /**
     * Create a UserPrefGroupType
     */
    @Service(
        name = "createUserPrefGroupType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a UserPrefGroupType",
        defaultEntityName = "UserPrefGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateUserPrefGroupType {}

    /**
     * Update a UserPrefGroupType
     */
    @Service(
        name = "updateUserPrefGroupType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a UserPrefGroupType",
        defaultEntityName = "UserPrefGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateUserPrefGroupType {}

    /**
     * Delete a UserPrefGroupType
     */
    @Service(
        name = "deleteUserPrefGroupType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a UserPrefGroupType",
        defaultEntityName = "UserPrefGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteUserPrefGroupType {}

    /**
     * Create UomType Record
     */
    @Service(
        name = "createUomType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create UomType Record",
        defaultEntityName = "UomType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateUomType {}

    /**
     * Update UomType Record
     */
    @Service(
        name = "updateUomType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update UomType Record",
        defaultEntityName = "UomType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateUomType {}

    /**
     * Delete UomType Record
     */
    @Service(
        name = "deleteUomType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete UomType Record",
        defaultEntityName = "UomType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteUomType {}

    /**
     * Create UomGroup record
     */
    @Service(
        name = "createUomGroup",
        engine = "entity-auto",
        invoke = "create",
        description = "Create UomGroup record",
        defaultEntityName = "UomGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateUomGroup {}

    /**
     * Delete UomGroup record
     */
    @Service(
        name = "deleteUomGroup",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete UomGroup record",
        defaultEntityName = "UomGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteUomGroup {}

    /**
     * Create a StatusValidChange
     */
    @Service(
        name = "createStatusValidChange",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a StatusValidChange",
        defaultEntityName = "StatusValidChange",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateStatusValidChange {}

    /**
     * Update a StatusValidChange
     */
    @Service(
        name = "updateStatusValidChange",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a StatusValidChange",
        defaultEntityName = "StatusValidChange",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateStatusValidChange {}

    /**
     * Delete a StatusValidChange
     */
    @Service(
        name = "deleteStatusValidChange",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a StatusValidChange",
        defaultEntityName = "StatusValidChange",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteStatusValidChange {}

    /**
     * Create Uom Record
     */
    @Service(
        name = "createUom",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Uom Record",
        defaultEntityName = "Uom",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateUom {}

    /**
     * Update Uom Record
     */
    @Service(
        name = "updateUom",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Uom Record",
        defaultEntityName = "Uom",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateUom {}

    /**
     * Delete Uom Record
     */
    @Service(
        name = "deleteUom",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Uom Record",
        defaultEntityName = "Uom",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteUom {}

    /**
     * Set locale from browser.
     */
    @Service(
        name = "SetTimeZoneFromBrowser",
        engine = "groovy",
        location = "component://common/script/org/ofbiz/common/SetTimeZoneFromBrowser.groovy",
        invoke = "SetTimeZoneFromBrowser",
        description = "Set locale from browser.",
        auth = "true",
        attributes = {
            @Attribute(name = "localeName", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SetTimeZoneFromBrowser {}

}
