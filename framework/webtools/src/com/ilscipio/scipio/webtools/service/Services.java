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
package com.ilscipio.scipio.webtools.service;

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
        name = "commonEntityImportInterface",
        engine = "interface",
        attributes = {
            @Attribute(name = "allowEntity", type = "java.util.Set", mode = "IN", optional = "true", description = "SCIPIO: Allow entity names, exception on violations. Replaces \"allowedEntityNames\". Added 2018-09-17."),
            @Attribute(name = "allowedEntityNames", type = "java.util.Set", mode = "IN", optional = "true", description = "SCIPIO: DEPRECATED, use allowEntity instead. Added 2017-06-15."),
            @Attribute(name = "disallowEntity", type = "java.util.Set", mode = "IN", optional = "true", description = "SCIPIO: Disallow entity names, exception on violations. Added 2018-09-17."),
            @Attribute(name = "allowEntityWarn", type = "java.util.Set", mode = "IN", optional = "true", description = "SCIPIO: Allow entity names, violations warned. Added 2018-09-17."),
            @Attribute(name = "disallowEntityWarn", type = "java.util.Set", mode = "IN", optional = "true", description = "SCIPIO: Disallow entity names, violations warned. Added 2018-09-17."),
            @Attribute(name = "includeEntity", type = "java.util.Set", mode = "IN", optional = "true", description = "SCIPIO: Include entity names, violations ignored. Added 2018-09-17."),
            @Attribute(name = "excludeEntity", type = "java.util.Set", mode = "IN", optional = "true", description = "SCIPIO: Exclude entity names, violations ignored. Added 2018-09-17."),
            @Attribute(name = "disallowUnsafeEntityWarn", type = "Boolean", mode = "IN", optional = "true", description = "SCIPIO: Disallow dangerous entity names, violations warned. Default: false. Added 2018-09-17.")
        }
    )
    public interface CommonEntityImportInterface {}

    /**
     * Parses an entity xml file or an entity xml text
     */
    @Service(
        name = "parseEntityXmlFile",
        location = "org.ofbiz.webtools.WebToolsServices",
        invoke = "parseEntityXmlFile",
        description = "Parses an entity xml file or an entity xml text",
        auth = "true",
        useTransaction = "false",
        implemented = {@Implements(service = "commonEntityImportInterface")},
        attributes = {
            @Attribute(name = "url", type = "java.net.URL", mode = "IN", optional = "true"),
            @Attribute(name = "xmltext", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "mostlyInserts", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "maintainTimeStamps", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "txTimeout", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "createDummyFks", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "checkDataOnly", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "placeholderValues", type = "java.util.Map", mode = "IN", optional = "true"),
            @Attribute(name = "rowProcessed", type = "Long", mode = "OUT")
        },
        permissionService = @PermissionService(service = "entityMaintPermCheck", mainAction = "VIEW")
    )
    public interface ParseEntityXmlFile {}

    /**
     * Imports an entity xml file or text string
     */
    @Service(
        name = "entityImport",
        location = "org.ofbiz.webtools.WebToolsServices",
        invoke = "entityImport",
        description = "Imports an entity xml file or text string",
        auth = "true",
        useTransaction = "false",
        implemented = {@Implements(service = "uploadFileInterface"), @Implements(service = "commonEntityImportInterface")},
        attributes = {
            @Attribute(name = "filename", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "fmfilename", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "fulltext", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "isUrl", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mostlyInserts", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "maintainTimeStamps", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "createDummyFks", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "checkDataOnly", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "txTimeout", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "placeholderValues", type = "java.util.Map", mode = "IN", optional = "true"),
            @Attribute(name = "messages", type = "List", mode = "OUT")
        },
        permissionService = @PermissionService(service = "entityMaintPermCheck", mainAction = "VIEW")
    )
    public interface EntityImport {}

    /**
     * Imports all entity xml files contained in a directory
     */
    @Service(
        name = "entityImportDir",
        location = "org.ofbiz.webtools.WebToolsServices",
        invoke = "entityImportDir",
        description = "Imports all entity xml files contained in a directory",
        auth = "true",
        useTransaction = "false",
        implemented = {@Implements(service = "commonEntityImportInterface")},
        attributes = {
            @Attribute(name = "path", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mostlyInserts", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "maintainTimeStamps", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "createDummyFks", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "checkDataOnly", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "deleteFiles", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "txTimeout", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "filePause", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "placeholderValues", type = "java.util.Map", mode = "IN", optional = "true"),
            @Attribute(name = "messages", type = "List", mode = "OUT")
        },
        permissionService = @PermissionService(service = "entityMaintPermCheck", mainAction = "VIEW")
    )
    public interface EntityImportDir {}

    /**
     * Imports an entity xml file or text string
     */
    @Service(
        name = "entityImportReaders",
        location = "org.ofbiz.webtools.WebToolsServices",
        invoke = "entityImportReaders",
        description = "Imports an entity xml file or text string",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "readers", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "overrideDelegator", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "overrideGroup", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mostlyInserts", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "maintainTimeStamps", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "createDummyFks", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "checkDataOnly", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "txTimeout", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "messages", type = "List", mode = "OUT")
        },
        permissionService = @PermissionService(service = "entityMaintPermCheck", mainAction = "VIEW")
    )
    public interface EntityImportReaders {}

    /**
     * Exports all entities into xml files
     */
    @Service(
        name = "entityExportAll",
        location = "org.ofbiz.webtools.WebToolsServices",
        invoke = "entityExportAll",
        description = "Exports all entities into xml files",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "outpath", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "txTimeout", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "results", type = "List", mode = "OUT")
        },
        permissionService = @PermissionService(service = "entityMaintPermCheck", mainAction = "VIEW")
    )
    public interface EntityExportAll {}

    /**
     * Gets the entity reference data - for the entity reference screen. See org.ofbiz.webtools.WebToolsServices.getEntityRefData().
     */
    @Service(
        name = "getEntityRefData",
        location = "org.ofbiz.webtools.WebToolsServices",
        invoke = "getEntityRefData",
        description = "Gets the entity reference data - for the entity reference screen. See org.ofbiz.webtools.WebToolsServices.getEntityRefData().",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "numberOfEntities", type = "java.lang.Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "packagesList", type = "java.util.List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "entityMaintPermCheck", mainAction = "VIEW")
    )
    public interface GetEntityRefData {}

    /**
     * Saves specified set of entities to an Apple EOModelBundle file.             See org.ofbiz.webtools.WebToolsServices.exportEoModelBundle().             Specify either entityPackageName or entityGroupId, or leave both empty for ALL entities in the data model.         
     */
    @Service(
        name = "exportEntityEoModelBundle",
        location = "org.ofbiz.webtools.WebToolsServices",
        invoke = "exportEntityEoModelBundle",
        description = "Saves specified set of entities to an Apple EOModelBundle file.\n            See org.ofbiz.webtools.WebToolsServices.exportEoModelBundle().\n            Specify either entityPackageName or entityGroupId, or leave both empty for ALL entities in the data model.\n        ",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "eomodeldFullPath", type = "java.lang.String", mode = "IN"),
            @Attribute(name = "entityPackageName", type = "java.lang.String", mode = "IN", optional = "true"),
            @Attribute(name = "entityGroupId", type = "java.lang.String", mode = "IN", optional = "true"),
            @Attribute(name = "datasourceName", type = "java.lang.String", mode = "IN", optional = "true"),
            @Attribute(name = "entityNamePrefix", type = "java.lang.String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "entityMaintPermCheck", mainAction = "VIEW")
    )
    public interface ExportEntityEoModelBundle {}

    /**
     * Performs an entity maintenance security check. Returns hasPermission=true           if the user has the ENTITY_MAINT permission.
     */
    @Service(
        name = "entityMaintPermCheck",
        location = "org.ofbiz.webtools.WebToolsServices",
        invoke = "entityMaintPermCheck",
        description = "Performs an entity maintenance security check. Returns hasPermission=true\n          if the user has the ENTITY_MAINT permission.",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface EntityMaintPermCheck {}

    /**
     * Saves service and related artifacts diagram to an Apple EOModelBundle file.         
     */
    @Service(
        name = "exportServiceEoModelBundle",
        location = "org.ofbiz.webtools.WebToolsServices",
        invoke = "exportServiceEoModelBundle",
        description = "Saves service and related artifacts diagram to an Apple EOModelBundle file.\n        ",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "eomodeldFullPath", type = "java.lang.String", mode = "IN"),
            @Attribute(name = "serviceName", type = "java.lang.String", mode = "IN")
        },
        permissionService = @PermissionService(service = "entityMaintPermCheck", mainAction = "VIEW")
    )
    public interface ExportServiceEoModelBundle {}

    /**
     * Save labels to xml file
     */
    @Service(
        name = "saveLabelsToXmlFile",
        location = "org.ofbiz.webtools.labelmanager.SaveLabelsToXmlFile",
        invoke = "saveLabelsToXmlFile",
        description = "Save labels to xml file",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "key", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "keyComment", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "update_label", type = "String", mode = "IN"),
            @Attribute(name = "fileName", type = "String", mode = "IN"),
            @Attribute(name = "confirm", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "removeLabel", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "localeNames", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "localeValues", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "localeComments", type = "List", mode = "IN", optional = "true")
        }
    )
    public interface SaveLabelsToXmlFile {}

    /**
     * Returns a list of Maps, containing the contentIds (List of requested paths) and hitcount (number of requests) grouped by DateInterval
     */
    @Service(
        name = "getServerRequests",
        engine = "groovy",
        location = "component://webtools/script/com/ilscipio/data/ServerData.groovy",
        invoke = "getServerRequests",
        description = "Returns a list of Maps, containing the contentIds (List of requested paths) and hitcount (number of requests) grouped by DateInterval",
        auth = "true",
        log = "quiet",
        attributes = {
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", description = "NOTE: fromDate is used as the base date from which to determine the buckets using bucketMinutes, so the value should usually\n                be aligned to the last bucket, otherwise some ServetHits may be missed."),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "dateInterval", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "bucketMinutes", type = "Integer", mode = "IN", optional = "true", description = "Causes final results to be bucketed every 5, 10, 15, etc. minutes as specified (0-based)"),
            @Attribute(name = "serverHostName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "requests", type = "Map", mode = "OUT", optional = "true", description = "NOTE: If serverHostName is false (consult all servers), this contains the sum of all serverRequests for each summer"),
            @Attribute(name = "serverRequests", type = "Map", mode = "OUT", optional = "true", description = "Maps serverHostName to requests maps - only meaningfull if serverHostName is left empty (all servers)")
        }
    )
    public interface GetServerRequests {}

    /**
     * Returns a list of Maps, containing the contentIds (List of requested paths) and hitcount (number of requests) grouped by minute
     */
    @Service(
        name = "getServerRequestsThisHour",
        engine = "groovy",
        location = "component://webtools/script/com/ilscipio/data/ServerData.groovy",
        invoke = "getServerRequestsThisHour",
        description = "Returns a list of Maps, containing the contentIds (List of requested paths) and hitcount (number of requests) grouped by minute",
        auth = "true",
        attributes = {
            @Attribute(name = "requests", type = "Map", mode = "OUT", optional = "true")
        }
    )
    public interface GetServerRequestsThisHour {}

    /**
     * Returns a list of Maps, containing the contentIds (List of requested paths) and hitcount (number of requests) grouped by hour
     */
    @Service(
        name = "getServerRequestsToday",
        engine = "groovy",
        location = "component://webtools/script/com/ilscipio/data/ServerData.groovy",
        invoke = "getServerRequestsToday",
        description = "Returns a list of Maps, containing the contentIds (List of requested paths) and hitcount (number of requests) grouped by hour",
        auth = "true",
        attributes = {
            @Attribute(name = "requests", type = "Map", mode = "OUT", optional = "true")
        }
    )
    public interface GetServerRequestsToday {}

    /**
     * Returns a list of Maps, containing the contentIds (List of requested paths) and hitcount (number of requests) grouped by day
     */
    @Service(
        name = "getServerRequestsThisWeek",
        engine = "groovy",
        location = "component://webtools/script/com/ilscipio/data/ServerData.groovy",
        invoke = "getServerRequestsThisWeek",
        description = "Returns a list of Maps, containing the contentIds (List of requested paths) and hitcount (number of requests) grouped by day",
        auth = "true",
        attributes = {
            @Attribute(name = "requests", type = "Map", mode = "OUT", optional = "true")
        }
    )
    public interface GetServerRequestsThisWeek {}

    /**
     * Returns a list of Maps, containing the contentIds (List of requested paths) and hitcount (number of requests) grouped by month
     */
    @Service(
        name = "getServerRequestsThisMonth",
        engine = "groovy",
        location = "component://webtools/script/com/ilscipio/data/ServerData.groovy",
        invoke = "getServerRequestsThisMonth",
        description = "Returns a list of Maps, containing the contentIds (List of requested paths) and hitcount (number of requests) grouped by month",
        auth = "true",
        attributes = {
            @Attribute(name = "requests", type = "Map", mode = "OUT", optional = "true")
        }
    )
    public interface GetServerRequestsThisMonth {}

    /**
     * Returns a list of Maps, containing the contentIds (List of requested paths) and hitcount (number of requests) grouped by year
     */
    @Service(
        name = "getServerRequestsThisYear",
        engine = "groovy",
        location = "component://webtools/script/com/ilscipio/data/ServerData.groovy",
        invoke = "getServerRequestsThisYear",
        description = "Returns a list of Maps, containing the contentIds (List of requested paths) and hitcount (number of requests) grouped by year",
        auth = "true",
        attributes = {
            @Attribute(name = "requests", type = "Map", mode = "OUT", optional = "true")
        }
    )
    public interface GetServerRequestsThisYear {}

    /**
     * Generates a ZIP file, comprising of a database dump for selected entities. File is stored in the entity 'EntityExport' for future use.
     */
    @Service(
        name = "getEntityExport",
        location = "com.ilscipio.scipio.webtools.WebToolsServices",
        invoke = "getEntityExport",
        description = "Generates a ZIP file, comprising of a database dump for selected entities. File is stored in the entity 'EntityExport' for future use.",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "entityList", type = "List", mode = "IN", description = "List of entity-names"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "exportId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "entityMaintPermCheck", mainAction = "VIEW")
    )
    public interface GetEntityExport {}

    @Service(
        name = "deleteEntityExport",
        engine = "entity-auto",
        invoke = "delete",
        defaultEntityName = "EntityExport",
        auth = "true",
        attributes = {
            @Attribute(name = "exportId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "entityMaintPermCheck", mainAction = "DELETE")
    )
    public interface DeleteEntityExport {}

    /**
     * Tool to verify that a bunch of paths actually exist in the system, to detect problems (SCIPIO)
     */
    @Service(
        name = "validateSystemLocations",
        location = "com.ilscipio.scipio.webtools.WebToolsServices",
        invoke = "validateSystemLocations",
        description = "Tool to verify that a bunch of paths actually exist in the system, to detect problems (SCIPIO)",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "pathList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "paths", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "classNameList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "classNames", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "pathResults", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "classNameResults", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "entityMaintPermCheck", mainAction = "VIEW")
    )
    public interface ValidateSystemLocations {}

    /**
     * Update or create category content based on an Excel file
     */
    @Service(
        name = "excelI18nImport",
        location = "com.ilscipio.scipio.util.CatalogImportExportServices",
        invoke = "excelI18nImport",
        description = "Update or create category content based on an Excel file",
        useTransaction = "false",
        log = "quiet",
        logEca = "quiet",
        hideResultInLog = "true",
        implemented = {@Implements(service = "uploadFileInterface", optional = "true")},
        attributes = {
            @Attribute(name = "serviceMode", type = "String", mode = "IN", optional = "true", defaultValue = "sync", description = "One of: sync, async, async-persist"),
            @Attribute(name = "startRow", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "endRow", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "templateName", type = "String", mode = "IN"),
            @Attribute(name = "logLevel", type = "String", mode = "IN", optional = "true", defaultValue = "info", description = "Service-relative log level; pne of: info, verbose, important, warning")
        }
    )
    public interface ExcelI18nImport {}

}
