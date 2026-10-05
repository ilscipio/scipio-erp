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
package com.ilscipio.scipio.cms.service;

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
        name = "cmsGenericPermission",
        engine = "simple",
        location = "component://cms/script/com/ilscipio/scipio/cms/CmsServices.xml",
        invoke = "cmsGenericPermission",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface CmsGenericPermission {}

    /**
     * CMS data export functionality formatting options
     */
    @Service(
        name = "cmsExportDataFormatInterface",
        engine = "interface",
        description = "CMS data export functionality formatting options",
        attributes = {
            @Attribute(name = "outputMode", type = "String", mode = "IN", description = "Output mode - one of: SF_IL (inline result as resultText),\n                SF_FS (single file on server), MF_FS (multi file on server),\n                SF_DL (no result; causes executeExport=false and caller must use worker)"),
            @Attribute(name = "executeExport", type = "Boolean", mode = "IN", optional = "true", description = "If true, perform the export immediately; if false, the dataExportWorker\n                is created, prepared and returned, but caller is responsible for\n                invoking the right executeExport method (default: true)"),
            @Attribute(name = "outPath", type = "String", mode = "IN", optional = "true", description = "Output location, either file for SF_FS or directory for MF_FS"),
            @Attribute(name = "maxRecordsPerFile", type = "Integer", mode = "IN", optional = "true", description = "For multi-file output: max records per file (default: 0 - unlimited)"),
            @Attribute(name = "resultText", type = "String", mode = "OUT", optional = "true", description = "Inline output for SF_DL mode"),
            @Attribute(name = "numberOfEntities", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "numberWritten", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "dataExportWorker", type = "com.ilscipio.scipio.cms.data.importexport.CmsDataExportWorker", mode = "OUT", optional = "true", description = "Returns the data export worker instance used to generate the query")
        }
    )
    public interface CmsExportDataFormatInterface {}

    /**
     * CMS data export functionality processing options
     */
    @Service(
        name = "cmsExportDataOptionsInterface",
        engine = "interface",
        description = "CMS data export functionality processing options",
        attributes = {
            @Attribute(name = "presetConfigName", type = "String", mode = "IN", optional = "true", description = "Name of a preset config that will be used to pre-fill other values such as targetEntityNames and influence behaviors"),
            @Attribute(name = "recordGrouping", type = "String", mode = "IN", optional = "true", description = "Method by which the entities will be traversed and grouped - one of: NONE (default), ENTITY_TYPE, MAJOR_OBJECT"),
            @Attribute(name = "targetEntityNames", type = "List", mode = "IN", optional = "true", description = "The entity names to query as part of the main query; NOTE: For MAJOR_OBJECT grouping only Major entities from this list are used, while non-major ones listed have no effect"),
            @Attribute(name = "exportFilesAsTextData", type = "Boolean", mode = "IN", optional = "true", description = "If true, all template files (DataResouce.objectInfo component://) will be read and output as\n                ElectronicText records (default: false)"),
            @Attribute(name = "includeContentRefs", type = "Boolean", mode = "IN", optional = "true", description = "If true, Content/DataResource/Electronic records are output; if false, they are omitted\n                entirely (default: true)"),
            @Attribute(name = "transTimeout", type = "Integer", mode = "IN", optional = "true", description = "Transaction timeout, for each main entity query (default: 3600)"),
            @Attribute(name = "entityCond", type = "org.ofbiz.entity.condition.EntityCondition", mode = "IN", optional = "true", description = "Condition applied to every entity in the main query;\n                NOTE: For MAJOR_OBJECT grouping, this is only applied to the main query entities and not the lateral-visiting related entities"),
            @Attribute(name = "entityDateCond", type = "org.ofbiz.entity.condition.EntityCondition", mode = "IN", optional = "true", description = "Date condition applied to every entity in the main query that supports timestamps;\n                NOTE: For MAJOR_OBJECT grouping, this is only applied to the main query entities and not the lateral-visiting related entities"),
            @Attribute(name = "entityCondMap", type = "Map", mode = "IN", optional = "true", description = "Map of entity name to conditions to apply to it, for entities in the main query;\n                NOTE: For MAJOR_OBJECT grouping, this is only applied to the main query entities and not the lateral-visiting related entities"),
            @Attribute(name = "mainEfo", type = "org.ofbiz.entity.util.EntityFindOptions", mode = "IN", optional = "true", description = "EntityFindOptions to use on the main queries"),
            @Attribute(name = "useCommonEfo", type = "Boolean", mode = "IN", optional = "true", description = "If true, applies a common set of EntityFindOptions to the query; if false, does not apply any common find settings (default: false)"),
            @Attribute(name = "entityPkMap", type = "Map", mode = "IN", optional = "true", description = "Map of entity names to single-field PK values to target; WARN: only supports entities with single-field PK"),
            @Attribute(name = "entityNoPkFindAll", type = "Boolean", mode = "IN", optional = "true", description = "If true, entities part of targetEntityNames having no entries in entityPkMap or singlePkfEntityName\n                simply do a find-all operation (no PK filter); if false, entities not named are excluded from\n                the main query; WARN: entityNoPkFindAll=false only works properly for recordGrouping==MAJOR_OBJECT (default: true)"),
            @Attribute(name = "singlePkfEntityName", type = "String", mode = "IN", optional = "true", description = "Same as entityPkMap, but designates a single entity name whose PK values should be\n                listed in singlePkfIdList"),
            @Attribute(name = "singlePkfIdList", type = "List", mode = "IN", optional = "true", description = "Single-field PK values to target for the entity named by singlePkfEntityName; WARN: only supports entities with single-field PK"),
            @Attribute(name = "attribTmplAssocType", type = "String", mode = "IN", optional = "true", description = "For CmsAttributeTemplate main query, PAGE_TEMPLATE returns only page template attributes while\n                ASSET_TEMPLATE returns only asset template attributes - one of: PAGE_TEMPLATE, ASSET_TEMPLATE"),
            @Attribute(name = "pmpsMappingTypeId", type = "String", mode = "IN", optional = "true", description = "For CmsProcessMapping/CmsPageSpecialMapping main query, controls whether to include\n                only special page mappings (), "),
            @Attribute(name = "includeMajorDeps", type = "Boolean", mode = "IN", optional = "true", description = "For MAJOR_OBJECT grouping: If true, during lateral traversal (through entity relations) we will\n                include dependencies from Major entities other than the main queried one; if false, we\n                only query the main major entity and its non-major related entities (default: false)"),
            @Attribute(name = "enterPresetConfigName", type = "String", mode = "IN", optional = "true", description = "For MAJOR_OBJECT grouping: Name of a preset config whose entity names will be used to pre-fill\n                the enterEntityNames parameter specifically"),
            @Attribute(name = "enterEntityNames", type = "List", mode = "IN", optional = "true", description = "For MAJOR_OBJECT grouping: Limits which generic (non-major) entities we visit\n                laterally (through entity relations) to the specified entities"),
            @Attribute(name = "enterMajorPresetConfigName", type = "String", mode = "IN", optional = "true", description = "For MAJOR_OBJECT grouping: Name of a preset config whose entity names will be used to pre-fill\n                the enterMajorEntityNames parameter specifically"),
            @Attribute(name = "enterMajorEntityNames", type = "List", mode = "IN", optional = "true", description = "For MAJOR_OBJECT grouping: When includeMajorDeps==true, limits which Major entities we visit\n                laterally (through entity relations) to the specified entities")
        }
    )
    public interface CmsExportDataOptionsInterface {}

    /**
     * TODO: NOT IMPLEMENTED - CMS versatile data export service - supports inline, single file and multi-file
     */
    @Service(
        name = "cmsExportDataAsXml",
        engine = "java",
        location = "com.ilscipio.scipio.cms.data.importexport.CmsImportExportServices",
        invoke = "exportDataAsXml",
        description = "TODO: NOT IMPLEMENTED - CMS versatile data export service - supports inline, single file and multi-file",
        auth = "true",
        implemented = {@Implements(service = "cmsExportDataFormatInterface"), @Implements(service = "cmsExportDataOptionsInterface")},
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsExportDataAsXml {}

    /**
     * CMS data export service - returns the XML inline as output string
     */
    @Service(
        name = "cmsExportDataAsXmlInline",
        engine = "java",
        location = "com.ilscipio.scipio.cms.data.importexport.CmsImportExportServices",
        invoke = "exportDataAsXmlInline",
        description = "CMS data export service - returns the XML inline as output string",
        auth = "true",
        implemented = {@Implements(service = "cmsExportDataOptionsInterface")},
        attributes = {
            @Attribute(name = "resultText", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsExportDataAsXmlInline {}

    /**
     * Imports CMS data
     */
    @Service(
        name = "cmsImportXmlData",
        engine = "java",
        location = "com.ilscipio.scipio.cms.data.importexport.CmsImportExportServices",
        invoke = "importXmlData",
        description = "Imports CMS data",
        auth = "true",
        useTransaction = "false",
        implemented = {@Implements(service = "entityImport")},
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsImportXmlData {}

    /**
     * Adds a page and creates an empty first page template version to it
     */
    @Service(
        name = "cmsCreatePage",
        engine = "java",
        location = "com.ilscipio.scipio.cms.content.CmsPageServices",
        invoke = "createPage",
        description = "Adds a page and creates an empty first page template version to it",
        auth = "true",
        attributes = {
            @Attribute(name = "primaryPath", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "primaryPathFromContextRoot", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "primaryTargetPath", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "path", type = "String", mode = "INOUT", optional = "true", description = "DEPRECATED - use primaryPath"),
            @Attribute(name = "webSiteId", type = "String", mode = "INOUT"),
            @Attribute(name = "pageTemplateId", type = "String", mode = "IN"),
            @Attribute(name = "pageName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "txTimeout", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pageId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "CREATE")
    )
    public interface CmsCreatePage {}

    /**
     * Copies a page
     */
    @Service(
        name = "cmsCopyPage",
        engine = "java",
        location = "com.ilscipio.scipio.cms.content.CmsPageServices",
        invoke = "copyPage",
        description = "Copies a page",
        auth = "true",
        attributes = {
            @Attribute(name = "srcPageId", type = "String", mode = "IN"),
            @Attribute(name = "srcVersionId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "INOUT"),
            @Attribute(name = "primaryPath", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "primaryPathFromContextRoot", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pageName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pageId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "CREATE")
    )
    public interface CmsCopyPage {}

    @Service(
        name = "cmsUpdatePageInfo",
        engine = "java",
        location = "com.ilscipio.scipio.cms.content.CmsPageServices",
        invoke = "updatePageInfo",
        auth = "true",
        attributes = {
            @Attribute(name = "pageId", type = "String", mode = "IN"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "primaryPath", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "primaryPathFromContextRoot", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "primaryTargetPath", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "primaryPathIndexable", type = "String", mode = "IN", optional = "true", description = "The sitemap-indexable override indicator flag, for the page's primary process mapping"),
            @Attribute(name = "searchIndexable", type = "String", mode = "IN", optional = "true", description = "The search-indexable override indicator flag, can be used by various internal search engines"),
            @Attribute(name = "pageTemplateId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "txTimeout", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pageName", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsUpdatePageInfo {}

    /**
     * Gets a page from the server. Either requires pageId or primaryPath+websiteId to be specified.
     */
    @Service(
        name = "cmsGetPage",
        engine = "java",
        location = "com.ilscipio.scipio.cms.content.CmsPageServices",
        invoke = "getPage",
        description = "Gets a page from the server. Either requires pageId or primaryPath+websiteId to be specified.",
        auth = "true",
        attributes = {
            @Attribute(name = "request", type = "javax.servlet.http.HttpServletRequest", mode = "IN"),
            @Attribute(name = "response", type = "javax.servlet.http.HttpServletResponse", mode = "IN"),
            @Attribute(name = "pageId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "primaryPath", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "verifyWebSite", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "useStaticWebSite", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "versionId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pageId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "pageModel", type = "com.ilscipio.scipio.cms.content.CmsPage", mode = "OUT", optional = "true"),
            @Attribute(name = "permission", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "content", type = "Map", mode = "OUT", optional = "true"),
            @Attribute(name = "meta", type = "Map", mode = "OUT", optional = "true"),
            @Attribute(name = "variables", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "visitors", type = "String[]", mode = "OUT", optional = "true"),
            @Attribute(name = "template", type = "Map", mode = "OUT", optional = "true"),
            @Attribute(name = "versionId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "scriptTemplates", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetPage {}

    /**
     * Returns a list of available pages, including their description.
     */
    @Service(
        name = "cmsGetPages",
        engine = "java",
        location = "com.ilscipio.scipio.cms.content.CmsPageServices",
        invoke = "getPages",
        description = "Returns a list of available pages, including their description.",
        auth = "true",
        attributes = {
            @Attribute(name = "request", type = "javax.servlet.http.HttpServletRequest", mode = "IN", optional = "true"),
            @Attribute(name = "response", type = "javax.servlet.http.HttpServletResponse", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "short", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "editable", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "pages", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetPages {}

    /**
     * Returns a list of WebSites that are hooked into CMS system.
     */
    @Service(
        name = "cmsGetCmsWebSites",
        engine = "java",
        location = "com.ilscipio.scipio.cms.CmsServices",
        invoke = "getCmsWebSites",
        description = "Returns a list of WebSites that are hooked into CMS system.",
        auth = "true",
        attributes = {
            @Attribute(name = "webSiteList", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "webSiteIdSet", type = "Set", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetCmsWebSites {}

    /**
     * Sets a specific version of the page as live version.
     */
    @Service(
        name = "cmsActivatePageVersion",
        engine = "java",
        location = "com.ilscipio.scipio.cms.content.CmsPageServices",
        invoke = "activatePageVersion",
        description = "Sets a specific version of the page as live version.",
        auth = "true",
        attributes = {
            @Attribute(name = "pageId", type = "String", mode = "IN"),
            @Attribute(name = "versionId", type = "String", mode = "IN"),
            @Attribute(name = "pageId", type = "String", mode = "OUT"),
            @Attribute(name = "versionId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsActivatePageVersion {}

    /**
     * Unpublish page.
     */
    @Service(
        name = "cmsUnpublishPage",
        engine = "java",
        location = "com.ilscipio.scipio.cms.content.CmsPageServices",
        invoke = "unpublishPage",
        description = "Unpublish page.",
        auth = "true",
        attributes = {
            @Attribute(name = "pageId", type = "String", mode = "INOUT")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsUnpublishPage {}

    /**
     * Adds a new version of a page.
     */
    @Service(
        name = "cmsAddPageVersion",
        engine = "java",
        location = "com.ilscipio.scipio.cms.content.CmsPageServices",
        invoke = "addPageVersion",
        description = "Adds a new version of a page.",
        auth = "true",
        attributes = {
            @Attribute(name = "request", type = "javax.servlet.http.HttpServletRequest", mode = "IN", optional = "true"),
            @Attribute(name = "pageId", type = "String", mode = "IN"),
            @Attribute(name = "content", type = "String", mode = "IN", allowHtml = "any"),
            @Attribute(name = "comment", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "versionId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsAddPageVersion {}

    @Service(
        name = "cmsDeletePage",
        engine = "java",
        location = "com.ilscipio.scipio.cms.content.CmsPageServices",
        invoke = "deletePage",
        auth = "true",
        attributes = {
            @Attribute(name = "pageId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeletePage {}

    @Service(
        name = "cmsUpdateScriptAssocInterface",
        engine = "interface",
        attributes = {
            @Attribute(name = "scriptAssocId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "scriptTemplateId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inputPosition", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "templateName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "scriptLang", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateLocation", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "standalone", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateBody", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateSource", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "invokeName", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CmsUpdateScriptAssocInterface {}

    @Service(
        name = "cmsUpdatePageScript",
        engine = "java",
        location = "com.ilscipio.scipio.cms.content.CmsPageServices",
        invoke = "updatePageScript",
        auth = "true",
        implemented = {@Implements(service = "cmsUpdateScriptAssocInterface")},
        attributes = {
            @Attribute(name = "pageId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsUpdatePageScript {}

    @Service(
        name = "cmsDeletePageScriptAssoc",
        engine = "entity-auto",
        invoke = "delete",
        defaultEntityName = "CmsPageScriptAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeletePageScriptAssoc {}

    @Service(
        name = "cmsDeleteScriptAndPageAssoc",
        engine = "group",
        auth = "true",
        invokes = {@GroupInvoke(name = "cmsDeletePageScriptAssoc", resultToContext = "false"), @GroupInvoke(name = "cmsDeleteScriptTemplateIfOrphan", resultToContext = "false")}
    )
    public interface CmsDeleteScriptAndPageAssoc {}

    /**
     * Creates a new PageTemplate
     */
    @Service(
        name = "cmsCreatePageTemplate",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsPageTemplateServices",
        invoke = "createPageTemplate",
        description = "Creates a new PageTemplate",
        auth = "true",
        attributes = {
            @Attribute(name = "pageTemplateId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "templateName", type = "String", mode = "IN"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateBody", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "templateLocation", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateSource", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "txTimeout", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "CREATE")
    )
    public interface CmsCreatePageTemplate {}

    /**
     * Copies a PageTemplate
     */
    @Service(
        name = "cmsCopyPageTemplate",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsPageTemplateServices",
        invoke = "copyPageTemplate",
        description = "Copies a PageTemplate",
        auth = "true",
        attributes = {
            @Attribute(name = "srcPageTemplateId", type = "String", mode = "IN"),
            @Attribute(name = "srcVersionId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateName", type = "String", mode = "IN"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pageTemplateId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "CREATE")
    )
    public interface CmsCopyPageTemplate {}

    /**
     * Updates basic page template info (only that can be changed).
     */
    @Service(
        name = "cmsUpdatePageTemplateInfo",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsPageTemplateServices",
        invoke = "updatePageTemplateInfo",
        description = "Updates basic page template info (only that can be changed).",
        auth = "true",
        attributes = {
            @Attribute(name = "pageTemplateId", type = "String", mode = "IN"),
            @Attribute(name = "templateName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "txTimeout", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsUpdatePageTemplateInfo {}

    /**
     * Gets a page template
     */
    @Service(
        name = "cmsGetPageTemplate",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsPageTemplateServices",
        invoke = "getPageTemplate",
        description = "Gets a page template",
        auth = "true",
        attributes = {
            @Attribute(name = "pageTemplateId", type = "String", mode = "IN"),
            @Attribute(name = "pageTemplate", type = "Map", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetPageTemplate {}

    /**
     * Gets a page template version.
     */
    @Service(
        name = "cmsGetPageTemplateVersion",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsPageTemplateServices",
        invoke = "getPageTemplateVersion",
        description = "Gets a page template version.",
        auth = "true",
        attributes = {
            @Attribute(name = "versionId", type = "String", mode = "IN"),
            @Attribute(name = "pageTemplateId", type = "String", mode = "IN"),
            @Attribute(name = "minimalInfoOnly", type = "String", mode = "IN", optional = "true", defaultValue = "N", description = "If true, only gets basic info and template body of version (optimization hint)"),
            @Attribute(name = "version", type = "Map", mode = "OUT", optional = "true", description = "Map representing the template version. For available fields,\n            refer to com.ilscipio.scipio.cms.template.CmsPageTemplateVersion.putVersionFieldsIntoMap.")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetPageTemplateVersion {}

    /**
     * Gets all page template versions.
     */
    @Service(
        name = "cmsGetPageTemplateVersions",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsPageTemplateServices",
        invoke = "getPageTemplateVersions",
        description = "Gets all page template versions.",
        auth = "true",
        attributes = {
            @Attribute(name = "pageTemplateId", type = "String", mode = "IN"),
            @Attribute(name = "minimalInfoOnly", type = "String", mode = "IN", optional = "true", defaultValue = "N", description = "If true, only gets basic info and template body of version (optimization hint)"),
            @Attribute(name = "versions", type = "List", mode = "OUT", optional = "true", description = "A list of maps, one per page template version. For available fields,\n            refer to com.ilscipio.scipio.cms.template.CmsPageTemplateVersion.putVersionFieldsIntoMap.")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetPageTemplateVersions {}

    /**
     * Gets a structure containing the page template and all its versions.
     */
    @Service(
        name = "cmsGetPageTemplateAndVersions",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsPageTemplateServices",
        invoke = "getPageTemplateAndVersions",
        description = "Gets a structure containing the page template and all its versions.",
        auth = "true",
        attributes = {
            @Attribute(name = "pageTemplateId", type = "String", mode = "IN"),
            @Attribute(name = "versionId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "currentVersionId", type = "String", mode = "IN", optional = "true", description = "Optional ID of a version to mark as 'isCurrent' in the returned structured. The\n            the significance of 'current' is up to the caller."),
            @Attribute(name = "useMarkedAsCurrent", type = "String", mode = "IN", optional = "true", description = "If set and currentVersionId is not found or not specified, will mark as 'current' the\n            version that satisfies the mark specified in this parameter (one of isActive, isLast, isFirst, etc.)"),
            @Attribute(name = "pageTmpAndVersions", type = "Map", mode = "OUT", optional = "true", description = "A map representing the template, a field named 'pageTmpVersions' containing a map with versions current,\n            active, last, first, and all (list - ordered last to first), and more")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetPageTemplateAndVersions {}

    /**
     * Creates a new page template version in the repository.
     */
    @Service(
        name = "cmsAddPageTemplateVersion",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsPageTemplateServices",
        invoke = "addPageTemplateVersion",
        description = "Creates a new page template version in the repository.",
        auth = "true",
        attributes = {
            @Attribute(name = "pageTemplateId", type = "String", mode = "IN"),
            @Attribute(name = "versionComment", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateBody", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "templateLocation", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateSource", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "versionId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsAddPageTemplateVersion {}

    /**
     * Gets a page template
     */
    @Service(
        name = "cmsGetAvailablePageTemplates",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsPageTemplateServices",
        invoke = "getAvailablePageTemplates",
        description = "Gets a page template",
        auth = "true",
        attributes = {
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pageTemplates", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetAvailablePageTemplates {}

    /**
     * Sets a page template version as live version.
     */
    @Service(
        name = "cmsActivatePageTemplateVersion",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsPageTemplateServices",
        invoke = "activatePageTemplateVersion",
        description = "Sets a page template version as live version.",
        auth = "true",
        attributes = {
            @Attribute(name = "pageTemplateId", type = "String", mode = "INOUT"),
            @Attribute(name = "versionId", type = "String", mode = "INOUT")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsActivatePageTemplateVersion {}

    /**
     * Update or add an asset to a page template
     */
    @Service(
        name = "cmsCreateUpdateAssetAssoc",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsPageTemplateServices",
        invoke = "createUpdateAssetAssoc",
        description = "Update or add an asset to a page template",
        auth = "true",
        attributes = {
            @Attribute(name = "pageAssetTemplateAssocId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pageTemplateId", type = "String", mode = "IN"),
            @Attribute(name = "assetTemplateId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "importName", type = "String", mode = "IN"),
            @Attribute(name = "displayName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inputPosition", type = "Long", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsCreateUpdateAssetAssoc {}

    @Service(
        name = "cmsDeleteAssetAssoc",
        engine = "entity-auto",
        invoke = "delete",
        defaultEntityName = "CmsPageTemplateAssetAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeleteAssetAssoc {}

    @Service(
        name = "cmsUpdatePageTemplateScript",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsPageTemplateServices",
        invoke = "updatePageTemplateScript",
        auth = "true",
        implemented = {@Implements(service = "cmsUpdateScriptAssocInterface")},
        attributes = {
            @Attribute(name = "pageTemplateId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsUpdatePageTemplateScript {}

    @Service(
        name = "cmsDeletePageTemplateScriptAssoc",
        engine = "entity-auto",
        invoke = "delete",
        defaultEntityName = "CmsPageTemplateScriptAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeletePageTemplateScriptAssoc {}

    @Service(
        name = "cmsDeleteScriptAndPageTemplateAssoc",
        engine = "group",
        auth = "true",
        invokes = {@GroupInvoke(name = "cmsDeletePageTemplateScriptAssoc", resultToContext = "false"), @GroupInvoke(name = "cmsDeleteScriptTemplateIfOrphan", resultToContext = "false")}
    )
    public interface CmsDeleteScriptAndPageTemplateAssoc {}

    @Service(
        name = "cmsDeletePageTemplate",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsPageTemplateServices",
        invoke = "deletePageTemplate",
        auth = "true",
        attributes = {
            @Attribute(name = "pageTemplateId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeletePageTemplate {}

    /**
     * Creates a new asset
     */
    @Service(
        name = "cmsCreateUpdateAsset",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsAssetTemplateServices",
        invoke = "createUpdateAsset",
        description = "Creates a new asset",
        auth = "true",
        attributes = {
            @Attribute(name = "assetTemplateId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "assetType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateBody", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "templateLocation", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateSource", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "active", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "txTimeout", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsCreateUpdateAsset {}

    /**
     * Creates a new asset
     */
    @Service(
        name = "cmsCopyAsset",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsAssetTemplateServices",
        invoke = "copyAsset",
        description = "Creates a new asset",
        auth = "true",
        attributes = {
            @Attribute(name = "srcAssetTemplateId", type = "String", mode = "IN"),
            @Attribute(name = "templateName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "assetTemplateId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsCopyAsset {}

    @Service(
        name = "cmsUpdateAssetTemplateInfo",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsAssetTemplateServices",
        invoke = "updateAssetTemplateInfo",
        auth = "true",
        attributes = {
            @Attribute(name = "assetTemplateId", type = "String", mode = "IN"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "txTimeout", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsUpdateAssetTemplateInfo {}

    /**
     * Gets all asset template types (ContentType).
     */
    @Service(
        name = "cmsGetAssetTemplateTypes",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsAssetTemplateServices",
        invoke = "getAssetTemplateTypes",
        description = "Gets all asset template types (ContentType).",
        auth = "true",
        attributes = {
            @Attribute(name = "deep", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "contentTypeValues", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetAssetTemplateTypes {}

    /**
     * Gets all asset templates, or limited to a certain type and/or webSiteId.
     */
    @Service(
        name = "cmsGetAssetTemplates",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsAssetTemplateServices",
        invoke = "getAssetTemplates",
        description = "Gets all asset templates, or limited to a certain type and/or webSiteId.",
        auth = "true",
        attributes = {
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteOptional", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "contentTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "assetTemplateValues", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetAssetTemplates {}

    /**
     * Gets all asset template attributes for an asset template.
     */
    @Service(
        name = "cmsGetAssetTemplateAttributes",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsAssetTemplateServices",
        invoke = "getAssetTemplateAttributes",
        description = "Gets all asset template attributes for an asset template.",
        auth = "true",
        attributes = {
            @Attribute(name = "assetTemplateId", type = "String", mode = "IN"),
            @Attribute(name = "attributeValues", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetAssetTemplateAttributes {}

    /**
     * Gets an asset template.
     */
    @Service(
        name = "cmsGetAssetTemplate",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsAssetTemplateServices",
        invoke = "getAssetTemplate",
        description = "Gets an asset template.",
        auth = "true",
        attributes = {
            @Attribute(name = "assetTemplateId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "assetTemplate", type = "com.ilscipio.scipio.cms.template.CmsAssetTemplate", mode = "OUT", optional = "true"),
            @Attribute(name = "assetTemplateValue", type = "java.util.Map", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetAssetTemplate {}

    /**
     * Gets the active/default asset template version of the given asset template.
     */
    @Service(
        name = "cmsGetActiveAssetTemplateVersion",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsAssetTemplateServices",
        invoke = "getActiveAssetTemplateVersion",
        description = "Gets the active/default asset template version of the given asset template.",
        auth = "true",
        attributes = {
            @Attribute(name = "webSiteId", type = "String", mode = "IN"),
            @Attribute(name = "templateName", type = "String", mode = "IN"),
            @Attribute(name = "assetTemplate", type = "java.util.Map", mode = "OUT", optional = "true"),
            @Attribute(name = "assetTemplateModel", type = "com.ilscipio.scipio.cms.template.CmsAssetTemplate", mode = "OUT", optional = "true"),
            @Attribute(name = "assetTemplateVersion", type = "java.util.Map", mode = "OUT", optional = "true"),
            @Attribute(name = "assetTemplateVersionModel", type = "com.ilscipio.scipio.cms.template.CmsAssetTemplateVersion", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetActiveAssetTemplateVersion {}

    /**
     * Gets the active/default asset template version related to the given asset template version.
     */
    @Service(
        name = "cmsGetRelatedActiveAssetTemplateVersion",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsAssetTemplateServices",
        invoke = "getRelatedActiveAssetTemplateVersion",
        description = "Gets the active/default asset template version related to the given asset template version.",
        auth = "true",
        attributes = {
            @Attribute(name = "relatedVersionId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "assetTemplateId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "assetTemplate", type = "java.util.Map", mode = "OUT", optional = "true"),
            @Attribute(name = "assetTemplateModel", type = "com.ilscipio.scipio.cms.template.CmsAssetTemplate", mode = "OUT", optional = "true"),
            @Attribute(name = "assetTemplateVersion", type = "java.util.Map", mode = "OUT", optional = "true"),
            @Attribute(name = "assetTemplateVersionModel", type = "com.ilscipio.scipio.cms.template.CmsAssetTemplateVersion", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetRelatedActiveAssetTemplateVersion {}

    /**
     * Sets an asset template version as live version.
     */
    @Service(
        name = "cmsActivateAssetTemplateVersion",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsAssetTemplateServices",
        invoke = "activateAssetTemplateVersion",
        description = "Sets an asset template version as live version.",
        auth = "true",
        attributes = {
            @Attribute(name = "versionId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsActivateAssetTemplateVersion {}

    @Service(
        name = "cmsUpdateAssetTemplateScript",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsAssetTemplateServices",
        invoke = "updateAssetTemplateScript",
        auth = "true",
        implemented = {@Implements(service = "cmsUpdateScriptAssocInterface")},
        attributes = {
            @Attribute(name = "assetTemplateId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsUpdateAssetTemplateScript {}

    @Service(
        name = "cmsDeleteAssetTemplateScriptAssoc",
        engine = "entity-auto",
        invoke = "delete",
        defaultEntityName = "CmsAssetTemplateScriptAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeleteAssetTemplateScriptAssoc {}

    @Service(
        name = "cmsDeleteScriptAndAssetTemplateAssoc",
        engine = "group",
        auth = "true",
        invokes = {@GroupInvoke(name = "cmsDeleteAssetTemplateScriptAssoc", resultToContext = "false"), @GroupInvoke(name = "cmsDeleteScriptTemplateIfOrphan", resultToContext = "false")}
    )
    public interface CmsDeleteScriptAndAssetTemplateAssoc {}

    @Service(
        name = "cmsDeleteAssetTemplate",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsAssetTemplateServices",
        invoke = "deleteAssetTemplate",
        auth = "true",
        attributes = {
            @Attribute(name = "assetTemplateId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeleteAssetTemplate {}

    /**
     * Creates a new asset
     */
    @Service(
        name = "cmsCreateUpdateScriptTemplate",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsScriptTemplateServices",
        invoke = "createUpdateScriptTemplate",
        description = "Creates a new asset",
        auth = "true",
        attributes = {
            @Attribute(name = "scriptTemplateId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "scriptLang", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateBody", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "templateLocation", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateSource", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "standalone", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "active", type = "Boolean", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsCreateUpdateScriptTemplate {}

    /**
     * Creates a new asset
     */
    @Service(
        name = "cmsCopyScriptTemplate",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsScriptTemplateServices",
        invoke = "copyScriptTemplate",
        description = "Creates a new asset",
        auth = "true",
        attributes = {
            @Attribute(name = "srcScriptTemplateId", type = "String", mode = "IN"),
            @Attribute(name = "templateName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "scriptTemplateId", type = "String", mode = "INOUT")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsCopyScriptTemplate {}

    @Service(
        name = "cmsUpdateScriptTemplateInfo",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsScriptTemplateServices",
        invoke = "updateScriptTemplateInfo",
        auth = "true",
        attributes = {
            @Attribute(name = "scriptTemplateId", type = "String", mode = "IN"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsUpdateScriptTemplateInfo {}

    @Service(
        name = "cmsGetScriptTemplate",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsScriptTemplateServices",
        invoke = "getScriptTemplate",
        auth = "true",
        attributes = {
            @Attribute(name = "scriptTemplateId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "scriptTemplate", type = "com.ilscipio.scipio.cms.template.CmsScriptTemplate", mode = "OUT", optional = "true"),
            @Attribute(name = "scriptTemplateValue", type = "java.util.Map", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetScriptTemplate {}

    /**
     * Deletes the CmsScriptTemplate record and any associations pointing to it.
     */
    @Service(
        name = "cmsDeleteScriptTemplate",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsScriptTemplateServices",
        invoke = "deleteScriptTemplate",
        description = "Deletes the CmsScriptTemplate record and any associations pointing to it.",
        auth = "true",
        attributes = {
            @Attribute(name = "scriptTemplateId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeleteScriptTemplate {}

    /**
     * Deletes the CmsScriptTemplate record only if it is marked non-standalone and there are no more associations pointing to it.
     */
    @Service(
        name = "cmsDeleteScriptTemplateIfOrphan",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsScriptTemplateServices",
        invoke = "deleteScriptTemplateIfOrphan",
        description = "Deletes the CmsScriptTemplate record only if it is marked non-standalone and there are no more associations pointing to it.",
        auth = "true",
        attributes = {
            @Attribute(name = "scriptTemplateId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeleteScriptTemplateIfOrphan {}

    @Service(
        name = "cmsCreateUpdateAttribute",
        engine = "java",
        location = "com.ilscipio.scipio.cms.template.CmsTemplateServices",
        invoke = "createUpdateAttribute",
        auth = "true",
        attributes = {
            @Attribute(name = "request", type = "javax.servlet.http.HttpServletRequest", mode = "IN", optional = "true"),
            @Attribute(name = "attributeTemplateId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "assetTemplateId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pageTemplateId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "attributeName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "displayName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "defaultValue", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inputHelp", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inputType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inputPosition", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "maxLength", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "inputType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "required", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "expandLang", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "expandPosition", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "targetType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "regularExpression", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "inheritMode", type = "String", mode = "IN", optional = "true", allowHtml = "any")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsCreateUpdateAttribute {}

    @Service(
        name = "cmsDeleteAttribute",
        engine = "entity-auto",
        invoke = "delete",
        defaultEntityName = "CmsAttributeTemplate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeleteAttribute {}

    @Service(
        name = "cmsUploadMediaFileInterface",
        engine = "interface",
        attributes = {
            @Attribute(name = "uploadedFile", type = "java.nio.ByteBuffer", mode = "IN"),
            @Attribute(name = "contentName", type = "String", mode = "IN"),
            @Attribute(name = "dataResourceTypeId", type = "String", mode = "IN"),
            @Attribute(name = "_uploadedFile_size", type = "String", mode = "IN"),
            @Attribute(name = "_uploadedFile_fileName", type = "String", mode = "IN"),
            @Attribute(name = "_uploadedFile_contentType", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "localeString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "isPublic", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "autoVariants", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "mediaProfile", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentPath", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "imageVariantConfig", type = "org.ofbiz.common.image.ImageVariantConfig", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "dataResourceId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "dataResourceTypeId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CmsUploadMediaFileInterface {}

    /**
     * Imports and processes a media file and stores it in the database. Autodetects content-type, defaulting to Binary.
     */
    @Service(
        name = "cmsUploadMediaFile",
        engine = "java",
        location = "com.ilscipio.scipio.cms.media.CmsMediaServices",
        invoke = "uploadMediaFile",
        description = "Imports and processes a media file and stores it in the database. Autodetects content-type, defaulting to Binary.",
        auth = "true",
        transactionTimeout = "7200",
        implemented = {@Implements(service = "cmsUploadMediaFileInterface")},
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "CREATE")
    )
    public interface CmsUploadMediaFile {}

    /**
     * Imports and processes a image media file using custom variant sizes.
     */
    @Service(
        name = "cmsUploadMediaFileImageCustomVariantSizes",
        engine = "java",
        location = "com.ilscipio.scipio.cms.media.CmsMediaServices",
        invoke = "uploadMediaFileImageCustomVariantSizes",
        description = "Imports and processes a image media file using custom variant sizes.",
        auth = "true",
        transactionTimeout = "7200",
        implemented = {@Implements(service = "cmsUploadMediaFileInterface")},
        attributes = {
            @Attribute(name = "customVariantSizeMethod", type = "String", mode = "IN"),
            @Attribute(name = "customVariantSizesImgProps", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "customVariantSizesPreset", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "variantSizeName", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "variantSizeWidth", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "variantSizeHeight", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "variantSizeFormat", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "variantSizeUpscaleMode", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "variantSizeSequenceNum", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "saveAsPreset", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "presetName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "presetId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "parentProfile", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "srcsetModeEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "viewPortMediaQuery", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "viewPortLength", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "viewPortSequenceNum", type = "List", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "CREATE")
    )
    public interface CmsUploadMediaFileImageCustomVariantSizes {}

    @Service(
        name = "cmsUpdateMediaFile",
        engine = "java",
        location = "com.ilscipio.scipio.cms.media.CmsMediaServices",
        invoke = "updateMediaFile",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "dataResourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "isPublic", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "contentPath", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "dataResourceId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "dataResourceTypeId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsUpdateMediaFile {}

    @Service(
        name = "cmsRebuildMediaVariantList",
        engine = "java",
        location = "com.ilscipio.scipio.cms.media.CmsMediaServices",
        invoke = "rebuildMediaVariantList",
        auth = "true",
        transactionTimeout = "144000",
        attributes = {
            @Attribute(name = "contentIdList", type = "List", mode = "IN", description = "contentId of images for which to recreate their variants; if omitted, applies to all images (slow)"),
            @Attribute(name = "forceCreate", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If contentIdList omitted, if true, forces recreation of resized images for those that had none (default: false);\n                by default, only recreates for those that already had resized images"),
            @Attribute(name = "recreateExisting", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "If true (default), recreates existing, if false, only creates missing sizes"),
            @Attribute(name = "deleteOld", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "Not recommended unless corruption: delete the old ContentAssoc/Content/DataResource before creating new ones (old behavior)"),
            @Attribute(name = "sepTrans", type = "Boolean", mode = "IN", optional = "true", description = "If true, each contentId gets its own transaction (default: true if contentIdList is omitted, false if contentIdList is specified)"),
            @Attribute(name = "createdDate", type = "Timestamp", mode = "IN", optional = "true", description = "Optional createdDate for Content and DataResource"),
            @Attribute(name = "doLog", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "imageVariantConfig", type = "org.ofbiz.common.image.ImageVariantConfig", mode = "IN", optional = "true"),
            @Attribute(name = "customImageSizes", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "imgCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "successCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "skipCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "failCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "errorCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "variantSuccessCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "variantFailCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "variantSkipCount", type = "Integer", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsRebuildMediaVariantList {}

    /**
     * Recreates (deletes + creates) auto-resized images for specified or all images
     */
    @Service(
        name = "cmsRebuildMediaVariants",
        engine = "java",
        location = "com.ilscipio.scipio.cms.media.CmsMediaServices",
        invoke = "rebuildMediaVariants",
        description = "Recreates (deletes + creates) auto-resized images for specified or all images",
        auth = "true",
        transactionTimeout = "144000",
        semaphore = "fail",
        implemented = {@Implements(service = "cmsRebuildMediaVariantList")},
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "contentIdList", optional = "true"),
            @OverrideAttribute(name = "doLog", defaultValue = "true")
        }
    )
    public interface CmsRebuildMediaVariants {}

    /**
     * Aborts cmsRebuildMediaVariants at the next iterated image
     */
    @Service(
        name = "cmsAbortRebuildMediaVariants",
        engine = "java",
        location = "com.ilscipio.scipio.cms.media.CmsMediaServices",
        invoke = "abortRebuildMediaVariants",
        description = "Aborts cmsRebuildMediaVariants at the next iterated image",
        auth = "true",
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsAbortRebuildMediaVariants {}

    /**
     * Removes (deletes + creates) auto-resized images for specified or all images
     */
    @Service(
        name = "cmsRemoveMediaVariants",
        engine = "java",
        location = "com.ilscipio.scipio.cms.media.CmsMediaServices",
        invoke = "removeMediaVariants",
        description = "Removes (deletes + creates) auto-resized images for specified or all images",
        auth = "true",
        transactionTimeout = "14400",
        attributes = {
            @Attribute(name = "contentIdList", type = "List", mode = "IN", optional = "true", description = "contentId of images to apply; if omitted, applies to all images")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsRemoveMediaVariants {}

    @Service(
        name = "cmsDeleteMediaFile",
        engine = "java",
        location = "com.ilscipio.scipio.cms.media.CmsMediaServices",
        invoke = "deleteMediaFile",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "dataResourceId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeleteMediaFile {}

    /**
     * Creates a list of all available Media Files.
     */
    @Service(
        name = "cmsGetMediaFiles",
        engine = "java",
        location = "com.ilscipio.scipio.cms.media.CmsMediaServices",
        invoke = "getMediaFiles",
        description = "Creates a list of all available Media Files.",
        auth = "true",
        attributes = {
            @Attribute(name = "dataResourceTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inputFields", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "viewSize", type = "Integer", mode = "INOUT", optional = "true"),
            @Attribute(name = "viewIndex", type = "Integer", mode = "INOUT", optional = "true"),
            @Attribute(name = "listSize", type = "Integer", mode = "INOUT", optional = "true"),
            @Attribute(name = "mediaFiles", type = "Object", mode = "OUT")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetMediaFiles {}

    /**
     * Creates custom image media presets.
     */
    @Service(
        name = "cmsCreateCustomImageSizePreset",
        engine = "java",
        location = "com.ilscipio.scipio.cms.media.CmsMediaServices",
        invoke = "createCustomImageSizePreset",
        description = "Creates custom image media presets.",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "variantSizeName", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "variantSizeWidth", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "variantSizeHeight", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "variantSizeFormat", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "variantSizeUpscaleMode", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "variantSizeSequenceNum", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "presetName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "presetId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "parentProfile", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "CREATE")
    )
    public interface CmsCreateCustomImageSizePreset {}

    /**
     * Updates custom image media presets.
     */
    @Service(
        name = "cmsUpdateCustomImageSizePreset",
        engine = "entity-auto",
        invoke = "update",
        description = "Updates custom image media presets.",
        defaultEntityName = "ImageSizePreset",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsUpdateCustomImageSizePreset {}

    @Service(
        name = "cmsCmsPageInfoInterface",
        engine = "interface",
        attributes = {
            @Attribute(name = "pageId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CmsCmsPageInfoInterface {}

    /**
     * Create or update a process mapping
     */
    @Service(
        name = "cmsCreateUpdateProcessMapping",
        engine = "java",
        location = "com.ilscipio.scipio.cms.control.CmsControlDataServices",
        invoke = "createUpdateProcessMapping",
        description = "Create or update a process mapping",
        auth = "true",
        implemented = {@Implements(service = "cmsCmsPageInfoInterface")},
        entityAttributes = {
            @EntityAttributes(entityName = "CmsProcessMapping", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "CmsProcessMapping", mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsCreateUpdateProcessMapping {}

    /**
     * Delete a process mapping
     */
    @Service(
        name = "cmsDeleteProcessMapping",
        engine = "java",
        location = "com.ilscipio.scipio.cms.control.CmsControlDataServices",
        invoke = "deleteProcessMapping",
        description = "Delete a process mapping",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CmsProcessMapping", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeleteProcessMapping {}

    /**
     * Create or update a process view mapping, along with page info (if applicable); page relation is abstracted
     */
    @Service(
        name = "cmsCreateUpdateProcessViewMapping",
        engine = "java",
        location = "com.ilscipio.scipio.cms.control.CmsControlDataServices",
        invoke = "createUpdateProcessViewMapping",
        description = "Create or update a process view mapping, along with page info (if applicable); page relation is abstracted",
        auth = "true",
        implemented = {@Implements(service = "cmsCmsPageInfoInterface")},
        entityAttributes = {
            @EntityAttributes(entityName = "CmsProcessViewMapping", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "CmsProcessViewMapping", mode = "IN", include = "nonpk", optional = "true", excludeFields = {"targetPageId"})
        },
        attributes = {
            @Attribute(name = "webSiteId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsCreateUpdateProcessViewMapping {}

    /**
     * Delete a process view mapping, along with page info (if applicable)
     */
    @Service(
        name = "cmsDeleteProcessViewMapping",
        engine = "java",
        location = "com.ilscipio.scipio.cms.control.CmsControlDataServices",
        invoke = "deleteProcessViewMapping",
        description = "Delete a process view mapping, along with page info (if applicable)",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CmsProcessViewMapping", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeleteProcessViewMapping {}

    /**
     * Create or update a (simple) view mapping, along with page info (if applicable); page relation is abstracted
     */
    @Service(
        name = "cmsCreateUpdateViewMapping",
        engine = "java",
        location = "com.ilscipio.scipio.cms.control.CmsControlDataServices",
        invoke = "createUpdateViewMapping",
        description = "Create or update a (simple) view mapping, along with page info (if applicable); page relation is abstracted",
        auth = "true",
        implemented = {@Implements(service = "cmsCmsPageInfoInterface")},
        entityAttributes = {
            @EntityAttributes(entityName = "CmsViewMapping", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "CmsViewMapping", mode = "IN", include = "nonpk", optional = "true", excludeFields = {"pageId"})
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsCreateUpdateViewMapping {}

    /**
     * Delete a (simple) view mapping, along with page info (if applicable)
     */
    @Service(
        name = "cmsDeleteViewMapping",
        engine = "java",
        location = "com.ilscipio.scipio.cms.control.CmsControlDataServices",
        invoke = "deleteViewMapping",
        description = "Delete a (simple) view mapping, along with page info (if applicable)",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CmsViewMapping", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeleteViewMapping {}

    /**
     * Clear CMS mapping caches for all Servers listening to the topic (SCIPIO)
     */
    @Service(
        name = "cmsDistributedClearMappingCaches",
        engine = "jms",
        location = "serviceMessenger",
        invoke = "cmsClearMappingCaches",
        description = "Clear CMS mapping caches for all Servers listening to the topic (SCIPIO)",
        auth = "true",
        useTransaction = "false",
        log = "quiet",
        logEca = "quiet",
        attributes = {
            @Attribute(name = "clearMemoryCaches", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "clearEntityCaches", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true")
        }
    )
    public interface CmsDistributedClearMappingCaches {}

    /**
     * Clear front-end mapping caches
     */
    @Service(
        name = "cmsClearMappingCaches",
        engine = "java",
        location = "com.ilscipio.scipio.cms.control.CmsControlDataServices",
        invoke = "clearMappingCaches",
        description = "Clear front-end mapping caches",
        auth = "true",
        export = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "clearMemoryCaches", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "clearEntityCaches", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "distribute", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false")
        }
    )
    public interface CmsClearMappingCaches {}

    /**
     * Delete all Cms view, process, etc. mapping and related entity records from the system
     */
    @Service(
        name = "cmsDeleteAllMappingRecords",
        engine = "java",
        location = "com.ilscipio.scipio.cms.control.CmsControlDataServices",
        invoke = "deleteAllMappingRecords",
        description = "Delete all Cms view, process, etc. mapping and related entity records from the system",
        auth = "true",
        attributes = {
            @Attribute(name = "includePrimary", type = "Boolean", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeleteAllMappingRecords {}

    /**
     * Delete all Cms view, process, etc. mapping and related entity records from the system for a specific web site
     */
    @Service(
        name = "cmsDeleteWebSiteMappingRecords",
        engine = "java",
        location = "com.ilscipio.scipio.cms.control.CmsControlDataServices",
        invoke = "deleteWebSiteMappingRecords",
        description = "Delete all Cms view, process, etc. mapping and related entity records from the system for a specific web site",
        auth = "true",
        attributes = {
            @Attribute(name = "webSiteId", type = "String", mode = "IN"),
            @Attribute(name = "includePrimary", type = "Boolean", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeleteWebSiteMappingRecords {}

    /**
     * Adds and/or removes view mappings for a page.             Mappings are identified by webSiteId::viewName (if contains '::') or viewMappingId (otherwise).
     */
    @Service(
        name = "cmsAddRemovePageViewMappings",
        engine = "java",
        location = "com.ilscipio.scipio.cms.control.CmsControlDataServices",
        invoke = "addRemovePageViewMappings",
        description = "Adds and/or removes view mappings for a page.\n            Mappings are identified by webSiteId::viewName (if contains '::') or viewMappingId (otherwise).",
        auth = "true",
        attributes = {
            @Attribute(name = "pageId", type = "String", mode = "IN"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "addViewNameList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "removeViewNameList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "removeIdList", type = "List", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "UPDATE")
    )
    public interface CmsAddRemovePageViewMappings {}

    /**
     * Returns a list of active indexable process mapping (page) URIs for the website, normalized from the webapp context root (not including the webapp context root)
     */
    @Service(
        name = "cmsGetWebsiteIndexableProcessMappingUris",
        engine = "java",
        location = "com.ilscipio.scipio.cms.control.CmsControlDataServices",
        invoke = "getWebsiteIndexableProcessMappingUris",
        description = "Returns a list of active indexable process mapping (page) URIs for the website, normalized from the webapp context root (not including the webapp context root)",
        auth = "true",
        attributes = {
            @Attribute(name = "webSiteId", type = "String", mode = "IN"),
            @Attribute(name = "defaultLocale", type = "Locale", mode = "IN", optional = "true"),
            @Attribute(name = "contentLocale", type = "Locale", mode = "IN", optional = "true", description = "DEPRECATED"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "uriList", type = "List", mode = "OUT", optional = "true", description = "List of com.ilscipio.scipio.cms.control.CmsProcessMapping.UriInfo")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetWebsiteIndexableProcessMappingUris {}

    /**
     * Adds a Menu and creates an empty first json map
     */
    @Service(
        name = "cmsCreateUpdateMenu",
        engine = "java",
        location = "com.ilscipio.scipio.cms.menu.CmsMenuServices",
        invoke = "createUpdateMenu",
        description = "Adds a Menu and creates an empty first json map",
        auth = "true",
        attributes = {
            @Attribute(name = "menuId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "websiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "menuName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "menuJson", type = "String", mode = "INOUT")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "CREATE")
    )
    public interface CmsCreateUpdateMenu {}

    @Service(
        name = "cmsDeleteMenu",
        engine = "java",
        location = "com.ilscipio.scipio.cms.menu.CmsMenuServices",
        invoke = "deleteMenu",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "menuId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "DELETE")
    )
    public interface CmsDeleteMenu {}

    /**
     * Returns the JSON Object of a single menu
     */
    @Service(
        name = "cmsGetMenu",
        engine = "java",
        location = "com.ilscipio.scipio.cms.menu.CmsMenuServices",
        invoke = "getMenu",
        description = "Returns the JSON Object of a single menu",
        auth = "true",
        attributes = {
            @Attribute(name = "menuId", type = "String", mode = "IN"),
            @Attribute(name = "menuJson", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetMenu {}

    /**
     * Creates a JSON list of all available Menus.
     */
    @Service(
        name = "cmsGetMenus",
        engine = "java",
        location = "com.ilscipio.scipio.cms.menu.CmsMenuServices",
        invoke = "getMenus",
        description = "Creates a JSON list of all available Menus.",
        auth = "true",
        attributes = {
            @Attribute(name = "websiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "menuJson", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetMenus {}

    /**
     * Get a JSON list of all redirects
     */
    @Service(
        name = "cmsGetRedirects",
        engine = "java",
        location = "com.ilscipio.scipio.cms.media.CmsMediaServices",
        invoke = "getRedirects",
        description = "Get a JSON list of all redirects",
        auth = "true",
        attributes = {
            @Attribute(name = "websiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "redirectsJson", type = "List", mode = "OUT")
        },
        permissionService = @PermissionService(service = "cmsGenericPermission", mainAction = "VIEW")
    )
    public interface CmsGetRedirects {}

    /**
     * Get Bing to index a new product page (requires key to be set in properties)
     */
    @Service(
        name = "submitProductToBingIndex",
        engine = "java",
        location = "com.ilscipio.scipio.cms.CmsServices",
        invoke = "submitProductToBingIndex",
        description = "Get Bing to index a new product page (requires key to be set in properties)",
        auth = "true",
        maxRetry = "2",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SubmitProductToBingIndex {}

    /**
     * SCIPIO: 4.0.0: Public demo: the CMS back to the data files (hourly job CMS_DEMO_RESET, data/CmsDemoResetJob.xml).
     */
    @Service(
        name = "cmsDemoReset",
        engine = "java",
        location = "com.ilscipio.scipio.cms.demo.CmsDemoReset",
        invoke = "cmsDemoReset",
        description = "Public demo: puts the CMS back to the data files - removes the pages, versions, templates, mappings and media "
            + "that visitors added and restores what they changed. Does nothing unless general.properties demo.reset.enabled=true.",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "dryRun", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "restoredRows", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "removedRows", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "failedRows", type = "Integer", mode = "OUT", optional = "true")
        }
    )
    public interface CmsDemoResetService {}

}
