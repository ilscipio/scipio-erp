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
package com.ilscipio.scipio.content.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Services {

    /**
     * Contains necessary parameters for all file upload requests via service event handler
     */
    @Service(
        name = "uploadFileInterface",
        engine = "interface",
        description = "Contains necessary parameters for all file upload requests via service event handler",
        attributes = {
            @Attribute(name = "uploadedFile", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_uploadedFile_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_uploadedFile_contentType", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UploadFileInterface {}

    /**
     * Get Content and resource information
     */
    @Service(
        name = "getPublicForumMessage",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "getPublicForumMessage",
        description = "Get Content and resource information",
        defaultEntityName = "Content",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "resultData", type = "java.util.Map", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "VIEW")
    )
    public interface GetPublicForumMessage {}

    /**
     * Get Content and resource information
     */
    @Service(
        name = "getSubContentWithPermCheck",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "getSubContentWithPermCheck",
        description = "Get Content and resource information",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "contentAssocTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mapKey", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mainAction", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentOperationId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "filterByDate", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "subContentList", type = "java.util.List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "VIEW")
    )
    public interface GetSubContentWithPermCheck {}

    /**
     * Get Content associated with Content
     */
    @Service(
        name = "getSubSubContentWithPermCheck",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "getSubSubContentWithPermCheck",
        description = "Get Content associated with Content",
        auth = "true",
        implemented = {@Implements(service = "getSubContentWithPermCheck")},
        attributes = {
            @Attribute(name = "subContentAssocTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "subMapKey", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "subSubContentList", type = "java.util.List", mode = "OUT", optional = "true")
        }
    )
    public interface GetSubSubContentWithPermCheck {}

    /**
     * Get Content and resource information
     */
    @Service(
        name = "getContentAndDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "getContentAndDataResource",
        description = "Get Content and resource information",
        defaultEntityName = "Content",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "resultData", type = "java.util.Map", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "VIEW")
    )
    public interface GetContentAndDataResource {}

    /**
     * Get Content and resource information
     */
    @Service(
        name = "getDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "getDataResource",
        description = "Get Content and resource information",
        defaultEntityName = "DataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "resultData", type = "java.util.Map", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "VIEW")
    )
    public interface GetDataResource {}

    /**
     * Create a DataCategory
     */
    @Service(
        name = "createDataCategory",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a DataCategory",
        defaultEntityName = "DataCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateDataCategory {}

    /**
     * Update a DataCategory
     */
    @Service(
        name = "updateDataCategory",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateDataCategory",
        description = "Update a DataCategory",
        defaultEntityName = "DataCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateDataCategory {}

    /**
     * Remove DataCategory
     */
    @Service(
        name = "removeDataCategory",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "removeDataCategory",
        description = "Remove DataCategory",
        defaultEntityName = "DataCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveDataCategory {}

    /**
     * Create a DataResourceAttribute
     */
    @Service(
        name = "createDataResourceAttribute",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createDataResourceAttribute",
        description = "Create a DataResourceAttribute",
        defaultEntityName = "DataResourceAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "CREATE")
    )
    public interface CreateDataResourceAttribute {}

    /**
     * Update a DataResourceAttribute
     */
    @Service(
        name = "updateDataResourceAttribute",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateDataResourceAttribute",
        description = "Update a DataResourceAttribute",
        defaultEntityName = "DataResourceAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "UPDATE")
    )
    public interface UpdateDataResourceAttribute {}

    /**
     * Remove DataResourceAttribute
     */
    @Service(
        name = "removeDataResourceAttribute",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "removeDataResourceAttribute",
        description = "Remove DataResourceAttribute",
        defaultEntityName = "DataResourceAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "DELETE")
    )
    public interface RemoveDataResourceAttribute {}

    /**
     * Create a DataResourceRole
     */
    @Service(
        name = "createDataResourceRole",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createDataResourceRole",
        description = "Create a DataResourceRole",
        defaultEntityName = "DataResourceRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "dataResourceId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "INOUT", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "CREATE")
    )
    public interface CreateDataResourceRole {}

    /**
     * Update a DataResourceRole
     */
    @Service(
        name = "updateDataResourceRole",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateDataResourceRole",
        description = "Update a DataResourceRole",
        defaultEntityName = "DataResourceRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "UPDATE")
    )
    public interface UpdateDataResourceRole {}

    /**
     * Remove DataResourceRole
     */
    @Service(
        name = "removeDataResourceRole",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "removeDataResourceRole",
        description = "Remove DataResourceRole",
        defaultEntityName = "DataResourceRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "DELETE")
    )
    public interface RemoveDataResourceRole {}

    @Service(
        name = "createEmailContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createEmailContent",
        entityAttributes = {
            @EntityAttributes(entityName = "Content", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "subject", type = "String", mode = "IN"),
            @Attribute(name = "plainBody", type = "String", mode = "IN"),
            @Attribute(name = "htmlBody", type = "String", mode = "IN", optional = "true", allowHtml = "any")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "contentId", mode = "INOUT")
        }
    )
    public interface CreateEmailContent {}

    @Service(
        name = "updateEmailContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateEmailContent",
        attributes = {
            @Attribute(name = "subjectDataResourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "subject", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "plainBodyDataResourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "plainBody", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "htmlBodyDataResourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "htmlBody", type = "String", mode = "IN", optional = "true", allowHtml = "any")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateEmailContent {}

    @Service(
        name = "createDownloadContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createDownloadContent",
        entityAttributes = {
            @EntityAttributes(entityName = "Content", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "file", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "contentId", mode = "INOUT")
        }
    )
    public interface CreateDownloadContent {}

    @Service(
        name = "updateDownloadContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateDownloadContent",
        attributes = {
            @Attribute(name = "fileDataResourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "file", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateDownloadContent {}

    @Service(
        name = "createSimpleTextContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createSimpleTextContent",
        entityAttributes = {
            @EntityAttributes(entityName = "Content", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "DataResource", mode = "IN", optional = "true", prefix = "dr")
        },
        attributes = {
            @Attribute(name = "text", type = "String", mode = "IN", allowHtml = "any")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "contentId", mode = "INOUT")
        }
    )
    public interface CreateSimpleTextContent {}

    @Service(
        name = "updateSimpleTextContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateSimpleTextContent",
        attributes = {
            @Attribute(name = "textDataResourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "text", type = "String", mode = "IN", optional = "true", allowHtml = "any")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateSimpleTextContent {}

    @Service(
        name = "findAssocContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "findAssocContent",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "mapKeys", type = "List", mode = "IN"),
            @Attribute(name = "contentAssocs", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface FindAssocContent {}

    /**
     * Get a ContentAssocDataResourceView
     */
    @Service(
        name = "getAssocAndContentAndDataResourceCache",
        engine = "java",
        location = "org.ofbiz.content.content.ContentServicesComplex",
        invoke = "getAssocAndContentAndDataResourceCache",
        description = "Get a ContentAssocDataResourceView",
        defaultEntityName = "ContentAssocDataResourceViewFrom",
        entityAttributes = {
            @EntityAttributes(entityName = "ContentAssocDataResourceViewFrom", mode = "OUT", optional = "true", excludeFields = {"contentIdStart"})
        },
        attributes = {
            @Attribute(name = "assocTypes", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "assocTypesString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentTypes", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentTypesString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mapKey", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "direction", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDateStr", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "thruDateStr", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "nullThruDatesOnly", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "contentAssocPredicateId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentIdFrom", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "entityList", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "view", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true")
        }
    )
    public interface GetAssocAndContentAndDataResourceCache {}

    /**
     * Get a ContentAssocDataResourceView
     */
    @Service(
        name = "getAssocAndContentAndDataResource",
        engine = "java",
        location = "org.ofbiz.content.content.ContentServicesComplex",
        invoke = "getAssocAndContentAndDataResource",
        description = "Get a ContentAssocDataResourceView",
        defaultEntityName = "ContentAssocDataResourceViewFrom",
        entityAttributes = {
            @EntityAttributes(entityName = "ContentAssocDataResourceViewFrom", mode = "OUT", optional = "true", excludeFields = {"contentIdStart"})
        },
        attributes = {
            @Attribute(name = "assocTypes", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentTypes", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "mapKey", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "direction", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDateStr", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "thruDateStr", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "nullThruDatesOnly", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "entityList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface GetAssocAndContentAndDataResource {}

    /**
     * Follow a content and return descendants
     */
    @Service(
        name = "traverseContent",
        engine = "java",
        location = "org.ofbiz.content.content.ContentServices",
        invoke = "traverseContent",
        description = "Follow a content and return descendants",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "fromDateStr", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "thruDateStr", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "followWhen", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pickWhen", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "returnBeforePickWhen", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "returnAfterPickWhen", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "direction", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pickList", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "nodeMap", type = "Map", mode = "OUT", optional = "true")
        }
    )
    public interface TraverseContent {}

    /**
     * Get Content of passed contentId
     */
    @Service(
        name = "getContent",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "getContent",
        description = "Get Content of passed contentId",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "view", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true")
        }
    )
    public interface GetContent {}

    /**
     * Get subContent of passed contentId/mapKey or subContentId
     */
    @Service(
        name = "getSubContent",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "getSubContent",
        description = "Get subContent of passed contentId/mapKey or subContentId",
        attributes = {
            @Attribute(name = "mapKey", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "subContentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "assocTypes", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "assocTypesString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentTypes", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "view", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "content", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true")
        }
    )
    public interface GetSubContent {}

    /**
     * Create a Content, DataResource and/or ContentAssoc
     */
    @Service(
        name = "persistContentAndAssoc",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "persistContentAndAssoc",
        description = "Create a Content, DataResource and/or ContentAssoc",
        auth = "true",
        transactionTimeout = "7200",
        entityAttributes = {
            @EntityAttributes(entityName = "ContentDataResourceView", mode = "IN", optional = "true", excludeFields = {"contentId"}),
            @EntityAttributes(entityName = "DataResource", mode = "IN", optional = "true", excludeFields = {"dataResourceId"}),
            @EntityAttributes(entityName = "ElectronicText", mode = "IN", optional = "true", excludeFields = {"dataResourceId"}),
            @EntityAttributes(entityName = "ContentAssoc", mode = "IN", optional = "true", excludeFields = {"contentIdTo", "contentId", "fromDate", "contentAssocTypeId"}),
            @EntityAttributes(entityName = "ContentAssocDataResourceViewTo", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "INOUT", optional = "true", entityName = "Content"),
            @Attribute(name = "dataResourceId", type = "String", mode = "INOUT", optional = "true", entityName = "DataResource"),
            @Attribute(name = "drDataResourceId", type = "String", mode = "INOUT", optional = "true", entityName = "DataResource"),
            @Attribute(name = "caContentIdTo", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "caContentId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "caContentAssocTypeId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "caFromDate", type = "Timestamp", mode = "INOUT", optional = "true"),
            @Attribute(name = "caSequenceNum", type = "Long", mode = "INOUT", optional = "true"),
            @Attribute(name = "ownerContentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "rootDir", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "targetOperationString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "imageData", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_imageData_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_imageData_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "deactivateExisting", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "forceElectronicText", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "displayFailCond", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeList", type = "List", mode = "INOUT", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "textData", allowHtml = "any")
        }
    )
    public interface PersistContentAndAssoc {}

    /**
     * Persist a  DataResource and data
     */
    @Service(
        name = "persistDataResourceAndData",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "persistDataResourceAndData",
        description = "Persist a  DataResource and data",
        auth = "true",
        transactionTimeout = "7200",
        entityAttributes = {
            @EntityAttributes(entityName = "DataResource", mode = "IN", optional = "true", excludeFields = {"dataResourceId"}),
            @EntityAttributes(entityName = "ElectronicText", mode = "IN", optional = "true", excludeFields = {"dataResourceId"})
        },
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "INOUT", optional = "true", entityName = "Content"),
            @Attribute(name = "dataResourceId", type = "String", mode = "INOUT", optional = "true", entityName = "DataResource"),
            @Attribute(name = "drDataResourceId", type = "String", mode = "INOUT", optional = "true", entityName = "DataResource"),
            @Attribute(name = "rootDir", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "targetOperationString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "imageData", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_imageData_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_imageData_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "forceElectronicText", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "displayFailCond", type = "Boolean", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "textData", allowHtml = "any")
        }
    )
    public interface PersistDataResourceAndData {}

    /**
     * Persist a CompDoc DataResource and data
     */
    @Service(
        name = "persistCompDocContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "persistCompDocContent",
        description = "Persist a CompDoc DataResource and data",
        auth = "true",
        transactionTimeout = "7200",
        implemented = {@Implements(service = "persistDataResourceAndData")},
        attributes = {
            @Attribute(name = "contentRevisionSeqId", type = "String", mode = "OUT"),
            @Attribute(name = "rootContentId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface PersistCompDocContent {}

    /**
     * Upload/save PDF, create Survey, populate Content
     */
    @Service(
        name = "persistCompDocPdf2Survey",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "persistCompDocPdf2Survey",
        description = "Upload/save PDF, create Survey, populate Content",
        auth = "true",
        transactionTimeout = "7200",
        implemented = {@Implements(service = "persistCompDocContent")},
        attributes = {
            @Attribute(name = "pdfName", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface PersistCompDocPdf2Survey {}

    @Service(
        name = "persistContentWithRevision",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "persistContentWithRevision",
        auth = "true",
        transactionTimeout = "7200",
        implemented = {@Implements(service = "persistContentAndAssoc")},
        attributes = {
            @Attribute(name = "masterRevisionContentId", type = "String", mode = "IN")
        }
    )
    public interface PersistContentWithRevision {}

    @Service(
        name = "findContentParents",
        engine = "java",
        location = "org.ofbiz.content.content.ContentServices",
        invoke = "findContentParents",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "contentAssocTypeId", type = "String", mode = "IN"),
            @Attribute(name = "direction", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "parentList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface FindContentParents {}

    /**
     * Supply thruDate to all ContentAssoc that come "before" current one
     */
    @Service(
        name = "deactivateAssocs",
        engine = "java",
        location = "org.ofbiz.content.content.ContentServices",
        invoke = "deactivateAssocs",
        description = "Supply thruDate to all ContentAssoc that come \"before\" current one",
        auth = "true",
        attributes = {
            @Attribute(name = "contentIdTo", type = "String", mode = "IN"),
            @Attribute(name = "mapKey", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "activeContentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentAssocTypeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "deactivatedList", type = "List", mode = "OUT")
        }
    )
    public interface DeactivateAssocs {}

    /**
     * Set thruDate to now for ContentAssoc entity
     */
    @Service(
        name = "deactivateContentAssoc",
        engine = "java",
        location = "org.ofbiz.content.content.ContentServices",
        invoke = "deactivateContentAssoc",
        description = "Set thruDate to now for ContentAssoc entity",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "contentIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentAssocTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface DeactivateContentAssoc {}

    /**
     * Creates content records for a blog entry
     */
    @Service(
        name = "createArticleContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createArticleContent",
        description = "Creates content records for a blog entry",
        auth = "true",
        transactionTimeout = "300",
        implemented = {@Implements(service = "createContentFromUploadedFile", optional = "true"), @Implements(service = "createTextContent", optional = "true")},
        attributes = {
            @Attribute(name = "contentIdFrom", type = "String", mode = "IN"),
            @Attribute(name = "pubPtContentId", type = "String", mode = "IN"),
            @Attribute(name = "threadContentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "summaryData", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateArticleContent {}

    /**
     * Get the subcontent and render
     */
    @Service(
        name = "renderSubContentAsText",
        engine = "java",
        location = "org.ofbiz.content.content.ContentServices",
        invoke = "renderSubContentAsText",
        description = "Get the subcontent and render",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mapKey", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "outWriter", type = "java.io.Writer", mode = "IN"),
            @Attribute(name = "subContentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateContext", type = "Map", mode = "IN"),
            @Attribute(name = "locale", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mimeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "subContentDataResourceView", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "view", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "textData", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface RenderSubContentAsText {}

    /**
     * Get the subcontent and render
     */
    @Service(
        name = "renderContentAsText",
        engine = "java",
        location = "org.ofbiz.content.content.ContentServices",
        invoke = "renderContentAsText",
        description = "Get the subcontent and render",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "outWriter", type = "java.io.Writer", mode = "IN", optional = "true"),
            @Attribute(name = "templateContext", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "locale", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mimeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "subContentDataResourceView", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "textData", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface RenderContentAsText {}

    /**
     * Get the dataResource and render
     */
    @Service(
        name = "renderDataResourceAsText",
        engine = "java",
        location = "org.ofbiz.content.data.DataServices",
        invoke = "renderDataResourceAsText",
        description = "Get the dataResource and render",
        attributes = {
            @Attribute(name = "dataResourceId", type = "String", mode = "IN"),
            @Attribute(name = "outWriter", type = "java.io.Writer", mode = "IN"),
            @Attribute(name = "templateContext", type = "Map", mode = "IN"),
            @Attribute(name = "locale", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mimeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "subContentDataResourceView", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "textData", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface RenderDataResourceAsText {}

    /**
     * Create a TOPIC type Content 
     */
    @Service(
        name = "createTopic",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createTopic",
        description = "Create a TOPIC type Content ",
        auth = "true",
        attributes = {
            @Attribute(name = "newTopicId", type = "String", mode = "IN"),
            @Attribute(name = "newTopicDescription", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateTopic {}

    /**
     * Update site roles
     */
    @Service(
        name = "updateSiteRoles",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "updateSiteRoles",
        description = "Update site roles",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "blogUser", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "blogAuthor", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "blogEditor", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "blogAdmin", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "blogPublisher", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "blogUserFromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "blogAuthorFromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "blogEditorFromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "blogAdminFromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "blogPublisherFromDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface UpdateSiteRoles {}

    /**
     * Attach content to publish point
     */
    @Service(
        name = "linkContentToPubPt",
        engine = "java",
        location = "org.ofbiz.content.content.ContentServices",
        invoke = "linkContentToPubPt",
        description = "Attach content to publish point",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "contentIdTo", type = "String", mode = "IN"),
            @Attribute(name = "publish", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "privilegeEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentAssocTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mapKey", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface LinkContentToPubPt {}

    /**
     * Update site roles
     */
    @Service(
        name = "updateSiteRolesDyn",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "updateSiteRolesDyn",
        description = "Update site roles",
        auth = "true",
        validate = "false",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateSiteRolesDyn {}

    /**
     * Update or remove a child entity based on value of "action"
     */
    @Service(
        name = "updateOrRemove",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "updateOrRemove",
        description = "Update or remove a child entity based on value of \"action\"",
        auth = "true",
        validate = "false",
        attributes = {
            @Attribute(name = "entityName", type = "String", mode = "IN"),
            @Attribute(name = "action", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pkFieldCount", type = "String", mode = "IN"),
            @Attribute(name = "fieldName0", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fieldValue1", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fieldName2", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fieldValue2", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fieldName3", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fieldValue3", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fieldName1", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fieldValue0", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateOrRemove {}

    /**
     * Reorder sequence numbers in ContentAssoc entities for a given parent id
     */
    @Service(
        name = "resequence",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "resequence",
        description = "Reorder sequence numbers in ContentAssoc entities for a given parent id",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "contentIdTo", type = "String", mode = "IN"),
            @Attribute(name = "seqInc", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "typeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentAssocTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "dir", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface Resequence {}

    /**
     * Moves dataResource to separate content associated with current content so that node can have only children, no content of its own
     */
    @Service(
        name = "changeLeafToNode",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "changeLeafToNode",
        description = "Moves dataResource to separate content associated with current content so that node can have only children, no content of its own",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true")
        }
    )
    public interface ChangeLeafToNode {}

    /**
     * Change contentTypeId to OUTLINE/PAGE/SUBPAGE_NODE
     */
    @Service(
        name = "updatePageType",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "updatePageType",
        description = "Change contentTypeId to OUTLINE/PAGE/SUBPAGE_NODE",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "pageMode", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdatePageType {}

    /**
     * Set content and kids to OUTLINE_NODE
     */
    @Service(
        name = "resetToOutlineMode",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "resetToOutlineMode",
        description = "Set content and kids to OUTLINE_NODE",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "pageMode", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ResetToOutlineMode {}

    /**
     * Clear cache
     */
    @Service(
        name = "clearContentAssocViewCache",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "clearContentAssocViewCache",
        description = "Clear cache",
        auth = "true",
        transactionTimeout = "7200"
    )
    public interface ClearContentAssocViewCache {}

    /**
     * Clear cache
     */
    @Service(
        name = "clearContentAssocDataResourceViewCache",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "clearContentAssocDataResourceViewCache",
        description = "Clear cache",
        auth = "true",
        transactionTimeout = "7200"
    )
    public interface ClearContentAssocDataResourceViewCache {}

    /**
     * Get children of content
     */
    @Service(
        name = "findSubNodes",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "findSubNodes",
        description = "Get children of content",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "_LIST_", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface FindSubNodes {}

    /**
     * Set childLeafCount and childBranchCount to appropriate values
     */
    @Service(
        name = "initContentChildCounts",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "initContentChildCounts",
        description = "Set childLeafCount and childBranchCount to appropriate values",
        attributes = {
            @Attribute(name = "content", type = "org.ofbiz.entity.GenericValue", mode = "IN")
        }
    )
    public interface InitContentChildCounts {}

    /**
     * Set childLeafCount and childBranchCount in parent Content entities
     */
    @Service(
        name = "incrementContentChildStats",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "incrementContentChildStats",
        description = "Set childLeafCount and childBranchCount in parent Content entities",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "contentAssocTypeId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface IncrementContentChildStats {}

    /**
     * Set childLeafCount and childBranchCount in parent Content entities
     */
    @Service(
        name = "decrementContentChildStats",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "decrementContentChildStats",
        description = "Set childLeafCount and childBranchCount in parent Content entities",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "contentAssocTypeId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface DecrementContentChildStats {}

    /**
     * Set childLeafCount and childBranchCount in passed id and those below
     */
    @Service(
        name = "updateContentChildStats",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "updateContentChildStats",
        description = "Set childLeafCount and childBranchCount in passed id and those below",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "contentAssocTypeId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateContentChildStats {}

    /**
     * Update image
     */
    @Service(
        name = "updateImage",
        engine = "java",
        location = "org.ofbiz.content.data.DataServices",
        invoke = "updateImage",
        description = "Update image",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "dataResourceId", type = "String", mode = "IN"),
            @Attribute(name = "imageData", type = "java.nio.ByteBuffer", mode = "IN")
        }
    )
    public interface UpdateImage {}

    /**
     * Create image
     */
    @Service(
        name = "createImage",
        engine = "java",
        location = "org.ofbiz.content.data.DataServices",
        invoke = "createImage",
        description = "Create image",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "dataResourceId", type = "String", mode = "IN"),
            @Attribute(name = "imageData", type = "java.nio.ByteBuffer", mode = "IN")
        }
    )
    public interface CreateImage {}

    /**
     * Creates or updates ContentRole
     */
    @Service(
        name = "updateContentSubscription",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "updateContentSubscription",
        description = "Creates or updates ContentRole",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "useRoleTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "useTimeUomId", type = "String", mode = "IN"),
            @Attribute(name = "useTime", type = "Integer", mode = "IN")
        }
    )
    public interface UpdateContentSubscription {}

    /**
     * Creates or updates ContentRole
     */
    @Service(
        name = "updateContentSubscriptionByProduct",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "updateContentSubscriptionByProduct",
        description = "Creates or updates ContentRole",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "orderCreatedDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "Integer", mode = "IN")
        }
    )
    public interface UpdateContentSubscriptionByProduct {}

    /**
     * Creates or updates ContentRole
     */
    @Service(
        name = "updateContentSubscriptionByOrder",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "updateContentSubscriptionByOrder",
        description = "Creates or updates ContentRole",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface UpdateContentSubscriptionByOrder {}

    /**
     * Descend thru content tree and execute service at each node
     */
    @Service(
        name = "followNodeChildren",
        engine = "java",
        location = "org.ofbiz.content.ContentManagementServices",
        invoke = "followNodeChildren",
        description = "Descend thru content tree and execute service at each node",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "serviceName", type = "String", mode = "IN"),
            @Attribute(name = "contentAssocTypeId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface FollowNodeChildren {}

    /**
     * Change statusId to published (CTNT_PUBLISHED)
     */
    @Service(
        name = "publishContent",
        engine = "java",
        location = "org.ofbiz.content.content.ContentServices",
        invoke = "publishContent",
        description = "Change statusId to published (CTNT_PUBLISHED)",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "content", type = "org.ofbiz.entity.GenericValue", mode = "IN")
        }
    )
    public interface PublishContent {}

    /**
     * Gets all the members of the input map that start with the prefix
     */
    @Service(
        name = "getPrefixedMembers",
        engine = "java",
        location = "org.ofbiz.content.content.ContentServices",
        invoke = "getPrefixedMembers",
        description = "Gets all the members of the input map that start with the prefix",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "mapIn", type = "java.util.Map", mode = "IN", optional = "true"),
            @Attribute(name = "prefix", type = "String", mode = "IN"),
            @Attribute(name = "mapOut", type = "java.util.Map", mode = "OUT", optional = "true")
        }
    )
    public interface GetPrefixedMembers {}

    /**
     * Splits input string 
     */
    @Service(
        name = "splitString",
        engine = "java",
        location = "org.ofbiz.content.content.ContentServices",
        invoke = "splitString",
        description = "Splits input string ",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "inputString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "delimiter", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "outputList", type = "java.util.List", mode = "OUT", optional = "true")
        }
    )
    public interface SplitString {}

    /**
     * Splits input string 
     */
    @Service(
        name = "joinString",
        engine = "java",
        location = "org.ofbiz.content.content.ContentServices",
        invoke = "joinString",
        description = "Splits input string ",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "inputList", type = "java.util.List", mode = "IN"),
            @Attribute(name = "delimiter", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "outputString", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface JoinString {}

    /**
     * URL encodes map
     */
    @Service(
        name = "urlEncodeArgs",
        engine = "java",
        location = "org.ofbiz.content.content.ContentServices",
        invoke = "urlEncodeArgs",
        description = "URL encodes map",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "mapIn", type = "java.util.Map", mode = "IN"),
            @Attribute(name = "outputString", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface UrlEncodeArgs {}

    /**
     * Update a ContentRevision and ContentRevisionItem
     */
    @Service(
        name = "persistContentRevisionAndItem",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "persistContentRevisionAndItem",
        description = "Update a ContentRevision and ContentRevisionItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ContentRevision", mode = "IN", optional = "true", excludeFields = {"contentRevisionSeqId"}),
            @EntityAttributes(entityName = "ContentRevisionItem", mode = "IN", optional = "true", excludeFields = {"contentRevisionSeqId"})
        },
        attributes = {
            @Attribute(name = "contentRevisionSeqId", type = "String", mode = "OUT")
        }
    )
    public interface PersistContentRevisionAndItem {}

    /**
     * Set ContentApprovals for approval process
     */
    @Service(
        name = "prepForApproval",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "prepForApproval",
        description = "Set ContentApprovals for approval process",
        auth = "true",
        attributes = {
            @Attribute(name = "rootContentId", type = "String", mode = "IN"),
            @Attribute(name = "rootContentRevisionSeqId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface PrepForApproval {}

    /**
     * Set ContentApprovals for approval process
     */
    @Service(
        name = "getFinalApprovalStatus",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "getFinalApprovalStatus",
        description = "Set ContentApprovals for approval process",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "contentRevisionSeqId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "approvalStatusId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "contentApprovalList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface GetFinalApprovalStatus {}

    /**
     * Get a list of ContentApprovals and permission indicators
     */
    @Service(
        name = "getApprovalsWithPermissions",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "getApprovalsWithPermissions",
        description = "Get a list of ContentApprovals and permission indicators",
        auth = "true",
        attributes = {
            @Attribute(name = "rootContentId", type = "String", mode = "IN"),
            @Attribute(name = "contentRevisionSeqId", type = "String", mode = "IN"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "checkPermission", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentApprovalList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface GetApprovalsWithPermissions {}

    /**
     * Determine permission status for record
     */
    @Service(
        name = "hasApprovalPermission",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "hasApprovalPermission",
        description = "Determine permission status for record",
        auth = "true",
        attributes = {
            @Attribute(name = "contentApprovalId", type = "String", mode = "IN"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "approvalPermExists", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface HasApprovalPermission {}

    /**
     * Generate parallel CompDoc Instance tree
     */
    @Service(
        name = "genCompDocInstance",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "genCompDocInstance",
        description = "Generate parallel CompDoc Instance tree",
        auth = "true",
        attributes = {
            @Attribute(name = "contentName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "instanceOfContentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "rootInstanceContentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "contentRevisionSeqId", type = "String", mode = "OUT")
        }
    )
    public interface GenCompDocInstance {}

    /**
     * Create a CompDoc Template entity and associated ContentRevision/Item entities
     */
    @Service(
        name = "persistCompDoc",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "persistCompDoc",
        description = "Create a CompDoc Template entity and associated ContentRevision/Item entities",
        auth = "true",
        implemented = {@Implements(service = "persistContentAndAssoc")},
        attributes = {
            @Attribute(name = "contentRevisionSeqId", type = "String", mode = "OUT"),
            @Attribute(name = "rootContentId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface PersistCompDoc {}

    /**
     * Bump the previous ContentApproval approvals up to current CDT
     */
    @Service(
        name = "cloneTemplateContentApprovals",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "cloneTemplateContentApprovals",
        description = "Bump the previous ContentApproval approvals up to current CDT",
        auth = "true",
        attributes = {
            @Attribute(name = "contentRevisionSeqId", type = "String", mode = "IN"),
            @Attribute(name = "contentId", type = "String", mode = "IN")
        }
    )
    public interface CloneTemplateContentApprovals {}

    /**
     * Bump the previous ContentApproval approvals up to current CDI
     */
    @Service(
        name = "cloneInstanceContentApprovals",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "cloneInstanceContentApprovals",
        description = "Bump the previous ContentApproval approvals up to current CDI",
        auth = "true",
        attributes = {
            @Attribute(name = "contentRevisionSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN")
        }
    )
    public interface CloneInstanceContentApprovals {}

    /**
     * Check to see if there are any ContentApproval records awaiting this user's action
     */
    @Service(
        name = "checkForWaitingApprovals",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "checkForWaitingApprovals",
        description = "Check to see if there are any ContentApproval records awaiting this user's action",
        auth = "true",
        attributes = {
            @Attribute(name = "thisUserLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "contentApprovalList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface CheckForWaitingApprovals {}

    /**
     * Look for most recent revision for contentId
     */
    @Service(
        name = "getMostRecentRevision",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "getMostRecentRevision",
        description = "Look for most recent revision for contentId",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "mostRecentRevisionSeqId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface GetMostRecentRevision {}

    /**
     * Use OpenOffice to convert between document types
     */
    @Service(
        name = "convertDocumentByteBuffer",
        engine = "java",
        location = "org.ofbiz.content.openoffice.OpenOfficeServices",
        invoke = "convertDocumentByteBuffer",
        description = "Use OpenOffice to convert between document types",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "oooHost", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "oooPort", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inputMimeType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "outputMimeType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inByteBuffer", type = "java.nio.ByteBuffer", mode = "IN"),
            @Attribute(name = "outByteBuffer", type = "java.nio.ByteBuffer", mode = "OUT")
        }
    )
    public interface ConvertDocumentByteBuffer {}

    /**
     * Use OpenOffice to convert between document types
     */
    @Service(
        name = "convertDocument",
        engine = "java",
        location = "org.ofbiz.content.openoffice.OpenOfficeServices",
        invoke = "convertDocument",
        description = "Use OpenOffice to convert between document types",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "oooHost", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "oooPort", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filenameFrom", type = "String", mode = "IN"),
            @Attribute(name = "filenameTo", type = "String", mode = "IN"),
            @Attribute(name = "convertFilterName", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ConvertDocument {}

    /**
     * Use OpenOffice to convert between document types
     */
    @Service(
        name = "convertDocumentFileToFile",
        engine = "java",
        location = "org.ofbiz.content.openoffice.OpenOfficeServices",
        invoke = "convertDocumentFileToFile",
        description = "Use OpenOffice to convert between document types",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "oooHost", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "oooPort", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filenameFrom", type = "String", mode = "IN"),
            @Attribute(name = "filenameTo", type = "String", mode = "IN"),
            @Attribute(name = "inputMimeType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "outputMimeType", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ConvertDocumentFileToFile {}

    /**
     * Use OpenOffice to compare two documents
     */
    @Service(
        name = "compareDocuments",
        engine = "java",
        location = "org.ofbiz.content.openoffice.OpenOfficeServices",
        invoke = "compareDocuments",
        description = "Use OpenOffice to compare two documents",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "oooHost", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "oooPort", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filenameFrom", type = "String", mode = "IN"),
            @Attribute(name = "filenameOriginal", type = "String", mode = "IN"),
            @Attribute(name = "filenameOut", type = "String", mode = "IN")
        }
    )
    public interface CompareDocuments {}

    /**
     * Convert all the CompDoc parts into PDF and concatenate and put in CMS
     */
    @Service(
        name = "renderCompDocPdf",
        engine = "java",
        location = "org.ofbiz.content.compdoc.CompDocServices",
        invoke = "renderCompDocPdf",
        description = "Convert all the CompDoc parts into PDF and concatenate and put in CMS",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "contentRevisionSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "https", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "rootDir", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "locale", type = "java.util.Locale", mode = "IN", optional = "true"),
            @Attribute(name = "outByteBuffer", type = "java.nio.ByteBuffer", mode = "OUT")
        }
    )
    public interface RenderCompDocPdf {}

    /**
     * Convert all the CompDoc parts into PDF and concatenate and put in CMS
     */
    @Service(
        name = "renderContentPdf",
        engine = "java",
        location = "org.ofbiz.content.compdoc.CompDocServices",
        invoke = "renderContentPdf",
        description = "Convert all the CompDoc parts into PDF and concatenate and put in CMS",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "contentRevisionSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "https", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "rootDir", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "locale", type = "java.util.Locale", mode = "IN", optional = "true"),
            @Attribute(name = "outByteBuffer", type = "java.nio.ByteBuffer", mode = "OUT")
        }
    )
    public interface RenderContentPdf {}

    /**
     * Creates content records for a blog entry
     */
    @Service(
        name = "createBlogEntry",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/blog/BlogServices.xml",
        invoke = "createBlogEntry",
        description = "Creates content records for a blog entry",
        auth = "true",
        implemented = {@Implements(service = "uploadFileInterface", optional = "true")},
        attributes = {
            @Attribute(name = "blogContentId", type = "String", mode = "INOUT"),
            @Attribute(name = "contentId", type = "String", mode = "OUT"),
            @Attribute(name = "contentName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateDataResourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "articleData", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "summaryData", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateBlogEntry {}

    /**
     * Updates content records for a blog entry
     */
    @Service(
        name = "updateBlogEntry",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/blog/BlogServices.xml",
        invoke = "updateBlogEntry",
        description = "Updates content records for a blog entry",
        auth = "true",
        implemented = {@Implements(service = "uploadFileInterface", optional = "true")},
        attributes = {
            @Attribute(name = "blogContentId", type = "String", mode = "INOUT"),
            @Attribute(name = "contentId", type = "String", mode = "INOUT"),
            @Attribute(name = "contentName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "templateDataResourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "articleData", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "summaryData", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateBlogEntry {}

    /**
     * Retrieves content records for a blog entry
     */
    @Service(
        name = "getBlogEntry",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/blog/BlogServices.xml",
        invoke = "getBlogEntry",
        description = "Retrieves content records for a blog entry",
        auth = "true",
        attributes = {
            @Attribute(name = "blogContentId", type = "String", mode = "INOUT"),
            @Attribute(name = "contentId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "contentName", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "templateDataResourceId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "summaryData", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "articleData", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "imageContentId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "articleContentId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "summaryContentId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface GetBlogEntry {}

    @Service(
        name = "getOwnedOrPublishedBlogEntries",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/blog/BlogServices.xml",
        invoke = "getOwnedOrPublishedBlogEntries",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "blogList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface GetOwnedOrPublishedBlogEntries {}

    /**
     * Blog RSS Feed
     */
    @Service(
        name = "generateBlogRssFeed",
        engine = "java",
        location = "org.ofbiz.content.blog.BlogRssServices",
        invoke = "generateBlogRssFeed",
        description = "Blog RSS Feed",
        implemented = {@Implements(service = "rssFeedInterface")},
        attributes = {
            @Attribute(name = "blogContentId", type = "String", mode = "IN")
        }
    )
    public interface GenerateBlogRssFeed {}

    /**
     * Fixes the ContentAssoc IDs based on passed parameters
     */
    @Service(
        name = "checkContentAssocIds",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "checkContentAssocIds",
        description = "Fixes the ContentAssoc IDs based on passed parameters",
        attributes = {
            @Attribute(name = "contentIdFrom", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "contentIdTo", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface CheckContentAssocIds {}

    @Service(
        name = "contentManagerRolePermission",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/permission/ContentPermissionServices.xml",
        invoke = "contentManagerRolePermission",
        auth = "true",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface ContentManagerRolePermission {}

    @Service(
        name = "contentManagerPermission",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/permission/ContentPermissionServices.xml",
        invoke = "contentManagerPermission",
        auth = "true",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface ContentManagerPermission {}

    /**
     * Generic Content Permission Service; Takes mainAction to determine the mode.
     */
    @Service(
        name = "genericContentPermission",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/permission/ContentPermissionServices.xml",
        invoke = "genericContentPermission",
        description = "Generic Content Permission Service; Takes mainAction to determine the mode.",
        auth = "true",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "ownerContentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentIdFrom", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentOperationId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface GenericContentPermission {}

    /**
     * Generic DataResource Permission Service; Takes mainAction to determine the mode.
     */
    @Service(
        name = "genericDataResourcePermission",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/permission/DataResourcePermissionServices.xml",
        invoke = "genericDataResourcePermission",
        description = "Generic DataResource Permission Service; Takes mainAction to determine the mode.",
        auth = "true",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "dataResourceId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface GenericDataResourcePermission {}

    /**
     * Create Content Alternative URL
     */
    @Service(
        name = "createContentAlternativeUrl",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createContentAlternativeUrl",
        description = "Create Content Alternative URL",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentCreated", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateContentAlternativeUrl {}

    /**
     * Create WebPreferenceType record
     */
    @Service(
        name = "createWebPreferenceType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create WebPreferenceType record",
        defaultEntityName = "WebPreferenceType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateWebPreferenceType {}

    /**
     * Update WebPreferenceType record
     */
    @Service(
        name = "updateWebPreferenceType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update WebPreferenceType record",
        defaultEntityName = "WebPreferenceType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateWebPreferenceType {}

    /**
     * Delete ContentApproval record
     */
    @Service(
        name = "deleteWebPreferenceType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete ContentApproval record",
        defaultEntityName = "WebPreferenceType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteWebPreferenceType {}

    @Service(
        name = "createSimpleTextContentForAlternateLocale",
        engine = "java",
        location = "org.ofbiz.content.content.LocalizedContentServices$CreateSimpleTextContentForAlternateLocale",
        invoke = "exec",
        entityAttributes = {
            @EntityAttributes(entityName = "Content", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "DataResource", mode = "IN", include = "nonpk", optional = "true", prefix = "dr")
        },
        attributes = {
            @Attribute(name = "mainContentId", type = "String", mode = "IN"),
            @Attribute(name = "text", type = "String", mode = "IN", allowHtml = "any")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "localeString", optional = "false"),
            @OverrideAttribute(name = "contentId", mode = "INOUT")
        }
    )
    public interface CreateSimpleTextContentForAlternateLocale {}

    /**
     * SCIPIO: Updates ALTERNATE_LOCALE simple text content by contentId (added 2017-10-26)
     */
    @Service(
        name = "updateSimpleTextContentForAlternateLocale",
        engine = "java",
        location = "org.ofbiz.content.content.LocalizedContentServices$UpdateSimpleTextContentForAlternateLocale",
        invoke = "exec",
        description = "SCIPIO: Updates ALTERNATE_LOCALE simple text content by contentId (added 2017-10-26)",
        entityAttributes = {
            @EntityAttributes(entityName = "Content", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "mainContentId", type = "String", mode = "IN"),
            @Attribute(name = "text", type = "String", mode = "IN", allowHtml = "any")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "localeString", optional = "false")
        }
    )
    public interface UpdateSimpleTextContentForAlternateLocale {}

    /**
     * SCIPIO: Deletes ALTERNATE_LOCALE simple text content by contentId (added 2017-10-26)
     */
    @Service(
        name = "deleteSimpleTextContentForAlternateLocale",
        engine = "java",
        location = "org.ofbiz.content.content.LocalizedContentServices$DeleteSimpleTextContentForAlternateLocale",
        invoke = "exec",
        description = "SCIPIO: Deletes ALTERNATE_LOCALE simple text content by contentId (added 2017-10-26)",
        entityAttributes = {
            @EntityAttributes(entityName = "Content", mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "mainContentId", type = "String", mode = "IN")
        }
    )
    public interface DeleteSimpleTextContentForAlternateLocale {}

    /**
     * SCIPIO: Creates or updates simple text content for alternate locale             - supports either explicit contentId or relying on localeString to find record to update (added 2017-10-26)             - WARN: For high-level operation, use replaceContentLocalizedSimpleTexts
     */
    @Service(
        name = "createUpdateSimpleTextContentForAlternateLocale",
        engine = "java",
        location = "org.ofbiz.content.content.LocalizedContentServices$CreateUpdateSimpleTextContentForAlternateLocale",
        invoke = "exec",
        description = "SCIPIO: Creates or updates simple text content for alternate locale\n            - supports either explicit contentId or relying on localeString to find record to update (added 2017-10-26)\n            - WARN: For high-level operation, use replaceContentLocalizedSimpleTexts",
        entityAttributes = {
            @EntityAttributes(entityName = "Content", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "DataResource", mode = "IN", include = "nonpk", optional = "true", prefix = "dr")
        },
        attributes = {
            @Attribute(name = "mainContentId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "createMainContent", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "text", type = "String", mode = "IN", allowHtml = "any")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "localeString", optional = "false"),
            @OverrideAttribute(name = "contentId", mode = "INOUT")
        }
    )
    public interface CreateUpdateSimpleTextContentForAlternateLocale {}

    /**
     * SCIPIO: Intelligently creates, updates and deletes simple text contents for alternate locale             NOTE: This service will create and update the Main Content record as needed, but it cannot delete the main Content record (see allContentEmpty flag).
     */
    @Service(
        name = "replaceContentLocalizedSimpleTexts",
        engine = "java",
        location = "org.ofbiz.content.content.LocalizedContentServices",
        invoke = "replaceContentLocalizedSimpleTexts",
        description = "SCIPIO: Intelligently creates, updates and deletes simple text contents for alternate locale\n            NOTE: This service will create and update the Main Content record as needed, but it cannot delete the main Content record (see allContentEmpty flag).",
        attributes = {
            @Attribute(name = "mainContentId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "entries", type = "List", mode = "IN", optional = "true", description = "Lists of maps, each map supporting the keys: localeString, textData, and optional contentId.\n                If strictContent is false, when contentId is omitted, an associated alt locale content having the matching localeString is located and updated,\n                unless; if true, then entries with no contentId are assumed to be new records. \n                The first entry of each list is assumed to be the main content record."),
            @Attribute(name = "newContentFields", type = "Map", mode = "IN", optional = "true", description = "Common fields for new Content records (overrides defaults)"),
            @Attribute(name = "newDataResourceFields", type = "Map", mode = "IN", optional = "true", description = "Common fields for new DataResource records (overrides defaults)"),
            @Attribute(name = "allContentEmpty", type = "Boolean", mode = "OUT", optional = "true", description = "If the replacements result in a single empty main Content record,\n                this will be set to true, indicating that the caller may delete main Content and associations\n                if desired (this service cannot delete the associations to the main Content record)")
        }
    )
    public interface ReplaceContentLocalizedSimpleTexts {}

    /**
     * SCIPIO: Interface for simple text content for alternate locale intelligent create/update/delete services linked to a major entity (ProductContent, ProductCategoryContent, etc.)
     */
    @Service(
        name = "replaceEntityContentLocalizedSimpleTextsInterface",
        engine = "interface",
        description = "SCIPIO: Interface for simple text content for alternate locale intelligent create/update/delete services linked to a major entity (ProductContent, ProductCategoryContent, etc.)",
        attributes = {
            @Attribute(name = "contentFields", type = "Map", mode = "IN", optional = "true", description = "Map of content type IDs to lists of maps, each map supporting the keys: localeString, textData, and optional contentId.\n                If strictContent is false, when contentId is omitted, an associated alt locale content having the matching localeString is located and updated; \n                if true, then entries with no contentId are assumed to be new records. \n                The first entry of each list is assumed to be the main content record (for ProductContent assoc).\n                The list-of-maps may also be represented by special strings to allow submit over request parameters, following\n                the implementation provided by org.ofbiz.content.content.LocalizedContentWorker#parseLocalizedSimpleTextContentFieldParams"),
            @Attribute(name = "mainContentDelete", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "If true (default), if no text content is passed at all, the main content record may be deleted.\n                NOTE: This will only delete the main record if its alt locale records also contain\n                    no text; otherwise it is essential even if empty because the alt texts are linked to it.")
        }
    )
    public interface ReplaceEntityContentLocalizedSimpleTextsInterface {}

    /**
     * Prewarms the cache - runs after restart and on a full cache clear. Fetches Urls from Website entity. Useful to increase firstload performance
     */
    @Service(
        name = "prewarmContentCacheFromDb",
        engine = "java",
        location = "com.ilscipio.scipio.cms.content.CmsPageServices",
        invoke = "prewarmContentCacheFromDb",
        description = "Prewarms the cache - runs after restart and on a full cache clear. Fetches Urls from Website entity. Useful to increase firstload performance",
        auth = "true",
        transactionTimeout = "600000",
        maxRetry = "2",
        semaphoreSleep = "10",
        log = "quiet"
    )
    public interface PrewarmContentCacheFromDb {}

    /**
     * Prewarms the cache. Fetches a range of urls.
     */
    @Service(
        name = "prewarmContentCacheByUrl",
        engine = "java",
        location = "com.ilscipio.scipio.cms.content.CmsPageServices",
        invoke = "prewarmContentCacheByUrl",
        description = "Prewarms the cache. Fetches a range of urls.",
        auth = "true",
        transactionTimeout = "600000",
        log = "quiet",
        attributes = {
            @Attribute(name = "urls", type = "java.util.List", mode = "IN", optional = "true"),
            @Attribute(name = "url", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface PrewarmContentCacheByUrl {}

}
