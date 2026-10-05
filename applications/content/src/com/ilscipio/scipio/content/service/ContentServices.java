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
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ContentServices {

    /**
     * Create a Content
     */
    @Service(
        name = "createContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createContent",
        description = "Create a Content",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "targetOperationString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "skipPermissionCheck", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "displayFailCond", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeList", type = "List", mode = "INOUT", optional = "true"),
            @Attribute(name = "contentPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentAssocTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentIdFrom", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mapKey", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceNum", type = "Long", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "contentTypeId", defaultValue = "DOCUMENT"),
            @OverrideAttribute(name = "contentName", allowHtml = "any"),
            @OverrideAttribute(name = "description", allowHtml = "any")
        }
    )
    public interface CreateContent {}

    /**
     * Creates text content and optional uploaded sub-content
     */
    @Service(
        name = "createTextAndUploadedContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createTextAndUploadedContent",
        description = "Creates text content and optional uploaded sub-content",
        auth = "true",
        implemented = {@Implements(service = "createTextContent"), @Implements(service = "uploadFileInterface", optional = "true"), @Implements(service = "createContentFromUploadedFile", optional = "true")}
    )
    public interface CreateTextAndUploadedContent {}

    /**
     * Creates a Text Document DataResource and Content Records
     */
    @Service(
        name = "createTextContent",
        engine = "group",
        description = "Creates a Text Document DataResource and Content Records",
        auth = "true",
        invokes = {@GroupInvoke(name = "createDataText", resultToContext = "true"), @GroupInvoke(name = "createContent", resultToContext = "true")}
    )
    public interface CreateTextContent {}

    /**
     * Creates content record from data resource and allows all content fields to be set
     */
    @Service(
        name = "createContentFromDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createContentFromDataResource",
        description = "Creates content record from data resource and allows all content fields to be set",
        implemented = {@Implements(service = "createContent", optional = "true")},
        attributes = {
            @Attribute(name = "dataResourceId", type = "String", mode = "IN"),
            @Attribute(name = "contentId", type = "String", mode = "OUT"),
            @Attribute(name = "uploadedFile", type = "java.nio.ByteBuffer", mode = "IN", optional = "true")
        }
    )
    public interface CreateContentFromDataResource {}

    /**
     * Accepts uploaded content and attaches to an existing data resource
     */
    @Service(
        name = "attachUploadToDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "attachUploadToDataResource",
        description = "Accepts uploaded content and attaches to an existing data resource",
        transactionTimeout = "300",
        implemented = {@Implements(service = "uploadFileInterface")},
        attributes = {
            @Attribute(name = "dataResourceId", type = "String", mode = "INOUT"),
            @Attribute(name = "dataResourceTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mimeTypeId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "rootDir", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface AttachUploadToDataResource {}

    /**
     * Accepts file upload, creates DataResource and Content records.
     */
    @Service(
        name = "createContentFromUploadedFile",
        engine = "group",
        description = "Accepts file upload, creates DataResource and Content records.",
        transactionTimeout = "300",
        invokes = {@GroupInvoke(name = "createDataResource", resultToContext = "true"), @GroupInvoke(name = "attachUploadToDataResource", resultToContext = "true"), @GroupInvoke(name = "createContentFromDataResource", resultToContext = "false")}
    )
    public interface CreateContentFromUploadedFile {}

    /**
     * Accepts file upload, updates DataResource and Content records.
     */
    @Service(
        name = "updateContentAndUploadedFile",
        engine = "group",
        description = "Accepts file upload, updates DataResource and Content records.",
        transactionTimeout = "300",
        invokes = {@GroupInvoke(name = "updateDataResource", resultToContext = "true"), @GroupInvoke(name = "attachUploadToDataResource", resultToContext = "true"), @GroupInvoke(name = "updateContent", resultToContext = "false")}
    )
    public interface UpdateContentAndUploadedFile {}

    /**
     * Copy a Content, e;ectronic text and assocs
     */
    @Service(
        name = "copyContentAndElectronicTextandAssoc",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "copyContentAndElectronicTextandAssoc",
        description = "Copy a Content, e;ectronic text and assocs",
        defaultEntityName = "Content",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CopyContentAndElectronicTextandAssoc {}

    /**
     * Update a Content
     */
    @Service(
        name = "updateContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateContent",
        description = "Update a Content",
        auth = "true",
        implemented = {@Implements(service = "updateContentAssoc", optional = "true")},
        entityAttributes = {
            @EntityAttributes(entityName = "Content", mode = "INOUT", include = "pk"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contentPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentIdFrom", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "skipPermissionCheck", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "displayFailCond", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeList", type = "List", mode = "INOUT", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "contentName", allowHtml = "any"),
            @OverrideAttribute(name = "description", allowHtml = "any")
        }
    )
    public interface UpdateContent {}

    /**
     * Updates a Text Document DataResource and Content Records
     */
    @Service(
        name = "updateTextContent",
        engine = "group",
        description = "Updates a Text Document DataResource and Content Records",
        auth = "true",
        invokes = {@GroupInvoke(name = "updateDataText", resultToContext = "true"), @GroupInvoke(name = "updateContent", resultToContext = "true")}
    )
    public interface UpdateTextContent {}

    /**
     * Remove Content
     */
    @Service(
        name = "removeContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "removeContent",
        description = "Remove Content",
        defaultEntityName = "Content",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeList", type = "List", mode = "INOUT", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "DELETE")
    )
    public interface RemoveContent {}

    /**
     * Remove Content and related entities (SCIPIO: enhanced)
     */
    @Service(
        name = "removeContentAndRelated",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "removeContentAndRelated",
        description = "Remove Content and related entities (SCIPIO: enhanced)",
        defaultEntityName = "Content",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeList", type = "List", mode = "INOUT", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "DELETE")
    )
    public interface RemoveContentAndRelated {}

    /**
     * SCIPIO: Remove Content and related entities and recursively removes the "to" ("child") associated Content records
     */
    @Service(
        name = "removeContentAndRelatedRecursiveTo",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "removeContentAndRelatedRecursiveTo",
        description = "SCIPIO: Remove Content and related entities and recursively removes the \"to\" (\"child\") associated Content records",
        defaultEntityName = "Content",
        auth = "true",
        implemented = {@Implements(service = "removeContentAndRelated")},
        attributes = {
            @Attribute(name = "recursiveTarget", type = "String", mode = "IN", optional = "true", defaultValue = "all", description = "Controls which related Content (To) to recursively remove: \"all\", \"active\" (non-expired)")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "DELETE")
    )
    public interface RemoveContentAndRelatedRecursiveTo {}

    /**
     * Check for permission to perform operation on Content
     */
    @Service(
        name = "checkContentPermission",
        engine = "java",
        location = "org.ofbiz.content.content.ContentPermissionServices",
        invoke = "checkContentPermission",
        description = "Check for permission to perform operation on Content",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "targetOperationString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeList", type = "List", mode = "INOUT", optional = "true"),
            @Attribute(name = "currentContent", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "entityOperation", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "privilegeEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quickCheckContentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "displayFailCond", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "displayPassCond", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userLoginId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "permissionStatus", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "permissionRecorder", type = "org.ofbiz.content.content.PermissionRecorder", mode = "OUT", optional = "true")
        }
    )
    public interface CheckContentPermission {}

    /**
     * Create a Content
     */
    @Service(
        name = "findRelatedContent",
        engine = "java",
        location = "org.ofbiz.content.content.ContentServices",
        invoke = "findRelatedContent",
        description = "Create a Content",
        defaultEntityName = "Content",
        auth = "true",
        attributes = {
            @Attribute(name = "currentContent", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "toFrom", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "contentAssocTypeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentTypeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "entityOperation", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentList", type = "List", mode = "OUT")
        }
    )
    public interface FindRelatedContent {}

    /**
     * Check for permission to perform operation on Content
     */
    @Service(
        name = "checkAssocPermission",
        engine = "java",
        location = "org.ofbiz.content.content.ContentPermissionServices",
        invoke = "checkAssocPermission",
        description = "Check for permission to perform operation on Content",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeList", type = "List", mode = "INOUT", optional = "true"),
            @Attribute(name = "userLogin", type = "GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "contentIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentIdFrom", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "privilegeEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "entityOperation", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentAssocTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentAssocPredicateId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "displayFailCond", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "permissionStatus", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "permissionRecorderTo", type = "org.ofbiz.content.content.PermissionRecorder", mode = "OUT", optional = "true"),
            @Attribute(name = "permissionRecorder", type = "org.ofbiz.content.content.PermissionRecorder", mode = "OUT", optional = "true")
        }
    )
    public interface CheckAssocPermission {}

    /**
     * Check for permission to perform operation on Content
     */
    @Service(
        name = "assocContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "assocContent",
        description = "Check for permission to perform operation on Content",
        defaultEntityName = "ContentAssoc",
        auth = "true",
        attributes = {
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeList", type = "List", mode = "INOUT", optional = "true"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "contentIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentIdFrom", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "entityOperation", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentAssocTypeId", type = "String", mode = "IN")
        }
    )
    public interface AssocContent {}

    /**
     * Create a ContentAssoc
     */
    @Service(
        name = "createContentAssoc",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createContentAssoc",
        description = "Create a ContentAssoc",
        defaultEntityName = "ContentAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "targetOperationString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "deactivateExisting", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "skipPermissionCheck", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "displayFailCond", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeList", type = "List", mode = "INOUT", optional = "true"),
            @Attribute(name = "contentIdFrom", type = "String", mode = "INOUT", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", mode = "INOUT")
        }
    )
    public interface CreateContentAssoc {}

    /**
     * Update a ContentAssoc
     */
    @Service(
        name = "updateContentAssoc",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateContentAssoc",
        description = "Update a ContentAssoc",
        defaultEntityName = "ContentAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "skipPermissionCheck", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "deactivateExisting", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "displayFailCond", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeList", type = "List", mode = "INOUT", optional = "true"),
            @Attribute(name = "contentIdFrom", type = "String", mode = "INOUT", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "UPDATE")
    )
    public interface UpdateContentAssoc {}

    /**
     * Expire a ContentAssoc
     */
    @Service(
        name = "expireContentAssoc",
        engine = "entity-auto",
        invoke = "expire",
        description = "Expire a ContentAssoc",
        defaultEntityName = "ContentAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "UPDATE")
    )
    public interface ExpireContentAssoc {}

    /**
     * Remove ContentAssoc
     */
    @Service(
        name = "removeContentAssoc",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "removeContentAssoc",
        description = "Remove ContentAssoc",
        defaultEntityName = "ContentAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "DELETE")
    )
    public interface RemoveContentAssoc {}

    /**
     * Set the Content Status
     */
    @Service(
        name = "setContentStatus",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "setContentStatus",
        description = "Set the Content Status",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "UPDATE")
    )
    public interface SetContentStatus {}

    /**
     * Create a ContentRole
     */
    @Service(
        name = "createContentRole",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createContentRole",
        description = "Create a ContentRole",
        defaultEntityName = "ContentRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateContentRole {}

    /**
     * Update a ContentRole
     */
    @Service(
        name = "updateContentRole",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateContentRole",
        description = "Update a ContentRole",
        defaultEntityName = "ContentRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "UPDATE")
    )
    public interface UpdateContentRole {}

    /**
     * Deactivate all ContentRoles
     */
    @Service(
        name = "deactivateAllContentRoles",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "deactivateAllContentRoles",
        description = "Deactivate all ContentRoles",
        defaultEntityName = "ContentRole",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "UPDATE")
    )
    public interface DeactivateAllContentRoles {}

    /**
     * Remove ContentRole
     */
    @Service(
        name = "removeContentRole",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "removeContentRole",
        description = "Remove ContentRole",
        defaultEntityName = "ContentRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ContentRole", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "DELETE")
    )
    public interface RemoveContentRole {}

    /**
     * Create missing Content Alternative URLs
     */
    @Service(
        name = "createMissingContentAltUrls",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createMissingContentAltUrls",
        description = "Create missing Content Alternative URLs",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "prodCatalogId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentsNotUpdated", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "contentsUpdated", type = "Integer", mode = "OUT", optional = "true")
        }
    )
    public interface CreateMissingContentAltUrls {}

    /**
     * Create a ContentMetaData
     */
    @Service(
        name = "createContentMetaData",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createContentMetaData",
        description = "Create a ContentMetaData",
        defaultEntityName = "ContentMetaData",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "CREATE")
    )
    public interface CreateContentMetaData {}

    /**
     * Update a ContentMetaData
     */
    @Service(
        name = "updateContentMetaData",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateContentMetaData",
        description = "Update a ContentMetaData",
        defaultEntityName = "ContentMetaData",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "UPDATE")
    )
    public interface UpdateContentMetaData {}

    /**
     * Remove ContentMetaData
     */
    @Service(
        name = "removeContentMetaData",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "removeContentMetaData",
        description = "Remove ContentMetaData",
        defaultEntityName = "ContentMetaData",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "DELETE")
    )
    public interface RemoveContentMetaData {}

    /**
     * Create a ContentOperation
     */
    @Service(
        name = "createContentOperation",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createContentOperation",
        description = "Create a ContentOperation",
        defaultEntityName = "ContentOperation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateContentOperation {}

    /**
     * Update a ContentOperation
     */
    @Service(
        name = "updateContentOperation",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateContentOperation",
        description = "Update a ContentOperation",
        defaultEntityName = "ContentOperation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateContentOperation {}

    /**
     * Remove ContentOperation
     */
    @Service(
        name = "removeContentOperation",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "removeContentOperation",
        description = "Remove ContentOperation",
        defaultEntityName = "ContentOperation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveContentOperation {}

    /**
     * Create a ContentPurpose
     */
    @Service(
        name = "createContentPurpose",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createContentPurpose",
        description = "Create a ContentPurpose",
        defaultEntityName = "ContentPurpose",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateContentPurpose {}

    /**
     * Update a ContentPurpose
     */
    @Service(
        name = "updateContentPurpose",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateContentPurpose",
        description = "Update a ContentPurpose",
        defaultEntityName = "ContentPurpose",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateContentPurpose {}

    /**
     * Remove ContentPurpose
     */
    @Service(
        name = "removeContentPurpose",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "removeContentPurpose",
        description = "Remove ContentPurpose",
        defaultEntityName = "ContentPurpose",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveContentPurpose {}

    /**
     * Removes content purposes and creates a new one
     */
    @Service(
        name = "updateSingleContentPurpose",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateSingleContentPurpose",
        description = "Removes content purposes and creates a new one",
        defaultEntityName = "ContentPurpose",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateSingleContentPurpose {}

    /**
     * Create a ContentPurposeOperation
     */
    @Service(
        name = "createContentPurposeOperation",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createContentPurposeOperation",
        description = "Create a ContentPurposeOperation",
        defaultEntityName = "ContentPurposeOperation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateContentPurposeOperation {}

    /**
     * Update a ContentPurposeOperation
     */
    @Service(
        name = "updateContentPurposeOperation",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateContentPurposeOperation",
        description = "Update a ContentPurposeOperation",
        defaultEntityName = "ContentPurposeOperation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateContentPurposeOperation {}

    /**
     * Remove ContentPurposeOperation
     */
    @Service(
        name = "removeContentPurposeOperation",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "removeContentPurposeOperation",
        description = "Remove ContentPurposeOperation",
        defaultEntityName = "ContentPurposeOperation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveContentPurposeOperation {}

    /**
     * Create a ContentAttribute
     */
    @Service(
        name = "createContentAttribute",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createContentAttribute",
        description = "Create a ContentAttribute",
        defaultEntityName = "ContentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "CREATE")
    )
    public interface CreateContentAttribute {}

    /**
     * Update a ContentAttribute
     */
    @Service(
        name = "updateContentAttribute",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateContentAttribute",
        description = "Update a ContentAttribute",
        defaultEntityName = "ContentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "UPDATE")
    )
    public interface UpdateContentAttribute {}

    /**
     * Remove ContentAttribute
     */
    @Service(
        name = "removeContentAttribute",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "removeContentAttribute",
        description = "Remove ContentAttribute",
        defaultEntityName = "ContentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "DELETE")
    )
    public interface RemoveContentAttribute {}

    /**
     * Create a ContentKeyword
     */
    @Service(
        name = "createContentKeyword",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createContentKeyword",
        description = "Create a ContentKeyword",
        defaultEntityName = "ContentKeyword",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "CREATE")
    )
    public interface CreateContentKeyword {}

    /**
     * Update a ContentKeyword
     */
    @Service(
        name = "updateContentKeyword",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateContentKeyword",
        description = "Update a ContentKeyword",
        defaultEntityName = "ContentKeyword",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "UPDATE")
    )
    public interface UpdateContentKeyword {}

    /**
     * Delete a ContentKeyword
     */
    @Service(
        name = "deleteContentKeyword",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "deleteContentKeyword",
        description = "Delete a ContentKeyword",
        defaultEntityName = "ContentKeyword",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "DELETE")
    )
    public interface DeleteContentKeyword {}

    /**
     * Delete all the keywords of a content
     */
    @Service(
        name = "deleteContentKeywords",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "deleteContentKeywords",
        description = "Delete all the keywords of a content",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "DELETE")
    )
    public interface DeleteContentKeywords {}

    /**
     * Index the Keywords for a Content
     */
    @Service(
        name = "indexContentKeywords",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "indexContentKeywords",
        description = "Index the Keywords for a Content",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "contentInstance", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true")
        }
    )
    public interface IndexContentKeywords {}

    /**
     * Induce all the keywords of a content, ignoring the flag in the Content.
     */
    @Service(
        name = "forceIndexContentKeywords",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "forceIndexContentKeywords",
        description = "Induce all the keywords of a content, ignoring the flag in the Content.",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "CREATE")
    )
    public interface ForceIndexContentKeywords {}

    /**
     * Create a ContentRevision
     */
    @Service(
        name = "createContentRevision",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "createContentRevision",
        description = "Create a ContentRevision",
        defaultEntityName = "ContentRevision",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateContentRevision {}

    /**
     * Update a ContentRevision
     */
    @Service(
        name = "updateContentRevision",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "updateContentRevision",
        description = "Update a ContentRevision",
        defaultEntityName = "ContentRevision",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateContentRevision {}

    /**
     * Remove ContentRevision
     */
    @Service(
        name = "removeContentRevision",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "removeContentRevision",
        description = "Remove ContentRevision",
        defaultEntityName = "ContentRevision",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveContentRevision {}

    /**
     * Create a ContentRevisionItem
     */
    @Service(
        name = "createContentRevisionItem",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "createContentRevisionItem",
        description = "Create a ContentRevisionItem",
        defaultEntityName = "ContentRevisionItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateContentRevisionItem {}

    /**
     * Update a ContentRevisionItem
     */
    @Service(
        name = "updateContentRevisionItem",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "updateContentRevisionItem",
        description = "Update a ContentRevisionItem",
        defaultEntityName = "ContentRevisionItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateContentRevisionItem {}

    /**
     * Remove ContentRevisionItem
     */
    @Service(
        name = "removeContentRevisionItem",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "removeContentRevisionItem",
        description = "Remove ContentRevisionItem",
        defaultEntityName = "ContentRevisionItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveContentRevisionItem {}

    /**
     * Create a ContentApproval
     */
    @Service(
        name = "createContentApproval",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "createContentApproval",
        description = "Create a ContentApproval",
        defaultEntityName = "ContentApproval",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateContentApproval {}

    /**
     * Update a ContentApproval
     */
    @Service(
        name = "updateContentApproval",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "updateContentApproval",
        description = "Update a ContentApproval",
        defaultEntityName = "ContentApproval",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateContentApproval {}

    /**
     * Remove ContentApproval
     */
    @Service(
        name = "removeContentApproval",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml",
        invoke = "removeContentApproval",
        description = "Remove ContentApproval",
        defaultEntityName = "ContentApproval",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveContentApproval {}

}
