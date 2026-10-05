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
public class ContenttypesServices {

    /**
     * Create a ContentType
     */
    @Service(
        name = "createContentType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createContentType",
        description = "Create a ContentType",
        defaultEntityName = "ContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateContentType {}

    /**
     * Update a ContentType
     */
    @Service(
        name = "updateContentType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateContentType",
        description = "Update a ContentType",
        defaultEntityName = "ContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateContentType {}

    /**
     * Remove ContentType
     */
    @Service(
        name = "removeContentType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "removeContentType",
        description = "Remove ContentType",
        defaultEntityName = "ContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveContentType {}

    /**
     * Create a ContentAssocType
     */
    @Service(
        name = "createContentAssocType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createContentAssocType",
        description = "Create a ContentAssocType",
        defaultEntityName = "ContentAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateContentAssocType {}

    /**
     * Update a ContentAssocType
     */
    @Service(
        name = "updateContentAssocType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateContentAssocType",
        description = "Update a ContentAssocType",
        defaultEntityName = "ContentAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateContentAssocType {}

    /**
     * Remove ContentAssocType
     */
    @Service(
        name = "removeContentAssocType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "removeContentAssocType",
        description = "Remove ContentAssocType",
        defaultEntityName = "ContentAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveContentAssocType {}

    /**
     * Create a ContentTypeAttr
     */
    @Service(
        name = "createContentTypeAttr",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createContentTypeAttr",
        description = "Create a ContentTypeAttr",
        defaultEntityName = "ContentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateContentTypeAttr {}

    /**
     * Remove ContentTypeAttr
     */
    @Service(
        name = "removeContentTypeAttr",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "removeContentTypeAttr",
        description = "Remove ContentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ContentTypeAttr", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveContentTypeAttr {}

    /**
     * Create a ContentAssocPredicate
     */
    @Service(
        name = "createContentAssocPredicate",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createContentAssocPredicate",
        description = "Create a ContentAssocPredicate",
        defaultEntityName = "ContentAssocPredicate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateContentAssocPredicate {}

    /**
     * Update a ContentAssocPredicate
     */
    @Service(
        name = "updateContentAssocPredicate",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateContentAssocPredicate",
        description = "Update a ContentAssocPredicate",
        defaultEntityName = "ContentAssocPredicate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateContentAssocPredicate {}

    /**
     * Remove ContentAssocPredicate
     */
    @Service(
        name = "removeContentAssocPredicate",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "removeContentAssocPredicate",
        description = "Remove ContentAssocPredicate",
        defaultEntityName = "ContentAssocPredicate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ContentAssocPredicate", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveContentAssocPredicate {}

    /**
     * Create a ContentPurposeType
     */
    @Service(
        name = "createContentPurposeType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createContentPurposeType",
        description = "Create a ContentPurposeType",
        defaultEntityName = "ContentPurposeType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateContentPurposeType {}

    /**
     * Update a ContentPurposeType
     */
    @Service(
        name = "updateContentPurposeType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateContentPurposeType",
        description = "Update a ContentPurposeType",
        defaultEntityName = "ContentPurposeType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateContentPurposeType {}

    /**
     * Remove ContentPurposeType
     */
    @Service(
        name = "removeContentPurposeType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "removeContentPurposeType",
        description = "Remove ContentPurposeType",
        defaultEntityName = "ContentPurposeType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveContentPurposeType {}

    /**
     * Create a CharacterSet
     */
    @Service(
        name = "createCharacterSet",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createCharacterSet",
        description = "Create a CharacterSet",
        defaultEntityName = "CharacterSet",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateCharacterSet {}

    /**
     * Update a CharacterSet
     */
    @Service(
        name = "updateCharacterSet",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateCharacterSet",
        description = "Update a CharacterSet",
        defaultEntityName = "CharacterSet",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateCharacterSet {}

    /**
     * Remove CharacterSet
     */
    @Service(
        name = "removeCharacterSet",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "removeCharacterSet",
        description = "Remove CharacterSet",
        defaultEntityName = "CharacterSet",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveCharacterSet {}

    /**
     * Create a DataResourceType
     */
    @Service(
        name = "createDataResourceType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createDataResourceType",
        description = "Create a DataResourceType",
        defaultEntityName = "DataResourceType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateDataResourceType {}

    /**
     * Update a DataResourceType
     */
    @Service(
        name = "updateDataResourceType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateDataResourceType",
        description = "Update a DataResourceType",
        defaultEntityName = "DataResourceType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateDataResourceType {}

    /**
     * Remove DataResourceType
     */
    @Service(
        name = "removeDataResourceType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "removeDataResourceType",
        description = "Remove DataResourceType",
        defaultEntityName = "DataResourceType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveDataResourceType {}

    /**
     * Create a DataResourceTypeAttr
     */
    @Service(
        name = "createDataResourceTypeAttr",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createDataResourceTypeAttr",
        description = "Create a DataResourceTypeAttr",
        defaultEntityName = "DataResourceTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateDataResourceTypeAttr {}

    /**
     * Update a DataResourceTypeAttr
     */
    @Service(
        name = "updateDataResourceTypeAttr",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateDataResourceTypeAttr",
        description = "Update a DataResourceTypeAttr",
        defaultEntityName = "DataResourceTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateDataResourceTypeAttr {}

    /**
     * Remove DataResourceTypeAttr
     */
    @Service(
        name = "removeDataResourceTypeAttr",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "removeDataResourceTypeAttr",
        description = "Remove DataResourceTypeAttr",
        defaultEntityName = "DataResourceTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveDataResourceTypeAttr {}

    /**
     * Create a FileExtension
     */
    @Service(
        name = "createFileExtension",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createFileExtension",
        description = "Create a FileExtension",
        defaultEntityName = "FileExtension",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateFileExtension {}

    /**
     * Update a FileExtension
     */
    @Service(
        name = "updateFileExtension",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateFileExtension",
        description = "Update a FileExtension",
        defaultEntityName = "FileExtension",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateFileExtension {}

    /**
     * Remove FileExtension
     */
    @Service(
        name = "removeFileExtension",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "removeFileExtension",
        description = "Remove FileExtension",
        defaultEntityName = "FileExtension",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveFileExtension {}

    /**
     * Create a MetaDataPredicate
     */
    @Service(
        name = "createMetaDataPredicate",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createMetaDataPredicate",
        description = "Create a MetaDataPredicate",
        defaultEntityName = "MetaDataPredicate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateMetaDataPredicate {}

    /**
     * Update a MetaDataPredicate
     */
    @Service(
        name = "updateMetaDataPredicate",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateMetaDataPredicate",
        description = "Update a MetaDataPredicate",
        defaultEntityName = "MetaDataPredicate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateMetaDataPredicate {}

    /**
     * Remove MetaDataPredicate
     */
    @Service(
        name = "removeMetaDataPredicate",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "removeMetaDataPredicate",
        description = "Remove MetaDataPredicate",
        defaultEntityName = "MetaDataPredicate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveMetaDataPredicate {}

    /**
     * Create a MimeType
     */
    @Service(
        name = "createMimeType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createMimeType",
        description = "Create a MimeType",
        defaultEntityName = "MimeType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateMimeType {}

    /**
     * Update a MimeType
     */
    @Service(
        name = "updateMimeType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateMimeType",
        description = "Update a MimeType",
        defaultEntityName = "MimeType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateMimeType {}

    /**
     * Remove MimeType
     */
    @Service(
        name = "removeMimeType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "removeMimeType",
        description = "Remove MimeType",
        defaultEntityName = "MimeType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveMimeType {}

    /**
     * Create a MimeTypeHtmlTemplate
     */
    @Service(
        name = "createMimeTypeHtmlTemplate",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createMimeTypeHtmlTemplate",
        description = "Create a MimeTypeHtmlTemplate",
        defaultEntityName = "MimeTypeHtmlTemplate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateMimeTypeHtmlTemplate {}

    /**
     * Update a MimeTypeHtmlTemplate
     */
    @Service(
        name = "updateMimeTypeHtmlTemplate",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateMimeTypeHtmlTemplate",
        description = "Update a MimeTypeHtmlTemplate",
        defaultEntityName = "MimeTypeHtmlTemplate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateMimeTypeHtmlTemplate {}

    /**
     * Remove MimeTypeHtmlTemplate
     */
    @Service(
        name = "removeMimeTypeHtmlTemplate",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "removeMimeTypeHtmlTemplate",
        description = "Remove MimeTypeHtmlTemplate",
        defaultEntityName = "MimeTypeHtmlTemplate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveMimeTypeHtmlTemplate {}

}
