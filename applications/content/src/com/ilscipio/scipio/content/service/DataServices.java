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
public class DataServices {

    /**
     * Create a DataResource
     */
    @Service(
        name = "createDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createDataResource",
        description = "Create a DataResource",
        defaultEntityName = "DataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "dataResourceId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "skipPermissionCheck", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "dataResourceId", type = "String", mode = "OUT"),
            @Attribute(name = "dataResource", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "uploadedFile", type = "java.nio.ByteBuffer", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "objectInfo", allowHtml = "any"),
            @OverrideAttribute(name = "dataResourceName", allowHtml = "any")
        }
    )
    public interface CreateDataResource {}

    /**
     * Create a DataResource and link this data to the content present
     */
    @Service(
        name = "createDataResourceAndAssocToContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createDataResourceAndAssocToContent",
        description = "Create a DataResource and link this data to the content present",
        defaultEntityName = "DataResource",
        auth = "true",
        implemented = {@Implements(service = "createDataResource", optional = "true")},
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "INOUT"),
            @Attribute(name = "templateDataResource", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "CREATE")
    )
    public interface CreateDataResourceAndAssocToContent {}

    /**
     * Update a DataResource
     */
    @Service(
        name = "updateDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateDataResource",
        description = "Update a DataResource",
        defaultEntityName = "DataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "dataResourceId", type = "String", mode = "IN"),
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "skipPermissionCheck", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "dataResourceId", type = "String", mode = "OUT"),
            @Attribute(name = "dataResource", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "objectInfo", allowHtml = "any"),
            @OverrideAttribute(name = "dataResourceName", allowHtml = "any")
        }
    )
    public interface UpdateDataResource {}

    /**
     * Remove DataResource
     */
    @Service(
        name = "removeDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "deleteDataResource",
        description = "Remove DataResource",
        defaultEntityName = "DataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "skipPermissionCheck", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "DELETE")
    )
    public interface RemoveDataResource {}

    /**
     * Uses ECA to decide if we should call createElectronicText or just createDataResource (SHORT_TEXT)
     */
    @Service(
        name = "createDataText",
        engine = "route",
        description = "Uses ECA to decide if we should call createElectronicText or just createDataResource (SHORT_TEXT)",
        auth = "true",
        implemented = {@Implements(service = "createDataResource"), @Implements(service = "createElectronicText")}
    )
    public interface CreateDataText {}

    /**
     * Uses ECA to decide if we should call updateElectronicText or just updateDataResource (SHORT_TEXT)
     */
    @Service(
        name = "updateDataText",
        engine = "route",
        description = "Uses ECA to decide if we should call updateElectronicText or just updateDataResource (SHORT_TEXT)",
        auth = "true",
        implemented = {@Implements(service = "updateDataResource"), @Implements(service = "updateElectronicText")}
    )
    public interface UpdateDataText {}

    /**
     * Create a DataResource and, possibly, ElectronicText or ImageDataResource
     */
    @Service(
        name = "createDataResourceAndText",
        engine = "java",
        location = "org.ofbiz.content.data.DataServices",
        invoke = "createDataResourceAndText",
        description = "Create a DataResource and, possibly, ElectronicText or ImageDataResource",
        defaultEntityName = "DataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "textData", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "skipPermissionCheck", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateDataResourceAndText {}

    /**
     * Create a DataResource and, possibly, ElectronicText or ImageDataResource
     */
    @Service(
        name = "updateDataResourceAndText",
        engine = "java",
        location = "org.ofbiz.content.data.DataServices",
        invoke = "updateDataResourceAndText",
        description = "Create a DataResource and, possibly, ElectronicText or ImageDataResource",
        defaultEntityName = "DataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "textData", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "targetOperationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "contentPurposeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "skipPermissionCheck", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateDataResourceAndText {}

    /**
     * Create a ElectronicText
     */
    @Service(
        name = "createElectronicText",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createElectronicText",
        description = "Create a ElectronicText",
        defaultEntityName = "ElectronicText",
        auth = "true",
        implemented = {@Implements(service = "createDataResource")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "dataResourceTypeId", defaultValue = "ELECTRONIC_TEXT"),
            @OverrideAttribute(name = "textData", allowHtml = "any")
        }
    )
    public interface CreateElectronicText {}

    /**
     * Update a ElectronicText
     */
    @Service(
        name = "updateElectronicText",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateElectronicText",
        description = "Update a ElectronicText",
        defaultEntityName = "ElectronicText",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "INOUT", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "textData", allowHtml = "any")
        }
    )
    public interface UpdateElectronicText {}

    /**
     * Create a ElectronicText with Form code
     */
    @Service(
        name = "createElectronicTextForm",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createElectronicTextForm",
        description = "Create a ElectronicText with Form code",
        defaultEntityName = "ElectronicText",
        auth = "true",
        implemented = {@Implements(service = "createDataResource")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "dataResourceTypeId", defaultValue = "ELECTRONIC_TEXT"),
            @OverrideAttribute(name = "textData", allowHtml = "any")
        }
    )
    public interface CreateElectronicTextForm {}

    /**
     * Update a ElectronicText with Form code
     */
    @Service(
        name = "updateElectronicTextForm",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateElectronicTextForm",
        description = "Update a ElectronicText with Form code",
        defaultEntityName = "ElectronicText",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "INOUT", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "textData", allowHtml = "any")
        }
    )
    public interface UpdateElectronicTextForm {}

    /**
     * Remove ElectronicText
     */
    @Service(
        name = "removeElectronicText",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "removeElectronicText",
        description = "Remove ElectronicText",
        defaultEntityName = "ElectronicText",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "DELETE")
    )
    public interface RemoveElectronicText {}

    /**
     * Get a ElectronicText: Can pass either content value object or contentId
     */
    @Service(
        name = "getElectronicText",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "getElectronicText",
        description = "Get a ElectronicText: Can pass either content value object or contentId",
        defaultEntityName = "ElectronicText",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "content", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "dataResourceId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "textData", type = "String", mode = "OUT")
        }
    )
    public interface GetElectronicText {}

    /**
     * Create an ImageDataResource
     */
    @Service(
        name = "createImageDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createImageDataResource",
        description = "Create an ImageDataResource",
        defaultEntityName = "ImageDataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "CREATE")
    )
    public interface CreateImageDataResource {}

    /**
     * Update an ImageDataResource
     */
    @Service(
        name = "updateImageDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateImageDataResource",
        description = "Update an ImageDataResource",
        defaultEntityName = "ImageDataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "UPDATE")
    )
    public interface UpdateImageDataResource {}

    /**
     * Remove an ImageDataResource
     */
    @Service(
        name = "removeImageDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "removeImageDataResource",
        description = "Remove an ImageDataResource",
        defaultEntityName = "ImageDataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "DELETE")
    )
    public interface RemoveImageDataResource {}

    /**
     * Create a VideoDataResource
     */
    @Service(
        name = "createVideoDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createVideoDataResource",
        description = "Create a VideoDataResource",
        defaultEntityName = "VideoDataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "CREATE")
    )
    public interface CreateVideoDataResource {}

    /**
     * Update an VideoDataResource
     */
    @Service(
        name = "updateVideoDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateVideoDataResource",
        description = "Update an VideoDataResource",
        defaultEntityName = "VideoDataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "UPDATE")
    )
    public interface UpdateVideoDataResource {}

    /**
     * Remove an VideoDataResource
     */
    @Service(
        name = "removeVideoDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "removeVideoDataResource",
        description = "Remove an VideoDataResource",
        defaultEntityName = "VideoDataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "DELETE")
    )
    public interface RemoveVideoDataResource {}

    /**
     * Create an AudioDataResource
     */
    @Service(
        name = "createAudioDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createAudioDataResource",
        description = "Create an AudioDataResource",
        defaultEntityName = "AudioDataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "CREATE")
    )
    public interface CreateAudioDataResource {}

    /**
     * Update an AudioDataResource
     */
    @Service(
        name = "updateAudioDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateAudioDataResource",
        description = "Update an AudioDataResource",
        defaultEntityName = "AudioDataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "UPDATE")
    )
    public interface UpdateAudioDataResource {}

    /**
     * Remove an AudioDataResource
     */
    @Service(
        name = "removeAudioDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "removeAudioDataResource",
        description = "Remove an AudioDataResource",
        defaultEntityName = "AudioDataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "DELETE")
    )
    public interface RemoveAudioDataResource {}

    /**
     * Create an OtherDataResource
     */
    @Service(
        name = "createOtherDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "createOtherDataResource",
        description = "Create an OtherDataResource",
        defaultEntityName = "OtherDataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "CREATE")
    )
    public interface CreateOtherDataResource {}

    /**
     * Update an OtherDataResource
     */
    @Service(
        name = "updateOtherDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "updateOtherDataResource",
        description = "Update an OtherDataResource",
        defaultEntityName = "OtherDataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "UPDATE")
    )
    public interface UpdateOtherDataResource {}

    /**
     * Remove an OtherDataResource
     */
    @Service(
        name = "removeOtherDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/data/DataServices.xml",
        invoke = "removeOtherDataResource",
        description = "Remove an OtherDataResource",
        defaultEntityName = "OtherDataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "DELETE")
    )
    public interface RemoveOtherDataResource {}

    /**
     * Create an DataResourceMetaData
     */
    @Service(
        name = "createDataResourceMetaData",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an DataResourceMetaData",
        defaultEntityName = "DataResourceMetaData",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "CREATE")
    )
    public interface CreateDataResourceMetaData {}

    /**
     * Update an DataResourceMetaData
     */
    @Service(
        name = "updateDataResourceMetaData",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an DataResourceMetaData",
        defaultEntityName = "DataResourceMetaData",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "UPDATE")
    )
    public interface UpdateDataResourceMetaData {}

    /**
     * Remove an DataResourceMetaData
     */
    @Service(
        name = "removeDataResourceMetaData",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove an DataResourceMetaData",
        defaultEntityName = "DataResourceMetaData",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "DELETE")
    )
    public interface RemoveDataResourceMetaData {}

    /**
     * Create an DataResourcePurpose
     */
    @Service(
        name = "createDataResourcePurpose",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an DataResourcePurpose",
        defaultEntityName = "DataResourcePurpose",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "CREATE")
    )
    public interface CreateDataResourcePurpose {}

    /**
     * Update an DataResourcePurpose
     */
    @Service(
        name = "updateDataResourcePurpose",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an DataResourcePurpose",
        defaultEntityName = "DataResourcePurpose",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "UPDATE")
    )
    public interface UpdateDataResourcePurpose {}

    /**
     * Remove an DataResourcePurpose
     */
    @Service(
        name = "removeDataResourcePurpose",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove an DataResourcePurpose",
        defaultEntityName = "DataResourcePurpose",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "genericDataResourcePermission", mainAction = "DELETE")
    )
    public interface RemoveDataResourcePurpose {}

    /**
     * Create a File
     */
    @Service(
        name = "createFile",
        engine = "java",
        location = "org.ofbiz.content.data.DataServices",
        invoke = "createFile",
        description = "Create a File",
        auth = "true",
        implemented = {@Implements(service = "createDataResource")},
        attributes = {
            @Attribute(name = "dataResource", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "binData", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "textData", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "rootDir", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "dataResourceTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "objectInfo", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateFile {}

    /**
     * Create a File No Permission Required
     */
    @Service(
        name = "createAnonFile",
        engine = "java",
        location = "org.ofbiz.content.data.DataServices",
        invoke = "createFileNoPerm",
        description = "Create a File No Permission Required",
        implemented = {@Implements(service = "createFile")}
    )
    public interface CreateAnonFile {}

    /**
     * Update a File
     */
    @Service(
        name = "updateFile",
        engine = "java",
        location = "org.ofbiz.content.data.DataServices",
        invoke = "updateFile",
        description = "Update a File",
        auth = "true",
        attributes = {
            @Attribute(name = "dataResource", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "binData", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "textData", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "rootDir", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "dataResourceTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "objectInfo", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateFile {}

    @Service(
        name = "clearAssociatedRenderCache",
        engine = "java",
        location = "org.ofbiz.content.data.DataServices",
        invoke = "clearAssociatedRenderCache",
        defaultEntityName = "DataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface ClearAssociatedRenderCache {}

    /**
     * Create a Data Template Type
     */
    @Service(
        name = "createDataTemplateType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Data Template Type",
        defaultEntityName = "DataTemplateType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateDataTemplateType {}

    /**
     * Update a Data Template Type
     */
    @Service(
        name = "updateDataTemplateType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Data Template Type",
        defaultEntityName = "DataTemplateType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateDataTemplateType {}

    /**
     * Delete a Data Template Type
     */
    @Service(
        name = "deleteDataTemplateType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Data Template Type",
        defaultEntityName = "DataTemplateType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteDataTemplateType {}

}
