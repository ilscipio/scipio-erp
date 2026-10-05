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
public class WebsiteServices {

    /**
     * Create a WebSite
     */
    @Service(
        name = "createWebSite",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "createWebSite",
        description = "Create a WebSite",
        defaultEntityName = "WebSite",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "siteName", optional = "false")
        }
    )
    public interface CreateWebSite {}

    /**
     * Update a WebSite
     */
    @Service(
        name = "updateWebSite",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "updateWebSite",
        description = "Update a WebSite",
        defaultEntityName = "WebSite",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateWebSite {}

    /**
     * Create a WebSite Content
     */
    @Service(
        name = "createWebSiteContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "createWebSiteContent",
        description = "Create a WebSite Content",
        defaultEntityName = "WebSiteContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateWebSiteContent {}

    /**
     * Update a WebSite Content
     */
    @Service(
        name = "updateWebSiteContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "updateWebSiteContent",
        description = "Update a WebSite Content",
        defaultEntityName = "WebSiteContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateWebSiteContent {}

    /**
     * Remove a WebSite Content
     */
    @Service(
        name = "removeWebSiteContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "removeWebSiteContent",
        description = "Remove a WebSite Content",
        defaultEntityName = "WebSiteContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveWebSiteContent {}

    /**
     * Create a WebSite ContentType
     */
    @Service(
        name = "createWebSiteContentType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "createWebSiteContentType",
        description = "Create a WebSite ContentType",
        defaultEntityName = "WebSiteContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateWebSiteContentType {}

    /**
     * Update a WebSite ContentType
     */
    @Service(
        name = "updateWebSiteContentType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "updateWebSiteContentType",
        description = "Update a WebSite ContentType",
        defaultEntityName = "WebSiteContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateWebSiteContentType {}

    /**
     * Remove a WebSite ContentType
     */
    @Service(
        name = "removeWebSiteContentType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "removeWebSiteContentType",
        description = "Remove a WebSite ContentType",
        defaultEntityName = "WebSiteContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveWebSiteContentType {}

    /**
     * Create a WebSite Path Alias
     */
    @Service(
        name = "createWebSitePathAlias",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "createWebSitePathAlias",
        description = "Create a WebSite Path Alias",
        defaultEntityName = "WebSitePathAlias",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateWebSitePathAlias {}

    /**
     * Update a WebSite Path Alias
     */
    @Service(
        name = "updateWebSitePathAlias",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "updateWebSitePathAlias",
        description = "Update a WebSite Path Alias",
        defaultEntityName = "WebSitePathAlias",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateWebSitePathAlias {}

    /**
     * Remove a WebSite Path Alias
     */
    @Service(
        name = "removeWebSitePathAlias",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "removeWebSitePathAlias",
        description = "Remove a WebSite Path Alias",
        defaultEntityName = "WebSitePathAlias",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveWebSitePathAlias {}

    /**
     * Get a WebSite Path Alias
     */
    @Service(
        name = "getWebSitePathAlias",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "getWebSitePathAlias",
        description = "Get a WebSite Path Alias",
        defaultEntityName = "WebSitePathAlias",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "OUT", include = "nonpk")
        }
    )
    public interface GetWebSitePathAlias {}

    /**
     * WebSite Role Interface
     */
    @Service(
        name = "webSiteRoleInterface",
        engine = "interface",
        description = "WebSite Role Interface",
        entityAttributes = {
            @EntityAttributes(entityName = "WebSiteRole", mode = "IN")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "thruDate", optional = "true"),
            @OverrideAttribute(name = "sequenceNum", optional = "true")
        }
    )
    public interface WebSiteRoleInterface {}

    /**
     * Add WebSite Role; NOTE: This service is being deprecated in favor of createWebSiteRole
     */
    @Service(
        name = "addWebSiteRole",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "createWebSiteRole",
        description = "Add WebSite Role; NOTE: This service is being deprecated in favor of createWebSiteRole",
        auth = "true",
        implemented = {@Implements(service = "webSiteRoleInterface")},
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface AddWebSiteRole {}

    /**
     * Add WebSite Role
     */
    @Service(
        name = "createWebSiteRole",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "createWebSiteRole",
        description = "Add WebSite Role",
        auth = "true",
        implemented = {@Implements(service = "webSiteRoleInterface")},
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateWebSiteRole {}

    /**
     * Add WebSite Role
     */
    @Service(
        name = "updateWebSiteRole",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "updateWebSiteRole",
        description = "Add WebSite Role",
        auth = "true",
        implemented = {@Implements(service = "webSiteRoleInterface")},
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateWebSiteRole {}

    /**
     * Remove WebSite Role
     */
    @Service(
        name = "removeWebSiteRole",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "removeWebSiteRole",
        description = "Remove WebSite Role",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "WebSiteRole", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface RemoveWebSiteRole {}

    /**
     * Auto Create Content Publish Points
     */
    @Service(
        name = "autoCreateWebSiteContent",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "quickCreateWebSiteContent",
        description = "Auto Create Content Publish Points",
        auth = "true",
        attributes = {
            @Attribute(name = "webSiteId", type = "String", mode = "IN"),
            @Attribute(name = "webSiteContentTypeId", type = "List", mode = "IN")
        }
    )
    public interface AutoCreateWebSiteContent {}

    /**
     * Generate Missing Seo URL's for Website
     */
    @Service(
        name = "generateMissingSeoUrlForWebsite",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/website/WebSiteServices.xml",
        invoke = "generateMissingSeoUrlForWebsite",
        description = "Generate Missing Seo URL's for Website",
        auth = "true",
        transactionTimeout = "36000000",
        attributes = {
            @Attribute(name = "webSiteId", type = "String", mode = "IN"),
            @Attribute(name = "prodCatalogId", type = "String", mode = "IN"),
            @Attribute(name = "typeGenerate", type = "List", mode = "IN")
        }
    )
    public interface GenerateMissingSeoUrlForWebsite {}

}
