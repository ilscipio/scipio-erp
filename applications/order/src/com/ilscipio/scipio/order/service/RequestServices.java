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
package com.ilscipio.scipio.order.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class RequestServices {

    /**
     *              Performs a security check for CustRequest. The user, if enters a request for someone else,             must have one of the base ORDERMGR_CRQ CRUD+ADMIN permissions.         
     */
    @Service(
        name = "custRequestPermissionCheck",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "custRequestPermissionCheck",
        description = "\n            Performs a security check for CustRequest. The user, if enters a request for someone else,\n            must have one of the base ORDERMGR_CRQ CRUD+ADMIN permissions.\n        ",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "fromPartyId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CustRequestPermissionCheck {}

    /**
     * Create a custRequest record and optionally create a custRequest item.
     */
    @Service(
        name = "createCustRequest",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "createCustRequest",
        description = "Create a custRequest record and optionally create a custRequest item.",
        defaultEntityName = "CustRequest",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "CustRequestItem", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "custRequestPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "custRequestName", allowHtml = "any"),
            @OverrideAttribute(name = "description", allowHtml = "any"),
            @OverrideAttribute(name = "story", allowHtml = "any")
        }
    )
    public interface CreateCustRequest {}

    /**
     * Update a custRequest record
     */
    @Service(
        name = "updateCustRequest",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "updateCustRequest",
        description = "Update a custRequest record",
        defaultEntityName = "CustRequest",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT"),
            @Attribute(name = "story", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "custRequestName", allowHtml = "any"),
            @OverrideAttribute(name = "description", allowHtml = "any")
        }
    )
    public interface UpdateCustRequest {}

    /**
     * Delete a custRequest record in draft status
     */
    @Service(
        name = "deleteCustRequest",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "deleteCustRequest",
        description = "Delete a custRequest record in draft status",
        defaultEntityName = "CustRequest",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCustRequest {}

    /**
     * Create CustRequestAttribute record
     */
    @Service(
        name = "createCustRequestAttribute",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "createCustRequestAttribute",
        description = "Create CustRequestAttribute record",
        auth = "true",
        attributes = {
            @Attribute(name = "custRequestId", type = "String", mode = "IN"),
            @Attribute(name = "attrName", type = "String", mode = "IN"),
            @Attribute(name = "attrValue", type = "String", mode = "IN")
        }
    )
    public interface CreateCustRequestAttribute {}

    /**
     * Update CustRequestAttribute record
     */
    @Service(
        name = "updateCustRequestAttribute",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "updateCustRequestAttribute",
        description = "Update CustRequestAttribute record",
        auth = "true",
        attributes = {
            @Attribute(name = "custRequestId", type = "String", mode = "IN"),
            @Attribute(name = "attrName", type = "String", mode = "IN"),
            @Attribute(name = "attrValue", type = "String", mode = "IN")
        }
    )
    public interface UpdateCustRequestAttribute {}

    /**
     * Create a CustRequestItem record
     */
    @Service(
        name = "createCustRequestItem",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "createCustRequestItem",
        description = "Create a CustRequestItem record",
        defaultEntityName = "CustRequestItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "custRequestItemSeqId", optional = "true"),
            @OverrideAttribute(name = "story", allowHtml = "any"),
            @OverrideAttribute(name = "description", allowHtml = "any")
        }
    )
    public interface CreateCustRequestItem {}

    /**
     * Update a CustRequestItem record
     */
    @Service(
        name = "updateCustRequestItem",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "updateCustRequestItem",
        description = "Update a CustRequestItem record",
        defaultEntityName = "CustRequestItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "story", allowHtml = "any"),
            @OverrideAttribute(name = "description", allowHtml = "any")
        }
    )
    public interface UpdateCustRequestItem {}

    /**
     * Copy a CustRequest
     */
    @Service(
        name = "copyCustRequestItem",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "copyCustRequestItem",
        description = "Copy a CustRequest",
        defaultEntityName = "CustRequestItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "custRequestIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "custRequestItemSeqIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "copyLinkedQuotes", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CopyCustRequestItem {}

    /**
     * Remove a CustRequestItem record
     */
    @Service(
        name = "removeCustRequestItem",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "removeCustRequestItem",
        description = "Remove a CustRequestItem record",
        defaultEntityName = "CustRequestItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface RemoveCustRequestItem {}

    /**
     * Create a CustRequestParty record
     */
    @Service(
        name = "createCustRequestParty",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "createCustRequestParty",
        description = "Create a CustRequestParty record",
        defaultEntityName = "CustRequestParty",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface CreateCustRequestParty {}

    /**
     * Update CustRequestParty record
     */
    @Service(
        name = "updateCustRequestParty",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "updateCustRequestParty",
        description = "Update CustRequestParty record",
        defaultEntityName = "CustRequestParty",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCustRequestParty {}

    /**
     * Delete a CustRequestParty record (SCIPIO: NOTE: 2018-09-10: This now actually deletes CustRequestParty; previously only expired it; use expireCustRequestParty to expire)
     */
    @Service(
        name = "deleteCustRequestParty",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "deleteCustRequestParty",
        description = "Delete a CustRequestParty record (SCIPIO: NOTE: 2018-09-10: This now actually deletes CustRequestParty; previously only expired it; use expireCustRequestParty to expire)",
        defaultEntityName = "CustRequestParty",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCustRequestParty {}

    /**
     * Expire a CustRequestParty record
     */
    @Service(
        name = "expireCustRequestParty",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "expireCustRequestParty",
        description = "Expire a CustRequestParty record",
        defaultEntityName = "CustRequestParty",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface ExpireCustRequestParty {}

    /**
     * Create a note for a CustRequest
     */
    @Service(
        name = "createCustRequestNote",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "createCustRequestNote",
        description = "Create a note for a CustRequest",
        auth = "true",
        attributes = {
            @Attribute(name = "custRequestId", type = "String", mode = "IN"),
            @Attribute(name = "noteInfo", type = "String", mode = "IN", allowHtml = "any"),
            @Attribute(name = "noteId", type = "String", mode = "OUT"),
            @Attribute(name = "fromPartyId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "custRequestName", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateCustRequestNote {}

    /**
     * Update CustRequest Note
     */
    @Service(
        name = "updateCustRequestNote",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "updateCustRequestNote",
        description = "Update CustRequest Note",
        auth = "true",
        attributes = {
            @Attribute(name = "custRequestId", type = "String", mode = "IN"),
            @Attribute(name = "noteId", type = "String", mode = "IN", allowHtml = "any"),
            @Attribute(name = "noteInfo", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateCustRequestNote {}

    /**
     * Create a note for a CustRequestItem
     */
    @Service(
        name = "createCustRequestItemNote",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "createCustRequestItemNote",
        description = "Create a note for a CustRequestItem",
        auth = "true",
        attributes = {
            @Attribute(name = "custRequestId", type = "String", mode = "IN"),
            @Attribute(name = "custRequestItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "note", type = "String", mode = "IN", allowHtml = "any"),
            @Attribute(name = "noteId", type = "String", mode = "OUT"),
            @Attribute(name = "partyId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "fromPartyId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "custRequestName", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateCustRequestItemNote {}

    /**
     * Creates a new request from a shopping cart
     */
    @Service(
        name = "createCustRequestFromCart",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "createCustRequestFromCart",
        description = "Creates a new request from a shopping cart",
        auth = "true",
        attributes = {
            @Attribute(name = "cart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN"),
            @Attribute(name = "custRequestName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "custRequestId", type = "String", mode = "OUT")
        }
    )
    public interface CreateCustRequestFromCart {}

    /**
     * Creates a new quote from a shopping list
     */
    @Service(
        name = "createCustRequestFromShoppingList",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "createCustRequestFromShoppingList",
        description = "Creates a new quote from a shopping list",
        auth = "true",
        attributes = {
            @Attribute(name = "shoppingListId", type = "String", mode = "IN"),
            @Attribute(name = "custRequestId", type = "String", mode = "OUT")
        }
    )
    public interface CreateCustRequestFromShoppingList {}

    /**
     * Get CustRequests Associated By Role
     */
    @Service(
        name = "getCustRequestsByRole",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "getCustRequestsByRole",
        description = "Get CustRequests Associated By Role",
        auth = "true",
        attributes = {
            @Attribute(name = "roleTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "custRequestAndRoles", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetCustRequestsByRole {}

    /**
     * Set the Customer Request  Status
     */
    @Service(
        name = "setCustRequestStatus",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "setCustRequestStatus",
        description = "Set the Customer Request  Status",
        auth = "true",
        attributes = {
            @Attribute(name = "custRequestId", type = "String", mode = "INOUT"),
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "reason", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "fromPartyId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "custRequestName", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SetCustRequestStatus {}

    /**
     * Create a Customer request from a commEvent(email)
     */
    @Service(
        name = "createCustRequestFromCommEvent",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "createCustRequestFromCommEvent",
        description = "Create a Customer request from a commEvent(email)",
        defaultEntityName = "CommunicationEvent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "custRequestId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "custRequestId", type = "String", mode = "OUT"),
            @Attribute(name = "custRequestTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "custRequestName", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "story", type = "String", mode = "IN", optional = "true", allowHtml = "any")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "content", allowHtml = "any")
        }
    )
    public interface CreateCustRequestFromCommEvent {}

    /**
     * Create a Cust Request Status Record
     */
    @Service(
        name = "createCustRequestStatus",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "createCustRequestStatus",
        description = "Create a Cust Request Status Record",
        defaultEntityName = "CustRequestStatus",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "custRequestStatusId", type = "String", mode = "OUT")
        }
    )
    public interface CreateCustRequestStatus {}

    /**
     * Imports and processes a media file and stores it in the database. Autodetects content-type, defaulting to Binary.
     */
    @Service(
        name = "CustRequestUploadContentFile",
        engine = "java",
        location = "com.ilscipio.scipio.order.quote.content.CustRequestServices",
        invoke = "createCustRequestContent",
        description = "Imports and processes a media file and stores it in the database. Autodetects content-type, defaulting to Binary.",
        auth = "true",
        transactionTimeout = "7200",
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
            @Attribute(name = "custRequestId", type = "String", mode = "INOUT"),
            @Attribute(name = "contentId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "dataResourceId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "dataResourceTypeId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "custRequestPermissionCheck", mainAction = "CREATE")
    )
    public interface CustRequestUploadContentFile {}

    /**
     * Create a Customer Request Content
     */
    @Service(
        name = "createCustRequestContent",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "createCustRequestContent",
        description = "Create a Customer Request Content",
        defaultEntityName = "CustRequestContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateCustRequestContent {}

    /**
     * Update a Customer Request Content
     */
    @Service(
        name = "deleteCustRequestContent",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/request/CustRequestServices.xml",
        invoke = "deleteCustRequestContent",
        description = "Update a Customer Request Content",
        defaultEntityName = "CustRequestContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface DeleteCustRequestContent {}

    /**
     * Create a CustRequestCategory record
     */
    @Service(
        name = "createCustRequestCategory",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a CustRequestCategory record",
        defaultEntityName = "CustRequestCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCustRequestCategory {}

    /**
     * Update a CustRequestCategory record
     */
    @Service(
        name = "updateCustRequestCategory",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a CustRequestCategory record",
        defaultEntityName = "CustRequestCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCustRequestCategory {}

    /**
     * Delete a CustRequestCategory record
     */
    @Service(
        name = "deleteCustRequestCategory",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a CustRequestCategory record",
        defaultEntityName = "CustRequestCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCustRequestCategory {}

    /**
     * Create a CustRequestResolution record
     */
    @Service(
        name = "createCustRequestResolution",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a CustRequestResolution record",
        defaultEntityName = "CustRequestResolution",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCustRequestResolution {}

    /**
     * Update a CustRequestResolution record
     */
    @Service(
        name = "updateCustRequestResolution",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a CustRequestResolution record",
        defaultEntityName = "CustRequestResolution",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCustRequestResolution {}

    /**
     * Delete a CustRequestResolution record
     */
    @Service(
        name = "deleteCustRequestResolution",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a CustRequestResolution record",
        defaultEntityName = "CustRequestResolution",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCustRequestResolution {}

    /**
     * Create a CustRequestType record
     */
    @Service(
        name = "createCustRequestType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a CustRequestType record",
        defaultEntityName = "CustRequestType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCustRequestType {}

    /**
     * Update a CustRequestType record
     */
    @Service(
        name = "updateCustRequestType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a CustRequestType record",
        defaultEntityName = "CustRequestType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCustRequestType {}

    /**
     * Delete a CustRequestType record
     */
    @Service(
        name = "deleteCustRequestType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a CustRequestType record",
        defaultEntityName = "CustRequestType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCustRequestType {}

    /**
     * Create a CustRequestTypeAttr record
     */
    @Service(
        name = "createCustRequestTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a CustRequestTypeAttr record",
        defaultEntityName = "CustRequestTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCustRequestTypeAttr {}

    /**
     * Update a CustRequestTypeAttr record
     */
    @Service(
        name = "updateCustRequestTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a CustRequestTypeAttr record",
        defaultEntityName = "CustRequestTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCustRequestTypeAttr {}

    /**
     * Delete a CustRequestTypeAttr record
     */
    @Service(
        name = "deleteCustRequestTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a CustRequestTypeAttr record",
        defaultEntityName = "CustRequestTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCustRequestTypeAttr {}

    /**
     * Create a RespondingParty record
     */
    @Service(
        name = "createRespondingParty",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a RespondingParty record",
        defaultEntityName = "RespondingParty",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateRespondingParty {}

    /**
     * Update a RespondingParty record
     */
    @Service(
        name = "updateRespondingParty",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a RespondingParty record",
        defaultEntityName = "RespondingParty",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateRespondingParty {}

    /**
     * Delete a RespondingParty record
     */
    @Service(
        name = "deleteRespondingParty",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a RespondingParty record",
        defaultEntityName = "RespondingParty",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteRespondingParty {}

}
