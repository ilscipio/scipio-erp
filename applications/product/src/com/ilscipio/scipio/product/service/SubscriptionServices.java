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
package com.ilscipio.scipio.product.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class SubscriptionServices {

    /**
     * Create a Subscription Record
     */
    @Service(
        name = "createSubscription",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/subscription/SubscriptionServices.xml",
        invoke = "createSubscription",
        description = "Create a Subscription Record",
        defaultEntityName = "Subscription",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "subscriptionPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateSubscription {}

    /**
     * Update a Subscription Record
     */
    @Service(
        name = "updateSubscription",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Subscription Record",
        defaultEntityName = "Subscription",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "subscriptionPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateSubscription {}

    /**
     * Check if a particular party has at this moment a subscription
     */
    @Service(
        name = "isSubscribed",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/subscription/SubscriptionServices.xml",
        invoke = "isSubscribed",
        description = "Check if a particular party has at this moment a subscription",
        defaultEntityName = "Subscription",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "filterByDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "isSubscribed", type = "Boolean", mode = "OUT"),
            @Attribute(name = "subscriptionId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "subscriptionPermissionCheck", mainAction = "VIEW"),
        overrideAttributes = {
            @OverrideAttribute(name = "partyId", mode = "IN", optional = "false")
        }
    )
    public interface IsSubscribed {}

    /**
     * Retrieve a single Subscription Entity Record
     */
    @Service(
        name = "getSubscriptionEnt",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/subscription/SubscriptionServices.xml",
        invoke = "getSubscription",
        description = "Retrieve a single Subscription Entity Record",
        defaultEntityName = "Subscription",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk")
        },
        attributes = {
            @Attribute(name = "subscription", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "subscriptionPermissionCheck", mainAction = "VIEW")
    )
    public interface GetSubscriptionEnt {}

    /**
     * Create a SubscriptionResource Record
     */
    @Service(
        name = "createSubscriptionResource",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SubscriptionResource Record",
        defaultEntityName = "SubscriptionResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "subscriptionPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateSubscriptionResource {}

    /**
     * Update a SubscriptionResource Record
     */
    @Service(
        name = "updateSubscriptionResource",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SubscriptionResource Record",
        defaultEntityName = "SubscriptionResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "subscriptionPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateSubscriptionResource {}

    /**
     * Create a ProductSubscriptionResource Record
     */
    @Service(
        name = "createProductSubscriptionResource",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductSubscriptionResource Record",
        defaultEntityName = "ProductSubscriptionResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "subscriptionPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateProductSubscriptionResource {}

    /**
     * Update a ProductSubscriptionResource Record
     */
    @Service(
        name = "updateProductSubscriptionResource",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductSubscriptionResource Record",
        defaultEntityName = "ProductSubscriptionResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "subscriptionPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateProductSubscriptionResource {}

    /**
     * Delete a ProductSubscriptionResource Record
     */
    @Service(
        name = "deleteProductSubscriptionResource",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductSubscriptionResource Record",
        defaultEntityName = "ProductSubscriptionResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "subscriptionPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteProductSubscriptionResource {}

    /**
     * Creates or updates Subscription record
     */
    @Service(
        name = "processExtendSubscription",
        engine = "java",
        location = "org.ofbiz.product.subscription.SubscriptionServices",
        invoke = "processExtendSubscription",
        description = "Creates or updates Subscription record",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "subscriptionResourceId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "useRoleTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "useTimeUomId", type = "String", mode = "IN"),
            @Attribute(name = "useTime", type = "Integer", mode = "IN"),
            @Attribute(name = "automaticExtend", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "canclAutmExtTime", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "canclAutmExtTimeUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "alwaysCreateNewRecord", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "gracePeriodOnExpiry", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "gracePeriodOnExpiryUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "subscriptionId", type = "String", mode = "OUT")
        }
    )
    public interface ProcessExtendSubscription {}

    /**
     * Creates or updates Subscription record
     */
    @Service(
        name = "processExtendSubscriptionByProduct",
        engine = "java",
        location = "org.ofbiz.product.subscription.SubscriptionServices",
        invoke = "processExtendSubscriptionByProduct",
        description = "Creates or updates Subscription record",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderCreatedDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "Integer", mode = "IN"),
            @Attribute(name = "subscriptionId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface ProcessExtendSubscriptionByProduct {}

    /**
     * Creates or updates Subscription record
     */
    @Service(
        name = "processExtendSubscriptionByOrder",
        engine = "java",
        location = "org.ofbiz.product.subscription.SubscriptionServices",
        invoke = "processExtendSubscriptionByOrder",
        description = "Creates or updates Subscription record",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "subscriptionId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface ProcessExtendSubscriptionByOrder {}

    /**
     * Create a SubscriptionAttribute
     */
    @Service(
        name = "createSubscriptionAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SubscriptionAttribute",
        defaultEntityName = "SubscriptionAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateSubscriptionAttribute {}

    /**
     * Create (when not exist) or update (when exist) a Subscription attribute
     */
    @Service(
        name = "updateSubscriptionAttribute",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/subscription/SubscriptionServices.xml",
        invoke = "updateSubscriptionAttribute",
        description = "Create (when not exist) or update (when exist) a Subscription attribute",
        defaultEntityName = "SubscriptionAttribute",
        auth = "true",
        attributes = {
            @Attribute(name = "subscriptionId", type = "String", mode = "INOUT"),
            @Attribute(name = "attrName", type = "String", mode = "IN"),
            @Attribute(name = "attrValue", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "subscriptionPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateSubscriptionAttribute {}

    /**
     * Delete a SubscriptionAttribute
     */
    @Service(
        name = "deleteSubscriptionAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SubscriptionAttribute",
        defaultEntityName = "SubscriptionAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSubscriptionAttribute {}

    /**
     * Create a Subscription Communication Event
     */
    @Service(
        name = "createSubscriptionCommEvent",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Subscription Communication Event",
        defaultEntityName = "SubscriptionCommEvent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "subscriptionPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateSubscriptionCommEvent {}

    /**
     * Remove a Subscription Communication Event
     */
    @Service(
        name = "removeSubscriptionCommEvent",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a Subscription Communication Event",
        defaultEntityName = "SubscriptionCommEvent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "subscriptionPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveSubscriptionCommEvent {}

    /**
     * Subscription Permission Checking Logic
     */
    @Service(
        name = "subscriptionPermissionCheck",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/subscription/SubscriptionServices.xml",
        invoke = "subscriptionPermissionCheck",
        description = "Subscription Permission Checking Logic",
        auth = "true",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface SubscriptionPermissionCheck {}

    /**
     * A service designed to be automatically run by job scheduler to trigger another service to run for each subscription which has expired.             This is done by looking for all subscriptions for which thruDate and gracePeriodOnExpiry are expired and where the automaticExtend flag is set to "N".             The service to run is found in SubscriptionResource.ServiceNameOnExpiry (by default OOTB: runSubscriptionExpired, see below)
     */
    @Service(
        name = "runServiceOnSubscriptionExpiry",
        engine = "java",
        location = "org.ofbiz.product.subscription.SubscriptionServices",
        invoke = "runServiceOnSubscriptionExpiry",
        description = "A service designed to be automatically run by job scheduler to trigger another service to run for each subscription which has expired.\n            This is done by looking for all subscriptions for which thruDate and gracePeriodOnExpiry are expired and where the automaticExtend flag is set to \"N\".\n            The service to run is found in SubscriptionResource.ServiceNameOnExpiry (by default OOTB: runSubscriptionExpired, see below)",
        auth = "true"
    )
    public interface RunServiceOnSubscriptionExpiry {}

    /**
     * A dummy service to test subscription expiration, expected to change depending upon the specific service logic that providers will write.              See https://issues.apache.org/jira/browse/OFBIZ-5333 for more information
     */
    @Service(
        name = "runSubscriptionExpired",
        engine = "java",
        location = "org.ofbiz.product.subscription.SubscriptionServices",
        invoke = "runSubscriptionExpired",
        description = "A dummy service to test subscription expiration, expected to change depending upon the specific service logic that providers will write. \n            See https://issues.apache.org/jira/browse/OFBIZ-5333 for more information",
        auth = "true",
        attributes = {
            @Attribute(name = "subscriptionId", type = "String", mode = "IN")
        }
    )
    public interface RunSubscriptionExpired {}

    /**
     * Create a SubscriptionActivity
     */
    @Service(
        name = "createSubscriptionActivity",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SubscriptionActivity",
        defaultEntityName = "SubscriptionActivity",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateSubscriptionActivity {}

    /**
     * Update a SubscriptionActivity
     */
    @Service(
        name = "updateSubscriptionActivity",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SubscriptionActivity",
        defaultEntityName = "SubscriptionActivity",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateSubscriptionActivity {}

    /**
     * Delete a SubscriptionActivity
     */
    @Service(
        name = "deleteSubscriptionActivity",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SubscriptionActivity",
        defaultEntityName = "SubscriptionActivity",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSubscriptionActivity {}

    /**
     * Create a SubscriptionType
     */
    @Service(
        name = "createSubscriptionType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SubscriptionType",
        defaultEntityName = "SubscriptionType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateSubscriptionType {}

    /**
     * Update a SubscriptionType
     */
    @Service(
        name = "updateSubscriptionType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SubscriptionType",
        defaultEntityName = "SubscriptionType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateSubscriptionType {}

    /**
     * Delete a SubscriptionType
     */
    @Service(
        name = "deleteSubscriptionType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SubscriptionType",
        defaultEntityName = "SubscriptionType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSubscriptionType {}

    /**
     * Create a SubscriptionTypeAttr
     */
    @Service(
        name = "createSubscriptionTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SubscriptionTypeAttr",
        defaultEntityName = "SubscriptionTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateSubscriptionTypeAttr {}

    /**
     * Update a SubscriptionTypeAttr
     */
    @Service(
        name = "updateSubscriptionTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SubscriptionTypeAttr",
        defaultEntityName = "SubscriptionTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateSubscriptionTypeAttr {}

    /**
     * Delete a SubscriptionTypeAttr
     */
    @Service(
        name = "deleteSubscriptionTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SubscriptionTypeAttr",
        defaultEntityName = "SubscriptionTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSubscriptionTypeAttr {}

}
