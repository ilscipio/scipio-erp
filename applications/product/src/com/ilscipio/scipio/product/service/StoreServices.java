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
public class StoreServices {

    /**
     * Create a Product Store
     */
    @Service(
        name = "createProductStore",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/store/ProductStoreServices.xml",
        invoke = "createProductStore",
        description = "Create a Product Store",
        defaultEntityName = "ProductStore",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "OUT", include = "pk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "storeName", optional = "false")
        }
    )
    public interface CreateProductStore {}

    /**
     * Update a Product Store
     */
    @Service(
        name = "updateProductStore",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/store/ProductStoreServices.xml",
        invoke = "updateProductStore",
        description = "Update a Product Store",
        defaultEntityName = "ProductStore",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductStore {}

    /**
     * Reserve Inventory in a Product Store
     */
    @Service(
        name = "reserveStoreInventory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/store/ProductStoreServices.xml",
        invoke = "reserveStoreInventory",
        description = "Reserve Inventory in a Product Store",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantityNotReserved", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface ReserveStoreInventory {}

    /**
     * Checks if Store Inventory is Required
     */
    @Service(
        name = "isStoreInventoryRequired",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/store/ProductStoreServices.xml",
        invoke = "isStoreInventoryRequired",
        description = "Checks if Store Inventory is Required",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "productStore", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "product", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "requireInventory", type = "String", mode = "OUT")
        }
    )
    public interface IsStoreInventoryRequired {}

    /**
     * Checks if Store Inventory is Required
     */
    @Service(
        name = "isStoreInventoryAvailable",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/store/ProductStoreServices.xml",
        invoke = "isStoreInventoryAvailable",
        description = "Checks if Store Inventory is Required",
        log = "quiet",
        logEca = "quiet",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "productStore", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "product", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "available", type = "String", mode = "OUT"),
            @Attribute(name = "useEntityCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, entity cache when looking up records, where possible (SCIPIO)"),
            @Attribute(name = "useInventoryCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, use ProductFacility.lastInventoryCount or other inventory cache. Current default: false; legacy default: false (SCIPIO)")
        }
    )
    public interface IsStoreInventoryAvailable {}

    /**
     * Checks if Store Inventory is Required
     */
    @Service(
        name = "isStoreInventoryAvailableOrNotRequired",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/store/ProductStoreServices.xml",
        invoke = "isStoreInventoryAvailableOrNotRequired",
        description = "Checks if Store Inventory is Required",
        log = "debug",
        logEca = "quiet",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "productStore", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "product", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "availableOrNotRequired", type = "String", mode = "OUT"),
            @Attribute(name = "useEntityCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, entity cache when looking up records, where possible (SCIPIO)"),
            @Attribute(name = "useInventoryCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, use ProductFacility.lastInventoryCount or other inventory cache. Current default: false; legacy default: false (SCIPIO)")
        }
    )
    public interface IsStoreInventoryAvailableOrNotRequired {}

    /**
     * Create ProductStoreRole
     */
    @Service(
        name = "createProductStoreRole",
        engine = "entity-auto",
        invoke = "create",
        description = "Create ProductStoreRole",
        defaultEntityName = "ProductStoreRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateProductStoreRole {}

    /**
     * Update a Product Store Role
     */
    @Service(
        name = "updateProductStoreRole",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Product Store Role",
        defaultEntityName = "ProductStoreRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductStoreRole {}

    /**
     * Remove ProductStoreRole
     */
    @Service(
        name = "removeProductStoreRole",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove ProductStoreRole",
        defaultEntityName = "ProductStoreRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "DELETE")
    )
    public interface RemoveProductStoreRole {}

    /**
     * Ensure ProductStoreRole
     */
    @Service(
        name = "ensureProductStoreRole",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/store/ProductStoreServices.xml",
        invoke = "ensureProductStoreRole",
        description = "Ensure ProductStoreRole",
        defaultEntityName = "ProductStoreRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "updateOptFields", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, optional fields such as sequenceNum will be updated on the existing record if different;\n                otherwise they are only set if creating a new record")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", mode = "INOUT", optional = "true")
        }
    )
    public interface EnsureProductStoreRole {}

    /**
     * Create ProductStoreCatalog
     */
    @Service(
        name = "createProductStoreCatalog",
        engine = "entity-auto",
        invoke = "create",
        description = "Create ProductStoreCatalog",
        defaultEntityName = "ProductStoreCatalog",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateProductStoreCatalog {}

    /**
     * Update ProductStoreCatalog
     */
    @Service(
        name = "updateProductStoreCatalog",
        engine = "entity-auto",
        invoke = "update",
        description = "Update ProductStoreCatalog",
        defaultEntityName = "ProductStoreCatalog",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductStoreCatalog {}

    /**
     * Delete ProductStoreCatalog
     */
    @Service(
        name = "deleteProductStoreCatalog",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete ProductStoreCatalog",
        defaultEntityName = "ProductStoreCatalog",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteProductStoreCatalog {}

    /**
     * Create ProductStorePaymentSetting
     */
    @Service(
        name = "createProductStorePaymentSetting",
        engine = "entity-auto",
        invoke = "create",
        description = "Create ProductStorePaymentSetting",
        defaultEntityName = "ProductStorePaymentSetting",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "paymentCustomMethodId", optional = "true"),
            @OverrideAttribute(name = "paymentGatewayConfigId", optional = "true"),
            @OverrideAttribute(name = "paymentPropertiesPath", optional = "true"),
            @OverrideAttribute(name = "paymentService", optional = "true")
        }
    )
    public interface CreateProductStorePaymentSetting {}

    /**
     * Update ProductStorePaymentSetting
     */
    @Service(
        name = "updateProductStorePaymentSetting",
        engine = "entity-auto",
        invoke = "update",
        description = "Update ProductStorePaymentSetting",
        defaultEntityName = "ProductStorePaymentSetting",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "paymentCustomMethodId", optional = "true"),
            @OverrideAttribute(name = "paymentGatewayConfigId", optional = "true"),
            @OverrideAttribute(name = "paymentPropertiesPath", optional = "true"),
            @OverrideAttribute(name = "paymentService", optional = "true")
        }
    )
    public interface UpdateProductStorePaymentSetting {}

    /**
     * Delete ProductStorePaymentSetting
     */
    @Service(
        name = "deleteProductStorePaymentSetting",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete ProductStorePaymentSetting",
        defaultEntityName = "ProductStorePaymentSetting",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteProductStorePaymentSetting {}

    /**
     * Create a Product Store Email Setting
     */
    @Service(
        name = "createProductStoreEmailSetting",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Product Store Email Setting",
        defaultEntityName = "ProductStoreEmailSetting",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ProductStoreEmailSetting", mode = "IN")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "xslfoAttachScreenLocation", optional = "true"),
            @OverrideAttribute(name = "ccAddress", optional = "true"),
            @OverrideAttribute(name = "bccAddress", optional = "true"),
            @OverrideAttribute(name = "contentType", optional = "true")
        }
    )
    public interface CreateProductStoreEmailSetting {}

    /**
     * Update a Product Store Email Setting
     */
    @Service(
        name = "updateProductStoreEmailSetting",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Product Store Email Setting",
        defaultEntityName = "ProductStoreEmailSetting",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ProductStoreEmailSetting", mode = "IN")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "xslfoAttachScreenLocation", optional = "true"),
            @OverrideAttribute(name = "ccAddress", optional = "true"),
            @OverrideAttribute(name = "bccAddress", optional = "true"),
            @OverrideAttribute(name = "contentType", optional = "true")
        }
    )
    public interface UpdateProductStoreEmailSetting {}

    /**
     * Remove a Product Store Email Setting
     */
    @Service(
        name = "removeProductStoreEmailSetting",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a Product Store Email Setting",
        defaultEntityName = "ProductStoreEmailSetting",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ProductStoreEmailSetting", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "DELETE")
    )
    public interface RemoveProductStoreEmailSetting {}

    /**
     * Create a Product Store Shipment Method
     */
    @Service(
        name = "createProductStoreShipMeth",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Product Store Shipment Method",
        defaultEntityName = "ProductStoreShipmentMeth",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", optional = "true"),
            @EntityAttributes(mode = "OUT", include = "pk")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "productStoreId", optional = "false"),
            @OverrideAttribute(name = "shipmentMethodTypeId", optional = "false"),
            @OverrideAttribute(name = "partyId", optional = "false"),
            @OverrideAttribute(name = "roleTypeId", optional = "false")
        }
    )
    public interface CreateProductStoreShipMeth {}

    /**
     * Update a Product Store Shipment Method
     */
    @Service(
        name = "updateProductStoreShipMeth",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Product Store Shipment Method",
        defaultEntityName = "ProductStoreShipmentMeth",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductStoreShipMeth {}

    /**
     * Remove a Product Store Shipment Method
     */
    @Service(
        name = "removeProductStoreShipMeth",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a Product Store Shipment Method",
        defaultEntityName = "ProductStoreShipmentMeth",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "DELETE")
    )
    public interface RemoveProductStoreShipMeth {}

    /**
     * Create a Product Store Keyword Override
     */
    @Service(
        name = "createProductStoreKeywordOvrd",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Product Store Keyword Override",
        defaultEntityName = "ProductStoreKeywordOvrd",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "target", optional = "false"),
            @OverrideAttribute(name = "fromDate", optional = "true"),
            @OverrideAttribute(name = "targetTypeEnumId", optional = "false")
        }
    )
    public interface CreateProductStoreKeywordOvrd {}

    /**
     * Update a Product Store Keyword Override
     */
    @Service(
        name = "updateProductStoreKeywordOvrd",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Product Store Keyword Override",
        defaultEntityName = "ProductStoreKeywordOvrd",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductStoreKeywordOvrd {}

    /**
     * Delete a Product Store Keyword Override
     */
    @Service(
        name = "deleteProductStoreKeywordOvrd",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Product Store Keyword Override",
        defaultEntityName = "ProductStoreKeywordOvrd",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteProductStoreKeywordOvrd {}

    /**
     * Create a Product Store Survey Appl
     */
    @Service(
        name = "createProductStoreSurveyAppl",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Product Store Survey Appl",
        defaultEntityName = "ProductStoreSurveyAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "OUT", include = "pk")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "CREATE")
    )
    public interface CreateProductStoreSurveyAppl {}

    /**
     * Delete a Product Store Survey Appl
     */
    @Service(
        name = "deleteProductStoreSurveyAppl",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Product Store Survey Appl",
        defaultEntityName = "ProductStoreSurveyAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteProductStoreSurveyAppl {}

    /**
     * Create ProductStorePromoAppl
     */
    @Service(
        name = "createProductStorePromoAppl",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "createProductStorePromoAppl",
        description = "Create ProductStorePromoAppl",
        defaultEntityName = "ProductStorePromoAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateProductStorePromoAppl {}

    /**
     * Update ProductStorePromoAppl
     */
    @Service(
        name = "updateProductStorePromoAppl",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "updateProductStorePromoAppl",
        description = "Update ProductStorePromoAppl",
        defaultEntityName = "ProductStorePromoAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductStorePromoAppl {}

    /**
     * Delete ProductStorePromoAppl
     */
    @Service(
        name = "deleteProductStorePromoAppl",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "deleteProductStorePromoAppl",
        description = "Delete ProductStorePromoAppl",
        defaultEntityName = "ProductStorePromoAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductStorePromoAppl {}

    /**
     * Create ProductStoreFinActSetting
     */
    @Service(
        name = "createProductStoreFinActSetting",
        engine = "entity-auto",
        invoke = "create",
        description = "Create ProductStoreFinActSetting",
        defaultEntityName = "ProductStoreFinActSetting",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "CREATE")
    )
    public interface CreateProductStoreFinActSetting {}

    /**
     * Update ProductStoreFinActSetting
     */
    @Service(
        name = "updateProductStoreFinActSetting",
        engine = "entity-auto",
        invoke = "update",
        description = "Update ProductStoreFinActSetting",
        defaultEntityName = "ProductStoreFinActSetting",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductStoreFinActSetting {}

    /**
     * Remove ProductStoreFinActSetting
     */
    @Service(
        name = "removeProductStoreFinActSetting",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove ProductStoreFinActSetting",
        defaultEntityName = "ProductStoreFinActSetting",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "DELETE")
    )
    public interface RemoveProductStoreFinActSetting {}

    @Service(
        name = "createProductStoreVendorPayment",
        engine = "entity-auto",
        invoke = "create",
        defaultEntityName = "ProductStoreVendorPayment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "CREATE")
    )
    public interface CreateProductStoreVendorPayment {}

    @Service(
        name = "deleteProductStoreVendorPayment",
        engine = "entity-auto",
        invoke = "delete",
        defaultEntityName = "ProductStoreVendorPayment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteProductStoreVendorPayment {}

    @Service(
        name = "createProductStoreVendorShipment",
        engine = "entity-auto",
        invoke = "create",
        defaultEntityName = "ProductStoreVendorShipment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "CREATE")
    )
    public interface CreateProductStoreVendorShipment {}

    @Service(
        name = "deleteProductStoreVendorShipment",
        engine = "entity-auto",
        invoke = "delete",
        defaultEntityName = "ProductStoreVendorShipment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteProductStoreVendorShipment {}

    /**
     * Create a ProductStoreFacility
     */
    @Service(
        name = "createProductStoreFacility",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductStoreFacility",
        defaultEntityName = "ProductStoreFacility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "CREATE")
    )
    public interface CreateProductStoreFacility {}

    /**
     * Update a ProductStoreFacility
     */
    @Service(
        name = "updateProductStoreFacility",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductStoreFacility",
        defaultEntityName = "ProductStoreFacility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductStoreFacility {}

    /**
     * Delete a ProductStoreFacility
     */
    @Service(
        name = "deleteProductStoreFacility",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductStoreFacility",
        defaultEntityName = "ProductStoreFacility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteProductStoreFacility {}

    /**
     * Create a ProductStoreGroup
     */
    @Service(
        name = "createProductStoreGroup",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductStoreGroup",
        defaultEntityName = "ProductStoreGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductStoreGroup {}

    /**
     * Update a ProductStoreGroup
     */
    @Service(
        name = "updateProductStoreGroup",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductStoreGroup",
        defaultEntityName = "ProductStoreGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductStoreGroup {}

    /**
     * Delete a ProductStoreGroup
     */
    @Service(
        name = "deleteProductStoreGroup",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductStoreGroup",
        defaultEntityName = "ProductStoreGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductStoreGroup {}

    /**
     * Create a ProductStoreGroupMember
     */
    @Service(
        name = "createProductStoreGroupMember",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductStoreGroupMember",
        defaultEntityName = "ProductStoreGroupMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductStoreGroupMember {}

    /**
     * Update a ProductStoreGroupMember
     */
    @Service(
        name = "updateProductStoreGroupMember",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductStoreGroupMember",
        defaultEntityName = "ProductStoreGroupMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductStoreGroupMember {}

    /**
     * Create a ProductStoreGroupRollup
     */
    @Service(
        name = "createProductStoreGroupRollup",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductStoreGroupRollup",
        defaultEntityName = "ProductStoreGroupRollup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductStoreGroupRollup {}

    /**
     * Update a ProductStoreGroupRollup
     */
    @Service(
        name = "updateProductStoreGroupRollup",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductStoreGroupRollup",
        defaultEntityName = "ProductStoreGroupRollup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductStoreGroupRollup {}

    /**
     * Delete a ProductStoreGroupRollup
     */
    @Service(
        name = "deleteProductStoreGroupRollup",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductStoreGroupRollup",
        defaultEntityName = "ProductStoreGroupRollup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductStoreGroupRollup {}

    /**
     * Check if a productStoreGroupId with a primaryParentGroupId has related productStoreGroupRollup or for first ProductStoreGroupRollup on a ProductStoreGroup set relation on primaryParentGroupId
     */
    @Service(
        name = "checkProductStoreGroupRollup",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/store/ProductStoreServices.xml",
        invoke = "checkProductStoreGroupRollup",
        description = "Check if a productStoreGroupId with a primaryParentGroupId has related productStoreGroupRollup or for first ProductStoreGroupRollup on a ProductStoreGroup set relation on primaryParentGroupId",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreGroupId", type = "String", mode = "IN"),
            @Attribute(name = "primaryParentGroupId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "parentGroupId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CheckProductStoreGroupRollup {}

    @Service(
        name = "productStoreGenericPermission",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/store/ProductStoreServices.xml",
        invoke = "productStoreGenericPermission",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface ProductStoreGenericPermission {}

    /**
     * Create a ProductStoreGroupRole
     */
    @Service(
        name = "createProductStoreGroupRole",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductStoreGroupRole",
        defaultEntityName = "ProductStoreGroupRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductStoreGroupRole {}

    /**
     * Delete a ProductStoreGroupRole
     */
    @Service(
        name = "deleteProductStoreGroupRole",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductStoreGroupRole",
        defaultEntityName = "ProductStoreGroupRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductStoreGroupRole {}

    /**
     * Create a ProductStoreGroupType
     */
    @Service(
        name = "createProductStoreGroupType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductStoreGroupType",
        defaultEntityName = "ProductStoreGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductStoreGroupType {}

    /**
     * Update a ProductStoreGroupType
     */
    @Service(
        name = "updateProductStoreGroupType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductStoreGroupType",
        defaultEntityName = "ProductStoreGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductStoreGroupType {}

    /**
     * Delete a ProductStoreGroupType
     */
    @Service(
        name = "deleteProductStoreGroupType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductStoreGroupType",
        defaultEntityName = "ProductStoreGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductStoreGroupType {}

    /**
     * SCIPIO: Change the default WebSite for the given ProductStore
     */
    @Service(
        name = "setProductStoreDefaultWebSite",
        engine = "java",
        location = "org.ofbiz.product.store.ProductStoreServices",
        invoke = "setProductStoreDefaultWebSite",
        description = "SCIPIO: Change the default WebSite for the given ProductStore",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "UPDATE")
    )
    public interface SetProductStoreDefaultWebSite {}

    /**
     * Force the given ProductStores' localeStrings filters to their values of defaultLocaleString (to act as override)
     */
    @Service(
        name = "setProductStoreLocaleStringsToDefault",
        engine = "java",
        location = "org.ofbiz.product.store.ProductStoreServices$SetProductStoreLocaleStringsToDefault",
        invoke = "exec",
        description = "Force the given ProductStores' localeStrings filters to their values of defaultLocaleString (to act as override)",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreIdList", type = "List", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "UPDATE")
    )
    public interface SetProductStoreLocaleStringsToDefault {}

}
