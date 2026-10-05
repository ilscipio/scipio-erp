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
public class FeatureServices {

    /**
     * Create a ProductFeatureCategory record
     */
    @Service(
        name = "createProductFeatureCategory",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductFeatureCategory record",
        defaultEntityName = "ProductFeatureCategory",
        auth = "true",
        attributes = {
            @Attribute(name = "parentCategoryId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN"),
            @Attribute(name = "productFeatureCategoryId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE")
    )
    public interface CreateProductFeatureCategory {}

    /**
     * Update a ProductFeatureCategory record
     */
    @Service(
        name = "updateProductFeatureCategory",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductFeatureCategory record",
        defaultEntityName = "ProductFeatureCategory",
        auth = "true",
        attributes = {
            @Attribute(name = "productFeatureCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "parentCategoryId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductFeatureCategory {}

    /**
     * Create a ProductFeature record
     */
    @Service(
        name = "createProductFeature",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductFeature record",
        defaultEntityName = "ProductFeature",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "productFeatureId", type = "String", mode = "OUT")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productFeatureTypeId", mode = "IN", optional = "false"),
            @OverrideAttribute(name = "description", mode = "IN", optional = "false")
        }
    )
    public interface CreateProductFeature {}

    /**
     * Update a ProductFeature record
     */
    @Service(
        name = "updateProductFeature",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductFeature record",
        defaultEntityName = "ProductFeature",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productFeatureTypeId", mode = "IN", optional = "false"),
            @OverrideAttribute(name = "description", mode = "IN", optional = "false")
        }
    )
    public interface UpdateProductFeature {}

    /**
     * Apply a ProductFeature to a Product; a fromDate can be used             to specify when the feature will be applied, if no fromDate is specified,             it will be applied now.
     */
    @Service(
        name = "applyFeatureToProduct",
        engine = "entity-auto",
        invoke = "create",
        description = "Apply a ProductFeature to a Product; a fromDate can be used\n            to specify when the feature will be applied, if no fromDate is specified,\n            it will be applied now.",
        defaultEntityName = "ProductFeatureAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "productFeatureApplTypeId", mode = "IN", optional = "false"),
            @OverrideAttribute(name = "fromDate", mode = "IN", optional = "true")
        }
    )
    public interface ApplyFeatureToProduct {}

    /**
     * Update a ProductFeature to Product Application
     */
    @Service(
        name = "updateFeatureToProductApplication",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductFeature to Product Application",
        defaultEntityName = "ProductFeatureAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "productFeatureApplTypeId", mode = "IN", optional = "false")
        }
    )
    public interface UpdateFeatureToProductApplication {}

    /**
     * Remove a ProductFeature from a Product
     */
    @Service(
        name = "removeFeatureFromProduct",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a ProductFeature from a Product",
        defaultEntityName = "ProductFeatureAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface RemoveFeatureFromProduct {}

    /**
     * Apply a ProductFeature to a Product
     */
    @Service(
        name = "applyFeatureToProductFromTypeAndCode",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/feature/ProductFeatureServices.xml",
        invoke = "applyFeatureToProductFromTypeAndCode",
        description = "Apply a ProductFeature to a Product",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "productFeatureTypeId", type = "String", mode = "IN"),
            @Attribute(name = "idCode", type = "String", mode = "IN"),
            @Attribute(name = "productFeatureApplTypeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "java.sql.Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "java.sql.Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceNum", type = "Long", mode = "IN", optional = "true")
        }
    )
    public interface ApplyFeatureToProductFromTypeAndCode {}

    /**
     * Create a ProductFeatureCategory to ProductCategory Application
     */
    @Service(
        name = "createProductFeatureCategoryAppl",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductFeatureCategory to ProductCategory Application",
        defaultEntityName = "ProductFeatureCategoryAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateProductFeatureCategoryAppl {}

    /**
     * Update a ProductFeatureCategory to ProductCategory Application
     */
    @Service(
        name = "updateProductFeatureCategoryAppl",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductFeatureCategory to ProductCategory Application",
        defaultEntityName = "ProductFeatureCategoryAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductFeatureCategoryAppl {}

    /**
     * Remove a ProductFeatureCategory to ProductCategory Application
     */
    @Service(
        name = "removeProductFeatureCategoryAppl",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a ProductFeatureCategory to ProductCategory Application",
        defaultEntityName = "ProductFeatureCategoryAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface RemoveProductFeatureCategoryAppl {}

    /**
     * Create a ProductFeatureGroup to ProductCategory Application
     */
    @Service(
        name = "createProductFeatureCatGrpAppl",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductFeatureGroup to ProductCategory Application",
        defaultEntityName = "ProductFeatureCatGrpAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateProductFeatureCatGrpAppl {}

    /**
     * Update a ProductFeatureGroup to ProductCategory Application
     */
    @Service(
        name = "updateProductFeatureCatGrpAppl",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductFeatureGroup to ProductCategory Application",
        defaultEntityName = "ProductFeatureCatGrpAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductFeatureCatGrpAppl {}

    /**
     * Remove a ProductFeatureGroup to ProductCategory Application
     */
    @Service(
        name = "removeProductFeatureCatGrpAppl",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a ProductFeatureGroup to ProductCategory Application",
        defaultEntityName = "ProductFeatureCatGrpAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface RemoveProductFeatureCatGrpAppl {}

    /**
     * Create a ProductFeatureGroup
     */
    @Service(
        name = "createProductFeatureGroup",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductFeatureGroup",
        defaultEntityName = "ProductFeatureGroup",
        auth = "true",
        attributes = {
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productFeatureGroupId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE")
    )
    public interface CreateProductFeatureGroup {}

    /**
     * Create a ProductFeatureGroup
     */
    @Service(
        name = "updateProductFeatureGroup",
        engine = "entity-auto",
        invoke = "update",
        description = "Create a ProductFeatureGroup",
        defaultEntityName = "ProductFeatureGroup",
        auth = "true",
        attributes = {
            @Attribute(name = "productFeatureGroupId", type = "String", mode = "IN"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductFeatureGroup {}

    /**
     * Create a ProductFeatureGroup to ProductFeature Application
     */
    @Service(
        name = "createProductFeatureGroupAppl",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductFeatureGroup to ProductFeature Application",
        defaultEntityName = "ProductFeatureGroupAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", mode = "IN", optional = "true")
        }
    )
    public interface CreateProductFeatureGroupAppl {}

    /**
     * Update a ProductFeatureGroup to ProductFeature Application
     */
    @Service(
        name = "updateProductFeatureGroupAppl",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductFeatureGroup to ProductFeature Application",
        defaultEntityName = "ProductFeatureGroupAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductFeatureGroupAppl {}

    /**
     * Remove a ProductFeatureGroup to ProductFeature Application
     */
    @Service(
        name = "removeProductFeatureGroupAppl",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a ProductFeatureGroup to ProductFeature Application",
        defaultEntityName = "ProductFeatureGroupAppl",
        auth = "true",
        attributes = {
            @Attribute(name = "productFeatureGroupId", type = "String", mode = "IN"),
            @Attribute(name = "productFeatureId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "java.sql.Timestamp", mode = "IN")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface RemoveProductFeatureGroupAppl {}

    /**
     * Create a ProductFeatureIactn
     */
    @Service(
        name = "createProductFeatureIactn",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductFeatureIactn",
        defaultEntityName = "ProductFeatureIactn",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "productFeatureIactnTypeId", mode = "IN", optional = "false")
        }
    )
    public interface CreateProductFeatureIactn {}

    /**
     * Remove a ProductFeatureIactn
     */
    @Service(
        name = "removeProductFeatureIactn",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a ProductFeatureIactn",
        defaultEntityName = "ProductFeatureIactn",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface RemoveProductFeatureIactn {}

    /**
     * Create a ProductFeatureType
     */
    @Service(
        name = "createProductFeatureType",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/feature/ProductFeatureServices.xml",
        invoke = "createProductFeatureType",
        description = "Create a ProductFeatureType",
        defaultEntityName = "ProductFeatureType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productFeatureTypeId", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateProductFeatureType {}

    /**
     * Update a ProductFeatureType
     */
    @Service(
        name = "updateProductFeatureType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductFeatureType",
        defaultEntityName = "ProductFeatureType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductFeatureType {}

    /**
     * Remove a ProductFeatureType
     */
    @Service(
        name = "removeProductFeatureType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a ProductFeatureType",
        defaultEntityName = "ProductFeatureType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface RemoveProductFeatureType {}

    /**
     * Create a ProductFeatureApplAttr
     */
    @Service(
        name = "createProductFeatureApplAttr",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/feature/ProductFeatureServices.xml",
        invoke = "createProductFeatureApplAttr",
        description = "Create a ProductFeatureApplAttr",
        defaultEntityName = "ProductFeatureApplAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateProductFeatureApplAttr {}

    /**
     * Update a ProductFeatureApplAttr
     */
    @Service(
        name = "updateProductFeatureApplAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductFeatureApplAttr",
        defaultEntityName = "ProductFeatureApplAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductFeatureApplAttr {}

    /**
     * Remove a ProductFeatureApplAttr
     */
    @Service(
        name = "removeProductFeatureApplAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a ProductFeatureApplAttr",
        defaultEntityName = "ProductFeatureApplAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface RemoveProductFeatureApplAttr {}

    /**
     * Create a Feature Price
     */
    @Service(
        name = "createFeaturePrice",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Feature Price",
        defaultEntityName = "ProductFeaturePrice",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "productFeatureId", mode = "IN", optional = "true"),
            @OverrideAttribute(name = "fromDate", mode = "INOUT", optional = "true"),
            @OverrideAttribute(name = "price", optional = "false")
        }
    )
    public interface CreateFeaturePrice {}

    /**
     * Update a Feature Price
     */
    @Service(
        name = "updateFeaturePrice",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Feature Price",
        defaultEntityName = "ProductFeaturePrice",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "price", optional = "false")
        }
    )
    public interface UpdateFeaturePrice {}

    /**
     * Delete a Feature Price
     */
    @Service(
        name = "deleteFeaturePrice",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Feature Price",
        defaultEntityName = "ProductFeaturePrice",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteFeaturePrice {}

    /**
     * Create a ProductFeatureApplType
     */
    @Service(
        name = "createProductFeatureApplType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductFeatureApplType",
        defaultEntityName = "ProductFeatureApplType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateProductFeatureApplType {}

    /**
     * Update a ProductFeatureApplType
     */
    @Service(
        name = "updateProductFeatureApplType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductFeatureApplType",
        defaultEntityName = "ProductFeatureApplType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductFeatureApplType {}

    /**
     * Delete a ProductFeatureApplType
     */
    @Service(
        name = "deleteProductFeatureApplType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductFeatureApplType",
        defaultEntityName = "ProductFeatureApplType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductFeatureApplType {}

    /**
     * Create a ProductFeatureIactnType
     */
    @Service(
        name = "createProductFeatureIactnType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductFeatureIactnType",
        defaultEntityName = "ProductFeatureIactnType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateProductFeatureIactnType {}

    /**
     * Update a ProductFeatureIactnType
     */
    @Service(
        name = "updateProductFeatureIactnType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductFeatureIactnType",
        defaultEntityName = "ProductFeatureIactnType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductFeatureIactnType {}

    /**
     * Delete a ProductFeatureIactnType
     */
    @Service(
        name = "deleteProductFeatureIactnType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductFeatureIactnType",
        defaultEntityName = "ProductFeatureIactnType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductFeatureIactnType {}

}
