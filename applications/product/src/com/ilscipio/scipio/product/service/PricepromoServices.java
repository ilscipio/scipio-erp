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
public class PricepromoServices {

    /**
     * Calculate a Product's Price from ProductPriceRules
     */
    @Service(
        name = "calculateProductPrice",
        engine = "java",
        location = "org.ofbiz.product.price.PriceServices",
        invoke = "calculateProductPrice",
        description = "Calculate a Product's Price from ProductPriceRules",
        useTransaction = "false",
        log = "quiet",
        attributes = {
            @Attribute(name = "product", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "prodCatalogId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreGroupId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "agreementId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "currencyUomIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productPricePurposeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "termUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "autoUserLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "checkIncludeVat", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "findAllQuantityPrices", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "surveyResponseId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "customAttributes", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "basePrice", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "price", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "listPrice", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "defaultPrice", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "competitivePrice", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "averageCost", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "promoPrice", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "specialPromoPrice", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "isSale", type = "Boolean", mode = "OUT"),
            @Attribute(name = "validPriceFound", type = "Boolean", mode = "OUT"),
            @Attribute(name = "currencyUsed", type = "String", mode = "OUT"),
            @Attribute(name = "orderItemPriceInfos", type = "java.util.List", mode = "OUT"),
            @Attribute(name = "allQuantityPrices", type = "java.util.List", mode = "OUT", optional = "true"),
            @Attribute(name = "optimizeForLargeRuleSet", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "getMinimumVariantPrice", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "SCIPIO: useCache flag (default: true) - this should be set to false if called during updated services! (added 2017-12-19)")
        }
    )
    public interface CalculateProductPrice {}

    /**
     * Create an ProductPriceRule
     */
    @Service(
        name = "createProductPriceRule",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "createProductPriceRule",
        description = "Create an ProductPriceRule",
        defaultEntityName = "ProductPriceRule",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "ruleName", optional = "false")
        }
    )
    public interface CreateProductPriceRule {}

    /**
     * Update an ProductPriceRule
     */
    @Service(
        name = "updateProductPriceRule",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "updateProductPriceRule",
        description = "Update an ProductPriceRule",
        defaultEntityName = "ProductPriceRule",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductPriceRule {}

    /**
     * Delete an ProductPriceRule
     */
    @Service(
        name = "deleteProductPriceRule",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "deleteProductPriceRule",
        description = "Delete an ProductPriceRule",
        defaultEntityName = "ProductPriceRule",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPriceRule {}

    /**
     * Create an ProductPriceCond
     */
    @Service(
        name = "createProductPriceCond",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "createProductPriceCond",
        description = "Create an ProductPriceCond",
        defaultEntityName = "ProductPriceCond",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "condValueInput", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productPriceCondSeqId", mode = "OUT")
        }
    )
    public interface CreateProductPriceCond {}

    /**
     * Update an ProductPriceCond
     */
    @Service(
        name = "updateProductPriceCond",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "updateProductPriceCond",
        description = "Update an ProductPriceCond",
        defaultEntityName = "ProductPriceCond",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "condValueInput", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateProductPriceCond {}

    /**
     * Delete an ProductPriceCond
     */
    @Service(
        name = "deleteProductPriceCond",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "deleteProductPriceCond",
        description = "Delete an ProductPriceCond",
        defaultEntityName = "ProductPriceCond",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPriceCond {}

    /**
     * Create an ProductPriceAction
     */
    @Service(
        name = "createProductPriceAction",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "createProductPriceAction",
        description = "Create an ProductPriceAction",
        defaultEntityName = "ProductPriceAction",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productPriceActionSeqId", mode = "OUT")
        }
    )
    public interface CreateProductPriceAction {}

    /**
     * Update an ProductPriceAction
     */
    @Service(
        name = "updateProductPriceAction",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "updateProductPriceAction",
        description = "Update an ProductPriceAction",
        defaultEntityName = "ProductPriceAction",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductPriceAction {}

    /**
     * Delete an ProductPriceAction
     */
    @Service(
        name = "deleteProductPriceAction",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "deleteProductPriceAction",
        description = "Delete an ProductPriceAction",
        defaultEntityName = "ProductPriceAction",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPriceAction {}

    /**
     * Create a ProductPromo
     */
    @Service(
        name = "createProductPromo",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "createProductPromo",
        description = "Create a ProductPromo",
        defaultEntityName = "ProductPromo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        },
        overrideAttributes = {
            @OverrideAttribute(name = "promoName", optional = "false"),
            @OverrideAttribute(name = "promoText", allowHtml = "any")
        }
    )
    public interface CreateProductPromo {}

    /**
     * Update a ProductPromo
     */
    @Service(
        name = "updateProductPromo",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "updateProductPromo",
        description = "Update a ProductPromo",
        defaultEntityName = "ProductPromo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        },
        overrideAttributes = {
            @OverrideAttribute(name = "promoText", allowHtml = "any")
        }
    )
    public interface UpdateProductPromo {}

    /**
     * Delete a ProductPromo
     */
    @Service(
        name = "deleteProductPromo",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "deleteProductPromo",
        description = "Delete a ProductPromo",
        defaultEntityName = "ProductPromo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPromo {}

    /**
     * Create a ProductPromo
     */
    @Service(
        name = "createProductPromoAction",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "createProductPromoAction",
        description = "Create a ProductPromo",
        defaultEntityName = "ProductPromoAction",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productPromoActionSeqId", mode = "OUT"),
            @OverrideAttribute(name = "productPromoActionEnumId", optional = "false")
        }
    )
    public interface CreateProductPromoAction {}

    /**
     * Update a ProductPromo
     */
    @Service(
        name = "updateProductPromoAction",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "updateProductPromoAction",
        description = "Update a ProductPromo",
        defaultEntityName = "ProductPromoAction",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductPromoAction {}

    /**
     * Delete a ProductPromo
     */
    @Service(
        name = "deleteProductPromoAction",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "deleteProductPromoAction",
        description = "Delete a ProductPromo",
        defaultEntityName = "ProductPromoAction",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPromoAction {}

    /**
     * Create a ProductPromoCategory
     */
    @Service(
        name = "createProductPromoCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "createProductPromoCategory",
        description = "Create a ProductPromoCategory",
        defaultEntityName = "ProductPromoCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductPromoCategory {}

    /**
     * Update a ProductPromoCategory
     */
    @Service(
        name = "updateProductPromoCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "updateProductPromoCategory",
        description = "Update a ProductPromoCategory",
        defaultEntityName = "ProductPromoCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductPromoCategory {}

    /**
     * Delete a ProductPromoCategory
     */
    @Service(
        name = "deleteProductPromoCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "deleteProductPromoCategory",
        description = "Delete a ProductPromoCategory",
        defaultEntityName = "ProductPromoCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPromoCategory {}

    /**
     * Create a ProductPromoCode
     */
    @Service(
        name = "createProductPromoCode",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "createProductPromoCode",
        description = "Create a ProductPromoCode",
        defaultEntityName = "ProductPromoCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        }
    )
    public interface CreateProductPromoCode {}

    /**
     * Update a ProductPromoCode
     */
    @Service(
        name = "updateProductPromoCode",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "updateProductPromoCode",
        description = "Update a ProductPromoCode",
        defaultEntityName = "ProductPromoCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        }
    )
    public interface UpdateProductPromoCode {}

    /**
     * Delete a ProductPromoCode
     */
    @Service(
        name = "deleteProductPromoCode",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "deleteProductPromoCode",
        description = "Delete a ProductPromoCode",
        defaultEntityName = "ProductPromoCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPromoCode {}

    /**
     * Creates several ProductPromoCode from an uploaded list of promo codes (one code per line)
     */
    @Service(
        name = "createBulkProductPromoCode",
        engine = "java",
        location = "org.ofbiz.product.promo.PromoServices",
        invoke = "importPromoCodesFromFile",
        description = "Creates several ProductPromoCode from an uploaded list of promo codes (one code per line)",
        auth = "true",
        implemented = {@Implements(service = "uploadFileInterface")},
        entityAttributes = {
            @EntityAttributes(entityName = "ProductPromoCode", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateBulkProductPromoCode {}

    /**
     * Create a ProductPromoCodeEmail
     */
    @Service(
        name = "createProductPromoCodeEmail",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "createProductPromoCodeEmail",
        description = "Create a ProductPromoCodeEmail",
        defaultEntityName = "ProductPromoCodeEmail",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductPromoCodeEmail {}

    /**
     * Delete a ProductPromoCodeEmail
     */
    @Service(
        name = "deleteProductPromoCodeEmail",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "deleteProductPromoCodeEmail",
        description = "Delete a ProductPromoCodeEmail",
        defaultEntityName = "ProductPromoCodeEmail",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPromoCodeEmail {}

    /**
     * Create several ProductPromoCodeEmail from an uploaded list of emails (one address per line)
     */
    @Service(
        name = "createBulkProductPromoCodeEmail",
        engine = "java",
        location = "org.ofbiz.product.promo.PromoServices",
        invoke = "importPromoCodeEmailsFromFile",
        description = "Create several ProductPromoCodeEmail from an uploaded list of emails (one address per line)",
        auth = "true",
        implemented = {@Implements(service = "uploadFileInterface")},
        attributes = {
            @Attribute(name = "productPromoCodeId", type = "String", mode = "IN")
        }
    )
    public interface CreateBulkProductPromoCodeEmail {}

    /**
     * Create a ProductPromoCodeParty
     */
    @Service(
        name = "createProductPromoCodeParty",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "createProductPromoCodeParty",
        description = "Create a ProductPromoCodeParty",
        defaultEntityName = "ProductPromoCodeParty",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductPromoCodeParty {}

    /**
     * Delete a ProductPromoCodeParty
     */
    @Service(
        name = "deleteProductPromoCodeParty",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "deleteProductPromoCodeParty",
        description = "Delete a ProductPromoCodeParty",
        defaultEntityName = "ProductPromoCodeParty",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPromoCodeParty {}

    /**
     * Create a Product Promo Code Set
     */
    @Service(
        name = "createProductPromoCodeSet",
        engine = "java",
        location = "org.ofbiz.product.promo.PromoServices",
        invoke = "createProductPromoCodeSet",
        description = "Create a Product Promo Code Set",
        defaultEntityName = "ProductPromoCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        },
        attributes = {
            @Attribute(name = "quantity", type = "Long", mode = "IN"),
            @Attribute(name = "codeLength", type = "Integer", mode = "IN", optional = "true", defaultValue = "8"),
            @Attribute(name = "promoCodeLayout", type = "String", mode = "IN", optional = "true", defaultValue = "sequence")
        }
    )
    public interface CreateProductPromoCodeSet {}

    /**
     * Create a ProductPromo
     */
    @Service(
        name = "createProductPromoCond",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "createProductPromoCond",
        description = "Create a ProductPromo",
        defaultEntityName = "ProductPromoCond",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "carrierShipmentMethod", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productPromoCondSeqId", mode = "OUT")
        }
    )
    public interface CreateProductPromoCond {}

    /**
     * Update a ProductPromo
     */
    @Service(
        name = "updateProductPromoCond",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "updateProductPromoCond",
        description = "Update a ProductPromo",
        defaultEntityName = "ProductPromoCond",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "carrierShipmentMethod", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateProductPromoCond {}

    /**
     * Delete a ProductPromo
     */
    @Service(
        name = "deleteProductPromoCond",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "deleteProductPromoCond",
        description = "Delete a ProductPromo",
        defaultEntityName = "ProductPromoCond",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPromoCond {}

    /**
     * Create a ProductPromoProduct
     */
    @Service(
        name = "createProductPromoProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "createProductPromoProduct",
        description = "Create a ProductPromoProduct",
        defaultEntityName = "ProductPromoProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductPromoProduct {}

    /**
     * Update a ProductPromoProduct
     */
    @Service(
        name = "updateProductPromoProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "updateProductPromoProduct",
        description = "Update a ProductPromoProduct",
        defaultEntityName = "ProductPromoProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductPromoProduct {}

    /**
     * Delete a ProductPromoProduct
     */
    @Service(
        name = "deleteProductPromoProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "deleteProductPromoProduct",
        description = "Delete a ProductPromoProduct",
        defaultEntityName = "ProductPromoProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPromoProduct {}

    /**
     * Create a ProductPromo
     */
    @Service(
        name = "createProductPromoRule",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "createProductPromoRule",
        description = "Create a ProductPromo",
        defaultEntityName = "ProductPromoRule",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productPromoRuleId", mode = "OUT")
        }
    )
    public interface CreateProductPromoRule {}

    /**
     * Update a ProductPromo
     */
    @Service(
        name = "updateProductPromoRule",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "updateProductPromoRule",
        description = "Update a ProductPromo",
        defaultEntityName = "ProductPromoRule",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        }
    )
    public interface UpdateProductPromoRule {}

    /**
     * Delete a ProductPromo
     */
    @Service(
        name = "deleteProductPromoRule",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/promo/PromoServices.xml",
        invoke = "deleteProductPromoRule",
        description = "Delete a ProductPromo",
        defaultEntityName = "ProductPromoRule",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPromoRule {}

    /**
     * Calculate a Product's Purchase Price
     */
    @Service(
        name = "calculatePurchasePrice",
        engine = "java",
        location = "org.ofbiz.product.price.PriceServices",
        invoke = "calculatePurchasePrice",
        description = "Calculate a Product's Purchase Price",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "product", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "SCIPIO: useCache flag (default: true) - this should be set to false if called during updated services! (added 2017-12-19)"),
            @Attribute(name = "price", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "validPriceFound", type = "Boolean", mode = "OUT"),
            @Attribute(name = "orderItemPriceInfos", type = "java.util.List", mode = "OUT")
        }
    )
    public interface CalculatePurchasePrice {}

    /**
     * Set the Value options for selected Price Rule Condition Input
     */
    @Service(
        name = "getAssociatedPriceRulesConds",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "getAssociatedPriceRulesConds",
        description = "Set the Value options for selected Price Rule Condition Input",
        attributes = {
            @Attribute(name = "inputParamEnumId", type = "String", mode = "IN"),
            @Attribute(name = "productPriceRulesCondValues", type = "java.util.List", mode = "OUT", optional = "true")
        }
    )
    public interface GetAssociatedPriceRulesConds {}

    /**
     * Create a ProductPriceActionType
     */
    @Service(
        name = "createProductPriceActionType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductPriceActionType",
        defaultEntityName = "ProductPriceActionType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateProductPriceActionType {}

    /**
     * Update a ProductPriceActionType
     */
    @Service(
        name = "updateProductPriceActionType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductPriceActionType",
        defaultEntityName = "ProductPriceActionType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductPriceActionType {}

    /**
     * Delete a ProductPriceActionType
     */
    @Service(
        name = "deleteProductPriceActionType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductPriceActionType",
        defaultEntityName = "ProductPriceActionType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPriceActionType {}

    /**
     * Create a ProductPriceAutoNotice
     */
    @Service(
        name = "createProductPriceAutoNotice",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductPriceAutoNotice",
        defaultEntityName = "ProductPriceAutoNotice",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateProductPriceAutoNotice {}

    /**
     * Update a ProductPriceAutoNotice
     */
    @Service(
        name = "updateProductPriceAutoNotice",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductPriceAutoNotice",
        defaultEntityName = "ProductPriceAutoNotice",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductPriceAutoNotice {}

    /**
     * Delete a ProductPriceAutoNotice
     */
    @Service(
        name = "deleteProductPriceAutoNotice",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductPriceAutoNotice",
        defaultEntityName = "ProductPriceAutoNotice",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPriceAutoNotice {}

    @Service(
        name = "interfaceProductPromoCond",
        engine = "interface",
        attributes = {
            @Attribute(name = "productPromoCond", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN"),
            @Attribute(name = "nowTimestamp", type = "Timestamp", mode = "IN"),
            @Attribute(name = "directResult", type = "Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "compareBase", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "operatorEnumId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface InterfaceProductPromoCond {}

    /**
     * Product promo condition service on the product amount
     */
    @Service(
        name = "productPromoCondProductAmount",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productAmount",
        description = "Product promo condition service on the product amount",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondProductAmount {}

    /**
     * Product promo condition service on the product Total
     */
    @Service(
        name = "productPromoCondProductTotal",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productTotal",
        description = "Product promo condition service on the product Total",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondProductTotal {}

    /**
     * Product promo condition service on quantity 
     */
    @Service(
        name = "productPromoCondProductQuant",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productQuant",
        description = "Product promo condition service on quantity ",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondProductQuant {}

    /**
     * Product promo condition service on Account Days Since Created 
     */
    @Service(
        name = "productPromoCondNewACCT",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productNewACCT",
        description = "Product promo condition service on Account Days Since Created ",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondNewACCT {}

    /**
     * Product promo condition service on party ID 
     */
    @Service(
        name = "productPromoCondPartyID",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productPartyID",
        description = "Product promo condition service on party ID ",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondPartyID {}

    /**
     * Product promo condition service on party group member 
     */
    @Service(
        name = "productPromoCondPartyGM",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productPartyGM",
        description = "Product promo condition service on party group member ",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondPartyGM {}

    /**
     * Product promo condition service on party Classification 
     */
    @Service(
        name = "productPromoCondPartyClass",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productPartyClass",
        description = "Product promo condition service on party Classification ",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondPartyClass {}

    /**
     * Product promo condition service on role type 
     */
    @Service(
        name = "productPromoCondRoleType",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productRoleType",
        description = "Product promo condition service on role type ",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondRoleType {}

    /**
     * Product promo condition service on shipping destination 
     */
    @Service(
        name = "productPromoCondGeoID",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productGeoID",
        description = "Product promo condition service on shipping destination ",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondGeoID {}

    /**
     * Product promo condition service on cart sub-total 
     */
    @Service(
        name = "productPromoCondOrderTotal",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productOrderTotal",
        description = "Product promo condition service on cart sub-total ",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondOrderTotal {}

    /**
     * Product promo condition service on Order sub-total X in last Y Months 
     */
    @Service(
        name = "productPromoCondOrderHist",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productOrderHist",
        description = "Product promo condition service on Order sub-total X in last Y Months ",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondOrderHist {}

    /**
     * Product promo condition service on Order sub-total X since beginning of current year 
     */
    @Service(
        name = "productPromoCondOrderYear",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productOrderYear",
        description = "Product promo condition service on Order sub-total X since beginning of current year ",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondOrderYear {}

    /**
     * Product promo condition service on Order sub-total X last year 
     */
    @Service(
        name = "productPromoCondOrderLastYear",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productOrderLastYear",
        description = "Product promo condition service on Order sub-total X last year ",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondOrderLastYear {}

    /**
     * Product promo condition service on promotion recurrence 
     */
    @Service(
        name = "productPromoCondPromoRecurrence",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productPromoRecurrence",
        description = "Product promo condition service on promotion recurrence ",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondPromoRecurrence {}

    /**
     * Product promo condition service on promotion recurrence 
     */
    @Service(
        name = "productPromoCondOrderShipTotal",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productShipTotal",
        description = "Product promo condition service on promotion recurrence ",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondOrderShipTotal {}

    /**
     * Product promo condition service on shipping total 
     */
    @Service(
        name = "productPromoCondListPriceMinAmount",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productListPriceMinAmount",
        description = "Product promo condition service on shipping total ",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondListPriceMinAmount {}

    /**
     * Product promo condition service on shipping total 
     */
    @Service(
        name = "productPromoCondListPriceMinPercent",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoCondServices.groovy",
        invoke = "productListPriceMinPercent",
        description = "Product promo condition service on shipping total ",
        implemented = {@Implements(service = "interfaceProductPromoCond")}
    )
    public interface ProductPromoCondListPriceMinPercent {}

    @Service(
        name = "interfaceProductPromoAction",
        engine = "interface",
        attributes = {
            @Attribute(name = "productPromoAction", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN"),
            @Attribute(name = "nowTimestamp", type = "Timestamp", mode = "IN"),
            @Attribute(name = "actionResultInfo", type = "org.ofbiz.order.shoppingcart.product.ProductPromoWorker$ActionResultInfo", mode = "INOUT"),
            @Attribute(name = "cartItemModifyException", type = "org.ofbiz.order.shoppingcart.CartItemModifyException", mode = "OUT", optional = "true")
        }
    )
    public interface InterfaceProductPromoAction {}

    /**
     * Product promo Action gift with purchase 
     */
    @Service(
        name = "productPromoActGiftGWP",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoActionServices.groovy",
        invoke = "productGWP",
        description = "Product promo Action gift with purchase ",
        implemented = {@Implements(service = "interfaceProductPromoAction")}
    )
    public interface ProductPromoActGiftGWP {}

    /**
     * Product promo Action free shipping 
     */
    @Service(
        name = "productPromoActFreeShip",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoActionServices.groovy",
        invoke = "productActFreeShip",
        description = "Product promo Action free shipping ",
        implemented = {@Implements(service = "interfaceProductPromoAction")}
    )
    public interface ProductPromoActFreeShip {}

    /**
     * Product promo Action product discount % 
     */
    @Service(
        name = "productPromoActProdDISC",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoActionServices.groovy",
        invoke = "productDISC",
        description = "Product promo Action product discount % ",
        implemented = {@Implements(service = "interfaceProductPromoAction")}
    )
    public interface ProductPromoActProdDISC {}

    /**
     * Product promo Action product discount 
     */
    @Service(
        name = "productPromoActProdAMDISC",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoActionServices.groovy",
        invoke = "productAMDISC",
        description = "Product promo Action product discount ",
        implemented = {@Implements(service = "interfaceProductPromoAction")}
    )
    public interface ProductPromoActProdAMDISC {}

    /**
     * Product promo Action product price 
     */
    @Service(
        name = "productPromoActProdPrice",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoActionServices.groovy",
        invoke = "productPrice",
        description = "Product promo Action product price ",
        implemented = {@Implements(service = "interfaceProductPromoAction")}
    )
    public interface ProductPromoActProdPrice {}

    /**
     * Product promo Action order percent 
     */
    @Service(
        name = "productPromoActOrderPercent",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoActionServices.groovy",
        invoke = "productOrderPercent",
        description = "Product promo Action order percent ",
        implemented = {@Implements(service = "interfaceProductPromoAction")}
    )
    public interface ProductPromoActOrderPercent {}

    /**
     * Product promo Action order amount 
     */
    @Service(
        name = "productPromoActOrderAmount",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoActionServices.groovy",
        invoke = "productOrderAmount",
        description = "Product promo Action order amount ",
        implemented = {@Implements(service = "interfaceProductPromoAction")}
    )
    public interface ProductPromoActOrderAmount {}

    /**
     * Product promo Action product special price 
     */
    @Service(
        name = "productPromoActProdSpecialPrice",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoActionServices.groovy",
        invoke = "productSpecialPrice",
        description = "Product promo Action product special price ",
        implemented = {@Implements(service = "interfaceProductPromoAction")}
    )
    public interface ProductPromoActProdSpecialPrice {}

    /**
     * Product promo Action product tax percent 
     */
    @Service(
        name = "productPromoActTaxPercent",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoActionServices.groovy",
        invoke = "productTaxPercent",
        description = "Product promo Action product tax percent ",
        implemented = {@Implements(service = "interfaceProductPromoAction")}
    )
    public interface ProductPromoActTaxPercent {}

    /**
     * Product promo Action product shipping charge 
     */
    @Service(
        name = "productPromoActShipCharge",
        engine = "groovy",
        location = "component://product/script/org/ofbiz/product/promo/ProductPromoActionServices.groovy",
        invoke = "productShipCharge",
        description = "Product promo Action product shipping charge ",
        implemented = {@Implements(service = "interfaceProductPromoAction")}
    )
    public interface ProductPromoActShipCharge {}

}
