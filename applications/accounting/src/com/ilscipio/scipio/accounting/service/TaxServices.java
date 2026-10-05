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
package com.ilscipio.scipio.accounting.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class TaxServices {

    /**
     * Tax Calc Service Interface
     */
    @Service(
        name = "calcTaxInterface",
        engine = "interface",
        description = "Tax Calc Service Interface",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "payToPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billToPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "itemProductList", type = "java.util.List", mode = "IN"),
            @Attribute(name = "itemAmountList", type = "java.util.List", mode = "IN"),
            @Attribute(name = "itemPriceList", type = "java.util.List", mode = "IN"),
            @Attribute(name = "itemQuantityList", type = "java.util.List", mode = "IN", optional = "true"),
            @Attribute(name = "itemShippingList", type = "java.util.List", mode = "IN", optional = "true"),
            @Attribute(name = "orderShippingAmount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "orderPromotionsAmount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "shippingAddress", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "orderAdjustments", type = "java.util.List", mode = "OUT"),
            @Attribute(name = "itemAdjustments", type = "java.util.List", mode = "OUT"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", description = "SCIPIO: useCache flag (default: true) - this should be set to false if called during updated services! (added 2017-12-19)")
        }
    )
    public interface CalcTaxInterface {}

    /**
     * Tax Calc Total For Display Service Interface
     */
    @Service(
        name = "calcTaxTotalForDisplayInterface",
        engine = "interface",
        description = "Tax Calc Total For Display Service Interface",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "billToPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "basePrice", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "shippingPrice", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "taxTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "taxPercentage", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "priceWithTax", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", description = "SCIPIO: useCache flag (default: true) - this should be set to false if called during updated services! (added 2017-12-19)")
        }
    )
    public interface CalcTaxTotalForDisplayInterface {}

    /**
     * Tax Authority Rate Product Calc Service
     */
    @Service(
        name = "calcTax",
        engine = "java",
        location = "org.ofbiz.accounting.tax.TaxAuthorityServices",
        invoke = "rateProductTaxCalc",
        description = "Tax Authority Rate Product Calc Service",
        implemented = {@Implements(service = "calcTaxInterface")}
    )
    public interface CalcTax {}

    /**
     * Tax Authority Rate Product Calc Service
     */
    @Service(
        name = "calcTaxForDisplay",
        engine = "java",
        location = "org.ofbiz.accounting.tax.TaxAuthorityServices",
        invoke = "rateProductTaxCalcForDisplay",
        description = "Tax Authority Rate Product Calc Service",
        log = "quiet",
        logEca = "quiet",
        implemented = {@Implements(service = "calcTaxTotalForDisplayInterface")}
    )
    public interface CalcTaxForDisplay {}

    /**
     * Create TaxAuthority
     */
    @Service(
        name = "createTaxAuthority",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "createTaxAuthority",
        description = "Create TaxAuthority",
        defaultEntityName = "TaxAuthority",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateTaxAuthority {}

    /**
     * Update TaxAuthority
     */
    @Service(
        name = "updateTaxAuthority",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "updateTaxAuthority",
        description = "Update TaxAuthority",
        defaultEntityName = "TaxAuthority",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateTaxAuthority {}

    /**
     * Delete TaxAuthority
     */
    @Service(
        name = "deleteTaxAuthority",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "deleteTaxAuthority",
        description = "Delete TaxAuthority",
        defaultEntityName = "TaxAuthority",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteTaxAuthority {}

    /**
     * Create TaxAuthorityAssoc
     */
    @Service(
        name = "createTaxAuthorityAssoc",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "createTaxAuthorityAssoc",
        description = "Create TaxAuthorityAssoc",
        defaultEntityName = "TaxAuthorityAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateTaxAuthorityAssoc {}

    /**
     * Update TaxAuthorityAssoc
     */
    @Service(
        name = "updateTaxAuthorityAssoc",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "updateTaxAuthorityAssoc",
        description = "Update TaxAuthorityAssoc",
        defaultEntityName = "TaxAuthorityAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateTaxAuthorityAssoc {}

    /**
     * Delete TaxAuthorityAssoc
     */
    @Service(
        name = "deleteTaxAuthorityAssoc",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "deleteTaxAuthorityAssoc",
        description = "Delete TaxAuthorityAssoc",
        defaultEntityName = "TaxAuthorityAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteTaxAuthorityAssoc {}

    /**
     * Create TaxAuthorityCategory
     */
    @Service(
        name = "createTaxAuthorityCategory",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "createTaxAuthorityCategory",
        description = "Create TaxAuthorityCategory",
        defaultEntityName = "TaxAuthorityCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateTaxAuthorityCategory {}

    /**
     * Update TaxAuthorityCategory
     */
    @Service(
        name = "updateTaxAuthorityCategory",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "updateTaxAuthorityCategory",
        description = "Update TaxAuthorityCategory",
        defaultEntityName = "TaxAuthorityCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateTaxAuthorityCategory {}

    /**
     * Delete TaxAuthorityCategory
     */
    @Service(
        name = "deleteTaxAuthorityCategory",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "deleteTaxAuthorityCategory",
        description = "Delete TaxAuthorityCategory",
        defaultEntityName = "TaxAuthorityCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteTaxAuthorityCategory {}

    /**
     * Create TaxAuthorityGlAccount
     */
    @Service(
        name = "createTaxAuthorityGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "createTaxAuthorityGlAccount",
        description = "Create TaxAuthorityGlAccount",
        defaultEntityName = "TaxAuthorityGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateTaxAuthorityGlAccount {}

    /**
     * Update TaxAuthorityGlAccount
     */
    @Service(
        name = "updateTaxAuthorityGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "updateTaxAuthorityGlAccount",
        description = "Update TaxAuthorityGlAccount",
        defaultEntityName = "TaxAuthorityGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateTaxAuthorityGlAccount {}

    /**
     * Delete TaxAuthorityGlAccount
     */
    @Service(
        name = "deleteTaxAuthorityGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "deleteTaxAuthorityGlAccount",
        description = "Delete TaxAuthorityGlAccount",
        defaultEntityName = "TaxAuthorityGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteTaxAuthorityGlAccount {}

    /**
     * Create TaxAuthorityRateProduct
     */
    @Service(
        name = "createTaxAuthorityRateProduct",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "createTaxAuthorityRateProduct",
        description = "Create TaxAuthorityRateProduct",
        defaultEntityName = "TaxAuthorityRateProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateTaxAuthorityRateProduct {}

    /**
     * Update TaxAuthorityRateProduct
     */
    @Service(
        name = "updateTaxAuthorityRateProduct",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "updateTaxAuthorityRateProduct",
        description = "Update TaxAuthorityRateProduct",
        defaultEntityName = "TaxAuthorityRateProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateTaxAuthorityRateProduct {}

    /**
     * Delete TaxAuthorityRateProduct
     */
    @Service(
        name = "deleteTaxAuthorityRateProduct",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "deleteTaxAuthorityRateProduct",
        description = "Delete TaxAuthorityRateProduct",
        defaultEntityName = "TaxAuthorityRateProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteTaxAuthorityRateProduct {}

    /**
     * Create PartyTaxAuthInfo
     */
    @Service(
        name = "createPartyTaxAuthInfo",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "createPartyTaxAuthInfo",
        description = "Create PartyTaxAuthInfo",
        defaultEntityName = "PartyTaxAuthInfo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", mode = "INOUT", optional = "true")
        }
    )
    public interface CreatePartyTaxAuthInfo {}

    /**
     * Update PartyTaxAuthInfo
     */
    @Service(
        name = "updatePartyTaxAuthInfo",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "updatePartyTaxAuthInfo",
        description = "Update PartyTaxAuthInfo",
        defaultEntityName = "PartyTaxAuthInfo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePartyTaxAuthInfo {}

    /**
     * Delete PartyTaxAuthInfo
     */
    @Service(
        name = "deletePartyTaxAuthInfo",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "deletePartyTaxAuthInfo",
        description = "Delete PartyTaxAuthInfo",
        defaultEntityName = "PartyTaxAuthInfo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePartyTaxAuthInfo {}

    /**
     * Create Customer PartyTaxAuthInfo
     */
    @Service(
        name = "createCustomerTaxAuthInfo",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/tax/TaxAuthorityServices.xml",
        invoke = "createCustomerTaxAuthInfo",
        description = "Create Customer PartyTaxAuthInfo",
        defaultEntityName = "PartyTaxAuthInfo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "taxAuthPartyGeoIds", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateCustomerTaxAuthInfo {}

    /**
     * Import ZipSales Flat File
     */
    @Service(
        name = "importZipSalesTaxData",
        engine = "java",
        location = "org.ofbiz.order.thirdparty.zipsales.ZipSalesServices",
        invoke = "importFlatTable",
        description = "Import ZipSales Flat File",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "taxFileLocation", type = "String", mode = "IN"),
            @Attribute(name = "ruleFileLocation", type = "String", mode = "IN")
        }
    )
    public interface ImportZipSalesTaxData {}

    /**
     * Zip Sales Calc Tax Service - change this to calcTax to run
     */
    @Service(
        name = "flatZipSalesTaxCalc",
        engine = "java",
        location = "org.ofbiz.order.thirdparty.zipsales.ZipSalesServices",
        invoke = "flatTaxCalc",
        description = "Zip Sales Calc Tax Service - change this to calcTax to run",
        implemented = {@Implements(service = "calcTaxInterface")}
    )
    public interface FlatZipSalesTaxCalc {}

    /**
     * Create a TaxAuthorityAssocType
     */
    @Service(
        name = "createTaxAuthorityAssocType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a TaxAuthorityAssocType",
        defaultEntityName = "TaxAuthorityAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateTaxAuthorityAssocType {}

    /**
     * Update a TaxAuthorityAssocType
     */
    @Service(
        name = "updateTaxAuthorityAssocType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a TaxAuthorityAssocType",
        defaultEntityName = "TaxAuthorityAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateTaxAuthorityAssocType {}

    /**
     * Delete a TaxAuthorityAssocType
     */
    @Service(
        name = "deleteTaxAuthorityAssocType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a TaxAuthorityAssocType",
        defaultEntityName = "TaxAuthorityAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteTaxAuthorityAssocType {}

    /**
     * Create a TaxAuthorityRateType
     */
    @Service(
        name = "createTaxAuthorityRateType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a TaxAuthorityRateType",
        defaultEntityName = "TaxAuthorityRateType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateTaxAuthorityRateType {}

    /**
     * Update a TaxAuthorityRateType
     */
    @Service(
        name = "updateTaxAuthorityRateType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a TaxAuthorityRateType",
        defaultEntityName = "TaxAuthorityRateType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateTaxAuthorityRateType {}

    /**
     * Delete a TaxAuthorityRateType
     */
    @Service(
        name = "deleteTaxAuthorityRateType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a TaxAuthorityRateType",
        defaultEntityName = "TaxAuthorityRateType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteTaxAuthorityRateType {}

}
