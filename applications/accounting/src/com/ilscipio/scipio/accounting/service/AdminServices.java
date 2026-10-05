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
public class AdminServices {

    /**
     * Create accounting preferences for a party (organization)
     */
    @Service(
        name = "createPartyAcctgPreference",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/admin/AcctgAdminServices.xml",
        invoke = "createPartyAcctgPreference",
        description = "Create accounting preferences for a party (organization)",
        defaultEntityName = "PartyAcctgPreference",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgPrefPermissionCheck", mainAction = "CREATE")
    )
    public interface CreatePartyAcctgPreference {}

    /**
     * Update accounting preferences for a party (organization)
     */
    @Service(
        name = "updatePartyAcctgPreference",
        engine = "entity-auto",
        invoke = "update",
        description = "Update accounting preferences for a party (organization)",
        defaultEntityName = "PartyAcctgPreference",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"fiscalYearStartMonth", "fiscalYearStartDay", "taxFormId", "cogsMethodId", "baseCurrencyUomId", "oldInvoiceSequenceEnumId", "invoiceSeqCustMethId", "invoiceIdPrefix", "lastInvoiceNumber", "lastInvoiceRestartDate", "useInvoiceIdForReturns", "oldQuoteSequenceEnumId", "quoteSeqCustMethId", "quoteIdPrefix", "lastQuoteNumber", "oldOrderSequenceEnumId", "orderSeqCustMethId", "orderIdPrefix", "lastOrderNumber"})
        },
        permissionService = @PermissionService(service = "acctgPrefPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdatePartyAcctgPreference {}

    /**
     * Get accounting preferences for a party (organization)
     */
    @Service(
        name = "getPartyAccountingPreferences",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/admin/AcctgAdminServices.xml",
        invoke = "getPartyAccountingPreferences",
        description = "Get accounting preferences for a party (organization)",
        defaultEntityName = "PartyAcctgPreference",
        auth = "true",
        attributes = {
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN"),
            @Attribute(name = "partyAccountingPreference", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgPrefPermissionCheck", mainAction = "VIEW")
    )
    public interface GetPartyAccountingPreferences {}

    /**
     * Update the conversion rate between two currencies and expire the old conversion rates
     */
    @Service(
        name = "updateFXConversion",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/admin/AcctgAdminServices.xml",
        invoke = "updateFXConversion",
        description = "Update the conversion rate between two currencies and expire the old conversion rates",
        attributes = {
            @Attribute(name = "uomId", type = "String", mode = "IN"),
            @Attribute(name = "uomIdTo", type = "String", mode = "IN"),
            @Attribute(name = "conversionFactor", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "purposeEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "asOfTimestamp", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgFxPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateFXConversion {}

    /**
     * Define a default GL account for an Account Type for a certain organisation party.
     */
    @Service(
        name = "createGlAccountTypeDefault",
        engine = "entity-auto",
        invoke = "create",
        description = "Define a default GL account for an Account Type for a certain organisation party.",
        defaultEntityName = "GlAccountTypeDefault",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "GlAccountTypeDefault", mode = "IN")
        },
        permissionService = @PermissionService(service = "acctgPrefPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateGlAccountTypeDefault {}

    /**
     * Remove a default GL account for an Account Type for a certain organisation party.
     */
    @Service(
        name = "removeGlAccountTypeDefault",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a default GL account for an Account Type for a certain organisation party.",
        defaultEntityName = "GlAccountTypeDefault",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "GlAccountTypeDefault", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgPrefPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveGlAccountTypeDefault {}

    /**
     * add a override GL account number to a invoice Itemtype for a certain organisation party.
     */
    @Service(
        name = "addInvoiceItemTypeGlAssignment",
        engine = "entity-auto",
        invoke = "create",
        description = "add a override GL account number to a invoice Itemtype for a certain organisation party.",
        defaultEntityName = "InvoiceItemTypeGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "InvoiceItemTypeGlAccount", mode = "IN")
        },
        permissionService = @PermissionService(service = "acctgPrefPermissionCheck", mainAction = "CREATE")
    )
    public interface AddInvoiceItemTypeGlAssignment {}

    /**
     * Remove a override GL account number to a invoice type for a certain organisation party.
     */
    @Service(
        name = "removeInvoiceItemTypeGlAssignment",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a override GL account number to a invoice type for a certain organisation party.",
        defaultEntityName = "InvoiceItemTypeGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "InvoiceItemTypeGlAccount", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgPrefPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveInvoiceItemTypeGlAssignment {}

    /**
     * add a default GL account type to a payment type.
     */
    @Service(
        name = "addPaymentTypeGlAssignment",
        engine = "entity-auto",
        invoke = "create",
        description = "add a default GL account type to a payment type.",
        defaultEntityName = "PaymentGlAccountTypeMap",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "PaymentGlAccountTypeMap", mode = "IN")
        },
        permissionService = @PermissionService(service = "acctgPrefPermissionCheck", mainAction = "CREATE")
    )
    public interface AddPaymentTypeGlAssignment {}

    /**
     * Remove a default GL account type from a payment type.
     */
    @Service(
        name = "removePaymentTypeGlAssignment",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a default GL account type from a payment type.",
        defaultEntityName = "PaymentGlAccountTypeMap",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "PaymentGlAccountTypeMap", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgPrefPermissionCheck", mainAction = "DELETE")
    )
    public interface RemovePaymentTypeGlAssignment {}

    /**
     * add a default GL account number to a payment method type.
     */
    @Service(
        name = "addPaymentMethodTypeGlAssignment",
        engine = "entity-auto",
        invoke = "create",
        description = "add a default GL account number to a payment method type.",
        defaultEntityName = "PaymentMethodTypeGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "PaymentMethodTypeGlAccount", mode = "IN")
        },
        permissionService = @PermissionService(service = "acctgPrefPermissionCheck", mainAction = "CREATE")
    )
    public interface AddPaymentMethodTypeGlAssignment {}

    /**
     * Remove a default GL account number from a payment method type.
     */
    @Service(
        name = "removePaymentMethodTypeGlAssignment",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a default GL account number from a payment method type.",
        defaultEntityName = "PaymentMethodTypeGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "PaymentMethodTypeGlAccount", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgPrefPermissionCheck", mainAction = "DELETE")
    )
    public interface RemovePaymentMethodTypeGlAssignment {}

    /**
     * get the conversion rate
     */
    @Service(
        name = "getFXConversion",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/admin/AcctgAdminServices.xml",
        invoke = "getFXConversion",
        description = "get the conversion rate",
        attributes = {
            @Attribute(name = "uomId", type = "String", mode = "IN"),
            @Attribute(name = "uomIdTo", type = "String", mode = "IN"),
            @Attribute(name = "asOfTimestamp", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "conversionRate", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface GetFXConversion {}

}
