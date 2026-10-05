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
public class BillingServices {

    /**
     * Create a Billing Account
     */
    @Service(
        name = "createBillingAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/BillingServices.xml",
        invoke = "createBillingAccount",
        description = "Create a Billing Account",
        attributes = {
            @Attribute(name = "accountLimit", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "accountCurrencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billingAccountId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "acctgBillingAcctCheck", mainAction = "CREATE")
    )
    public interface CreateBillingAccount {}

    /**
     * Update a Billing Account
     */
    @Service(
        name = "updateBillingAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/BillingServices.xml",
        invoke = "updateBillingAccount",
        description = "Update a Billing Account",
        attributes = {
            @Attribute(name = "billingAccountId", type = "String", mode = "IN"),
            @Attribute(name = "accountLimit", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "accountCurrencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgBillingAcctCheck", mainAction = "UPDATE")
    )
    public interface UpdateBillingAccount {}

    /**
     * Create a Billing Account Role
     */
    @Service(
        name = "createBillingAccountRole",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/BillingServices.xml",
        invoke = "createBillingAccountRole",
        description = "Create a Billing Account Role",
        attributes = {
            @Attribute(name = "billingAccountId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgBillingAcctCheck", mainAction = "CREATE")
    )
    public interface CreateBillingAccountRole {}

    /**
     * Update a Billing Account Role
     */
    @Service(
        name = "updateBillingAccountRole",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/BillingServices.xml",
        invoke = "updateBillingAccountRole",
        description = "Update a Billing Account Role",
        attributes = {
            @Attribute(name = "billingAccountId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgBillingAcctCheck", mainAction = "UPDATE")
    )
    public interface UpdateBillingAccountRole {}

    /**
     * Remove a Billing Account Role
     */
    @Service(
        name = "removeBillingAccountRole",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/BillingServices.xml",
        invoke = "removeBillingAccountRole",
        description = "Remove a Billing Account Role",
        defaultEntityName = "BillingAccountRole",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgBillingAcctCheck", mainAction = "DELETE")
    )
    public interface RemoveBillingAccountRole {}

    /**
     * Create a Billing Account Term
     */
    @Service(
        name = "createBillingAccountTerm",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/BillingServices.xml",
        invoke = "createBillingAccountTerm",
        description = "Create a Billing Account Term",
        attributes = {
            @Attribute(name = "billingAccountId", type = "String", mode = "IN"),
            @Attribute(name = "termTypeId", type = "String", mode = "IN"),
            @Attribute(name = "termValue", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "uomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billingAccountTermId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "acctgBillingAcctCheck", mainAction = "CREATE")
    )
    public interface CreateBillingAccountTerm {}

    /**
     * Update a Billing Account Term
     */
    @Service(
        name = "updateBillingAccountTerm",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/BillingServices.xml",
        invoke = "updateBillingAccountTerm",
        description = "Update a Billing Account Term",
        attributes = {
            @Attribute(name = "billingAccountTermId", type = "String", mode = "IN"),
            @Attribute(name = "billingAccountId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "termTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "termValue", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "uomId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgBillingAcctCheck", mainAction = "UPDATE")
    )
    public interface UpdateBillingAccountTerm {}

    /**
     * Remove a Billing Account Term
     */
    @Service(
        name = "removeBillingAccountTerm",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/BillingServices.xml",
        invoke = "removeBillingAccountTerm",
        description = "Remove a Billing Account Term",
        attributes = {
            @Attribute(name = "billingAccountTermId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "acctgBillingAcctCheck", mainAction = "DELETE")
    )
    public interface RemoveBillingAccountTerm {}

    /**
     * Calculate the balance of a Billing Account
     */
    @Service(
        name = "calcBillingAccountBalance",
        engine = "java",
        location = "org.ofbiz.accounting.payment.BillingAccountWorker",
        invoke = "calcBillingAccountBalance",
        description = "Calculate the balance of a Billing Account",
        attributes = {
            @Attribute(name = "billingAccountId", type = "String", mode = "IN"),
            @Attribute(name = "accountBalance", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "netAccountBalance", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "availableBalance", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "availableToCapture", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "billingAccount", type = "org.ofbiz.entity.GenericValue", mode = "OUT")
        }
    )
    public interface CalcBillingAccountBalance {}

}
