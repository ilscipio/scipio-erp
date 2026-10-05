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
public class FinaccountServices {

    /**
     * Create a new Financial Account.  If no finAccountId is provided, an auto-sequenced one will be used.
     */
    @Service(
        name = "createFinAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "createFinAccount",
        description = "Create a new Financial Account.  If no finAccountId is provided, an auto-sequenced one will be used.",
        defaultEntityName = "FinAccount",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"actualBalance", "availableBalance"})
        }
    )
    public interface CreateFinAccount {}

    /**
     * Update a Financial Account
     */
    @Service(
        name = "updateFinAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "updateFinAccount",
        description = "Update a Financial Account",
        defaultEntityName = "FinAccount",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"actualBalance", "availableBalance"})
        },
        attributes = {
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "oldReplenishPaymentId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "oldReplenishLevel", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "replenishPaymentId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "replenishLevel", type = "BigDecimal", mode = "OUT", optional = "true")
        }
    )
    public interface UpdateFinAccount {}

    /**
     * Delete a Financial Account
     */
    @Service(
        name = "deleteFinAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "deleteFinAccount",
        description = "Delete a Financial Account",
        defaultEntityName = "FinAccount",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFinAccount {}

    /**
     * Update FinAccount.actualBalance and FinAccount.availableBalance based on a new FinAccountTrans; meant to be called as an EECA as it is for data maintenance
     */
    @Service(
        name = "updateFinAccountBalancesFromTrans",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "updateFinAccountBalancesFromTrans",
        description = "Update FinAccount.actualBalance and FinAccount.availableBalance based on a new FinAccountTrans; meant to be called as an EECA as it is for data maintenance",
        attributes = {
            @Attribute(name = "finAccountTransId", type = "String", mode = "IN")
        }
    )
    public interface UpdateFinAccountBalancesFromTrans {}

    /**
     * Update FinAccount.availableBalance based on a new FinAccountAuth; meant to be called as an EECA as it is for data maintenance
     */
    @Service(
        name = "updateFinAccountBalancesFromAuth",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "updateFinAccountBalancesFromAuth",
        description = "Update FinAccount.availableBalance based on a new FinAccountAuth; meant to be called as an EECA as it is for data maintenance",
        attributes = {
            @Attribute(name = "finAccountAuthId", type = "String", mode = "IN")
        }
    )
    public interface UpdateFinAccountBalancesFromAuth {}

    /**
     * Create a FinAccountStatus
     */
    @Service(
        name = "createFinAccountStatus",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FinAccountStatus",
        defaultEntityName = "FinAccountStatus",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", excludeFields = {"statusEndDate", "changeByUserLoginId"})
        },
        overrideAttributes = {
            @OverrideAttribute(name = "statusDate", mode = "IN", optional = "true")
        }
    )
    public interface CreateFinAccountStatus {}

    /**
     * Create a new Financial Account Transaction.  Will use current timestamp for entryDate and trasanctionDate if none is provided.
     */
    @Service(
        name = "createFinAccountTrans",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "createFinAccountTrans",
        description = "Create a new Financial Account Transaction.  Will use current timestamp for entryDate and trasanctionDate if none is provided.",
        defaultEntityName = "FinAccountTrans",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"performedByPartyId"})
        },
        attributes = {
            @Attribute(name = "glAccountId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateFinAccountTrans {}

    /**
     * Post a Financial Account Transaction to the General Ledger; meant to be called as an SECA
     */
    @Service(
        name = "postFinAccountTransToGl",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountGlPostServices.xml",
        invoke = "postFinAccountTransToGl",
        description = "Post a Financial Account Transaction to the General Ledger; meant to be called as an SECA",
        attributes = {
            @Attribute(name = "finAccountTransId", type = "String", mode = "IN"),
            @Attribute(name = "glAccountId", type = "String", mode = "IN")
        }
    )
    public interface PostFinAccountTransToGl {}

    /**
     * Create a new Financial Account Role
     */
    @Service(
        name = "createFinAccountRole",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "createFinAccountRole",
        description = "Create a new Financial Account Role",
        defaultEntityName = "FinAccountRole",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateFinAccountRole {}

    /**
     * Update a Financial Account Role
     */
    @Service(
        name = "updateFinAccountRole",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "updateFinAccountRole",
        description = "Update a Financial Account Role",
        defaultEntityName = "FinAccountRole",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFinAccountRole {}

    /**
     * Delete a Financial Account Role
     */
    @Service(
        name = "deleteFinAccountRole",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "deleteFinAccountRole",
        description = "Delete a Financial Account Role",
        defaultEntityName = "FinAccountRole",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFinAccountRole {}

    /**
     * Lower level service for creating authorization against a fin account.  Will use current time for authorizationDate and thruDate if not supplied.
     */
    @Service(
        name = "createFinAccountAuth",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "createFinAccountAuth",
        description = "Lower level service for creating authorization against a fin account.  Will use current time for authorizationDate and thruDate if not supplied.",
        defaultEntityName = "FinAccountAuth",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateFinAccountAuth {}

    /**
     * Expires a fin account authorization.  Will use current time if no time is supplied in parameter
     */
    @Service(
        name = "expireFinAccountAuth",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "expireFinAccountAuth",
        description = "Expires a fin account authorization.  Will use current time if no time is supplied in parameter",
        defaultEntityName = "FinAccountAuth",
        attributes = {
            @Attribute(name = "finAccountAuthId", type = "String", mode = "IN"),
            @Attribute(name = "expireDateTime", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface ExpireFinAccountAuth {}

    /**
     * Set financial account transaction status
     */
    @Service(
        name = "setFinAccountTransStatus",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "setFinAccountTransStatus",
        description = "Set financial account transaction status",
        attributes = {
            @Attribute(name = "finAccountTransId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SetFinAccountTransStatus {}

    /**
     * Update payment when FinAccountTrans status is set to Cancle, remove finAccountTransId form Payment entity.
     */
    @Service(
        name = "updatePaymentOnFinAccTransStatusSetToCancel",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "updatePaymentOnFinAccTransStatusSetToCancel",
        description = "Update payment when FinAccountTrans status is set to Cancle, remove finAccountTransId form Payment entity.",
        attributes = {
            @Attribute(name = "finAccountTransId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdatePaymentOnFinAccTransStatusSetToCancel {}

    /**
     * Creates a new FinAccount, using defaults from the ProductStoreFinActSetting for expiration date and to generate an automatic account code.             Note this would override any user values for from, thru, and acount code
     */
    @Service(
        name = "createFinAccountForStore",
        engine = "java",
        location = "org.ofbiz.accounting.finaccount.FinAccountServices",
        invoke = "createFinAccountForStore",
        description = "Creates a new FinAccount, using defaults from the ProductStoreFinActSetting for expiration date and to generate an automatic account code.\n            Note this would override any user values for from, thru, and acount code",
        defaultEntityName = "FinAccount",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "finAccountId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "finAccountCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "finAccountPin", type = "String", mode = "OUT", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "finAccountTypeId", mode = "IN", optional = "false")
        }
    )
    public interface CreateFinAccountForStore {}

    /**
     * Deposit Funds into a Financial Account
     */
    @Service(
        name = "finAccountDeposit",
        engine = "java",
        location = "org.ofbiz.accounting.finaccount.FinAccountPaymentServices",
        invoke = "finAccountDeposit",
        description = "Deposit Funds into a Financial Account",
        auth = "true",
        attributes = {
            @Attribute(name = "finAccountId", type = "String", mode = "IN"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "isRefund", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "currency", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "reasonEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "balance", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "previousBalance", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface FinAccountDeposit {}

    /**
     * Deposit Funds into a Financial Account
     */
    @Service(
        name = "finAccountWithdraw",
        engine = "java",
        location = "org.ofbiz.accounting.finaccount.FinAccountPaymentServices",
        invoke = "finAccountWithdraw",
        description = "Deposit Funds into a Financial Account",
        auth = "true",
        attributes = {
            @Attribute(name = "finAccountId", type = "String", mode = "IN"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "requireBalance", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "currency", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "reasonEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "balance", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "previousBalance", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface FinAccountWithdraw {}

    /**
     * Refunds the deposits to a financial account back to the source
     */
    @Service(
        name = "finAccountRefund",
        engine = "java",
        location = "org.ofbiz.accounting.finaccount.FinAccountServices",
        invoke = "refundFinAccount",
        description = "Refunds the deposits to a financial account back to the source",
        auth = "true",
        attributes = {
            @Attribute(name = "finAccountId", type = "String", mode = "IN")
        }
    )
    public interface FinAccountRefund {}

    /**
     * Auto-replenish a financial account
     */
    @Service(
        name = "finAccountReplenish",
        engine = "java",
        location = "org.ofbiz.accounting.finaccount.FinAccountPaymentServices",
        invoke = "finAccountReplenish",
        description = "Auto-replenish a financial account",
        auth = "true",
        maxRetry = "3",
        attributes = {
            @Attribute(name = "finAccountId", type = "String", mode = "IN"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface FinAccountReplenish {}

    /**
     * Checks the balance of the financial account
     */
    @Service(
        name = "checkFinAccountBalance",
        engine = "java",
        location = "org.ofbiz.accounting.finaccount.FinAccountServices",
        invoke = "checkFinAccountBalance",
        description = "Checks the balance of the financial account",
        auth = "true",
        attributes = {
            @Attribute(name = "finAccountId", type = "String", mode = "IN"),
            @Attribute(name = "availableBalance", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "balance", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "statusId", type = "String", mode = "OUT")
        }
    )
    public interface CheckFinAccountBalance {}

    /**
     * Checks the status of the financial account; may set statusId to FNACT_MANFROZEN or FNACT_ACTIVE
     */
    @Service(
        name = "checkFinAccountStatus",
        engine = "java",
        location = "org.ofbiz.accounting.finaccount.FinAccountServices",
        invoke = "checkFinAccountStatus",
        description = "Checks the status of the financial account; may set statusId to FNACT_MANFROZEN or FNACT_ACTIVE",
        auth = "true",
        attributes = {
            @Attribute(name = "finAccountAuthId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "finAccountId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CheckFinAccountStatus {}

    /**
     * Financial Account Transaction List and Totals
     */
    @Service(
        name = "getFinAccountTransListAndTotals",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "getFinAccountTransListAndTotals",
        description = "Financial Account Transaction List and Totals",
        attributes = {
            @Attribute(name = "finAccountId", type = "String", mode = "IN"),
            @Attribute(name = "finAccountTransTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "glReconciliationId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromTransactionDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruTransactionDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "fromEntryDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruEntryDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "openingBalance", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "finAccountTransList", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "searchedNumberOfRecords", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "grandTotal", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "createdGrandTotal", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "totalCreatedTransactions", type = "Long", mode = "OUT", optional = "true"),
            @Attribute(name = "approvedGrandTotal", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "totalApprovedTransactions", type = "Long", mode = "OUT", optional = "true"),
            @Attribute(name = "createdApprovedGrandTotal", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "totalCreatedApprovedTransactions", type = "Long", mode = "OUT", optional = "true"),
            @Attribute(name = "glReconciliationApprovedGrandTotal", type = "BigDecimal", mode = "OUT", optional = "true")
        }
    )
    public interface GetFinAccountTransListAndTotals {}

    /**
     * Financial Account Running Total
     */
    @Service(
        name = "getFinAccountTransRunningTotalAndBalances",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "getFinAccountTransRunningTotalAndBalances",
        description = "Financial Account Running Total",
        attributes = {
            @Attribute(name = "finAccountTransId", type = "String", mode = "IN"),
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN"),
            @Attribute(name = "openingBalance", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "reconciledBalance", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "runningTotal", type = "BigDecimal", mode = "INOUT", optional = "true"),
            @Attribute(name = "numberOfTransactions", type = "Long", mode = "INOUT", optional = "true"),
            @Attribute(name = "finAccountTransRunningTotal", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "endingBalance", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface GetFinAccountTransRunningTotalAndBalances {}

    /**
     * Reconcile Financial Accounting Financial Transactions
     */
    @Service(
        name = "reconcileFinAccountTrans",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "reconcileFinAccountTrans",
        description = "Reconcile Financial Accounting Financial Transactions",
        attributes = {
            @Attribute(name = "finAccountTransId", type = "String", mode = "IN"),
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN"),
            @Attribute(name = "glAccountId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "debitCreditFlag", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ReconcileFinAccountTrans {}

    /**
     * Reconcile Financial Accounting Financial Transactions
     */
    @Service(
        name = "reconcileAdjustmentFinAcctgTrans",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "reconcileAdjustmentFinAcctgTrans",
        description = "Reconcile Financial Accounting Financial Transactions",
        attributes = {
            @Attribute(name = "finAccountTrans", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN")
        }
    )
    public interface ReconcileAdjustmentFinAcctgTrans {}

    /**
     * Reconcile Financial Accounting Financial Transactions
     */
    @Service(
        name = "reconcileDepositFinAcctgTrans",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "reconcileDepositFinAcctgTrans",
        description = "Reconcile Financial Accounting Financial Transactions",
        attributes = {
            @Attribute(name = "finAccountTrans", type = "org.ofbiz.entity.GenericValue", mode = "IN")
        }
    )
    public interface ReconcileDepositFinAcctgTrans {}

    /**
     * Reconcile Financial Accounting Financial Transactions
     */
    @Service(
        name = "reconcileWithdrawalFinAcctgTrans",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "reconcileWithdrawalFinAcctgTrans",
        description = "Reconcile Financial Accounting Financial Transactions",
        attributes = {
            @Attribute(name = "finAccountTrans", type = "org.ofbiz.entity.GenericValue", mode = "IN")
        }
    )
    public interface ReconcileWithdrawalFinAcctgTrans {}

    /**
     * Service to Get Reconciliation closing balance.
     */
    @Service(
        name = "getReconciliationClosingBalance",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "getReconciliationClosingBalance",
        description = "Service to Get Reconciliation closing balance.",
        attributes = {
            @Attribute(name = "glReconciliationId", type = "String", mode = "IN"),
            @Attribute(name = "closingBalance", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface GetReconciliationClosingBalance {}

    @Service(
        name = "createServiceCredit",
        engine = "java",
        location = "org.ofbiz.accounting.finaccount.FinAccountServices",
        invoke = "createAccountAndCredit",
        auth = "true",
        attributes = {
            @Attribute(name = "finAccountId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "finAccountName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "reasonEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "finAccountTypeId", type = "String", mode = "IN", defaultValue = "SVCCRED_ACCOUNT")
        }
    )
    public interface CreateServiceCredit {}

    @Service(
        name = "createFinAccountAndCredit",
        engine = "java",
        location = "org.ofbiz.accounting.finaccount.FinAccountServices",
        invoke = "createAccountAndCredit",
        auth = "true",
        attributes = {
            @Attribute(name = "finAccountId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "finAccountName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "reasonEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "replenishPaymentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "replenishLevel", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "finAccountTypeId", type = "String", mode = "IN")
        }
    )
    public interface CreateFinAccountAndCredit {}

    @Service(
        name = "createPartyFinAccountFromPurchase",
        engine = "java",
        location = "org.ofbiz.accounting.finaccount.FinAccountProductServices",
        invoke = "createPartyFinAccountFromPurchase",
        auth = "true",
        implemented = {@Implements(service = "itemFulfillmentInterface")},
        attributes = {
            @Attribute(name = "finAccountId", type = "String", mode = "OUT")
        }
    )
    public interface CreatePartyFinAccountFromPurchase {}

    /**
     * Authorize a potential transaction against a financial account
     */
    @Service(
        name = "ofbFaAuthorize",
        engine = "java",
        location = "org.ofbiz.accounting.finaccount.FinAccountPaymentServices",
        invoke = "finAccountPreAuth",
        description = "Authorize a potential transaction against a financial account",
        auth = "true",
        implemented = {@Implements(service = "paymentProcessInterface")},
        attributes = {
            @Attribute(name = "finAccountCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "finAccountPin", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "finAccountId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface OfbFaAuthorize {}

    /**
     * Capture funds from a pre-authroized financial account transaction
     */
    @Service(
        name = "ofbFaCapture",
        engine = "java",
        location = "org.ofbiz.accounting.finaccount.FinAccountPaymentServices",
        invoke = "finAccountCapture",
        description = "Capture funds from a pre-authroized financial account transaction",
        auth = "true",
        implemented = {@Implements(service = "ccCaptureInterface")}
    )
    public interface OfbFaCapture {}

    /**
     * Release authorizations back to a financial account.
     */
    @Service(
        name = "ofbFaRelease",
        engine = "java",
        location = "org.ofbiz.accounting.finaccount.FinAccountPaymentServices",
        invoke = "finAccountReleaseAuth",
        description = "Release authorizations back to a financial account.",
        auth = "true",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface OfbFaRelease {}

    /**
     * Return funds back to a financial account.
     */
    @Service(
        name = "ofbFaRefund",
        engine = "java",
        location = "org.ofbiz.accounting.finaccount.FinAccountPaymentServices",
        invoke = "finAccountRefund",
        description = "Return funds back to a financial account.",
        auth = "true",
        implemented = {@Implements(service = "paymentRefundInterface")},
        attributes = {
            @Attribute(name = "finAccountId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface OfbFaRefund {}

    /**
     * Generate a Gift Certificate number/pin and store as a FinAccount
     */
    @Service(
        name = "createGiftCertificate",
        engine = "java",
        location = "org.ofbiz.accounting.payment.GiftCertificateServices",
        invoke = "createGiftCertificate",
        description = "Generate a Gift Certificate number/pin and store as a FinAccount",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "initialAmount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "currency", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "cardNumber", type = "String", mode = "OUT"),
            @Attribute(name = "pinNumber", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "responseCode", type = "String", mode = "OUT"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface CreateGiftCertificate {}

    /**
     * Add funds to a Gift Certificate
     */
    @Service(
        name = "addFundsToGiftCertificate",
        engine = "java",
        location = "org.ofbiz.accounting.payment.GiftCertificateServices",
        invoke = "addFundsToGiftCertificate",
        description = "Add funds to a Gift Certificate",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "pinNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "currency", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "balance", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "previousBalance", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "responseCode", type = "String", mode = "OUT"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface AddFundsToGiftCertificate {}

    /**
     * Deduct funds from a Gift Certificate
     */
    @Service(
        name = "redeemGiftCertificate",
        engine = "java",
        location = "org.ofbiz.accounting.payment.GiftCertificateServices",
        invoke = "redeemGiftCertificate",
        description = "Deduct funds from a Gift Certificate",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "pinNumber", type = "String", mode = "IN"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "currency", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "balance", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "previousBalance", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "responseCode", type = "String", mode = "OUT"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface RedeemGiftCertificate {}

    /**
     * Obtain the balanace of a Gift Certificate
     */
    @Service(
        name = "checkGiftCertificateBalance",
        engine = "java",
        location = "org.ofbiz.accounting.payment.GiftCertificateServices",
        invoke = "checkGiftCertificateBalance",
        description = "Obtain the balanace of a Gift Certificate",
        auth = "true",
        attributes = {
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "pinNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "currency", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "balance", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface CheckGiftCertificateBalance {}

    /**
     * Creates the fulfillment log
     */
    @Service(
        name = "createGcFulFillmentRecord",
        engine = "java",
        location = "org.ofbiz.accounting.payment.GiftCertificateServices",
        invoke = "createFulfillmentRecord",
        description = "Creates the fulfillment log",
        auth = "true",
        requireNewTransaction = "true",
        attributes = {
            @Attribute(name = "typeEnumId", type = "String", mode = "IN"),
            @Attribute(name = "merchantId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "surveyResponseId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pinNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "responseCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "referenceNum", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "authCode", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateGcFulFillmentRecord {}

    /**
     * Creates return for reload on failure
     */
    @Service(
        name = "refundGcPurchase",
        engine = "java",
        location = "org.ofbiz.accounting.payment.GiftCertificateServices",
        invoke = "refundGcPurchase",
        description = "Creates return for reload on failure",
        auth = "true",
        requireNewTransaction = "true",
        attributes = {
            @Attribute(name = "orderItem", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN")
        }
    )
    public interface RefundGcPurchase {}

    /**
     * Process a sale using FinAccount Gift Certificate
     */
    @Service(
        name = "ofbGcProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.payment.GiftCertificateServices",
        invoke = "giftCertificateProcessor",
        description = "Process a sale using FinAccount Gift Certificate",
        auth = "true",
        implemented = {@Implements(service = "giftCardProcessInterface")}
    )
    public interface OfbGcProcessor {}

    /**
     * Authorize a potential transaction against a Gift Certificate
     */
    @Service(
        name = "ofbGcAuthorize",
        engine = "java",
        location = "org.ofbiz.accounting.payment.GiftCertificateServices",
        invoke = "giftCertificateAuthorize",
        description = "Authorize a potential transaction against a Gift Certificate",
        auth = "true",
        implemented = {@Implements(service = "giftCardProcessInterface")}
    )
    public interface OfbGcAuthorize {}

    /**
     * Release authorizations back to a Gift Certificate.  No amount is added back, but an authorization is cancelled.
     */
    @Service(
        name = "ofbGcRelease",
        engine = "java",
        location = "org.ofbiz.accounting.payment.GiftCertificateServices",
        invoke = "giftCertificateRelease",
        description = "Release authorizations back to a Gift Certificate.  No amount is added back, but an authorization is cancelled.",
        auth = "true",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface OfbGcRelease {}

    /**
     * Return funds back to a Gift Certificate.  Amounts are added back to the gift certificate.
     */
    @Service(
        name = "ofbGcRefund",
        engine = "java",
        location = "org.ofbiz.accounting.payment.GiftCertificateServices",
        invoke = "giftCertificateRefund",
        description = "Return funds back to a Gift Certificate.  Amounts are added back to the gift certificate.",
        auth = "true",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface OfbGcRefund {}

    /**
     * Automatic Gift Certificate Purchase Fulfillment Service
     */
    @Service(
        name = "ofbGcPurchase",
        engine = "java",
        location = "org.ofbiz.accounting.payment.GiftCertificateServices",
        invoke = "giftCertificatePurchase",
        description = "Automatic Gift Certificate Purchase Fulfillment Service",
        auth = "true",
        implemented = {@Implements(service = "itemFulfillmentInterface")}
    )
    public interface OfbGcPurchase {}

    /**
     * Automatic Gift Certificate Reload Service
     */
    @Service(
        name = "ofbGcReload",
        engine = "java",
        location = "org.ofbiz.accounting.payment.GiftCertificateServices",
        invoke = "giftCertificateReload",
        description = "Automatic Gift Certificate Reload Service",
        auth = "true",
        implemented = {@Implements(service = "itemFulfillmentInterface")}
    )
    public interface OfbGcReload {}

    /**
     * Deposit withdraw payments
     */
    @Service(
        name = "depositWithdrawPayments",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "depositWithdrawPayments",
        description = "Deposit withdraw payments",
        attributes = {
            @Attribute(name = "paymentIds", type = "List", mode = "IN"),
            @Attribute(name = "finAccountId", type = "String", mode = "IN"),
            @Attribute(name = "groupInOneTransaction", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentGroupTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentGroupName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "finAccountTransId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "paymentGroupId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface DepositWithdrawPayments {}

    /**
     * expire payment associations with paymentGroup on finAccountTrans cancel
     */
    @Service(
        name = "expirePaymentAssociationsOnFinAccountTransCancel",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "expirePaymentAssociationsOnFinAccountTransCancel",
        description = "expire payment associations with paymentGroup on finAccountTrans cancel",
        attributes = {
            @Attribute(name = "finAccountTransId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ExpirePaymentAssociationsOnFinAccountTransCancel {}

    /**
     * create new payment and associate with respective financial account in FinAccountTrans Entity.
     */
    @Service(
        name = "createPaymentAndFinAccountTrans",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "createPaymentAndFinAccountTrans",
        description = "create new payment and associate with respective financial account in FinAccountTrans Entity.",
        auth = "true",
        implemented = {@Implements(service = "createPayment")},
        attributes = {
            @Attribute(name = "isDepositWithDrawPayment", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "finAccountTransTypeId", type = "String", mode = "IN"),
            @Attribute(name = "paymentGroupTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreatePaymentAndFinAccountTrans {}

    /**
     * Transaction Total By GlReconcile Id
     */
    @Service(
        name = "getTransactionTotalByGlReconcileId",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "getTransactionTotalByGlReconcileId",
        description = "Transaction Total By GlReconcile Id",
        attributes = {
            @Attribute(name = "glReconciliationId", type = "String", mode = "IN"),
            @Attribute(name = "grandTotal", type = "BigDecimal", mode = "OUT", optional = "true")
        }
    )
    public interface GetTransactionTotalByGlReconcileId {}

    /**
     * Assignment of Gl Reconciliation to Fin Account Trans
     */
    @Service(
        name = "assignGlRecToFinAccTrans",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "assignGlRecToFinAccTrans",
        description = "Assignment of Gl Reconciliation to Fin Account Trans",
        attributes = {
            @Attribute(name = "finAccountTransId", type = "String", mode = "IN"),
            @Attribute(name = "glReconciliationId", type = "String", mode = "IN")
        }
    )
    public interface AssignGlRecToFinAccTrans {}

    /**
     * Remove finaAccountTrans association with gl reconciliation
     */
    @Service(
        name = "removeFinAccountTransFromReconciliation",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "removeFinAccountTransFromReconciliation",
        description = "Remove finaAccountTrans association with gl reconciliation",
        attributes = {
            @Attribute(name = "finAccountTransId", type = "String", mode = "IN")
        }
    )
    public interface RemoveFinAccountTransFromReconciliation {}

    /**
     * Check GlReconciliation is Reconciled or not
     */
    @Service(
        name = "isGlReconciliationReconciled",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "isGlReconciliationReconciled",
        description = "Check GlReconciliation is Reconciled or not",
        attributes = {
            @Attribute(name = "glReconciliationId", type = "String", mode = "IN"),
            @Attribute(name = "isReconciled", type = "Boolean", mode = "OUT")
        }
    )
    public interface IsGlReconciliationReconciled {}

    /**
     * Cancel bank reconciliation.
     */
    @Service(
        name = "cancelBankReconciliation",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "cancelBankReconciliation",
        description = "Cancel bank reconciliation.",
        attributes = {
            @Attribute(name = "glReconciliationId", type = "String", mode = "IN")
        }
    )
    public interface CancelBankReconciliation {}

    /**
     * Get associated acctgTransEntries with finAccountTrans
     */
    @Service(
        name = "getAssociatedAcctgTransEntriesWithFinAccountTrans",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "getAssociatedAcctgTransEntriesWithFinAccountTrans",
        description = "Get associated acctgTransEntries with finAccountTrans",
        attributes = {
            @Attribute(name = "finAccountTransId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransAndEntries", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface GetAssociatedAcctgTransEntriesWithFinAccountTrans {}

    /**
     * Auto Reconciled FinAccountTrans entries
     */
    @Service(
        name = "autoFinAccountReconciliation",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "autoFinAccountReconciliation",
        description = "Auto Reconciled FinAccountTrans entries",
        auth = "true"
    )
    public interface AutoFinAccountReconciliation {}

    /**
     * Create a FinAccountAttribute
     */
    @Service(
        name = "createFinAccountAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FinAccountAttribute",
        defaultEntityName = "FinAccountAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateFinAccountAttribute {}

    /**
     * Update a FinAccountAttribute
     */
    @Service(
        name = "updateFinAccountAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a FinAccountAttribute",
        defaultEntityName = "FinAccountAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFinAccountAttribute {}

    /**
     * Delete a FinAccountAttribute
     */
    @Service(
        name = "deleteFinAccountAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a FinAccountAttribute",
        defaultEntityName = "FinAccountAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFinAccountAttribute {}

    /**
     * Create a FinAccountTransAttribute
     */
    @Service(
        name = "createFinAccountTransAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FinAccountTransAttribute",
        defaultEntityName = "FinAccountTransAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateFinAccountTransAttribute {}

    /**
     * Update a FinAccountTransAttribute
     */
    @Service(
        name = "updateFinAccountTransAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a FinAccountTransAttribute",
        defaultEntityName = "FinAccountTransAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFinAccountTransAttribute {}

    /**
     * Delete a FinAccountTransAttribute
     */
    @Service(
        name = "deleteFinAccountTransAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a FinAccountTransAttribute",
        defaultEntityName = "FinAccountTransAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFinAccountTransAttribute {}

    /**
     * Create a FinAccountTransType record
     */
    @Service(
        name = "createFinAccountTransType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FinAccountTransType record",
        defaultEntityName = "FinAccountTransType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateFinAccountTransType {}

    /**
     * Update a FinAccountTransType record
     */
    @Service(
        name = "updateFinAccountTransType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a FinAccountTransType record",
        defaultEntityName = "FinAccountTransType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFinAccountTransType {}

    /**
     * Delete a FinAccountTransType record
     */
    @Service(
        name = "deleteFinAccountTransType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a FinAccountTransType record",
        defaultEntityName = "FinAccountTransType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFinAccountTransType {}

    /**
     * Create a FinAccountTransTypeAttr
     */
    @Service(
        name = "createFinAccountTransTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FinAccountTransTypeAttr",
        defaultEntityName = "FinAccountTransTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateFinAccountTransTypeAttr {}

    /**
     * Update a FinAccountTransType
     */
    @Service(
        name = "updateFinAccountTransTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a FinAccountTransType",
        defaultEntityName = "FinAccountTransTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFinAccountTransTypeAttr {}

    /**
     * Delete a FinAccountTransTypeAttr
     */
    @Service(
        name = "deleteFinAccountTransTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a FinAccountTransTypeAttr",
        defaultEntityName = "FinAccountTransTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFinAccountTransTypeAttr {}

    /**
     * Create a FinAccountType
     */
    @Service(
        name = "createFinAccountType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FinAccountType",
        defaultEntityName = "FinAccountType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateFinAccountType {}

    /**
     * Update a FinAccountType
     */
    @Service(
        name = "updateFinAccountType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a FinAccountType",
        defaultEntityName = "FinAccountType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFinAccountType {}

    /**
     * Delete a FinAccountType
     */
    @Service(
        name = "deleteFinAccountType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a FinAccountType",
        defaultEntityName = "FinAccountType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFinAccountType {}

    /**
     * Create a FinAccountTypeAttr
     */
    @Service(
        name = "createFinAccountTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FinAccountTypeAttr",
        defaultEntityName = "FinAccountTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateFinAccountTypeAttr {}

    /**
     * Update a FinAccountTypeAttr
     */
    @Service(
        name = "updateFinAccountTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a FinAccountTypeAttr",
        defaultEntityName = "FinAccountTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFinAccountTypeAttr {}

    /**
     * Delete a FinAccountTypeAttr
     */
    @Service(
        name = "deleteFinAccountTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a FinAccountTypeAttr",
        defaultEntityName = "FinAccountTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFinAccountTypeAttr {}

}
