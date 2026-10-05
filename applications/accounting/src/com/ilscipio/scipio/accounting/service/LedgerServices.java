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
public class LedgerServices {

    /**
     * Create a GlAccount record
     */
    @Service(
        name = "createGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createGlAccount",
        description = "Create a GlAccount record",
        defaultEntityName = "GlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "glAccountTypeId", optional = "false"),
            @OverrideAttribute(name = "glAccountClassId", optional = "false"),
            @OverrideAttribute(name = "glResourceTypeId", optional = "false"),
            @OverrideAttribute(name = "accountName", optional = "false")
        }
    )
    public interface CreateGlAccount {}

    /**
     * Update a GlAccount record
     */
    @Service(
        name = "updateGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "updateGlAccount",
        description = "Update a GlAccount record",
        defaultEntityName = "GlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateGlAccount {}

    /**
     * Delete a GlAccount record
     */
    @Service(
        name = "deleteGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "deleteGlAccount",
        description = "Delete a GlAccount record",
        defaultEntityName = "GlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteGlAccount {}

    /**
     * SCIPIO: Gets a GlAccount and its associations
     */
    @Service(
        name = "getGlAccountAndAssocs",
        engine = "java",
        location = "com.ilscipio.scipio.accounting.ledger.GeneralLedgerServices",
        invoke = "getGlAccountAndAssocs",
        description = "SCIPIO: Gets a GlAccount and its associations",
        defaultEntityName = "GlAccount",
        auth = "true",
        attributes = {
            @Attribute(name = "glAccountId", type = "String", mode = "IN"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "glAccount", type = "Map", mode = "OUT")
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "VIEW")
    )
    public interface GetGlAccountAndAssocs {}

    /**
     * Create a GlAccount record
     */
    @Service(
        name = "createGlAccountOrganization",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createGlAccountOrganization",
        description = "Create a GlAccount record",
        defaultEntityName = "GlAccountOrganization",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateGlAccountOrganization {}

    /**
     * Update a GlAccount record
     */
    @Service(
        name = "updateGlAccountOrganization",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "updateGlAccountOrganization",
        description = "Update a GlAccount record",
        defaultEntityName = "GlAccountOrganization",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateGlAccountOrganization {}

    /**
     * Delete a GlAccount record
     */
    @Service(
        name = "deleteGlAccountOrganization",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "deleteGlAccountOrganization",
        description = "Delete a GlAccount record",
        defaultEntityName = "GlAccountOrganization",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteGlAccountOrganization {}

    /**
     * Creates an AcctgTrans and two offsetting AcctgTransEntry records
     */
    @Service(
        name = "quickCreateAcctgTransAndEntries",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "quickCreateAcctgTransAndEntries",
        description = "Creates an AcctgTrans and two offsetting AcctgTransEntry records",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "AcctgTrans", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "AcctgTransEntry", mode = "IN", include = "nonpk", optional = "true", excludeFields = {"debitCreditFlag", "glAccountId"})
        },
        attributes = {
            @Attribute(name = "debitGlAccountId", type = "String", mode = "IN"),
            @Attribute(name = "creditGlAccountId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "amount", optional = "false")
        }
    )
    public interface QuickCreateAcctgTransAndEntries {}

    /**
     * Calculate Trial Balance for a GlJournal
     */
    @Service(
        name = "calculateGlJournalTrialBalance",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "calculateGlJournalTrialBalance",
        description = "Calculate Trial Balance for a GlJournal",
        defaultEntityName = "GlJournal",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "debitTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "creditTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "debitCreditDifference", type = "BigDecimal", mode = "OUT")
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "VIEW")
    )
    public interface CalculateGlJournalTrialBalance {}

    /**
     * Post a GlJournal
     */
    @Service(
        name = "postGlJournal",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "postGlJournal",
        description = "Post a GlJournal",
        defaultEntityName = "GlJournal",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface PostGlJournal {}

    /**
     * Create a GlJournal record
     */
    @Service(
        name = "createGlJournal",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createGlJournal",
        description = "Create a GlJournal record",
        defaultEntityName = "GlJournal",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"isPosted", "postedDate"})
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "organizationPartyId", optional = "false")
        }
    )
    public interface CreateGlJournal {}

    /**
     * Update a GlJournal record
     */
    @Service(
        name = "updateGlJournal",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "updateGlJournal",
        description = "Update a GlJournal record",
        defaultEntityName = "GlJournal",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"isPosted", "postedDate"})
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateGlJournal {}

    /**
     * Delete a GlJournal record
     */
    @Service(
        name = "deleteGlJournal",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "deleteGlJournal",
        description = "Delete a GlJournal record",
        defaultEntityName = "GlJournal",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteGlJournal {}

    /**
     * Create a GlReconciliation record
     */
    @Service(
        name = "createGlReconciliation",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createGlReconciliation",
        description = "Create a GlReconciliation record",
        defaultEntityName = "GlReconciliation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"createdByUserLogin", "lastModifiedByUserLogin"})
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "glReconciliationName", optional = "false")
        }
    )
    public interface CreateGlReconciliation {}

    /**
     * Update a GlReconciliation record
     */
    @Service(
        name = "updateGlReconciliation",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "updateGlReconciliation",
        description = "Update a GlReconciliation record",
        defaultEntityName = "GlReconciliation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"createdByUserLogin", "lastModifiedByUserLogin"})
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateGlReconciliation {}

    /**
     * Delete a GlReconciliation record
     */
    @Service(
        name = "deleteGlReconciliation",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "deleteGlReconciliation",
        description = "Delete a GlReconciliation record",
        defaultEntityName = "GlReconciliation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteGlReconciliation {}

    /**
     * Add an Entry to a GlReconciliation
     */
    @Service(
        name = "createGlReconciliationEntry",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createGlReconciliationEntry",
        description = "Add an Entry to a GlReconciliation",
        defaultEntityName = "GlReconciliationEntry",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        },
        attributes = {
            @Attribute(name = "statusId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateGlReconciliationEntry {}

    /**
     * Update an Entry to a GlReconciliation record
     */
    @Service(
        name = "updateGlReconciliationEntry",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "updateGlReconciliationEntry",
        description = "Update an Entry to a GlReconciliation record",
        defaultEntityName = "GlReconciliationEntry",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateGlReconciliationEntry {}

    /**
     * Remove an Entry from a GlReconciliation
     */
    @Service(
        name = "deleteGlReconciliationEntry",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "deleteGlReconciliationEntry",
        description = "Remove an Entry from a GlReconciliation",
        defaultEntityName = "GlReconciliationEntry",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteGlReconciliationEntry {}

    /**
     * Completes, if possible, the AcctgTransEntries using the mappings defined in the gl setup
     */
    @Service(
        name = "completeAcctgTransEntries",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "completeAcctgTransEntries",
        description = "Completes, if possible, the AcctgTransEntries using the mappings defined in the gl setup",
        defaultEntityName = "AcctgTrans",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgTransactionPermissionCheck", mainAction = "UPDATE")
    )
    public interface CompleteAcctgTransEntries {}

    @Service(
        name = "interfaceAcctgTrans",
        engine = "interface",
        defaultEntityName = "AcctgTrans",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"isPosted", "postedDate", "createdByUserLogin", "lastModifiedByUserLogin"})
        },
        overrideAttributes = {
            @OverrideAttribute(name = "acctgTransTypeId", optional = "false"),
            @OverrideAttribute(name = "transactionDate", optional = "false"),
            @OverrideAttribute(name = "glFiscalTypeId", optional = "false")
        }
    )
    public interface InterfaceAcctgTrans {}

    /**
     * Create a AcctgTrans record.  isPosted is forced to "N"
     */
    @Service(
        name = "createAcctgTrans",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/AcctgTransServices.xml",
        invoke = "createAcctgTrans",
        description = "Create a AcctgTrans record.  isPosted is forced to \"N\"",
        defaultEntityName = "AcctgTrans",
        auth = "true",
        implemented = {@Implements(service = "interfaceAcctgTrans")},
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgTransactionPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateAcctgTrans {}

    /**
     * Update a AcctgTrans record
     */
    @Service(
        name = "updateAcctgTrans",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/AcctgTransServices.xml",
        invoke = "updateAcctgTrans",
        description = "Update a AcctgTrans record",
        defaultEntityName = "AcctgTrans",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgTransactionPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateAcctgTrans {}

    /**
     * Delete a AcctgTrans record
     */
    @Service(
        name = "deleteAcctgTrans",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/AcctgTransServices.xml",
        invoke = "deleteAcctgTrans",
        description = "Delete a AcctgTrans record",
        defaultEntityName = "AcctgTrans",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgTransactionPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteAcctgTrans {}

    @Service(
        name = "interfaceAcctgTransEntry",
        engine = "interface",
        defaultEntityName = "AcctgTransEntry",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"reconcileStatusId"})
        },
        overrideAttributes = {
            @OverrideAttribute(name = "organizationPartyId", optional = "false"),
            @OverrideAttribute(name = "debitCreditFlag", optional = "false")
        }
    )
    public interface InterfaceAcctgTransEntry {}

    /**
     * Add an Entry to a AcctgTrans.  Will use baseCurrencyUomId in PartyAcctgPreference if no currencyUomId is in parameters.
     */
    @Service(
        name = "createAcctgTransEntry",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/AcctgTransServices.xml",
        invoke = "createAcctgTransEntry",
        description = "Add an Entry to a AcctgTrans.  Will use baseCurrencyUomId in PartyAcctgPreference if no currencyUomId is in parameters.",
        defaultEntityName = "AcctgTransEntry",
        auth = "true",
        implemented = {@Implements(service = "interfaceAcctgTransEntry")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "purposeEnumId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgTransactionPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "acctgTransEntrySeqId", mode = "OUT")
        }
    )
    public interface CreateAcctgTransEntry {}

    /**
     * Update an Entry to a AcctgTrans record
     */
    @Service(
        name = "updateAcctgTransEntry",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/AcctgTransServices.xml",
        invoke = "updateAcctgTransEntry",
        description = "Update an Entry to a AcctgTrans record",
        defaultEntityName = "AcctgTransEntry",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgTransactionPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateAcctgTransEntry {}

    /**
     * Remove an Entry from a AcctgTrans
     */
    @Service(
        name = "deleteAcctgTransEntry",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/AcctgTransServices.xml",
        invoke = "deleteAcctgTransEntry",
        description = "Remove an Entry from a AcctgTrans",
        defaultEntityName = "AcctgTransEntry",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgTransactionPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteAcctgTransEntry {}

    /**
     *              Takes a list of AcctgTransEntry entries, verifies that the list of entries are valid (GL account and organizationParty exist),             and then creates an AcctgTrans entry and stores all the AcctgTransEntries with the acctgTransId.  Note that this does not actually             check that the debits and credits balance out.  The idea is that unbalanced transactions can be created here, but they will need             to be created before they are actually posted, and a later posting service will actually check that the transaction is balanced.         
     */
    @Service(
        name = "createAcctgTransAndEntries",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransAndEntries",
        description = "\n            Takes a list of AcctgTransEntry entries, verifies that the list of entries are valid (GL account and organizationParty exist),\n            and then creates an AcctgTrans entry and stores all the AcctgTransEntries with the acctgTransId.  Note that this does not actually\n            check that the debits and credits balance out.  The idea is that unbalanced transactions can be created here, but they will need\n            to be created before they are actually posted, and a later posting service will actually check that the transaction is balanced.\n        ",
        defaultEntityName = "AcctgTrans",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "acctgTransEntries", type = "java.util.List", mode = "IN")
        },
        permissionService = @PermissionService(service = "acctgTransactionPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "acctgTransId", type = "String", mode = "OUT")
        }
    )
    public interface CreateAcctgTransAndEntries {}

    /**
     * Calculate Trial Balance for a AcctgTrans
     */
    @Service(
        name = "calculateAcctgTransTrialBalance",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/AcctgTransServices.xml",
        invoke = "calculateAcctgTransTrialBalance",
        description = "Calculate Trial Balance for a AcctgTrans",
        defaultEntityName = "AcctgTrans",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "debitTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "creditTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "debitCreditDifference", type = "BigDecimal", mode = "OUT")
        },
        permissionService = @PermissionService(service = "acctgTransactionPermissionCheck", mainAction = "CREATE")
    )
    public interface CalculateAcctgTransTrialBalance {}

    /**
     * Post a AcctgTrans and related entries.  This will make sure that the time period is not closed and that          the sum of the debits and credits are equal.         
     */
    @Service(
        name = "postAcctgTrans",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/AcctgTransServices.xml",
        invoke = "postAcctgTrans",
        description = "Post a AcctgTrans and related entries.  This will make sure that the time period is not closed and that\n         the sum of the debits and credits are equal.\n        ",
        defaultEntityName = "AcctgTrans",
        auth = "true",
        transactionTimeout = "600",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "verifyOnly", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgTransactionPermissionCheck", mainAction = "CREATE")
    )
    public interface PostAcctgTrans {}

    /**
     * Close a financial time period
     */
    @Service(
        name = "closeFinancialTimePeriod",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "closeFinancialTimePeriod",
        description = "Close a financial time period",
        defaultEntityName = "CustomTimePeriod",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface CloseFinancialTimePeriod {}

    /**
     * Compute the total debits, total credits, opening, ending balances of an account in a financial period
     */
    @Service(
        name = "computeGlAccountBalanceForTimePeriod",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "computeGlAccountBalanceForTimePeriod",
        description = "Compute the total debits, total credits, opening, ending balances of an account in a financial period",
        auth = "true",
        attributes = {
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN"),
            @Attribute(name = "customTimePeriodId", type = "String", mode = "IN"),
            @Attribute(name = "glAccountId", type = "String", mode = "IN"),
            @Attribute(name = "openingBalance", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "postedDebits", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "postedCredits", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "endingBalance", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface ComputeGlAccountBalanceForTimePeriod {}

    /**
     * Compute and store in a GlAccountHistory record the total debits, total credits, opening, ending balances of an account in a financial period
     */
    @Service(
        name = "computeAndStoreGlAccountHistoryBalance",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "computeAndStoreGlAccountHistoryBalance",
        description = "Compute and store in a GlAccountHistory record the total debits, total credits, opening, ending balances of an account in a financial period",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "GlAccountHistory", mode = "IN", include = "pk")
        }
    )
    public interface ComputeAndStoreGlAccountHistoryBalance {}

    /**
     * Prepare the data for the Income Statement
     */
    @Service(
        name = "prepareIncomeStatement",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "prepareIncomeStatement",
        description = "Prepare the data for the Income Statement",
        auth = "true",
        attributes = {
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "glFiscalTypeId", type = "String", mode = "IN"),
            @Attribute(name = "totalNetIncome", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "glAccountTotalsMap", type = "Map", mode = "OUT", optional = "true")
        }
    )
    public interface PrepareIncomeStatement {}

    /**
     * Look up a GlAccountId first in ProductGlAccount by productId and productGlAccountTypeId, if not found,             then in organizationPartyId and glAccountTypeId 
     */
    @Service(
        name = "getGlAccountFromAccountType",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "getGlAccountFromAccountType",
        description = "Look up a GlAccountId first in ProductGlAccount by productId and productGlAccountTypeId, if not found,\n            then in organizationPartyId and glAccountTypeId ",
        auth = "true",
        attributes = {
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN"),
            @Attribute(name = "glAccountTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "acctgTransTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "debitCreditFlag", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "invoiceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fixedAssetId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "glAccountId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface GetGlAccountFromAccountType {}

    /**
     * get an ownerPartyId from inventoryItemId 
     */
    @Service(
        name = "getInventoryItemOwner",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "getInventoryItemOwner",
        description = "get an ownerPartyId from inventoryItemId ",
        defaultEntityName = "InventoryItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "ownerPartyId", type = "String", mode = "OUT")
        }
    )
    public interface GetInventoryItemOwner {}

    /**
     * Create an accounting transaction for a sales shipment issuance (D: INVENTORY_ACCOUNT, C: COGS_ACCOUNT)
     */
    @Service(
        name = "createAcctgTransForSalesShipmentIssuance",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransForSalesShipmentIssuance",
        description = "Create an accounting transaction for a sales shipment issuance (D: INVENTORY_ACCOUNT, C: COGS_ACCOUNT)",
        auth = "true",
        attributes = {
            @Attribute(name = "itemIssuanceId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransForSalesShipmentIssuance {}

    /**
     * Create an accounting transaction for a canceled sales shipment issuance (D: INVENTORY_ACCOUNT, C: COGS_ACCOUNT)
     */
    @Service(
        name = "createAcctgTransForCanceledSalesShipmentIssuance",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransForCanceledSalesShipmentIssuance",
        description = "Create an accounting transaction for a canceled sales shipment issuance (D: INVENTORY_ACCOUNT, C: COGS_ACCOUNT)",
        auth = "true",
        attributes = {
            @Attribute(name = "itemIssuanceId", type = "String", mode = "IN"),
            @Attribute(name = "canceledQuantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransForCanceledSalesShipmentIssuance {}

    /**
     * Create accounting transaction when item cost is changed (D: INV_ADJ_VAL, C: INVENTORY_ACCOUNT)
     */
    @Service(
        name = "createAcctgTransForInventoryItemCostChange",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransForInventoryItemCostChange",
        description = "Create accounting transaction when item cost is changed (D: INV_ADJ_VAL, C: INVENTORY_ACCOUNT)",
        auth = "true",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN"),
            @Attribute(name = "inventoryItemDetailSeqId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransForInventoryItemCostChange {}

    /**
     * Create an accounting transactions for a shipment receipt (D: INVENTORY_ACCOUNT, C: UNINVOICED_SHIP_RCPT or COGS_ACCOUNT for returns)
     */
    @Service(
        name = "createAcctgTransForShipmentReceipt",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransForShipmentReceipt",
        description = "Create an accounting transactions for a shipment receipt (D: INVENTORY_ACCOUNT, C: UNINVOICED_SHIP_RCPT or COGS_ACCOUNT for returns)",
        auth = "true",
        attributes = {
            @Attribute(name = "receiptId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransForShipmentReceipt {}

    /**
     * Create a FinAccountTypeGlAccount
     */
    @Service(
        name = "createFinAccountTypeGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "createFinAccountTypeGlAccount",
        description = "Create a FinAccountTypeGlAccount",
        defaultEntityName = "FinAccountTypeGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        }
    )
    public interface CreateFinAccountTypeGlAccount {}

    /**
     * Update a FinAccountTypeGlAccount
     */
    @Service(
        name = "updateFinAccountTypeGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "updateFinAccountTypeGlAccount",
        description = "Update a FinAccountTypeGlAccount",
        defaultEntityName = "FinAccountTypeGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        }
    )
    public interface UpdateFinAccountTypeGlAccount {}

    /**
     * Delete a FinAccountTypeGlAccount
     */
    @Service(
        name = "deleteFinAccountTypeGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "deleteFinAccountTypeGlAccount",
        description = "Delete a FinAccountTypeGlAccount",
        defaultEntityName = "FinAccountTypeGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFinAccountTypeGlAccount {}

    /**
     * create a Variance Reason Gl Account
     */
    @Service(
        name = "createVarianceReasonGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "createVarianceReasonGlAccount",
        description = "create a Variance Reason Gl Account",
        defaultEntityName = "VarianceReasonGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        }
    )
    public interface CreateVarianceReasonGlAccount {}

    /**
     * Update a Variance Reason Gl Account
     */
    @Service(
        name = "updateVarianceReasonGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "updateVarianceReasonGlAccount",
        description = "Update a Variance Reason Gl Account",
        defaultEntityName = "VarianceReasonGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        }
    )
    public interface UpdateVarianceReasonGlAccount {}

    /**
     * delete a Variance Reason Gl Account
     */
    @Service(
        name = "deleteVarianceReasonGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml",
        invoke = "deleteVarianceReasonGlAccount",
        description = "delete a Variance Reason Gl Account",
        defaultEntityName = "VarianceReasonGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteVarianceReasonGlAccount {}

    /**
     * Create an accounting transaction for inventory that is issued to a work effort (Type: INVENTORY D: RAWMAT_INVENTORY, C: WIP_INVENTORY)
     */
    @Service(
        name = "createAcctgTransForWorkEffortIssuance",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransForWorkEffortIssuance",
        description = "Create an accounting transaction for inventory that is issued to a work effort (Type: INVENTORY D: RAWMAT_INVENTORY, C: WIP_INVENTORY)",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransForWorkEffortIssuance {}

    /**
     * Create an AcctgEntry for Physical Inventory variance
     */
    @Service(
        name = "createAcctgTransForPhysicalInventoryVariance",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransForPhysicalInventoryVariance",
        description = "Create an AcctgEntry for Physical Inventory variance",
        attributes = {
            @Attribute(name = "physicalInventoryId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransForPhysicalInventoryVariance {}

    /**
     * Create an accounting transaction for inventory that is produced by a work effort (Type: INVENTORY D: RAWMAT_INVENTORY, C: WIP_INVENTORY)
     */
    @Service(
        name = "createAcctgTransForWorkEffortInventoryProduced",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransForWorkEffortInventoryProduced",
        description = "Create an accounting transaction for inventory that is produced by a work effort (Type: INVENTORY D: RAWMAT_INVENTORY, C: WIP_INVENTORY)",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransForWorkEffortInventoryProduced {}

    /**
     * Create an accounting transaction for cost record created for a work effort
     */
    @Service(
        name = "createAcctgTransForWorkEffortCost",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransForWorkEffortCost",
        description = "Create an accounting transaction for cost record created for a work effort",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "costComponentId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransForWorkEffortCost {}

    /**
     * Create an accounting transactions for Inventory Item Owner Change (D: INVENTORY_ACCOUNT(old Owner) INVENTORY_ACCOUNT(new Owner), C: INVENTORY_XFER_IN(oldOwner) INVENTORY_XFER_OUT(new Owner))
     */
    @Service(
        name = "createAcctgTransForInventoryItemOwnerChange",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransForInventoryItemOwnerChange",
        description = "Create an accounting transactions for Inventory Item Owner Change (D: INVENTORY_ACCOUNT(old Owner) INVENTORY_ACCOUNT(new Owner), C: INVENTORY_XFER_IN(oldOwner) INVENTORY_XFER_OUT(new Owner))",
        auth = "true",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN"),
            @Attribute(name = "oldOwnerPartyId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransForInventoryItemOwnerChange {}

    /**
     * Create an accounting transaction for an incoming payment
     */
    @Service(
        name = "createAcctgTransAndEntriesForIncomingPayment",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransAndEntriesForIncomingPayment",
        description = "Create an accounting transaction for an incoming payment",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransAndEntriesForIncomingPayment {}

    /**
     * Create an accounting transaction for inventory that is issued for fixed asset maintenance (Type: INVENTORY D: INVENTORY_ACCOUNT, C: FIXED_ASSET_MAINT)
     */
    @Service(
        name = "createAcctgTransForFixedAssetMaintIssuance",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransForFixedAssetMaintIssuance",
        description = "Create an accounting transaction for inventory that is issued for fixed asset maintenance (Type: INVENTORY D: INVENTORY_ACCOUNT, C: FIXED_ASSET_MAINT)",
        auth = "true",
        attributes = {
            @Attribute(name = "itemIssuanceId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransForFixedAssetMaintIssuance {}

    /**
     * Create an accounting transaction for a Customer Return invoice
     */
    @Service(
        name = "createAcctgTransForCustomerReturnInvoice",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransForCustomerReturnInvoice",
        description = "Create an accounting transaction for a Customer Return invoice",
        auth = "true",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransForCustomerReturnInvoice {}

    /**
     * Create an accounting transaction for a purchase invoice
     */
    @Service(
        name = "createAcctgTransForPurchaseInvoice",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransForPurchaseInvoice",
        description = "Create an accounting transaction for a purchase invoice",
        auth = "true",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransForPurchaseInvoice {}

    /**
     * Create an accounting transaction for a sales invoice
     */
    @Service(
        name = "createAcctgTransForSalesInvoice",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransForSalesInvoice",
        description = "Create an accounting transaction for a sales invoice",
        auth = "true",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransForSalesInvoice {}

    /**
     * Create an accounting transaction for an outgoing payment
     */
    @Service(
        name = "createAcctgTransAndEntriesForOutgoingPayment",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransAndEntriesForOutgoingPayment",
        description = "Create an accounting transaction for an outgoing payment",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransAndEntriesForOutgoingPayment {}

    /**
     * Create an Acctg Trans And Entry(Duplicate or revert)
     */
    @Service(
        name = "copyAcctgTransAndEntries",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "copyAcctgTransAndEntries",
        description = "Create an Acctg Trans And Entry(Duplicate or revert)",
        auth = "true",
        attributes = {
            @Attribute(name = "fromAcctgTransId", type = "String", mode = "IN"),
            @Attribute(name = "revert", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CopyAcctgTransAndEntries {}

    /**
     * Create an accounting transaction for a payment application
     */
    @Service(
        name = "createAcctgTransAndEntriesForPaymentApplication",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransAndEntriesForPaymentApplication",
        description = "Create an accounting transaction for a payment application",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentApplicationId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransAndEntriesForPaymentApplication {}

    /**
     * Create an accounting transaction for a payment application
     */
    @Service(
        name = "createAcctgTransAndEntriesForCustomerRefundPaymentApplication",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createAcctgTransAndEntriesForCustomerRefundPaymentApplication",
        description = "Create an accounting transaction for a payment application",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentApplicationId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateAcctgTransAndEntriesForCustomerRefundPaymentApplication {}

    /**
     * Associate a party to a General Ledger Account
     */
    @Service(
        name = "createPartyGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createPartyGlAccount",
        description = "Associate a party to a General Ledger Account",
        defaultEntityName = "PartyGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "glAccountId", type = "String", mode = "IN")
        }
    )
    public interface CreatePartyGlAccount {}

    /**
     * Update an existing General Ledger Account of a Party
     */
    @Service(
        name = "updatePartyGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "updatePartyGlAccount",
        description = "Update an existing General Ledger Account of a Party",
        defaultEntityName = "PartyGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "glAccountId", type = "String", mode = "IN")
        }
    )
    public interface UpdatePartyGlAccount {}

    /**
     * Delete an existing General Ledger Account of a Party
     */
    @Service(
        name = "deletePartyGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "deletePartyGlAccount",
        description = "Delete an existing General Ledger Account of a Party",
        defaultEntityName = "PartyGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePartyGlAccount {}

    /**
     * Find CustomTimePeriod records, returns both general ones and those for the organizationPartyId passed
     */
    @Service(
        name = "findCustomTimePeriods",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/period/PeriodServices.xml",
        invoke = "findCustomTimePeriods",
        description = "Find CustomTimePeriod records, returns both general ones and those for the organizationPartyId passed",
        auth = "true",
        attributes = {
            @Attribute(name = "findDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "excludeNoOrganizationPeriods", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "onlyIncludePeriodTypeIdList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "customTimePeriodList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface FindCustomTimePeriods {}

    /**
     * Find the last date before findDate whose TimePeriod is marked isClosed="Y" for organizationPartyId and periodTypeId combination.             If none are found, ie, no closed CustomTimePeriod exists, then return the earliest date available for any             CustomTimePeriod of the organizationPartyId and periodTypeId.             If no findDate is given, then use the current moment (nowTimestamp).         
     */
    @Service(
        name = "findLastClosedDate",
        engine = "java",
        location = "org.ofbiz.accounting.period.PeriodServices",
        invoke = "findLastClosedDate",
        description = "Find the last date before findDate whose TimePeriod is marked isClosed=\"Y\" for organizationPartyId and periodTypeId combination.\n            If none are found, ie, no closed CustomTimePeriod exists, then return the earliest date available for any\n            CustomTimePeriod of the organizationPartyId and periodTypeId.\n            If no findDate is given, then use the current moment (nowTimestamp).\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "findDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "periodTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "lastClosedDate", type = "Timestamp", mode = "OUT", optional = "true"),
            @Attribute(name = "lastClosedTimePeriod", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true")
        }
    )
    public interface FindLastClosedDate {}

    /**
     * Return previous year with respect to the given year and if none found then return null.
     */
    @Service(
        name = "getPreviousTimePeriod",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/period/PeriodServices.xml",
        invoke = "getPreviousTimePeriod",
        description = "Return previous year with respect to the given year and if none found then return null.",
        auth = "true",
        attributes = {
            @Attribute(name = "customTimePeriodId", type = "String", mode = "IN"),
            @Attribute(name = "previousTimePeriod", type = "Map", mode = "OUT", optional = "true")
        }
    )
    public interface GetPreviousTimePeriod {}

    /**
     * Given AcctgTransAndEntire and Calculate acctg trans total for specific time period.
     */
    @Service(
        name = "getAcctgTransEntriesAndTransTotal",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/AcctgTransServices.xml",
        invoke = "getAcctgTransEntriesAndTransTotal",
        description = "Given AcctgTransAndEntire and Calculate acctg trans total for specific time period.",
        auth = "true",
        attributes = {
            @Attribute(name = "customTimePeriodStartDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "customTimePeriodEndDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN"),
            @Attribute(name = "isPosted", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "glAccountId", type = "String", mode = "IN"),
            @Attribute(name = "acctgTransAndEntries", type = "List", mode = "OUT"),
            @Attribute(name = "debitTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "creditTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "debitCreditDifference", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface GetAcctgTransEntriesAndTransTotal {}

    /**
     * Calculate Trial Balance for a GlAccount
     */
    @Service(
        name = "calculateGlAccountTrialBalance",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/AcctgTransServices.xml",
        invoke = "calculateGlAccountTrialBalance",
        description = "Calculate Trial Balance for a GlAccount",
        defaultEntityName = "GlAccount",
        auth = "true",
        attributes = {
            @Attribute(name = "glAccountId", type = "String", mode = "IN"),
            @Attribute(name = "isPosted", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "openingBalanceDebit", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "openingBalanceCredit", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "debitCreditDifference", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface CalculateGlAccountTrialBalance {}

    /**
     * Reverting Accounting Transaction And Entries on Canceling an Invoice
     */
    @Service(
        name = "revertAcctgTransOnCancelInvoice",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/AcctgTransServices.xml",
        invoke = "revertAcctgTransOnCancelInvoice",
        description = "Reverting Accounting Transaction And Entries on Canceling an Invoice",
        auth = "true",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN")
        }
    )
    public interface RevertAcctgTransOnCancelInvoice {}

    /**
     * Create Reverse Accounting Transaction and Entries on removing PaymentApplication records.
     */
    @Service(
        name = "revertAcctgTransOnRemovePaymentApplications",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/AcctgTransServices.xml",
        invoke = "revertAcctgTransOnRemovePaymentApplications",
        description = "Create Reverse Accounting Transaction and Entries on removing PaymentApplication records.",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentApplicationId", type = "String", mode = "IN")
        }
    )
    public interface RevertAcctgTransOnRemovePaymentApplications {}

    /**
     * Create GL Account Category
     */
    @Service(
        name = "createGlAccountCategory",
        engine = "entity-auto",
        invoke = "create",
        description = "Create GL Account Category",
        defaultEntityName = "GlAccountCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateGlAccountCategory {}

    /**
     * Update GL Account Category
     */
    @Service(
        name = "updateGlAccountCategory",
        engine = "entity-auto",
        invoke = "update",
        description = "Update GL Account Category",
        defaultEntityName = "GlAccountCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateGlAccountCategory {}

    /**
     * Create GL Account Category Member
     */
    @Service(
        name = "createGlAccountCategoryMember",
        engine = "entity-auto",
        invoke = "create",
        description = "Create GL Account Category Member",
        defaultEntityName = "GlAccountCategoryMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateGlAccountCategoryMember {}

    /**
     * Delete GL Account Category Member
     */
    @Service(
        name = "deleteGlAccountCategoryMember",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete GL Account Category Member",
        defaultEntityName = "GlAccountCategoryMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteGlAccountCategoryMember {}

    /**
     * Update GL Account Category Member
     */
    @Service(
        name = "updateGlAccountCategoryMember",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "updateGlAccountCategoryMember",
        description = "Update GL Account Category Member",
        defaultEntityName = "GlAccountCategoryMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateGlAccountCategoryMember {}

    /**
     * Create GlAccountCategoryMember from CostCenters
     */
    @Service(
        name = "createGlAcctCatMemFromCostCenters",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "createGlAcctCatMemFromCostCenters",
        description = "Create GlAccountCategoryMember from CostCenters",
        defaultEntityName = "GlAccountCategoryMember",
        auth = "true",
        attributes = {
            @Attribute(name = "glAccountId", type = "String", mode = "IN"),
            @Attribute(name = "glAccountCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "amountPercentage", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "totalAmountPercentage", type = "BigDecimal", mode = "IN", optional = "true")
        }
    )
    public interface CreateGlAcctCatMemFromCostCenters {}

    /**
     * Get amount percentage and glAccount for cost center
     */
    @Service(
        name = "getGlAcctgAndAmountPercentage",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "getGlAcctgAndAmountPercentage",
        description = "Get amount percentage and glAccount for cost center",
        attributes = {
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN"),
            @Attribute(name = "glAcctgAndAmountPercentageList", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "glAccountCategories", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface GetGlAcctgAndAmountPercentage {}

    /**
     * Inventory Valuation List
     */
    @Service(
        name = "getInventoryValuationList",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "getInventoryValuationList",
        description = "Inventory Valuation List",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productCategoryId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "cogsMethodId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryValuationList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface GetInventoryValuationList {}

    /**
     * Set Gl Reconciliation status
     */
    @Service(
        name = "setGlReconciliationStatus",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml",
        invoke = "setGlReconciliationStatus",
        description = "Set Gl Reconciliation status",
        attributes = {
            @Attribute(name = "glReconciliationId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SetGlReconciliationStatus {}

    /**
     * Create Update CostCenters
     */
    @Service(
        name = "createUpdateCostCenter",
        engine = "java",
        location = "org.ofbiz.accounting.ledger.GeneralLedgerServices",
        invoke = "createUpdateCostCenter",
        description = "Create Update CostCenters",
        auth = "true",
        attributes = {
            @Attribute(name = "glAccountId", type = "String", mode = "IN"),
            @Attribute(name = "amountPercentageMap", type = "Map", mode = "IN", optional = "true")
        }
    )
    public interface CreateUpdateCostCenter {}

    /**
     * Create AcctgTransAttribute
     */
    @Service(
        name = "createAcctgTransAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create AcctgTransAttribute",
        defaultEntityName = "AcctgTransAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateAcctgTransAttribute {}

    /**
     * Update AcctgTransAttribute
     */
    @Service(
        name = "updateAcctgTransAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update AcctgTransAttribute",
        defaultEntityName = "AcctgTransAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAcctgTransAttribute {}

    /**
     * Delete AcctgTransAttribute
     */
    @Service(
        name = "deleteAcctgTransAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete AcctgTransAttribute",
        defaultEntityName = "AcctgTransAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteAcctgTransAttribute {}

    /**
     * Create a AcctgTransTypeAttr entry
     */
    @Service(
        name = "createAcctgTransTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a AcctgTransTypeAttr entry",
        defaultEntityName = "AcctgTransTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateAcctgTransTypeAttr {}

    /**
     * Update a AcctgTransTypeAttr record
     */
    @Service(
        name = "updateAcctgTransTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a AcctgTransTypeAttr record",
        defaultEntityName = "AcctgTransTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAcctgTransTypeAttr {}

    /**
     * Delete a AcctgTransTypeAttr record
     */
    @Service(
        name = "deleteAcctgTransTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a AcctgTransTypeAttr record",
        defaultEntityName = "AcctgTransTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteAcctgTransTypeAttr {}

    @Service(
        name = "createAcctgTransEntryType",
        engine = "entity-auto",
        invoke = "create",
        defaultEntityName = "AcctgTransEntryType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateAcctgTransEntryType {}

    @Service(
        name = "updateAcctgTransEntryType",
        engine = "entity-auto",
        invoke = "update",
        defaultEntityName = "AcctgTransEntryType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAcctgTransEntryType {}

    @Service(
        name = "deleteAcctgTransEntryType",
        engine = "entity-auto",
        invoke = "delete",
        defaultEntityName = "AcctgTransEntryType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteAcctgTransEntryType {}

    /**
     * Create an AcctgTransType
     */
    @Service(
        name = "createAcctgTransType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an AcctgTransType",
        defaultEntityName = "AcctgTransType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateAcctgTransType {}

    /**
     * Update an AcctgTransType
     */
    @Service(
        name = "updateAcctgTransType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an AcctgTransType",
        defaultEntityName = "AcctgTransType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAcctgTransType {}

    /**
     * Remove an AcctgTransType
     */
    @Service(
        name = "removeAcctgTransType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove an AcctgTransType",
        defaultEntityName = "AcctgTransType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveAcctgTransType {}

    /**
     * Create GlAccountCategoryType
     */
    @Service(
        name = "createGlAccountCategoryType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create GlAccountCategoryType",
        defaultEntityName = "GlAccountCategoryType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateGlAccountCategoryType {}

    /**
     * Update GlAccountCategoryType
     */
    @Service(
        name = "updateGlAccountCategoryType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update GlAccountCategoryType",
        defaultEntityName = "GlAccountCategoryType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateGlAccountCategoryType {}

    /**
     * Delete GlAccountCategoryType
     */
    @Service(
        name = "deleteGlAccountCategoryType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete GlAccountCategoryType",
        defaultEntityName = "GlAccountCategoryType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteGlAccountCategoryType {}

    /**
     * Create a GlAccountClass
     */
    @Service(
        name = "createGlAccountClass",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a GlAccountClass",
        defaultEntityName = "GlAccountClass",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateGlAccountClass {}

    /**
     * Update a GlAccountClass
     */
    @Service(
        name = "updateGlAccountClass",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a GlAccountClass",
        defaultEntityName = "GlAccountClass",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateGlAccountClass {}

    /**
     * Delete a GlAccountClass
     */
    @Service(
        name = "deleteGlAccountClass",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a GlAccountClass",
        defaultEntityName = "GlAccountClass",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteGlAccountClass {}

    /**
     * Create a GlAccountGroup
     */
    @Service(
        name = "createGlAccountGroup",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a GlAccountGroup",
        defaultEntityName = "GlAccountGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateGlAccountGroup {}

    /**
     * Update a GlAccountGroup
     */
    @Service(
        name = "updateGlAccountGroup",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a GlAccountGroup",
        defaultEntityName = "GlAccountGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateGlAccountGroup {}

    /**
     * Delete a GlAccountGroup
     */
    @Service(
        name = "deleteGlAccountGroup",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a GlAccountGroup",
        defaultEntityName = "GlAccountGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteGlAccountGroup {}

    /**
     * Create a GlAccountGroupMember
     */
    @Service(
        name = "createGlAccountGroupMember",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a GlAccountGroupMember",
        defaultEntityName = "GlAccountGroupMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateGlAccountGroupMember {}

    /**
     * Update a GlAccountGroupMember
     */
    @Service(
        name = "updateGlAccountGroupMember",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a GlAccountGroupMember",
        defaultEntityName = "GlAccountGroupMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateGlAccountGroupMember {}

    /**
     * Delete a GlAccountGroupMember
     */
    @Service(
        name = "deleteGlAccountGroupMember",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a GlAccountGroupMember",
        defaultEntityName = "GlAccountGroupMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteGlAccountGroupMember {}

    /**
     * Create a GlAccountGroupType
     */
    @Service(
        name = "createGlAccountGroupType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a GlAccountGroupType",
        defaultEntityName = "GlAccountGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateGlAccountGroupType {}

    /**
     * Update a GlAccountGroupType
     */
    @Service(
        name = "updateGlAccountGroupType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a GlAccountGroupType",
        defaultEntityName = "GlAccountGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateGlAccountGroupType {}

    /**
     * Delete a GlAccountGroupType
     */
    @Service(
        name = "deleteGlAccountGroupType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a GlAccountGroupType",
        defaultEntityName = "GlAccountGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteGlAccountGroupType {}

    /**
     * Create a GlAccountRole
     */
    @Service(
        name = "createGlAccountRole",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a GlAccountRole",
        defaultEntityName = "GlAccountRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateGlAccountRole {}

    /**
     * Expire a GlAccountRole
     */
    @Service(
        name = "expireGlAccountRole",
        engine = "entity-auto",
        invoke = "expire",
        description = "Expire a GlAccountRole",
        defaultEntityName = "GlAccountRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface ExpireGlAccountRole {}

    /**
     * Create a GlAccountType
     */
    @Service(
        name = "createGlAccountType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a GlAccountType",
        defaultEntityName = "GlAccountType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateGlAccountType {}

    /**
     * Update a GlAccountType
     */
    @Service(
        name = "updateGlAccountType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a GlAccountType",
        defaultEntityName = "GlAccountType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateGlAccountType {}

    /**
     * Delete a GlAccountType
     */
    @Service(
        name = "deleteGlAccountType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a GlAccountType",
        defaultEntityName = "GlAccountType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteGlAccountType {}

    /**
     * Create a GlBudgetXref
     */
    @Service(
        name = "createGlBudgetXref",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a GlBudgetXref",
        defaultEntityName = "GlBudgetXref",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateGlBudgetXref {}

    /**
     * Update a GlBudgetXref
     */
    @Service(
        name = "updateGlBudgetXref",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a GlBudgetXref",
        defaultEntityName = "GlBudgetXref",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateGlBudgetXref {}

    /**
     * Expire a GlBudgetXref
     */
    @Service(
        name = "expireGlBudgetXref",
        engine = "entity-auto",
        invoke = "expire",
        description = "Expire a GlBudgetXref",
        defaultEntityName = "GlBudgetXref",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface ExpireGlBudgetXref {}

    /**
     * Create a GlFiscalType
     */
    @Service(
        name = "createGlFiscalType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a GlFiscalType",
        defaultEntityName = "GlFiscalType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateGlFiscalType {}

    /**
     * Update a GlFiscalType
     */
    @Service(
        name = "updateGlFiscalType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a GlFiscalType",
        defaultEntityName = "GlFiscalType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateGlFiscalType {}

    /**
     * Delete a GlFiscalType
     */
    @Service(
        name = "deleteGlFiscalType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a GlFiscalType",
        defaultEntityName = "GlFiscalType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteGlFiscalType {}

    /**
     * Create a GlResourceType
     */
    @Service(
        name = "createGlResourceType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a GlResourceType",
        defaultEntityName = "GlResourceType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateGlResourceType {}

    /**
     * Update a GlResourceType
     */
    @Service(
        name = "updateGlResourceType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a GlResourceType",
        defaultEntityName = "GlResourceType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateGlResourceType {}

    /**
     * Delete a GlResourceType
     */
    @Service(
        name = "deleteGlResourceType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a GlResourceType",
        defaultEntityName = "GlResourceType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteGlResourceType {}

    /**
     * Create a GlXbrlClass
     */
    @Service(
        name = "createGlXbrlClass",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a GlXbrlClass",
        defaultEntityName = "GlXbrlClass",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateGlXbrlClass {}

    /**
     * Update a GlXbrlClass
     */
    @Service(
        name = "updateGlXbrlClass",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a GlXbrlClass",
        defaultEntityName = "GlXbrlClass",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateGlXbrlClass {}

    /**
     * Delete a GlXbrlClass
     */
    @Service(
        name = "deleteGlXbrlClass",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a GlXbrlClass",
        defaultEntityName = "GlXbrlClass",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteGlXbrlClass {}

    /**
     * Create a SettlementTerm
     */
    @Service(
        name = "createSettlementTerm",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SettlementTerm",
        defaultEntityName = "SettlementTerm",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateSettlementTerm {}

    /**
     * Update a SettlementTerm
     */
    @Service(
        name = "updateSettlementTerm",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SettlementTerm",
        defaultEntityName = "SettlementTerm",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateSettlementTerm {}

    /**
     * Delete a SettlementTerm
     */
    @Service(
        name = "deleteSettlementTerm",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SettlementTerm",
        defaultEntityName = "SettlementTerm",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSettlementTerm {}

    /**
     * Create a ProductAverageCostType
     */
    @Service(
        name = "createProductAverageCostType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductAverageCostType",
        defaultEntityName = "ProductAverageCostType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateProductAverageCostType {}

    /**
     * Update a ProductAverageCostType
     */
    @Service(
        name = "updateProductAverageCostType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductAverageCostType",
        defaultEntityName = "ProductAverageCostType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductAverageCostType {}

    /**
     * Delete a ProductAverageCostType
     */
    @Service(
        name = "deleteProductAverageCostType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductAverageCostType",
        defaultEntityName = "ProductAverageCostType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductAverageCostType {}

    /**
     * Builds a tree containing glAccounts using a given glAccountId as starting point
     */
    @Service(
        name = "buildGlAccountTree",
        engine = "java",
        location = "com.ilscipio.scipio.accounting.ledger.GeneralLedgerServices",
        invoke = "buildGlAccountTree",
        description = "Builds a tree containing glAccounts using a given glAccountId as starting point",
        defaultEntityName = "GlAccount",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "library", type = "String", mode = "IN", optional = "true", defaultValue = "jsTree"),
            @Attribute(name = "mode", type = "String", mode = "IN", optional = "true", defaultValue = "full"),
            @Attribute(name = "state", type = "Map", mode = "IN", optional = "true", description = "Map of state attributes for top node: opened, selected, disabled. (added 2017-10-11)"),
            @Attribute(name = "includeGlAccountData", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "includeEmptyTop", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If false (default), empty glAccount or top node is omitted"),
            @Attribute(name = "treeList", type = "List", mode = "OUT")
        }
    )
    public interface BuildGlAccountTree {}

    /**
     * Builds a tree containing customTimePeriods using a given customTimePeriodId as starting point
     */
    @Service(
        name = "buildCustomPeriodTree",
        engine = "java",
        location = "com.ilscipio.scipio.accounting.period.PeriodServices",
        invoke = "buildCustomPeriodTree",
        description = "Builds a tree containing customTimePeriods using a given customTimePeriodId as starting point",
        defaultEntityName = "CustomTimePeriod",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "library", type = "String", mode = "IN", optional = "true", defaultValue = "jsTree"),
            @Attribute(name = "mode", type = "String", mode = "IN", optional = "true", defaultValue = "full"),
            @Attribute(name = "state", type = "Map", mode = "IN", optional = "true", description = "Map of state attributes for top node: opened, selected, disabled. (added 2017-10-11)"),
            @Attribute(name = "includeTimePeriodData", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "includeEmptyTop", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If false (default), empty customTimePeriod or top node is omitted"),
            @Attribute(name = "treeList", type = "List", mode = "OUT")
        }
    )
    public interface BuildCustomPeriodTree {}

    /**
     * SCIPIO: Gets a TimePeriod
     */
    @Service(
        name = "getTimePeriod",
        engine = "java",
        location = "com.ilscipio.scipio.accounting.period.PeriodServices",
        invoke = "getTimePeriod",
        description = "SCIPIO: Gets a TimePeriod",
        defaultEntityName = "CustomPeriodTime",
        auth = "true",
        attributes = {
            @Attribute(name = "customTimePeriodId", type = "String", mode = "IN"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "timePeriod", type = "Map", mode = "OUT")
        },
        permissionService = @PermissionService(service = "basicGeneralLedgerPermissionCheck", mainAction = "VIEW")
    )
    public interface GetTimePeriod {}

}
