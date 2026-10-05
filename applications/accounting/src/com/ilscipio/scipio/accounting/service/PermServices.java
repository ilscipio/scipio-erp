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
public class PermServices {

    /**
     * Accounting Agreement Permission Checking Logic
     */
    @Service(
        name = "acctgAgreementPermissionCheck",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/permissions/PermissionServices.xml",
        invoke = "acctgAgreementPermissionCheck",
        description = "Accounting Agreement Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface AcctgAgreementPermissionCheck {}

    /**
     * Accounting Basic Permission Checking Logic
     */
    @Service(
        name = "acctgBasePermissionCheck",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/permissions/PermissionServices.xml",
        invoke = "basePermissionCheck",
        description = "Accounting Basic Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface AcctgBasePermissionCheck {}

    /**
     * Basic Billing Account Permission Checking Logic
     */
    @Service(
        name = "acctgBillingAcctCheck",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/permissions/PermissionServices.xml",
        invoke = "acctgBillingAcctCheck",
        description = "Basic Billing Account Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface AcctgBillingAcctCheck {}

    /**
     * Accounting Commission Permission Checking Logic
     */
    @Service(
        name = "acctgCommissionPermissionCheck",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/permissions/PermissionServices.xml",
        invoke = "commissionPermissionCheck",
        description = "Accounting Commission Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface AcctgCommissionPermissionCheck {}

    /**
     * Accounting Cost Permission Checking Logic
     */
    @Service(
        name = "acctgCostPermissionCheck",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/permissions/PermissionServices.xml",
        invoke = "acctgCostPermissionCheck",
        description = "Accounting Cost Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface AcctgCostPermissionCheck {}

    /**
     * Accounting Financial Account Permission Checking Logic
     */
    @Service(
        name = "acctgFinAcctPermissionCheck",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/permissions/PermissionServices.xml",
        invoke = "acctgFinAcctPermissionCheck",
        description = "Accounting Financial Account Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface AcctgFinAcctPermissionCheck {}

    /**
     * Accounting Foreign Exchange Permission Checking Logic
     */
    @Service(
        name = "acctgFxPermissionCheck",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/permissions/PermissionServices.xml",
        invoke = "acctgFxPermissionCheck",
        description = "Accounting Foreign Exchange Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface AcctgFxPermissionCheck {}

    /**
     * Accounting Invoice Permission Checking Logic
     */
    @Service(
        name = "acctgInvoicePermissionCheck",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/permissions/PermissionServices.xml",
        invoke = "acctgInvoicePermissionCheck",
        description = "Accounting Invoice Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface AcctgInvoicePermissionCheck {}

    /**
     * Accounting Preferences Permission Checking Logic
     */
    @Service(
        name = "acctgPrefPermissionCheck",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/permissions/PermissionServices.xml",
        invoke = "preferencePermissionCheck",
        description = "Accounting Preferences Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface AcctgPrefPermissionCheck {}

    /**
     * Basic General Ledger Permission Checking Logic
     */
    @Service(
        name = "acctgTransactionPermissionCheck",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/permissions/PermissionServices.xml",
        invoke = "acctgTransactionPermissionCheck",
        description = "Basic General Ledger Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface AcctgTransactionPermissionCheck {}

    /**
     * Basic General Ledger Permission Checking Logic
     */
    @Service(
        name = "basicGeneralLedgerPermissionCheck",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/permissions/PermissionServices.xml",
        invoke = "basePermissionCheck",
        description = "Basic General Ledger Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface BasicGeneralLedgerPermissionCheck {}

    /**
     * Fixed Asset Permission Checking Logic
     */
    @Service(
        name = "fixedAssetPermissionCheck",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/permissions/PermissionServices.xml",
        invoke = "basePermissionCheck",
        description = "Fixed Asset Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface FixedAssetPermissionCheck {}

    /**
     * Accounting Payment Permission Checking Logic
     */
    @Service(
        name = "acctgPaymentPermissionCheck",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/permissions/PermissionServices.xml",
        invoke = "acctgPaymentPermissionCheck",
        description = "Accounting Payment Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface AcctgPaymentPermissionCheck {}

}
