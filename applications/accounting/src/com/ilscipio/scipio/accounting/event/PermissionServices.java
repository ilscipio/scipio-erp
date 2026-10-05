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
package com.ilscipio.scipio.accounting.event;

import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.service.DispatchContext;

import com.ilscipio.scipio.common.event.CommonPermissionServices;

/**
 * Accounting permission services.
 *
 * <p>Originally generated from component://accounting/script/org/ofbiz/accounting/permissions/PermissionServices.xml.
 * These implement {@code permissionInterface} and are invoked via the service engine
 * (e.g. {@code <if-service-permission>}), so they MUST use the service signature
 * {@code (DispatchContext, Map) -> Map} and delegate to
 * {@link CommonPermissionServices#genericBasePermissionCheck} with the appropriate
 * primary/alt permission, mirroring the original simple-methods.</p>
 *
 * <p>SCIPIO: 4.0.0.</p>
 */
public class PermissionServices {

    private static final String MODULE = PermissionServices.class.getName();

    /** Sets primary/alt permission (unless already supplied by caller) and delegates to the generic check. */
    private static Map<String, Object> check(DispatchContext dctx, Map<String, ?> context,
            String primaryPermission, String altPermission) {
        Map<String, Object> ctx = new HashMap<>(context);
        if (UtilValidate.isEmpty(ctx.get("primaryPermission"))) {
            ctx.put("primaryPermission", primaryPermission);
        }
        if (altPermission != null && UtilValidate.isEmpty(ctx.get("altPermission"))) {
            ctx.put("altPermission", altPermission);
        }
        return CommonPermissionServices.genericBasePermissionCheck(dctx, ctx);
    }

    /** Accounting component base permission logic. */
    public static Map<String, Object> basePermissionCheck(DispatchContext dctx, Map<String, ?> context) {
        return check(dctx, context, "ACCOUNTING", null);
    }

    /** Accounting component base + role permission logic. */
    public static Map<String, Object> basePlusRolePermissionCheck(DispatchContext dctx, Map<String, ?> context) {
        return check(dctx, context, "ACCOUNTING", "ACCOUNTING_ROLE");
    }

    /** Accounting preferences permission logic. */
    public static Map<String, Object> preferencePermissionCheck(DispatchContext dctx, Map<String, ?> context) {
        return check(dctx, context, "ACCTG_PREF", null);
    }

    /** Foreign exchange permission logic. */
    public static Map<String, Object> acctgFxPermissionCheck(DispatchContext dctx, Map<String, ?> context) {
        return check(dctx, context, "ACCTG_FX", null);
    }

    /** Accounting agreement permission logic. */
    public static Map<String, Object> acctgAgreementPermissionCheck(DispatchContext dctx, Map<String, ?> context) {
        return basePlusRolePermissionCheck(dctx, context);
    }

    /** Accounting commissions permission logic. */
    public static Map<String, Object> commissionPermissionCheck(DispatchContext dctx, Map<String, ?> context) {
        return check(dctx, context, "ACCOUNTING_COMM", null);
    }

    /** Accounting cost permission logic. */
    public static Map<String, Object> acctgCostPermissionCheck(DispatchContext dctx, Map<String, ?> context) {
        return basePermissionCheck(dctx, context);
    }

    /** Accounting financial account permission logic. */
    public static Map<String, Object> acctgFinAcctPermissionCheck(DispatchContext dctx, Map<String, ?> context) {
        return basePermissionCheck(dctx, context);
    }

    /** Accounting invoice permission logic. */
    public static Map<String, Object> acctgInvoicePermissionCheck(DispatchContext dctx, Map<String, ?> context) {
        return basePlusRolePermissionCheck(dctx, context);
    }

    /** Accounting transaction permission logic. */
    public static Map<String, Object> acctgTransactionPermissionCheck(DispatchContext dctx, Map<String, ?> context) {
        return check(dctx, context, "ACCTG_ATX", null);
    }

    /** Accounting billing account permission logic. */
    public static Map<String, Object> acctgBillingAcctCheck(DispatchContext dctx, Map<String, ?> context) {
        return basePermissionCheck(dctx, context);
    }

    /** Accounting payment permission logic. */
    public static Map<String, Object> acctgPaymentPermissionCheck(DispatchContext dctx, Map<String, ?> context) {
        return basePlusRolePermissionCheck(dctx, context);
    }
}
