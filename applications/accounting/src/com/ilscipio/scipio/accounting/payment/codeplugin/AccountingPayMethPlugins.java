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
package com.ilscipio.scipio.accounting.payment.codeplugin;

import java.util.List;

import com.ilscipio.scipio.order.payment.codeplugin.PayMethPlugins;

/**
 * SCIPIO: Accounting payment method plugin orchestrator, with processXxx methods
 * intended for insertion into stock ofbiz code.
 */
public class AccountingPayMethPlugins {

    private static final AccountingPayMethPlugins INSTANCE = new AccountingPayMethPlugins(
            PayMethPlugins.getPayMethPluginHandlersOfType(AccountingPayMethPluginHandler.class)
            );

    protected final List<AccountingPayMethPluginHandler> handlers;

    protected AccountingPayMethPlugins(List<AccountingPayMethPluginHandler> handlers) {
        this.handlers = handlers;
    }

    public static AccountingPayMethPlugins getInstance() {
        return INSTANCE;
    }

    public List<AccountingPayMethPluginHandler> getHandlers() {
        return handlers;
    }

    /*
     * ********************************************************************
     * Main processing methods, for insertion into stock ofbiz code
     * ********************************************************************
     * Generally, these return null if the payment method did not apply to any plugins.
     */

    /*
    public Object processForCheckout(String paymentMethodTypeId, Object... obj) {
        // TODO
        return null;
    }

    public Object processForCart(String paymentMethodTypeId, Object... obj) {
        // TODO
        return null;
    }
    */
}
