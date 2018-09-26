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
