package com.ilscipio.scipio.order.payment.codeplugin;

import java.util.List;

/**
 * SCIPIO: Order payment method plugin orchestrator, with processXxx methods
 * intended for insertion into stock ofbiz code.
 */
public class OrderPayMethPlugins {

    private static final OrderPayMethPlugins INSTANCE = new OrderPayMethPlugins(
            PayMethPlugins.getPayMethPluginHandlersOfType(OrderPayMethPluginHandler.class)
            );

    protected final List<OrderPayMethPluginHandler> handlers;

    protected OrderPayMethPlugins(List<OrderPayMethPluginHandler> handlers) {
        this.handlers = handlers;
    }

    public static OrderPayMethPlugins getInstance() {
        return INSTANCE;
    }

    public List<OrderPayMethPluginHandler> getHandlers() {
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
