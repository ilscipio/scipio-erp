package com.ilscipio.scipio.order.payment.codeplugin;

/**
 * Interface which classes defined in 
 * <code>scipio-codeplugins.properties#paymethplugin.handlerFactory.*</code>
 * entries must implement.
 */
public interface PayMethPluginFactory {

    /**
     * Returns a payment code plugin handler, which may implement 
     * <code>OrderPayMethPluginHandler</code>, 
     * <code>AccountingPayMethPluginHandler</code>,
     * etc.
     * <p>
     * This is allowed to return a singleton.
     */
    PayMethPluginHandler getHandler();

}
