package com.ilscipio.scipio.order.payment.codeplugin;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;

import com.ilscipio.scipio.ce.base.util.AdvancedPropertyUtil;
import com.ilscipio.scipio.ce.base.util.AdvancedPropertyUtil.ClassDef;
import com.ilscipio.scipio.ce.base.util.AdvancedPropertyUtil.ClassDefFactory;

/**
 * SCIPIO: Reads payment code plugins (callback handlers)
 * from all "config/scipio-codeplugins.properties" files in
 * all components having entries prefixed with "paymethplugin.handlerFactory".
 * <p>
 * The class must implement {@link PayMethPluginFactory}.
 */
public abstract class PayMethPlugins {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // DEV NOTE: DO NOT strip the .properties prefix here
    public static final String CONFIG_PROPRES = "scipio-codeplugins.properties";
    public static final String CONFIG_PROPPREFIX = "paymethplugin.handlerFactory";
    
    private static class Handlers { // prevents accident init too soon
        private static final List<PayMethPluginHandler> payMethPluginHandlers = Collections.unmodifiableList(readPayMethPluginHandlers());
    }
    
    protected PayMethPlugins() {
    }

    public static List<PayMethPluginHandler> getAllPayMethPluginHandlers() {
        return Handlers.payMethPluginHandlers;
    }
    
    public static <T extends PayMethPluginHandler> List<T> getPayMethPluginHandlersOfType(Class<T> targetCls) {
        ArrayList<T> filteredHandlers = new ArrayList<>(getAllPayMethPluginHandlers().size());
        for(PayMethPluginHandler handler : getAllPayMethPluginHandlers()) {
            if (targetCls.isAssignableFrom(handler.getClass())) {
                @SuppressWarnings("unchecked")
                T convertedHandler = (T) handler;
                filteredHandlers.add(convertedHandler);
            }
        }
        filteredHandlers.trimToSize();
        return filteredHandlers;
    }
    
    /**
     * Reads the plugin handlers. The handler instances are created only once
     * for the whole system so that the same instances will be used across
     * all components (accounting, order, etc.).
     */
    private static List<PayMethPluginHandler> readPayMethPluginHandlers() {
        List<ClassDef<PayMethPluginFactory>> pluginDefs = readClassDefs(CONFIG_PROPRES, 
                CONFIG_PROPPREFIX, PayMethPluginFactory.class);
        List<PayMethPluginHandler> handlers = new ArrayList<>(pluginDefs.size());
        for(ClassDef<PayMethPluginFactory> pluginDef : pluginDefs) {
            PayMethPluginHandler handler = pluginDef.getInstance().getHandler();
            handlers.add(handler);
        }
        return handlers;
    }
    
    protected static <H> List<ClassDef<H>> readClassDefs(String resource, String propPrefix, Class<H> handlerIf) {
        return AdvancedPropertyUtil.readClassDefsFromAllComponents(resource, 
                propPrefix, new ClassDefFactory<ClassDef<H>, H>() {
            @Override
            public ClassDef<H> newInstance(Map<String, String> props, ClassLoader classLoader) {
                try {
                    return new ClassDef<H>(props, classLoader, handlerIf);
                } catch(Exception e) {
                    Debug.logError("Scipio: Error initializing " + resource
                            + " " + propPrefix
                            + " handler '" + props + "': " + e.getMessage(), module);
                    return null;
                }
            }
        });
    }

}
