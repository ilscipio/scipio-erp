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
 * all components having entries prefixed with "paymethplugin.handler.".
 * <p>
 * Entries are in one of these forms:
 * <ul>
 * <li>paymethplugin.handler.[name].class=[class] / paymethplugin.handler.[name].prio=[priority] (name-based format)</li>
 * <li>paymethplugin.handler[priority]=[class] / paymethplugin.handler.name=[name] (priority-based start.properties format)</li>
 * </ul>
 * The class must implement {@link PayMethPluginHandler}, automatically through
 * OrderPayMethPluginHandler and/or AccountingPayMethPluginHandler
 */
public abstract class PayMethPlugins {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // DEV NOTE: DO NOT strip the .properties prefix here
    public static final String CONFIG_PROPRES = "scipio-codeplugins.properties";
    public static final String CONFIG_PROPPREFIX = "paymethplugin.handler";
    
    private static final List<PayMethPluginHandler> payMethPluginHandlers = Collections.unmodifiableList(readPayMethPluginHandlers());

    protected PayMethPlugins() {
    }

    public static List<PayMethPluginHandler> getAllPayMethPluginHandlers() {
        return payMethPluginHandlers;
    }
    
    public static <T extends PayMethPluginHandler> List<T> getPayMethPluginHandlersOfType(Class<T> targetCls) {
        ArrayList<T> filteredHandlers = new ArrayList<>(payMethPluginHandlers.size());
        for(PayMethPluginHandler handler : payMethPluginHandlers) {
            if (targetCls.isAssignableFrom(handler.getClass())) {
                @SuppressWarnings("unchecked")
                T convertedHandler = (T) handler;
                filteredHandlers.add(convertedHandler);
            }
        }
        filteredHandlers.trimToSize();
        return filteredHandlers;
    }
    
    private static List<PayMethPluginHandler> readPayMethPluginHandlers() {
        List<ClassDef<PayMethPluginHandler>> pluginDefs = readPayMethPluginHandlers(CONFIG_PROPRES, 
                CONFIG_PROPPREFIX, PayMethPluginHandler.class);
        List<PayMethPluginHandler> handlers = new ArrayList<>(pluginDefs.size());
        for(ClassDef<PayMethPluginHandler> pluginDef : pluginDefs) {
            handlers.add(pluginDef.getInstance());
        }
        return handlers;
    }
    
    protected static <H> List<ClassDef<H>> readPayMethPluginHandlers(String resource, String propPrefix, Class<H> handlerIf) {
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
