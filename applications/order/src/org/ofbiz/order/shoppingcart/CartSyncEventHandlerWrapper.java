package org.ofbiz.order.shoppingcart;

import java.util.Arrays;
import java.util.Iterator;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.webapp.control.ConfigXMLReader.Event;
import org.ofbiz.webapp.control.ConfigXMLReader.RequestMap;
import org.ofbiz.webapp.event.EventHandlerException;
import org.ofbiz.webapp.event.EventHandlerWrapper;

/**
 * SCIPIO: Special cart synchronization event handler wrapper: scans for event service invocations for
 * services marked with the "shoppingCart.update" property and automatically wraps the call in a safe cart update section.
 * Added 2018-11-26.
 */
public class CartSyncEventHandlerWrapper implements EventHandlerWrapper {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private Set<String> targetHandlers = UtilMisc.unmodifiableHashSet("service", "service-multi");

    @Override
    public void init(ServletContext context) throws EventHandlerException {
        String targetHandlersStr = context.getInitParameter("cartUpdateSyncServiceHandlers");
        if (UtilValidate.isNotEmpty(targetHandlersStr)) {
            targetHandlers = UtilMisc.unmodifiableHashSetCopy(Arrays.asList(targetHandlersStr.split(",")));
        }
    }

    @Override
    public String invoke(Iterator<EventHandlerWrapper> handlers, Event event, RequestMap requestMap,
            HttpServletRequest request, HttpServletResponse response) throws EventHandlerException {
        if (targetHandlers.contains(event.getType())) {
            DispatchContext dctx = ((LocalDispatcher) request.getAttribute("dispatcher")).getDispatchContext();
            String serviceName = event.getInvoke();
            ModelService modelService = null;
            try {
                modelService = dctx.getModelService(serviceName);
            } catch (GenericServiceException e) {
                Debug.logError(e, "Could not find service '" + serviceName + "', in event definition for uri '"
                        + requestMap.getUri() + "'; unable to ensure cart synchronization", module);
            }
            if (modelService != null) {
                if (Boolean.TRUE.equals(modelService.getProperty("shoppingCart.update"))) {
                    if (ShoppingCart.isDebug()) {
                        Debug.logInfo("Begin shoppingCart.update cart sync section for event service '" + serviceName + "'"
                                + getLogSuffix(), module);
                    }
                    String result;
                    final boolean createCartIfMissing = false;
                    try (CartUpdate cartUpdate = CartUpdate.updateSection(request, createCartIfMissing)) {
                        // NOTE: cartUpdate automatically populates the "shoppingCart" request attribute
                        // for the service event handler to read (and if committed, also after we leave the block)
                        ShoppingCart cart = cartUpdate.getCartForUpdate();

                        // NOTE: Cart may be null here (due to createCartIfMissing==false).
                        // NOTE: Even if cart null the invoke must be done within this synchronized block
                        // to ensure the session attribute shoppingCart is not changed between now
                        // and the time the ServiceEventHandler re-reads the attribute.

                        result = handlers.next().invoke(handlers, event, requestMap, request, response);

                        // Special case: shoppingCart INOUT
                        // If OUT cart changed, simply let it replace our cart copy through CartUpdate.commit
                        ShoppingCart outCart = (ShoppingCart) request.getAttribute("shoppingCart");
                        if (outCart != null && outCart != cart) {
                            if (ShoppingCart.isDebug()) {
                                if (cart != null) {
                                    Debug.logInfo("Event changed shoppingCart request attribute"
                                            + "; local cart copy will be replaced" + getLogSuffix(), module);
                                } else {
                                    Debug.logInfo("Event set new shoppingCart request attribute (was null)"
                                            + getLogSuffix(), module);
                                }
                            }
                            cart = outCart;
                        }

                        if (cart != null) {
                            if ("error".equals(result)) {
                                Debug.logWarning("Event service '" + serviceName
                                    + "' returned occur; discarding shoppingCart changes" + getLogSuffix(), module);
                            } else {
                                // Do the commit
                                cartUpdate.commit(cart);
                            }
                        }
                    }
                    if (ShoppingCart.isDebug()) {
                        Debug.logInfo("End shoppingCart.update cart sync section for event service '" + serviceName + "'"
                                + getLogSuffix(), module);
                    }
                    return result;
                }
            }
        }
        return handlers.next().invoke(handlers, event, requestMap, request, response);
    }

    private static String getLogSuffix() {
        return " (thread " + Thread.currentThread().getId() + ")";
    }
}
