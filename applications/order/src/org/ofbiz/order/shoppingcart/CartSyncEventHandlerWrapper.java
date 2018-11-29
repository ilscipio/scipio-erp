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
                    ShoppingCart prevRequestCart = (ShoppingCart) request.getAttribute("shoppingCart");
                    boolean committed = false;
                    String result;
                    try {
                        try (CartUpdate cartUpdate = CartUpdate.updateSection(request)) {
                            // NOTE: If there's no shoppingCart in session, there's nothing for the service to modify,
                            // so we don't need to do anything - HOWEVER we still need the synchronized block
                            // to (help) ensure the session attribute shoppingCart is not changed between the time
                            // we check it here and the time the ServiceEventHandler re-reads the attribute
                            if (prevRequestCart != null || request.getSession().getAttribute("shoppingCart") != null) {
                                ShoppingCart cart = cartUpdate.getCartForUpdate();
                                // NOTE: This will take priority over the session attribute shoppingCart
                                request.setAttribute("shoppingCart", cart);
                                result = handlers.next().invoke(handlers, event, requestMap, request, response);
                                if ("error".equals(result)) {
                                    Debug.logWarning("Event service '" + serviceName 
                                            + "' returned occur; discarding shoppingCart changes", module);
                                } else {
                                    // TODO?: handle case where shoppingCart is also OUT...
                                    // (for now, prefer avoid looking up the ModelService until tangible need)
                                    cartUpdate.commit(cart);
                                    committed = cartUpdate.isCommitted();
                                }
                                return result;
                            }
                        }
                    } finally {
                        if (committed) {
                            if (prevRequestCart == null) {
                                // If there was no shoppingCart req attribute before, there should be none after
                                request.removeAttribute("shoppingCart");
                            }
                        } else {
                            // Restore the request attribute
                            if (prevRequestCart == null) {
                                // There was no shoppingCart attribute before, so there should be none after;
                                // see CartUpdate behavior
                                request.removeAttribute("shoppingCart");
                            } else {
                                request.setAttribute("shoppingCart", prevRequestCart);
                            }
                        }
                    }
                }
            }
        }
        return handlers.next().invoke(handlers, event, requestMap, request, response);
    }

}
