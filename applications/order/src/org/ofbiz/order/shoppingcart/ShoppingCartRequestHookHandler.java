package org.ofbiz.order.shoppingcart;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.control.RequestHandlerHooks;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * SCIPIO: INTERNAL helper class for developer use - client code should not use at this time! Subject to change frequently
 * or may be removed at later date.
 */
public class ShoppingCartRequestHookHandler implements RequestHandlerHooks.HookHandler {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final int LOG_LEVEL = Debug.getLevelFromString(UtilProperties.getPropertyValue("order", "shoppingcart.debug.cartImmutabilityCheck.logLevel", "warning"));
    private static final ShoppingCartRequestHookHandler INSTANCE = new ShoppingCartRequestHookHandler();

    static void register() {
        RequestHandlerHooks.subscribe(INSTANCE);
    }

    static ShoppingCartRequestHookHandler getInstance() {
        return INSTANCE;
    }

    // maps the original cart to a copy of that cart
    private final ThreadLocal<Map<ShoppingCart, ShoppingCart>> origCartsLocal = new ThreadLocal<>();

    private void recordCarts(HttpServletRequest request) {
        ShoppingCart requestCart = (ShoppingCart) request.getAttribute("shoppingCart");
        HttpSession session = request.getSession(false);
        ShoppingCart sessionCart = (session != null) ? (ShoppingCart) session.getAttribute("shoppingCart") : null;
        boolean applicableRequestCart = (requestCart != null && !ShoppingCart.class.equals(requestCart.getClass())); // Must be web cart
        boolean applicableSessionCart = (sessionCart != null && !ShoppingCart.class.equals(sessionCart.getClass())); // Must be web cart
        if (!applicableRequestCart && !applicableSessionCart) {
            return;
        }
        Map<ShoppingCart, ShoppingCart> origCarts = origCartsLocal.get();
        if (origCarts == null) {
            origCarts = new HashMap<>();
        }
        if (applicableRequestCart && !origCarts.containsKey(requestCart)) {
            origCarts.put(requestCart, requestCart.copy(true));
        }
        if (applicableSessionCart && !origCarts.containsKey(sessionCart)) {
            origCarts.put(sessionCart, sessionCart.copy(true));
        }
        origCartsLocal.set(origCarts);
    }

    void recordCart(ShoppingCart cart) {
        if (cart == null || ShoppingCart.class.equals(cart.getClass())) { // Must be web cart
            return;
        }
        Map<ShoppingCart, ShoppingCart> origCarts = origCartsLocal.get();
        if (origCarts == null) {
            origCarts = new HashMap<>();
        } else if (origCarts.containsKey(cart)) {
            return;
        }
        origCarts.put(cart, cart.copy(true));
        origCartsLocal.set(origCarts);
    }

    private void verifyCarts(HttpServletRequest request, String eventName) {
        Map<ShoppingCart, ShoppingCart> origCarts = origCartsLocal.get();
        if (origCarts == null) {
            return;
        }
        for(Map.Entry<ShoppingCart, ShoppingCart> entry : origCarts.entrySet()) {
            List<String> errorMessages = new ArrayList<>(0);
            entry.getKey().ensureExactEquals(entry.getValue(), errorMessages);
            if (errorMessages.size() > 0) {
                Debug.log(LOG_LEVEL, null, eventName + ": ShoppingCart " + entry.getKey() + " modified in-place during request ["
                        + request.getPathInfo() + "]; please wrap in CartUpdate section: "
                        + errorMessages, module);
            }
        }
    }

    @Override
    public void beginDoRequest(HttpServletRequest request, HttpServletResponse response, RequestHandler requestHandler, RequestHandler.RequestState requestState) {
        recordCarts(request);
    }

    @Override
    public void postPreprocessorEvents(HttpServletRequest request, HttpServletResponse response, RequestHandler requestHandler, RequestHandler.RequestState requestState) {
        recordCarts(request);
    }

    @Override
    public void postEvents(HttpServletRequest request, HttpServletResponse response, RequestHandler requestHandler, RequestHandler.RequestState requestState) {
        try {
            verifyCarts(request, "post-events");
        } catch(Throwable t) {
            Debug.logError(t, "Unexpected error in verifyCarts", module);
        }
        recordCarts(request);
    }

    @Override
    public void endDoRequest(HttpServletRequest request, HttpServletResponse response, RequestHandler requestHandler, RequestHandler.RequestState requestState) {
        // DEV NOTE: This is called in a finally block by RequestHandler, so it will always run (unless something corrupts java extremely badly)
        if (requestState.getNestedLevel() <= 1) {
            try {
                verifyCarts(request, "post-request");
            } catch(Throwable t) {
                Debug.logError(t, "Unexpected error in verifyCarts", module);
            } finally {
                origCartsLocal.remove();
            }
        }
    }
}
