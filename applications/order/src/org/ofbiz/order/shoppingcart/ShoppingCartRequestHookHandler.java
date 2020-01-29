package org.ofbiz.order.shoppingcart;

import org.ofbiz.base.util.Debug;
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

    static void register() {
        RequestHandlerHooks.subscribe(new ShoppingCartRequestHookHandler());
    }

    // maps the original cart to a copy of that cart
    private final ThreadLocal<Map<ShoppingCart, ShoppingCart>> origCartsLocal = new ThreadLocal<>();

    private void recordCarts(HttpServletRequest request) {
        Map<ShoppingCart, ShoppingCart> origCarts = origCartsLocal.get();
        if (origCarts == null) {
            origCarts = new HashMap<>();
        }
        ShoppingCart cart = (ShoppingCart) request.getAttribute("shoppingCart");
        if (cart != null && !origCarts.containsKey(cart) && !ShoppingCart.class.equals(cart.getClass())) {
            origCarts.put(cart, cart.copy(true));
        }
        HttpSession session = request.getSession(false);
        if (session != null) {
            cart = (ShoppingCart) session.getAttribute("shoppingCart");
            if (cart != null && !origCarts.containsKey(cart) && !ShoppingCart.class.equals(cart.getClass())) {
                origCarts.put(cart, cart.copy(true));
            }
        }
        origCartsLocal.set(origCarts);
    }

    private void verifyCarts(HttpServletRequest request) {
        Map<ShoppingCart, ShoppingCart> origCarts = origCartsLocal.get();
        if (origCarts == null) {
            return;
        }
        for(Map.Entry<ShoppingCart, ShoppingCart> entry : origCarts.entrySet()) {
            List<String> errorMessages = new ArrayList<>(0);
            entry.getKey().ensureExactEquals(entry.getValue(), errorMessages);
            if (errorMessages.size() > 0) {
                Debug.logWarning("ShoppingCart " + entry.getKey() + " was modified in place during request, please use a CartUpdate section: "
                        + errorMessages, module);
            }
        }
    }

    @Override
    public void beginAllDoRequest(HttpServletRequest request, HttpServletResponse response, RequestHandler requestHandler) {
        recordCarts(request);
    }

    @Override
    public void postPreprocessorEvents(HttpServletRequest request, HttpServletResponse response, RequestHandler requestHandler) {
        recordCarts(request);
    }

    @Override
    public void postEvents(HttpServletRequest request, HttpServletResponse response, RequestHandler requestHandler) {
        recordCarts(request);
    }

    @Override
    public void endAllDoRequest(HttpServletRequest request, HttpServletResponse response, RequestHandler requestHandler) {
        verifyCarts(request);
        origCartsLocal.remove();
    }
}
