package org.ofbiz.order.shoppingcart;

import javax.servlet.http.HttpServletRequest;

/**
 * SCIPIO: Helper object to manage atomic, synchronized cart updates.
 * <p>
 * Uses auto-closeable to help manage nested updates;
 * prevents nested updates from actually updating the session.
 * <p>
 * The basic usage is:
 * <pre>{@code
 * try (CartUpdate cartUpdate = new CartUpdate(request)) { // SCIPIO
 *    synchronized (cartUpdate.getLockObject()) {
 *        ShoppingCart cart = cartUpdate.getCartForUpdate();
 *        
 *        // modify cart...
 *        
 *        cartUpdate.commit(cart);
 *    }
 * }
 * }</pre>
 * <p>
 * NOTE: It is extremely important that the {@link #getCartForUpdate()} call occurs within
 * the synchronized block, because the cart reference is subject to change outside the synchronized block.
 * <p>
 * If {@link #commit(ShoppingCart)} is never called, the cart changes will be discarded.
 * <p>
 * Added 2018-11-20.
 *
 * @see ShoppingCartEvents
 * @see ShoppingCartEvents#getCartLockObject(HttpServletRequest)
 */
public class CartUpdate implements AutoCloseable {
    private final HttpServletRequest request;
    private final Boolean prevPublish;
    
    public CartUpdate(HttpServletRequest request) {
        this.request = request;
        this.prevPublish = (Boolean) request.getAttribute("shoppingCartPublish");
        request.setAttribute("shoppingCartPublish", false); // for the nested updaters; we bypass
    }

    /**
     * A lock object the caller MUST synchronize on inside the try/catch block.
     */
    public Object getLockObject() {
        return ShoppingCartEvents.getCartLockObject(request);
    }

    /**
     * Returns a modifiable cart instance derived from the current (request/session) cart, 
     * which may be committed if changes successful (using {@link #commit(ShoppingCart)}).
     * This must ONLY be called inside a synchronized block that synchronizes
     * on the object returned by {@link #getLockObject()}.
     */
    public ShoppingCart getCartForUpdate() {
        // TODO
        //return ShoppingCartEvents.getCartObjectCopy(request);
        return ShoppingCartEvents.getCartObject(request);
    }

    /**
     * Returns a modifiable cart instance derived from the given cart,
     * which may be committed if changes successful (using {@link #commit(ShoppingCart)}).
     * This must ONLY be called inside a synchronized block that synchronizes
     * on the object returned by {@link #getLockObject()}.
     */
    public ShoppingCart getCartForUpdate(ShoppingCart cart) {
        // TODO
        //return cart.copyCart();
        return cart;
    }

    /**
     * Commits the cart changes.
     */
    public ShoppingCart commit(ShoppingCart cart) {
        if (!Boolean.FALSE.equals(prevPublish)) {
            ShoppingCartEvents.storeCart(request, cart);
        }
        return cart;
    }

    @Override
    public void close() {
        if (prevPublish == null) {
            request.removeAttribute("shoppingCartPublish");
        } else {
            request.setAttribute("shoppingCartPublish", prevPublish);
        }
    }
}