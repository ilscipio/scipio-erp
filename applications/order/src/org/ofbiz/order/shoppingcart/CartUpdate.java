package org.ofbiz.order.shoppingcart;

import java.io.Serializable;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;

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
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private final HttpServletRequest request;
    private final CartUpdateStatus status;
    private boolean commitCalled = false;
    
    public CartUpdate(HttpServletRequest request) {
        this.request = request;
        // DEV NOTE: could use a ThreadLocal in place of req attr, but this less likely to cause leaks in case of errors
        CartUpdateStatus status = getCartUpdateStatus(request);
        if (status == null) {
            status = new CartUpdateStatus(1, null);
            request.setAttribute("cartUpdateStatus", status);
        } else {
            status.nestedLevel++;
        }
        this.status = status;
        if (status.debug) {
            Debug.logInfo("Begin cart update section (depth: " + status.nestedLevel + ")" + getLogSuffix(), module);
        }
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
     * <p>
     * NOTE: This also acts as a "begin" section flag, at start of synchronized block.
     * <p>
     * WARN: This must ONLY be called inside a synchronized block that synchronizes
     * on the object returned by {@link #getLockObject()}.
     */
    public ShoppingCart getCartForUpdate() {
        if (status.cartForUpdate == null) {
            status.cartForUpdate = makeCartForUpdate(ShoppingCartEvents.getCartObject(request));
        }
        return status.cartForUpdate;
    }

    /**
     * Returns a modifiable cart instance derived from the given cart,
     * which may be committed if changes successful (using {@link #commit(ShoppingCart)}).
     * <p>
     * 2018-11-12: Switched to private because it will probably get misused and cause
     * problems.
     */
    private ShoppingCart makeCartForUpdate(ShoppingCart cart) {
        ShoppingCart newCart = cart.exactCopy();
        if (status.debug) {
            Debug.logInfo("Cloned cart " + getLogCartDesc(cart) + " to " + getLogCartDesc(newCart)
                + " for update" + getLogSuffix(), module);
        }
        return newCart;
    }

    /**
     * Commits the cart changes (if top level).
     */
    public ShoppingCart commit(ShoppingCart cart) {
        if (status.isTopLevel()) {
            if (status.debug) {
                Debug.logInfo("Committing modified shopping cart " + getLogCartDesc(cart) + getLogSuffix(), module);
            }
            ShoppingCartEvents.replaceCurrentCartObject(request, cart, true);
        } else {
            if (status.debug) {
                Debug.logInfo("Delaying shopping cart commit (nested)" + getLogSuffix(), module);
            }
        }
        commitCalled = true;
        return cart;
    }

    @Override
    public void close() {
        if (status.isTopLevel()) {
            request.removeAttribute("cartUpdateStatus");
        } else {
            status.nestedLevel--;
        }
        if (ShoppingCart.DEBUG || Debug.verboseOn()) {
            if (commitCalled) {
                Debug.logInfo("End cart update section (depth: " + status.nestedLevel + ")" + getLogSuffix(), module);
            } else {
                Debug.logWarning("End cart update section (depth: " + status.nestedLevel 
                        + ") - commit never invoked" + getLogSuffix(), module);
            }
        }
    }

    private String getLogCartDesc(ShoppingCart cart) {
        return "@" + Integer.toHexString(System.identityHashCode(cart));
    }
    
    private String getLogSuffix() {
        return "; threadId: " + Thread.currentThread().getId();
    }
    
    public static CartUpdateStatus getCartUpdateStatus(HttpServletRequest request) {
        return (CartUpdateStatus) request.getAttribute("cartUpdateStatus");
    }

    /**
     * Stored in request attribute cartUpdateStatus and records nesting level.
     * Also lowers need for attributes.
     */
    @SuppressWarnings("serial")
    public static class CartUpdateStatus implements Serializable {
        private int nestedLevel = 1;
        private ShoppingCart cartForUpdate;
        private final boolean debug = (ShoppingCart.DEBUG || Debug.verboseOn());
        
        protected CartUpdateStatus(int nestedLevel, ShoppingCart cartForUpdate) {
            this.nestedLevel = nestedLevel;
            this.cartForUpdate = cartForUpdate;
        }

        public int getNestedLevel() {
            return nestedLevel;
        }

        public ShoppingCart getCartForUpdate() {
            return cartForUpdate;
        }
        
        public boolean isTopLevel() {
            return (nestedLevel <= 1);
        }
    }
}