package org.ofbiz.order.shoppingcart;

import java.io.Serializable;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;

/**
 * SCIPIO: Helper object to manage atomic, synchronized cart updates.
 * <p>
 * Automatically synchronizes updates on a shared object (using a ReentrantLock instead
 * of a synchronized block) when {@link #updateSection(HttpServletRequest)} is called
 * and releases the lock when {@link #close()} is called. Automatically manages
 * nested update sections.
 * <strong>WARNING:</strong> If used in pre-java8 code, {@link #close()} must be called
 * from a <code>finally</code> block.
 * <p>
 * Inside the section, simply call {@link #getCartForUpdate()} to get a safe, modifiable cart instance.
 * The modifiable cart instance is a copy of the (immutable) session cart.
 * Once changes are made, call {@link #commit(ShoppingCart)} to commit the changes, or don't
 * call it to discard the changes in case of error.
 * <p>
 * Basic usage:
 * <pre>{@code
 * try (CartSync cartUpdate = CartUpdate.updateSection(request)) { // SCIPIO
 *    ShoppingCart cart = cartUpdate.getCartForUpdate(); 
 *    // modify cart...
 *    cartUpdate.commit(cart);
 * }</pre>
 * <p>
 * NOTE: If you need to synchronize on the cart lock without actually updating the cart itself,
 * use {@link CartSync#synchronizedSection(HttpServletRequest)} instead.
 * <p>
 * TODO: REVIEW: Currently this does NOT try to set the request attribute "shoppingCart" to the modifiable
 * cart at the beginning of the update section, because all known instances of this require caller modifications
 * which are made to use {@link {@link #getCartForUpdate()}}
 * instead. However, this has potential to cause problems for some nested code... it might be required...
 * <p>
 * Added 2018-11-20.
 *
 * @see CartSync
 */
public class CartUpdate implements AutoCloseable {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final boolean CART_COPIES_ENABLED = UtilProperties.getPropertyAsBoolean("order",
            "shoppingcart.update.useCartCopies", true);
    
    private static final ThreadLocal<CartUpdateStatus> currentStatus = new ThreadLocal<>();

    private final HttpServletRequest request;
    private final CartUpdateStatus status;
    private boolean localCommitCalled = false;
    
    private CartUpdate(HttpServletRequest request, CartUpdateStatus status) {
        this.request = request;
        this.status = status;
    }

    /**
     * Returns a full cart update section object. Must be closed using {@link #close()}, preferably
     * using try-with-resources. Automatically locks.
     */
    public static CartUpdate updateSection(HttpServletRequest request) {
        CartUpdateStatus status = currentStatus.get();
        if (status == null) {
            // Top level
            status = new CartUpdateStatus(1, null, CartSync.getCartLockObject(request));
        }
        CartUpdate cartUpdate = new CartUpdate(request, status);
        cartUpdate.begin();
        return cartUpdate;
    }

    
    protected void begin() {
        if (isDebug()) {
            Debug.logInfo("Begin cart update section (depth: " + status.nestedLevel + ")" 
                    + getLogSuffix(), module);
        }
        if (isTopLevel()) {
            currentStatus.set(status);
            status.cartSync.begin();
        } else {
            status.nestedLevel++;
        }
    }

    protected void end() {
        if (isTopLevel()) {
            status.cartSync.end();
            currentStatus.remove();
        } else {
            status.nestedLevel--;
        }
        if (isDebug()) {
            if (localCommitCalled) {
                Debug.logInfo("End cart update section (depth: " + status.nestedLevel + ")" 
                        + getLogSuffix(), module);
            } else {
                Debug.logWarning("End cart update section (depth: " + status.nestedLevel 
                        + ") - commit never invoked" + getLogSuffix(), module);
            }
        }
    }

    @Override
    public void close() {
        end();
    }
    
    /**
     * Returns a modifiable cart instance derived from the current (request/session) cart, 
     * which may be committed if changes successful (using {@link #commit(ShoppingCart)}).
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
        if (!CART_COPIES_ENABLED) {
            return cart;
        }
        ShoppingCart newCart = cart.exactCopy();
        if (isDebug()) {
            try {
                newCart.ensureExactEquals(cart);
                Debug.logInfo("Cloned cart " + getLogCartDesc(cart) + " to " + getLogCartDesc(newCart)
                + " for update" + getLogSuffix(), module);
            } catch(IllegalStateException e) {
                Debug.logError(e, "Cloned cart " + getLogCartDesc(cart) + " to " + getLogCartDesc(newCart)
                    + " for update, but differences encountered - please report this issue" 
                    + getLogSuffixDetailed(), module);
            }
        }
        return newCart;
    }

    /**
     * Commits the cart changes (if top level).
     */
    public ShoppingCart commit(ShoppingCart cart) {
        if (isTopLevel()) {
            if (isDebug()) {
                Debug.logInfo("Committing modified shopping cart " + getLogCartDesc(cart)
                    + getLogSuffix(), module);
            }
            ShoppingCartEvents.replaceCurrentCartObject(request, cart, true);
            status.committed = true;
        } else {
            if (isDebug()) {
                Debug.logInfo("Delaying shopping cart commit (nested)" + getLogSuffix(), module);
            }
        }
        localCommitCalled = true;
        return cart;
    }

    /**
     * Returns true if this section level called {@link #commit}, regardless of whether
     * it was actually committed or not.
     */
    public boolean isLocalCommitCalled() {
        return localCommitCalled;
    }

    /**
     * Returns true if this OR a nested CartUpdate section committed and switched out the cart already.
     */
    public boolean isCommitted() {
        return status.committed;
    }

    public boolean isDebug() {
        return status.debug;
    }
 
    public int getNestedLevel() {
        return status.nestedLevel;
    }

    public boolean isTopLevel() {
        return (status.nestedLevel <= 1);
    }
    
    /**
     * Returns the CartSync synchronization object in use by this update section.
     * <p>
     * NOTE: There is no need to manually lock or synchronize on this anymore; 
     * locked automatically by {@link #updateSection(HttpServletRequest)} and unlocked
     * by {@link #close()}.
     */
    public CartSync getLockObject() {
        return status.cartSync;
    }

    private String getLogCartDesc(ShoppingCart cart) {
        return "@" + Integer.toHexString(System.identityHashCode(cart));
    }

    private String getLogSuffix() {
        return status.cartSync.getLogSuffix();
    }

    private String getLogSuffixDetailed() {
        return status.cartSync.getLogSuffixDetailed();
    }

    @SuppressWarnings("serial")
    private static class CartUpdateStatus implements Serializable {
        private int nestedLevel = 1;
        private ShoppingCart cartForUpdate;
        private final boolean debug = (ShoppingCart.DEBUG || Debug.verboseOn());
        private boolean committed = false;
        /**
         * The cart synchronization provider (emulates synchronized block).
         * <p>
         * NOTE: Even if there are nested CartUpdate instances/calls, we only need
         * one CartSync for the entire thing.
         */
        private final CartSync cartSync;

        CartUpdateStatus(int nestedLevel, ShoppingCart cartForUpdate, CartSync cartSync) {
            this.nestedLevel = nestedLevel;
            this.cartForUpdate = cartForUpdate;
            this.cartSync = cartSync;
        }
    }
}