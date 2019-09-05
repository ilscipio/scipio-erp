package org.ofbiz.order.shoppingcart;

import java.io.Serializable;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.webapp.control.RequestVarScopes;

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
 * NOTE: When the section begins, as opposed to {@link ShoppingCartEvents#getCartObject(HttpServletRequest)}, this
 * tries to find the cart in the session first and request second; otherwise cart updates from other threads could be
 * lost. This means ideally you only want one major cart update section per cart-modifying request, because only
 * pure reads exploit the local request-cache shoppingCart instances, not updates.
 * <p>
 * NOTE: If you need to synchronize on the cart lock without actually updating the cart itself,
 * use {@link CartSync#synchronizedSection(HttpServletRequest)} instead.
 * <p>
 * NOTE: 2018-11-30: This now temporarily and on-success replaces the request attribute "shoppingCart" in case something
 * tries to access it using {@link ShoppingCartEvents#getCartObject(HttpServletRequest)}.
 * <p>
 * NOTE: Unlike {@link ShoppingCartEvents#getCartObject(HttpServletRequest)}, {@link #updateSection} reads
 * the cart to copy from the session first instead of the request attributes; so every new (top-level) update section
 * is effectively a volatile read from the session. This means there should be as few as possible
 * update sections in a request and they should be in controller events (not in screens).
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
    private final RequestVarScopes modifyScopesFilter;
    private final boolean createCartIfMissing;
    //private ShoppingCart localCommittedCart;
    private boolean committed;

    private ShoppingCart prevRequestCart;
    private CartUpdate parentUpdate = null;

    protected CartUpdate(HttpServletRequest request, CartUpdateStatus status, RequestVarScopes modifyScopesFilter,
            boolean createCartIfMissing) {
        this.request = request;
        this.status = status;
        this.createCartIfMissing = createCartIfMissing;
        this.modifyScopesFilter = (modifyScopesFilter != null) ? modifyScopesFilter : RequestVarScopes.ALL;
    }

    /**
     * Returns a full cart update section object, creating the cart if it does not exist.
     * Must be closed using {@link #close()}, preferably using try-with-resources. Automatically locks.
     */
    public static CartUpdate updateSection(HttpServletRequest request) {
        return updateSection(request, true, null, null, true);
    }

    /**
     * Returns a full cart update section object, with option to create cart if it does not exist.
     * Must be closed using {@link #close()}, preferably using try-with-resources. Automatically locks if useSyncSection true.
     */
    public static CartUpdate updateSection(HttpServletRequest request, boolean createCartIfMissing) {
        return updateSection(request, createCartIfMissing, null, null, true);
    }

    /**
     * Returns a full cart update section object, with option to create cart if it does not exist.
     * Must be closed using {@link #close()}, preferably using try-with-resources. Automatically locks if useSyncSection true.
     */
    public static CartUpdate updateSection(HttpServletRequest request, boolean createCartIfMissing, RequestVarScopes modifyScopesFilter) {
        return updateSection(request, createCartIfMissing, null, modifyScopesFilter, true);
    }

    /**
     * Returns a full cart update section object, with explicit source cart (assumed non-null).
     * Must be closed using {@link #close()}, preferably using try-with-resources. Automatically locks if useSyncSection true.
     */
    public static CartUpdate updateSection(HttpServletRequest request, ShoppingCart sourceCart) {
        return updateSection(request, false, sourceCart, null, true);
    }

    /**
     * Returns a full cart update section object, with explicit source cart (assumed non-null).
     * Must be closed using {@link #close()}, preferably using try-with-resources. Automatically locks if useSyncSection true.
     */
    public static CartUpdate updateSection(HttpServletRequest request, ShoppingCart sourceCart, RequestVarScopes modifyScopesFilter) {
        return updateSection(request, false, sourceCart, modifyScopesFilter, true);
    }

    /**
     * Returns a full cart update section object, with option to create cart if it does not exist, and
     * optionally omitting the synchronization (advance usage).
     * Must be closed using {@link #close()}, preferably using try-with-resources. Automatically locks if useSyncSection true.
     */
    protected static CartUpdate updateSection(HttpServletRequest request, boolean createCartIfMissing,
            ShoppingCart sourceCart, RequestVarScopes modifyScopesFilter, boolean useSyncSection) {
        CartUpdate cartUpdate = createInstance(request, createCartIfMissing, sourceCart, modifyScopesFilter, useSyncSection);
        cartUpdate.begin();
        return cartUpdate;
    }

    protected static CartUpdate createInstance(HttpServletRequest request, boolean createCartIfMissing,
            ShoppingCart sourceCart, RequestVarScopes modifyScopesFilter, boolean useSyncSection) {
        return new CartUpdate(request, getOrCreateCartUpdateStatus(request, sourceCart, useSyncSection),
                modifyScopesFilter, useSyncSection);
    }

    protected static CartUpdateStatus getOrCreateCartUpdateStatus(HttpServletRequest request, ShoppingCart sourceCart, boolean useSyncSection) {
        CartUpdateStatus status = currentStatus.get();
        if (status == null) {
            // Top level
            status = new CartUpdateStatus(0, useSyncSection ? CartSync.getCartLockObject(request) : CartSync.DUMMY, sourceCart,
                    ShoppingCartEvents.isCartChanged(request));
        }
        return status;
    }

    /**
     * Returns the current update section, or null if none is open.
     * NOTE: This should rarely if ever be needed by client code.
     */
    public static CartUpdate getCurrentUpdate(HttpServletRequest request) {
        CartUpdateStatus status = currentStatus.get();
        return (status != null) ? status.currentUpdate : null;
    }

    protected void begin() {
        status.nestedLevel++;
        if (isDebug()) {
            Debug.logInfo("Begin cart update section (depth: " + status.nestedLevel + ")"
                    + getLogSuffix(), module);
        }

        // NOTE: Does not distinguish between key presence and null value - see end(); see no reason to fix
        prevRequestCart = (ShoppingCart) request.getAttribute("shoppingCart");

        if (isTopLevel()) {
            currentStatus.set(status);
            // LOCK
            status.cartSync.begin(); // synchronized begin
        }

        this.parentUpdate = status.currentUpdate;
        status.currentUpdate = this;

        checkCartCopy();
    }

    /**
     * Checks if made a cart copy since the start of the top level, and if not, checks if our section required one,
     * and if so, copies and records it.
     */
    protected void checkCartCopy() {
        if (!status.cartCopied) { // Explicit flag more reliable: isTopLevel() || (createCartIfMissing && (status.currentCart == null))
            // Install the cart at beginning of session.
            // We must do it here (at least once here) because some events may try
            // to use ShoppingCartEvents.getCartObject instead of CartUpdate.getCartForUpdate().

            // IMPORTANT: READ CART FROM SESSION ATTRIBUTES FIRST, NOT REQUEST ATTRIBUTES!
            // Otherwise, there is a risk of lost updates due to request attribute carts crushing session ones from other threads.
            final boolean checkRequestFirst = false;

            ShoppingCart cart = status.sourceCart; // May be set manually by caller
            if (cart == null) {
                //if (createCartIfMissing) {
                // IMPORTANT: DO NOT MODIFY SESSION (yet; only in end())! Cannot allow session attr change until committed.
                // Prevent session modification using scope filter.
                // This will also prevent the setCartObject call done by getCartObject from calling our commit(ShoppingCart).
                final RequestVarScopes modifyScopesFilter = this.modifyScopesFilter.request() ?
                        RequestVarScopes.REQUEST : RequestVarScopes.NONE;

                cart = ShoppingCartEvents.getCartObject(request, createCartIfMissing, checkRequestFirst, modifyScopesFilter);
                //} else {
                //    cart = ShoppingCartEvents.getCartObjectIfExists(request, checkRequestFirst);
                //}
            }
            if (cart != null) {
                setCurrentCart(recordAndCopyCartForUpdate(cart));
                // Record the original copy so we can detect error cases
                status.cartCopy = status.currentCart;
                status.cartCopied = true;
            }
        }
    }

    protected void setCurrentCart(ShoppingCart cart) {
        status.currentCart = cart;
        if (modifyScopesFilter.request()) {
            // Set request attribute for client code that calls ShoppingCartEvents.getCartObject
            request.setAttribute("shoppingCart", cart);
        }
        // IMPORTANT: We must NOT update the session shoppingCart here! Done in end()
    }

    protected void end() {
        if (isTopLevel()) {
            // UNLOCK
            try {
                status.cartSync.end(); // synchronized end
            } finally { // finally prevents ThreadLocal leak in case something goes wrong in unlock
                currentStatus.remove();
            }
        }
        status.currentUpdate = this.parentUpdate; // MAKE SURE do this before the setCartObject call
        if (committed) {
            // COMMIT
            if (isTopLevel()) {
                // Set cart in session and request attributes (if allowed by modifyScopesFilter)
                ShoppingCartEvents.setCartObject(request, status.currentCart, modifyScopesFilter);

                /* 2018-11-30: Do not do this because in 99% of cases we want the code following the cart update
                 * to use our new cart via request attribute and explicitly AVOID re-querying the volatile session attributes
                // If there was no request attribute going into the section, don't leave one going out
                if (prevRequestCart == null) {
                    request.removeAttribute("shoppingCart");
                }
                */
            } else {
                ; // Nothing to do - already set by #commit(ShoppingCart) - simply continue in parent level
            }
            if (isDebug()) {
                if (status.cartCopy == status.currentCart) {
                    Debug.logInfo("End cart update section (depth: " + status.nestedLevel + ") - committed "
                            + (isTopLevel() ? "to session" : "locally") + getLogSuffix(), module);
                } else {
                    Debug.logInfo("End cart update section (depth: " + status.nestedLevel + ") - foreign cart committed "
                            + (isTopLevel() ? "to session" : "locally") + getLogSuffix(), module);
                }
            }
        } else { // (assuming we made a cart)
            // ROLLBACK - Restore old request attribute cart and leave session alone
            RequestVarScopes.REQUEST.setOrRemoveValue(request, modifyScopesFilter, "shoppingCart", prevRequestCart);
            if (isDebug()) {
                Debug.logWarning("End cart update section (depth: " + status.nestedLevel  + ") - not committed "
                    + (isTopLevel() ? "to session" : "locally") + getLogSuffix(), module);
            } else if (Debug.infoOn()) {
                // NOTE: This is now INFO because this already happens in some known non-problem cases
                Debug.logInfo("End cart update section (depth: " + status.nestedLevel  + ") - not committed "
                        + (isTopLevel() ? "to session" : "locally") + getLogSuffix(), module);
            }
        }
        if (!isTopLevel()) {
            status.nestedLevel--;
        }
    }

    @Override
    public void close() {
        end();
    }

    private ShoppingCart recordAndCopyCartForUpdate(ShoppingCart cart) {
        status.sourceCart = cart;
        return copyCartForUpdate(cart);
    }

    public ShoppingCart copyCartForUpdate(ShoppingCart cart) {
        if (!CART_COPIES_ENABLED) {
            return cart;
        }
        ShoppingCart newCart = cart.copy(true);
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
     * Commits the given cart for this CartUpdate section.
     * <p>
     * In a nested section, it is possible for a higher-level section to "rollback" this commit if it does not commit itself.
     * <p>
     * It is allowed to call commit several times per instance; will simply replace the last commit.
     * <p>
     * 2018-11-30: Null is ignored and will NOT remove the current cart (would not be safe because could trigger fallbacks
     * on the session attribute if {@link ShoppingCartEvents#getCartObject} were then called).
     * <p>
     * NOTE: The session attribute is only updated upon {@link #close()} of the top level section; if an exception occurs between
     * the commit and close calls, the cart may not be committed to session. This behavior ensures consistent
     * behavior regardless of whether this is top level or nested. The easiest way to not be affected by this is simply
     * to put the commit call near the end of the update block.
     */
    public ShoppingCart commit(ShoppingCart cart) {
        if (cart == null) {
            // TODO: REVIEW: Cannot use null to "unset" the current cart because null request attribute
            // is treated as a fallback on session attribute, such that calls within cart sync section
            // that fetch current cart using ShoppingCartEvents.getCartObject (instead of getCartForUpdate)
            // would end up getting the wrong cart.
            // DEV NOTE: If find workaround, do not modify this method; instead create explicit commitRemove().
            if (isDebug()) {
                Debug.logInfo("Commit called for cart update section with null cart - ignoring" + getLogSuffix(), module);
            }
            return cart;
        }
        if (isDebug()) {
            if (cart == status.cartCopy) {
                Debug.logInfo("Commit called for cart update section " + getLogCartDesc(cart) + getLogSuffix(), module);
            } else {
                Debug.logInfo("Commit called for cart update section with foreign cart " + getLogCartDesc(cart) + getLogSuffix(), module);
            }
        }
        setCurrentCart(cart);
        committed = true;
        // IMPORTANT: We must NOT update the session shoppingCart here! Done in end()
        return cart;
    }

    /**
     * Returns a cart instance to be updated, or in other words the current cart being updated.
     * <p>
     * Unless client code overrode this using {@link #commit(ShoppingCart)}, this will be a
     * modifiable cart instance derived from the main request/session "shoppingCart" object,
     * which may (should, usually) be committed if changes successful (using {@link #commit(ShoppingCart)}).
     * <p>
     * This may be null if an explicit commit with null was already made (including in a successful nesting section),
     * OR if <code>createCartIfMissing</code> was false on construction and no cart is found in request or session attributes.
     */
    public ShoppingCart getCartForUpdate() {
        return status.currentCart;
    }

    /**
     * Return the source/original cart used to make the modifiable cart copy.
     */
    public ShoppingCart getSourceCart() {
        return status.sourceCart;
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
     * Returns the parent update section, or null if top-level.
     */
    public CartUpdate getParentUpdate() {
        return parentUpdate;
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

    String getLogSuffix() {
        return status.cartSync.getLogSuffix();
    }

    String getLogSuffixDetailed() {
        return status.cartSync.getLogSuffixDetailed();
    }

    private static String getLogCartDesc(ShoppingCart cart) {
        if (cart == null) {
            return "@(none)";
        }
        return "@" + Integer.toHexString(System.identityHashCode(cart));
    }

    @SuppressWarnings("serial")
    private static class CartUpdateStatus implements Serializable {
        private int nestedLevel;
        private CartUpdate currentUpdate = null;
        private final CartSync cartSync;
        private boolean cartCopied = false; // Helps handle (cartCopy==null) case
        // NOTE: Most of the time cartCopy will equal currentCart, but we support client code specifying
        // a different cart (via #commit(ShoppingCart) in case it needs to build a new one instead.
        private ShoppingCart cartCopy;
        private ShoppingCart currentCart;
        private ShoppingCart sourceCart;
        // TODO: REVIEW: there's little to record this because we only set it at end(), while client code
        // should not be modifying the request flag...
        //private final boolean cartWasChanged;
        /**
         * The cart synchronization provider (emulates synchronized block).
         * <p>
         * NOTE: Even if there are nested CartUpdate instances/calls, we only need
         * one CartSync for the entire thing.
         */
        private final boolean debug = ShoppingCart.verboseOn();

        CartUpdateStatus(int nestedLevel, CartSync cartSync, ShoppingCart sourceCart, boolean cartWasChanged) {
            this.nestedLevel = nestedLevel;
            this.cartSync = cartSync;
            this.sourceCart = sourceCart;
            //this.cartWasChanged = cartWasChanged;
        }
    }
}