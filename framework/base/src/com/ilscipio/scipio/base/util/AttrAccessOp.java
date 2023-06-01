package com.ilscipio.scipio.base.util;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

/**
 * How and whether to get or set variables such as session variables (common pattern).
 *
 * <p>This allows avoiding 6 different overload for a get method having default/initial value calculation support.
 * See {@link AttrHandler#getSessionLocale(HttpServletRequest, HttpSession, AttrAccessOp, AttrHandler.Resolver)}
 * for implementation example of common pattern.</p>
 *
 * <p>For {@link AttrHandler}, when left null, behavior is determined by the value of
 * {@link AttrHandler#DEFAULT_ATTR_ACCESS_OP_PROPERTY}, which should be either {@link AttrAccessOp#GET} or
 * {@link AttrAccessOp#GET_SET}.</p>
 *
 * <p>SCIPIO: 3.0.0: Added.</p>
 */
public enum AttrAccessOp {

    /**
     * Returns cached variable or a resolved value/initial/default, without setting or initializing the cached variable again at the end.
     */
    GET(true, true, false, false),

    /**
     * Returns cached variable, or a resolved value/initial/default that then updates the cached value (mutable pattern).
     */
    GET_SET(true, true, true, false),

    /**
     * Returns cached variable, or a resolved value/initial/default that then updates the cached value (mutable pattern)
     * even if the resolved value is null.
     *
     * <p>NOTE: Most of the time this is moot because the get operation will have already determined that the previous
     * value was null; however in cases with concurrency this can make a difference.</p>
     */
    GET_SET_CLEAR(true, true, true, true),

    /**
     * Returns only the cached/existing/stored variable, nothing else, otherwise returns nothing (null).
     */
    GET_CACHE(true, false, false, false),

    /**
     * Ignores the cached variable and returns a resolved value/initial/default, without storing it in cache.
     */
    RESOLVE(false, true, false, false),

    /**
     * Ignores the cached variable in order to resolve the value/default and reset the cached value, even if the
     * resolved value is null.
     */
    RESET(false, true, true, true),

    /**
     * Ignores the cached variable in order to resolve the value/default and reset the cached value but only if
     * the resolved value is non-null.
     */
    RESET_NOCLEAR(false, true, true, false),

    /**
     * Clears the cached variable and returns its previous value in cache (only - special case).
     */
    CLEAR(true, false, true, true);

    private final boolean doGetCache;
    private final boolean doResolve;
    private final boolean doSetCache;
    private final boolean doClearCache;

    AttrAccessOp(boolean doGetCache, boolean doResolve, boolean doSetCache, boolean doClearCache) {
        this.doGetCache = doGetCache;
        this.doResolve = doResolve;
        this.doSetCache = doSetCache;
        this.doClearCache = doClearCache;
    }

    public boolean doGetCache() {
        return doGetCache;
    }

    public boolean doResolve() {
        return doResolve;
    }

    public boolean doSetCache() {
        return doSetCache;
    }

    public boolean doClearCache() {
        return doClearCache;
    }

    public boolean doClearCacheOnly() {
        return (this == CLEAR);
    }

}
