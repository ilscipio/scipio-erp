/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package org.ofbiz.base.util.collections;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.ObjectType;
import org.ofbiz.base.util.UtilIO;

import java.util.Collection;
import java.util.Map;

/**
 * SCIPIO: A {@link MapStack} with render-only features.
 * NOTE: Unlike MapStack, the key is always String, so this is simpler to use.
 * TODO: REVIEW: due to java compiler generics error, the "create" methods are renamed to "createRenderContext", sadly.
 * <p>
 * Features (non-comprehensive):
 * <ul>
 * <li>{@link org.ofbiz.entity.util.EntityListIterator} instances found in current level map values on {@link #pop()} are automatically closed.
 *     This can be bypassed by adding field names to the context field ArrayList, "scpCtxAutoCloseExcl" ({@link #AUTO_CLOSE_EXCL_FIELD}).</li>
 * </ul>
 */
public class RenderMapStack extends MapStack<String> {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final String AUTO_CLOSE_EXCL_FIELD = "scpCtxAutoCloseExcl"; // SCIPIO
    @SuppressWarnings("unchecked")
    private static final Class<? extends AutoCloseable> ELI_CLS = (Class<? extends AutoCloseable>) ObjectType.loadClassOrRuntimeEx("org.ofbiz.entity.util.EntityListIterator"); // SCIPIO
    @SuppressWarnings("unchecked")
    private static final Class<? extends AutoCloseable> ENTITY_CLS = (Class<? extends AutoCloseable>) ObjectType.loadClassOrRuntimeEx("org.ofbiz.entity.GenericEntity"); // SCIPIO
    private static final int LOG_DEBUG = Debug.INFO; // SCIPIO: TODO: SWITCH TO VERBOSE

    public static RenderMapStack createRenderContext() {
        RenderMapStack newValue = new RenderMapStack();
        // initialize with a single entry
        newValue.push();
        return newValue;
    }

    public static RenderMapStack createEmptyRenderContext() {
        return new RenderMapStack();
    }

    // NOTE: due to java compiler generics complaining, forced to rename this method... all were renamed due to this.
    //public static RenderMapStack create(Map<String, Object> baseMap) {
    public static RenderMapStack createRenderContext(Map<String, Object> baseMap) {
        RenderMapStack newValue;
        if (baseMap instanceof RenderMapStack) {
            newValue = new RenderMapStack((RenderMapStack) baseMap);
        } else {
            newValue = new RenderMapStack(baseMap);
        }
        return newValue;
    }

    /** Does a shallow copy of the internal stack of the passed MapStack; enables simultaneous stacks that share common parent Maps */
    public static RenderMapStack createRenderContext(RenderMapStack source) {
        return new RenderMapStack(source);
    }

    /** SCIPIO: If the passed baseMap is already a RenderMapStack, returns as-is; otherwise creates a new RenderMapStack **/
    public static RenderMapStack ensureRenderContext(Map<String, Object> baseMap) {
        if (baseMap instanceof RenderMapStack) { return (RenderMapStack) baseMap; }
        else if (baseMap == null) { return createRenderContext(); }
        else {
            if (baseMap instanceof MapStack) {
                Debug.logWarning("ensureRenderContext: Render context was a MapStack, but not a RenderMapStack" +
                        "; recreating as RenderMapStack; please report this issue or fix the calling code to use RenderMapStack", module);
            }
            return new RenderMapStack(baseMap);
        }
    }

    protected RenderMapStack() {
        super();
    }

    /**
     * SCIPIO: Shallow copy constructor.
     */
    protected RenderMapStack(RenderMapStack source) {
        super(source);
    }

    /**
     * SCIPIO: Initial map constructor.
     */
    protected RenderMapStack(Map<String, Object> baseMap) {
        super(baseMap);
    }

    /**
     * Creates a MapStack object that has the same Map objects on its stack;
     * meant to be used to enable a
     * situation where a parent and child context are operating simultaneously
     * using two different MapStack objects, but sharing the Maps in common
     */
    @Override
    public RenderMapStack standAloneStack() {
        RenderMapStack standAlone = RenderMapStack.createRenderContext(this);
        return standAlone;
    }

    /**
     * Creates a MapStack object that has the same Map objects on its stack,
     * but with a new Map pushed on the top; meant to be used to enable a
     * situation where a parent and child context are operating simultaneously
     * using two different MapStack objects, but sharing the Maps in common
     */
    @Override
    public RenderMapStack standAloneChildStack() {
        RenderMapStack standAloneChild = RenderMapStack.createRenderContext(this);
        standAloneChild.push();
        return standAloneChild;
    }

    /** Remove and returns the Map from the top of the stack; if there is only one Map on the stack it returns null and does not remove it.
     * SCIPIO: This method may perform additional event handler calls and cleanup commands (for render context, closes all EntityListIterators found in context values). */
    @Override
    public Map<String, Object> pop() {
        @SuppressWarnings("unchecked")
        int numClosed = UtilIO.closeValuesSafe(getCurrentMap(), ELI_CLS, getAutoCloseExclFieldList());
        if (Debug.isOn(LOG_DEBUG)) {   // SCIPIO: FIXME: change to verbose later
            if (numClosed > 0) {
                Debug.log(LOG_DEBUG, null, "Render context: pop: auto-closed " + numClosed + " EntityListIterators", module);
            }
        }
        return super.pop();
    }

    @SuppressWarnings("unchecked")
    protected Collection<String> getAutoCloseExclFieldList() {
        if (getCurrentMap() == null || ENTITY_CLS.isAssignableFrom(getCurrentMap().getClass())) {
            return null;
        }
        return (Collection<String>) getCurrentMap().get(AUTO_CLOSE_EXCL_FIELD);
    }
}
