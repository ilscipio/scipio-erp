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
package org.ofbiz.base;

import org.ofbiz.base.util.Debug;

import java.lang.reflect.Method;

/**
 * Helper for applications to query system state without start package (SCIPIO).
 */
public class SystemState {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final SystemState INSTANCE = new SystemState();
    private static final Class<?> START_CLS;
    private static final Method isShutdownAfterLoadMethod;
    static {
        try {
            START_CLS = Class.forName("org.ofbiz.base.start.StartSystemState");
            isShutdownAfterLoadMethod = START_CLS.getMethod("isShutdownAfterLoad");
        } catch (Exception e) {
            Debug.logError(e, "Invalid definition for org.ofbiz.base.start.StartSystemState, please ant clean build", module);
            throw new IllegalStateException("Invalid definition for org.ofbiz.base.start.StartSystemState, please ant clean build", e);
        }
    }

    protected SystemState() {
    }

    public static SystemState getInstance() {
        return INSTANCE;
    }

    public boolean isShutdownAfterLoad() {
        try {
            return (boolean) isShutdownAfterLoadMethod.invoke(null);
        } catch (Exception e) {
            Debug.logError(e, "Unexpected error: " + e.toString(), module);
            throw new IllegalStateException("Unexpected error: " + e.toString(), e);
        }
    }

    /**
     * Returns true if this run is determined to be a normal server execution, i.e. not a data load.
     * TODO: REVIEW: For now this uses shutdown-after-load approach to generalize this for code.
     */
    public boolean isServerExecution() {
        return !isShutdownAfterLoad();
    }

}
