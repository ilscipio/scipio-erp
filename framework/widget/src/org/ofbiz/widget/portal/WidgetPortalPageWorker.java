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
package org.ofbiz.widget.portal;

import org.ofbiz.base.util.Debug;

import java.lang.reflect.InvocationTargetException;

/**
 * PortalPageWorker Class
 * <p>
 * SCIPIO: NOTE: This class appears to do absolutely nothing whatsoever...?
 */
public final class WidgetPortalPageWorker {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    @SuppressWarnings("unused")
    private static PortalPageWorkerInterface portalPageWorker = null;

    private WidgetPortalPageWorker() {}

    static {
        try {
            ClassLoader loader = Thread.currentThread().getContextClassLoader();
            // note: loadClass is necessary for these since this class doesn't know anything about them at compile time
            portalPageWorker = (PortalPageWorkerInterface) loader.loadClass("org.ofbiz.widget.portal.PortalPageWorker").getConstructor().newInstance();
        } catch (ClassNotFoundException | IllegalAccessException | InstantiationException | NoSuchMethodException | InvocationTargetException e) {
            Debug.logError(e, "Could not pre-initialize dynamically loaded class: ", module);
        }
    }
}
