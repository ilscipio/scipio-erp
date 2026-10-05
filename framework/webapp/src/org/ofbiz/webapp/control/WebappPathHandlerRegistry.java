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
package org.ofbiz.webapp.control;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;

import com.ilscipio.scipio.ce.base.component.ComponentReflectInfo;
import com.ilscipio.scipio.ce.base.component.ComponentReflectRegistry;

/**
 * SCIPIO: 4.0.0: Lazily discovers {@link WebappPathHandler} implementations annotated with {@link WebappPathHandlerDef}
 * across all loaded components. The registry is built on first request; component reflect info is complete by then.
 */
public final class WebappPathHandlerRegistry {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static volatile Map<String, WebappPathHandler> handlers;

    private WebappPathHandlerRegistry() {}

    public static Map<String, WebappPathHandler> getHandlers() {
        Map<String, WebappPathHandler> h = handlers;
        if (h == null) {
            synchronized (WebappPathHandlerRegistry.class) {
                h = handlers;
                if (h == null) {
                    h = load();
                    handlers = h;
                }
            }
        }
        return h;
    }

    /** Returns the handler for a first path segment (without slashes), or null. */
    public static WebappPathHandler findHandler(String pathSegment) {
        if (pathSegment == null || pathSegment.isEmpty()) {
            return null;
        }
        return getHandlers().get(pathSegment);
    }

    /** Forces a rebuild on the next request (for tests and hot reload). */
    public static void reset() {
        handlers = null;
    }

    private static Map<String, WebappPathHandler> load() {
        List<WebappPathHandler> list = new ArrayList<>();
        Map<WebappPathHandler, Integer> priorities = new LinkedHashMap<>();
        for (ComponentReflectInfo cri : ComponentReflectRegistry.getReflectInfos()) {
            for (Class<?> cls : cri.getReflectQuery().getAnnotatedClasses(WebappPathHandlerDef.class)) {
                if (!WebappPathHandler.class.isAssignableFrom(cls)) {
                    Debug.logWarning("WebappPathHandlerDef class " + cls.getName() + " does not implement WebappPathHandler; ignored", module);
                    continue;
                }
                try {
                    WebappPathHandler handler = (WebappPathHandler) cls.getDeclaredConstructor().newInstance();
                    list.add(handler);
                    priorities.put(handler, cls.getAnnotation(WebappPathHandlerDef.class).priority());
                } catch (ReflectiveOperationException e) {
                    Debug.logError(e, "Could not instantiate WebappPathHandler " + cls.getName(), module);
                }
            }
        }
        list.sort(Comparator.comparingInt(priorities::get));
        Map<String, WebappPathHandler> map = new LinkedHashMap<>();
        for (WebappPathHandler handler : list) {
            String seg = handler.getPathSegment();
            if (seg == null || seg.isEmpty() || seg.contains("/")) {
                Debug.logWarning("WebappPathHandler " + handler.getClass().getName() + " has invalid path segment [" + seg + "]; ignored", module);
                continue;
            }
            if (map.putIfAbsent(seg, handler) == null) {
                Debug.logInfo("Registered webapp path handler /" + seg + " -> " + handler.getClass().getName(), module);
            }
        }
        return Collections.unmodifiableMap(map);
    }
}
