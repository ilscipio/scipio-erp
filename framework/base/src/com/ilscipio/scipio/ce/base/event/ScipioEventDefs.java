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
package com.ilscipio.scipio.ce.base.event;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;

import com.ilscipio.scipio.ce.base.util.AdvancedPropertyUtil;
import com.ilscipio.scipio.ce.base.util.AdvancedPropertyUtil.ClassDef;
import com.ilscipio.scipio.ce.base.util.AdvancedPropertyUtil.ClassDefFactory;

public class ScipioEventDefs<H> {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // DEV NOTE: DO NOT remove the .properties suffix from either of these; will break the scanning!
    public static final String EVENTS_PROPRES = "scipio-events.properties";
    public static final String EVENTS_PROPRES_STATIC = "org/ofbiz/base/start/scipio-events.properties";

    protected final List<EventDef<H>> eventDefs;

    protected ScipioEventDefs(List<EventDef<H>> eventDefs) {
        this.eventDefs = eventDefs;
    }

    public static <H> ScipioEventDefs<H> readEventDefs(String propPrefix, Class<H> handlerIf) {
        ClassLoader classLoader = AdvancedPropertyUtil.getComponentClassLoader();

        // static properties (compiled with Start.java)
        Map<String, Map<String, String>> staticMapDefs = new HashMap<>();
        AdvancedPropertyUtil.readClassDefMaps(staticMapDefs,
                AdvancedPropertyUtil.getResourcePropsSafe(ScipioEventDefs.EVENTS_PROPRES_STATIC, AdvancedPropertyUtil.class.getClassLoader()),
                propPrefix);

        // component properties
        List<EventDef<H>> allDefs = AdvancedPropertyUtil.readClassDefsFromAllComponents(staticMapDefs, ScipioEventDefs.EVENTS_PROPRES,
                propPrefix, null, classLoader, new ClassDefFactory<EventDef<H>, H>() {
            @Override
            public EventDef<H> newInstance(Map<String, String> props, ClassLoader classLoader) {
                try {
                    return new EventDef<>(props, classLoader, handlerIf);
                } catch(Exception e) {
                    Debug.logError("Scipio: Error initializing " + ScipioEventDefs.EVENTS_PROPRES
                            + " " + propPrefix
                            + " handler '" + props + "': " + e.getMessage(), module);
                    return null;
                }
            }
        });

        StringBuilder sb = new StringBuilder("Scipio: Registered event handlers for " + EVENTS_PROPRES + " " + propPrefix + ":");

        if (allDefs.isEmpty()) {
            sb.append("(none)");
        } else {
            for(EventDef<H> eventDef : allDefs) {
                sb.append("\n");
                sb.append(eventDef);
            }
        }
        Debug.logInfo(sb.toString(), module);

        return new ScipioEventDefs<H>(allDefs);
    }

    public List<EventDef<H>> getLoaderDefs() {
        return eventDefs;
    }

    public static class EventDef<H> extends ClassDef<H> {
        public EventDef(String name, Integer priority, H instance, Map<String, String> props) {
            super(name, priority, instance, props);
        }

        public EventDef(Map<String, String> props, ClassLoader classLoader, Class<H> handlerIf) throws IllegalArgumentException {
            super(props, classLoader, handlerIf);
        }

        public H getLoader() {
            return getInstance();
        }
    }
}
