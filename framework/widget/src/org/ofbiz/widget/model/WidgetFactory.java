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
package org.ofbiz.widget.model;

import java.io.IOException;
import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContainer;

/**
 * SCIPIO: Generic widget factory and utils AND base class.
 */
@SuppressWarnings("serial")
public abstract class WidgetFactory implements Serializable {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // SCIPIO: new: static factories
    protected static final ScreenFactory screenFactory = new ScreenFactory();
    protected static final FormFactory formFactory = new FormFactory();
    protected static final MenuFactory menuFactory = new MenuFactory();
    protected static final TreeFactory treeFactory = new TreeFactory();
    protected static final GridFactory gridFactory = new GridFactory();
    protected static final Map<String, WidgetFactory> factoryMap;
    static {
        Map<String, WidgetFactory> map = new HashMap<>();
        map.put("screen", screenFactory);
        map.put("form", formFactory);
        map.put("menu", menuFactory);
        map.put("tree", treeFactory);
        map.put("grid", gridFactory);
        factoryMap = map;
    }

    public static final Set<String> widgetTypes = Collections.unmodifiableSet(new HashSet<>(factoryMap.keySet()));

    @SuppressWarnings("unchecked")
    public static <T extends WidgetFactory> T getFactory(String type) throws IllegalArgumentException {
        WidgetFactory factory = factoryMap.get(type);
        if (factory == null) {
            throw new IllegalArgumentException("Unrecognized widget (factory) type: " + type);
        }
        return (T) factory;
    }

    public abstract ModelWidget getWidgetFromLocation(ModelLocation modelLoc) throws IOException, IllegalArgumentException;

    public abstract ModelWidget getWidgetFromLocationOrNull(ModelLocation modelLoc) throws IOException;

    @SuppressWarnings("unchecked")
    public static <T extends ModelWidget> T getWidgetFromLocation(String type, ModelLocation modelLoc) throws IOException {
        return (T) getFactory(type).getWidgetFromLocation(modelLoc);
    }

    public boolean isWidgetDefinedAtLocation(ModelLocation modelLoc) {
        try {
            ModelWidget widget = getWidgetFromLocationOrNull(modelLoc);
            return widget != null;
        } catch(Exception e) {
            return false;
        }
    }

    /**
     * Gets a default dispatch context.
     * <p>
     * NOTE: this is bad form, but generally shouldn't have been required for XML building
     * as MenuFactory and ScreenFactory stock code did not use one in getMenu/ScreenFromLocation.
     * So it shouldn't matter, exceptionally.
     */
    public static DispatchContext getDefaultDispatchContext() {
        Delegator delegator = DelegatorFactory.getDelegator("default");
        LocalDispatcher dispatcher = ServiceContainer.getLocalDispatcher(delegator.getDelegatorName(), delegator);
        return dispatcher.getDispatchContext();
    }
}
