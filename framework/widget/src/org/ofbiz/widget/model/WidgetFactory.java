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

    public static final String module = WidgetFactory.class.getName();
    
    protected static final Map<String, WidgetFactory> factoryMap;
    static {
        Map<String, WidgetFactory> map = new HashMap<>();
        map.put("screen", new ScreenFactory());
        map.put("form", new FormFactory());
        map.put("menu", new MenuFactory());
        map.put("tree", new TreeFactory());
        map.put("grid", new GridFactory());
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
