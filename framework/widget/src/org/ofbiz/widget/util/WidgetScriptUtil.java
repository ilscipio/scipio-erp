package org.ofbiz.widget.util;

import java.util.Map;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.widget.model.AbstractModelAction;
import org.ofbiz.widget.model.FormFactory;
import org.ofbiz.widget.model.MenuFactory;
import org.ofbiz.widget.model.ModelForm;
import org.ofbiz.widget.model.ModelLocation;
import org.ofbiz.widget.model.ModelMenu;
import org.ofbiz.widget.model.ModelScreen;
import org.ofbiz.widget.model.ScreenFactory;

/**
 * SCIPIO: Utility dedicated to providing helpers to invoke scripts and actions defined through screens.
 * <p>
 * Similar to {@link org.ofbiz.base.util.GroovyUtil} and {@link org.ofbiz.base.util.ScriptUtil}.
 * <p>
 * May be used from *.groovy files to invoke screen actions as scripts (see {@link #runScreenActionsAtLocation}).
 */
public abstract class WidgetScriptUtil {

    public static final String module = WidgetScriptUtil.class.getName();
    
    protected WidgetScriptUtil() {
    }

    /**
     * Runs the top-level screen actions defined by the given screen, on the given context,
     * effectively treating them as a script.
     * <p>
     * Usually this should be used on actions-only screens (new construct in Scipio),
     * but it is also possible to use on other screens.
     * However, as convention it is best not to rely on the actions of regular screens
     * because they can be arbitrarily refactored in ways such that they are no longer included
     * by this method.
     * <p>
     * WARN: this does not run actions outside of the top-level actions block and the only
     * actions transitively included are those included using <code>include-screen-actions</code>
     * from the top-level actions block. In other words, only works reliably on action-only screens or
     * screens that have same behavior as actions-only screens.
     * See <code>widget-screen.xsd</code> for more details on actions-only screens.
     * <p>
     * NOTE: 
     */
    public static Object runScreenActionsAtLocation(String location, String screenName, Map<String, Object> context) throws GeneralException, RuntimeException {
        ModelScreen widget;
        try {
            widget = ScreenFactory.getScreenFromLocation(location, screenName);
        } catch (Exception e) {
            throw new GeneralException("Error getting screen location to run screen actions: " + e.getMessage(), e);
        }
        AbstractModelAction.runSubActionsEx(widget.getSection().getActions(), context); // NOTE: wraps in RunTimeExceptions
        return null; // TODO? ever needed? made part of interface for future needs.
    }
    
    /**
     * Runs the top-level screen actions defined by the given screen, on the given context,
     * effectively treating them as a script.
     * 
     * @see #runScreenActionsAtLocation(String, String, Map)
     */
    public static Object runScreenActionsAtLocation(ModelLocation location, Map<String, Object> context) throws GeneralException {
        return runScreenActionsAtLocation(location.getResource(), location.getName(), context);
    }
    
    /**
     * Runs the top-level screen actions defined by the given screen, on the given context,
     * effectively treating them as a script.
     * 
     * @see #runScreenActionsAtLocation(String, String, Map)
     */
    public static Object runScreenActionsAtLocation(String combinedName, Map<String, Object> context) throws GeneralException {
        return runScreenActionsAtLocation(ModelLocation.fromAddress(combinedName), context);
    }
    
    public static Object runFormActionsAtLocation(String location, String screenName, Map<String, Object> context) throws GeneralException {
        return runFormActionsAtLocation(ModelLocation.fromResAndName(location, screenName), context);
    }
    
    public static Object runFormActionsAtLocation(ModelLocation location, Map<String, Object> context) throws GeneralException {
        ModelForm widget;
        try {
            widget = FormFactory.getFormFactory().getWidgetFromLocation(location);
        } catch (Exception e) {
            throw new GeneralException("Error getting screen location to run screen actions: " + e.getMessage(), e);
        }
        AbstractModelAction.runSubActionsEx(widget.getActions(), context); // NOTE: wraps in RunTimeExceptions
        return null; // TODO? ever needed? made part of interface for future needs.
    }
    
    public static Object runFormActionsAtLocation(String combinedName, Map<String, Object> context) throws GeneralException {
        return runFormActionsAtLocation(ModelLocation.fromAddress(combinedName), context);
    }
    
    public static Object runMenuActionsAtLocation(String location, String screenName, Map<String, Object> context) throws GeneralException {
        ModelMenu widget;
        try {
            widget = MenuFactory.getMenuFromLocation(location, screenName);
        } catch (Exception e) {
            throw new GeneralException("Error getting screen location to run screen actions: " + e.getMessage(), e);
        }
        AbstractModelAction.runSubActionsEx(widget.getActions(), context);
        return null;
    }
    
    public static Object runMenuActionsAtLocation(ModelLocation location, Map<String, Object> context) throws GeneralException {
        return runMenuActionsAtLocation(location.getResource(), location.getName(), context);
    }
    
    public static Object runMenuActionsAtLocation(String combinedName, Map<String, Object> context) throws GeneralException {
        return runMenuActionsAtLocation(ModelLocation.fromAddress(combinedName), context);
    }
    
}
