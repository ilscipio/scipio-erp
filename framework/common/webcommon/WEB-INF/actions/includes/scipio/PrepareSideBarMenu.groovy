/**
 * SCIPIO: Prepares side bar menu.
 */

import org.ofbiz.base.util.*;

final module = "PrepareSideBarMenu.groovy";

/* 
activeSubMenu is the location#name of a sub-menu element (<sub-menu name="..."/>).
If location not specified, a default is used, but generally we will no longer
need the location anymore in most cases.
 
The field name is determined
by selected-menu-context-field-name on the menu def.
When field is empty, it means the top level.
The name may be virtual and not actually refer to a real menu, but we keep names of the original top-level
menus (as much as possible) to make everything easier.
*/
activeSubMenuName = null;
activeSubMenuLoc = null;
activeSubMenu = context.activeSubMenu;
if (activeSubMenu) {
    if (activeSubMenu.contains("#")) {
        parts = context.activeSubMenu.split("#", 2);
        if (parts.length >= 2) {
            activeSubMenuLoc = parts[0];
            activeSubMenuName = parts[1];
        } else {
            activeSubMenuName = parts[0];
        }
    } else {
        activeSubMenuName = activeSubMenu;
    }
} 

origActiveSubMenuName = activeSubMenuName;

def appendSuffix(name, suffix) {
    if (name && suffix) {
        if (!name.endsWith(suffix)) {
            name += suffix;
        }
    }
    return name;
}

def stripSuffix(name, suffix) {
    if (name && suffix) {
        if (name.endsWith(suffix)) {
            name = name.substring(0, name.length() - suffix.length());
        }
    }
    return name;
}

// append menu name suffix if needed
if (context.advAddSuffix) {
    activeSubMenuName = appendSuffix(activeSubMenuName, context.advSuffix);
    // ignore advStripSuffix
} else {
    if (context.advStripSuffix) {
        activeSubMenuName = stripSuffix(activeSubMenuName, context.advSuffix);
    }
}

useAdvMenu = false;
if (!context.forceSimple) {
    if (activeSubMenuName) {
        // check if the advanced menu contains the named submenu.
        try {
            advMenuModel = org.ofbiz.widget.model.MenuFactory.getMenuFromLocation(
                context.advLoc, context.advName);
            if (advMenuModel == null) {
                throw new IllegalArgumentException("Could not find menu with name [" + context.advName 
                    + "] in location [" + context.advLoc + "]");
            }
        } catch (Exception e) {
            Debug.logError(e, "Error loading sidebar advanced menu model", module);
        }
        
        if (activeSubMenuName.equals(advMenuModel.getName()) || 
            advMenuModel.getModelSubMenuByName(activeSubMenuName)) {
            useAdvMenu = true;
        }
    } else {
        // by default assume this is a top level request for the advanced menu
        useAdvMenu = true;
    }
}

context.useAdvMenu = useAdvMenu;

if (useAdvMenu) {
    context[context.advSelSubField] = activeSubMenuName;
    
    /*
     To highlight the item, we simply transfer activeSubMenuItem to activeMainMenuItem (note scope is protected).
     The target activeSubMenuItem value is a leaf and implies which parents get selected.
     We should only use incoming activeMainMenuItem if we were originally rendering the top level menu alone, so when
     activeSubMenu is not set.
      */
    activeSubMenuItem = context.activeSubMenuItem;
    if (!activeSubMenuItem && !activeSubMenu) {
        activeSubMenuItem = context.activeMainMenuItem;
    }
    context[context.advSelItemField] = activeSubMenuItem;
} else {
    // fallback to simple menu

    // always append suffix for simple/fallback
    context.simpleName = appendSuffix(origActiveSubMenuName, context.simpleSuffix);

    // for location we use the part before # in activeSubMenu, or the configured default location
    context.simpleLoc = activeSubMenuLoc ?: context.defLoc;
}

