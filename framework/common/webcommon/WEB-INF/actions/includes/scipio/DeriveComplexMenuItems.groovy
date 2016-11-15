/**
 * SCIPIO: Applies the active activeSubMenu/activeSubMenuItem menu item specified in context to a
 * complex menu identified by menuCfg.name/location (usually the MainSideBarMenu in standard
 * Scipio backend) to automatically derive other menu item selection information.
 * 
 * This uses the same menuCfg parameters as CommonScreens ComplexMenu widget, EXCEPT there is an extra
 * menuCfg.menuType currently supporting either values: "Menu" or "SideBar".
 * This selects the default values/config for all the other menuCfg parameters.
 * 
 * Currently determines (if not explicitly set):
 * * activeMainMenuItem - required for main non-complex menus (e.g. regular/legacy MainAppBar)
 * 
 * Outputs the following:
 * * context.activeMainMenuItem - only if not already set
 * * globalContext.activeMainMenuItem - TENTATIVELY (see WARN below), transferred from local context if needed
 * * context.activeMainMenuItem_auto
 * * globalContext.activeMainMenuItem_auto
 * 
 * 2016-11-11: Added for 1.14.3.
 */

import org.ofbiz.base.util.Debug;

final module = "DeriveComplexMenuItems.groovy";

// GET SCRIPT PARAMETERS
activeMainMenuItem = context.activeMainMenuItem; // may already be manually set
activeMainMenuItem_auto = null;

menuCfg = context.menuCfg ?: [:];

cplxLoc = menuCfg.location;
cplxName = menuCfg.name;

cplxSuffix = menuCfg.nameSuffix ?: "Menu";
cplxAddSuffix = menuCfg.addNameSuffix; if (cplxAddSuffix == null) { cplxAddSuffix = false; };
cplxStripSuffix = menuCfg.stripNameSuffix; if (cplxStripSuffix == null) { cplxStripSuffix = true; };

cplxLocal = menuCfg.local;
if (cplxLocal == null) {
    cplxLocal = true;
}
cplxGlobal = menuCfg.global;
if (cplxGlobal == null) {
    cplxGlobal = true;
}

// CLEAR SCRIPT PARAMETERS
context.remove("menuCfg");

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

// NOTE: we run everything even if activeMainMenuItem is already set, so caller can still
// use activeMainMenuItem_auto for other purpose

// LOAD THE COMPLEX MENU
cplxMenuModel = null;
if (cplxLoc && cplxName) {
    cplxMenuModel = null;
    try {
        cplxMenuModel = org.ofbiz.widget.model.MenuFactory.getMenuFromLocation(
            cplxLoc, cplxName);
        if (cplxMenuModel == null) {
            throw new IllegalArgumentException("Could not find menu with name [" + cplxName
                + "] in location [" + cplxLoc + "]");
        }
    } catch (Exception e) {
        Debug.logError(e, "Error loading complex menu model", module);
    }
} else {
    if (!activeMainMenuItem) {
        Debug.logWarning("Could not find mainSideBarMenu.location/name context variables; " +
            "unable to auto-determine activeMainMenuItem", module);
    }
}

activeSubMenu = context.activeSubMenu;
activeSubMenuItem = context.activeSubMenuItem;
if (activeSubMenu && activeSubMenu.contains("#")) {
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

// append menu name suffix if needed
if (cplxAddSuffix) {
    activeSubMenuName = appendSuffix(activeSubMenuName, cplxSuffix);
    // ignore cplxStripSuffix
} else {
    if (cplxStripSuffix) {
        activeSubMenuName = stripSuffix(activeSubMenuName, cplxSuffix);
    }
}

if (cplxMenuModel != null && ((activeSubMenuName) || (activeSubMenuItem))) { // NOTE: don't attempt if both fields null
    subItem = cplxMenuModel.getModelMenuItemBySubName(activeSubMenuItem, activeSubMenuName);
    if (subItem != null) {
        topItem = subItem.getTopParentMenuItem();
        activeMainMenuItem_auto = topItem.getName();
    } else {
        Debug.logWarning("Could not find sub menu item activeSubMenu(Name)/activeSubMenuItem [" +
            activeSubMenuName + "/" + activeSubMenuItem + "] in menu [" + cplxLoc + "#" +
            cplxName + "]; unable to determine activeMainMenuItem", module);
    }
}

// STORE IN LOCAL CONTEXT
if (cplxLocal) {
    context.activeMainMenuItem_auto = activeMainMenuItem_auto;
    if (!activeMainMenuItem) { // store activeMainMenuItem only if not set
        if (Debug.verboseOn()) {
            Debug.logVerbose("Automatically determined activeMainMenuItem: " + activeMainMenuItem_auto, module);
        }
        context.activeMainMenuItem = activeMainMenuItem_auto;
    }
}

if (cplxGlobal) {
    // STORE IN GLOBAL CONTEXT (underneath / in addition to the local context)
    // NOTE/WARN: traditionally this caused context problems in stock ofbiz, but this may be the only way to handle the
    // case where a screen decides to include a main menu in its widgets outside the scope of the decorators
    // TODO?: investigate deeper, possible screen renderer patches involved if old issues surface
    if (context.globalContext != null) {
        if (!globalContext.activeMainMenuItem) {
            context.globalContext.activeMainMenuItem = context.activeMainMenuItem;
        }
        context.globalContext.activeMainMenuItem_auto = context.activeMainMenuItem_auto;
    }
}

//Debug.logInfo("activeMainMenuItem_auto: " + activeMainMenuItem_auto, module);

