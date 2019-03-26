/**
 * SCIPIO: Prepares complex menu.
 */

import org.ofbiz.base.util.*;

final module = "PrepareComplexMenu.groovy";

/* configurable render inputs */
menuCfg = context.menuCfg ?: [:];
cplxLoc = menuCfg.location ?: "";
cplxName = menuCfg.name ?: "";
cplxSubFilter = context.menuCfgSubMenuFilter ?: menuCfg.subMenuFilter ?: "current";
cplxMaxDepth = context.menuCfgMaxDepth ?: menuCfg.maxDepth ?: "";

cplxSelSubField = menuCfg.selMenuFieldName ?: ""; // set further below if needed: ?: "activeMainSubMenu";
cplxSelItemField = menuCfg.selMenuItemFieldName ?: ""; // set further below if needed: ?: "activeMainMenuItem";

cplxSuffix = menuCfg.nameSuffix ?: "Menu";
cplxAddSuffix = menuCfg.addNameSuffix; if (cplxAddSuffix == null) { cplxAddSuffix = false; };
cplxStripSuffix = menuCfg.stripNameSuffix; if (cplxStripSuffix == null) { cplxStripSuffix = true; };

smplSuffix = menuCfg.simpleNameSuffix ?: cplxSuffix;

defLoc = menuCfg.defLocation ?: "";

smplForce = menuCfg.forceSimple;
if (smplForce == null) {
    smplForce = false;
}
cplxAllowSubItemTop = menuCfg.allowSubItemTop;
if (cplxAllowSubItemTop == null) {
    cplxAllowSubItemTop = true;
}

activeSubField = menuCfg.activeSubField ?: "activeSubMenu";
activeSubItemField = menuCfg.activeSubItemField ?: "activeSubMenuItem";
activeMainItemField = menuCfg.activeMainItemField ?: "activeMainMenuItem";

/* screen inputs */
activeSubMenu = context[activeSubField];
activeSubMenuItem = context[activeSubItemField];
activeMainMenuItem = context[activeMainItemField];

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
if (cplxAddSuffix) {
    activeSubMenuName = appendSuffix(activeSubMenuName, cplxSuffix);
    // ignore cplxStripSuffix
} else {
    if (cplxStripSuffix) {
        activeSubMenuName = stripSuffix(activeSubMenuName, cplxSuffix);
    }
}

useCplxMenu = false;
org.ofbiz.widget.model.ModelMenu cplxMenuModel = null;
warnFallback = false;
if (!smplForce) {
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
    
    if (activeSubMenuName) {
        // check if the complex menu contains the named submenu.
        if (cplxMenuModel != null && cplxMenuModel.isMenuNameWithinMenu(activeSubMenuName)) {
            useCplxMenu = true;
        } else {
            warnFallback = true;
        }
    } else {
        // by default assume this is a top level request for the complex menu
        useCplxMenu = true;
    }
    
    if (useCplxMenu) {
        if (cplxMenuModel != null) {
            // SCIPIO: NOTE: 2018-09-04: The return values for the getSelectedMenu* methods without context
            // methods have been changed and they now return FlexibleMapAccessor (so .toString() has been added here).
            // TODO: REVIEW: Why did I not pass the context here again? Was it a bad context to use? Either way could cause issues...
            if (!cplxSelSubField) {
                cplxSelSubField = cplxMenuModel.getSelectedMenuContextFieldName().toString();
            }
            if (!cplxSelItemField) {
                cplxSelItemField = cplxMenuModel.getSelectedMenuItemContextFieldNameFirst().toString();
            }
        }
    }
}

// fallback defaults
cplxSelSubField = cplxSelSubField ?: "activeMainSubMenu";
cplxSelItemField = cplxSelItemField ?: "activeMainMenuItem";

smplName = null;
smplLoc = null;
if (!useCplxMenu) {
    // fallback to simple menu
    
    // always append suffix for simple/fallback
    smplName = appendSuffix(origActiveSubMenuName, smplSuffix);

    // for location we use the part before # in activeSubMenu, or the configured default location
    smplLoc = activeSubMenuLoc ?: defLoc;
    
    // if that didn't work, render the complex menu instead
    // this happens on some common screens when forceSimple enabled
    if (!smplName || !smplLoc) {
        useCplxMenu = true;
    } else {
        context.smplName = smplName;
        context.smplLoc = smplLoc;
    }
    
    if (warnFallback) {
        Debug.logWarning("Could not find (active) sub menu '" + activeSubMenuName + "'" +
            " in complex menu '" + cplxLoc + "#" + cplxName + "'; falling back to '" +
            smplLoc + "#" + smplName + "'", module);
    }
}

if (useCplxMenu) {
    context[cplxSelSubField] = activeSubMenuName;
    
    /*
     To highlight the item, we simply transfer activeSubMenuItem to activeMainMenuItem (note scope is protected).
     The target activeSubMenuItem value is a leaf and implies which parents get selected.
     We should only use incoming activeMainMenuItem if we were originally rendering the top level menu alone, so when
     activeSubMenu is not set.
      */
    if (cplxAllowSubItemTop) {
        if (!activeSubMenu && !activeSubMenuItem) {
            activeSubMenuItem = activeMainMenuItem;
        }
    } else {
        if (!activeSubMenu) {
            activeSubMenuItem = activeMainMenuItem;
        }
    }
    context[cplxSelItemField] = activeSubMenuItem;
    
}

context.useCplxMenu = useCplxMenu;

context.cplxName = cplxName;
context.cplxLoc = cplxLoc;
context.cplxSubFilter = cplxSubFilter;
context.cplxMaxDepth = cplxMaxDepth;

/*
if (true) {
    cplxMenuInfo = [:];
    cplxMenuInfo.useCplxMenu = useCplxMenu;
    
    cplxMenuInfo.cplxName = cplxName;
    cplxMenuInfo.cplxLoc = cplxLoc;
    cplxMenuInfo.cplxSubFilter = cplxSubFilter;
    cplxMenuInfo.cplxMaxDepth = cplxMaxDepth;
    
    cplxMenuInfo.cplxSelSubField = cplxSelSubField;
    cplxMenuInfo.cplxSelItemField = cplxSelItemField;
    cplxMenuInfo.cplxSuffix = cplxSuffix;
    cplxMenuInfo.cplxAddSuffix = cplxAddSuffix;
    cplxMenuInfo.cplxStripSuffix = cplxStripSuffix;
    cplxMenuInfo.smplSuffix = smplSuffix;
    cplxMenuInfo.defLoc = defLoc;
    cplxMenuInfo.smplForce = smplForce;
    
    cplxMenuInfo.activeSubMenuName = activeSubMenuName;
    cplxMenuInfo.activeSubMenuLoc = activeSubMenuLoc;
    cplxMenuInfo.activeSubMenu = activeSubMenu;
    cplxMenuInfo.activeSubMenuItem = activeSubMenuItem;
    cplxMenuInfo.activeMainMenuItem = activeMainMenuItem;
    
    Debug.logInfo("COMPLEX MENU INFO: " + cplxMenuInfo, module);
}
*/
