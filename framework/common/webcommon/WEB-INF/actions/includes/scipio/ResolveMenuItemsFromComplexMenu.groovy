/**
 * SCIPIO: Uses the activeSubMenu/activeSubMenuItem in context to automatically
 * determine other menu items (if not explicitly set):
 * * activeMainMenuItem
 * 
 * 2016-11-11: Added for 1.14.3.
 */

import org.ofbiz.base.util.Debug;

final module = "ResolveMenuItemsFromComplexMenu.groovy";

activeMainMenuItem = context.activeMainMenuItem;

if (!activeMainMenuItem) {
    cplxLoc = context.mainSideBarMenu?.location;
    cplxName = context.mainSideBarMenu?.name;
    
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
            Debug.logError(e, "Error loading complex menu model; unable to determine activeMainMenuItem", module);
        }
    } else {
        Debug.logWarning("Could not find mainSideBarMenu.location/name context variables; " +
            "unable to determine activeMainMenuItem", module);
    }

    if (cplxMenuModel != null) {
        activeSubMenu = context.activeSubMenu;
        activeSubMenuItem = context.activeSubMenuItem;
        
        subItem = cplxMenuModel.getModelMenuItemBySubName(activeSubMenuItem, activeSubMenu);
        if (subItem != null) {
            topItem = subItem.getTopParentMenuItem();
            activeMainMenuItem = topItem.getName();
            context.activeMainMenuItem = activeMainMenuItem;
            Debug.logInfo("Automatically determined activeMainMenuItem: " + activeMainMenuItem, module);
        } else {
            Debug.logWarning("Could not find sub menu item activeSubMenu/activeSubMenuItem [" +
                activeSubMenu + "/" + activeSubMenuItem + "] in menu [" + cplxLoc + "#" +
                cplxName + "]; unable to determine activeMainMenuItem", module);
        }
    }
}