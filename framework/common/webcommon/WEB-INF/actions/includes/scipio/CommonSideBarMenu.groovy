/**
 * SCIPIO: Default Common SideBar Menu logic.
 */

import org.ofbiz.base.util.Debug;

final module = "CommonSideBarMenuGroovy";
 
csbmArgs = context.commonSideBarMenu ?: [:];
 
userLogin = context.userLogin;

perm = csbmArgs.perm ?: null;
permAction = csbmArgs.permAction ?: null;
permList = csbmArgs.permList ?: [];
cond = csbmArgs.cond;
if (cond == null) {
    cond = true;
}
targetMenuName = csbmArgs.targetMenuName ?: "MainSideBarMenu";
if (targetMenuName == "NONE") {
    targetMenuName = "";
}
fallbackMenuName = csbmArgs.fallbackMenuName ?: "DefMainSideBarMenu";
if (fallbackMenuName == "NONE") {
    fallbackMenuName = "";
}
menuLoc = csbmArgs.menuLoc ?: parameters.mainDecoratorLocation;

// TODO: refactor out this logic in another script or class
showTargetMenu = true;
if (cond) {
    if (perm) {
        if (permAction) {
            showTargetMenu = security.hasEntityPermission(perm, permAction, userLogin);
            //Debug.logInfo("Checking single perm: " + perm + ":" + permAction + ": " + showTargetMenu, module);
        } else {
            showTargetMenu = security.hasPermission(perm, userLogin);
            //Debug.logInfo("Checked single perm: " + perm + ": " + showTargetMenu, module);
        }
    }
    if (showTargetMenu && (permList)) {
        for(permEntry in permList) {
            perm = null;
            permAction = null;
            if (permEntry instanceof Map) {
                perm = permEntry.perm;
                permAction = permEntry.action;
            } else if (permEntry instanceof List) {
                perm = permEntry[0];
                permAction = null;
                if (permEntry.size() >= 2) {
                    permAction = permEntry[1];
                }
            } else {
                perm = permEntry;
            }
            
            if (permAction) {
                showTargetMenu = security.hasEntityPermission(perm, permAction, userLogin);
                //Debug.logInfo("Checking perm list entry: " + perm + ":" + permAction + ": " + showTargetMenu, module);
            } else {
                showTargetMenu = security.hasPermission(perm, userLogin);
                //Debug.logInfo("Checking perm list entry: " + perm + ": " + showTargetMenu, module);
            }
            
            if (!showTargetMenu) {
                break;
            }
        }
    }
} else {
    showTargetMenu = false;
}

targetMenuName = showTargetMenu ? targetMenuName : fallbackMenuName;
targetMenuLoc = menuLoc;

context.showTargetMenu = showTargetMenu;
context.targetMenuName = targetMenuName;
context.targetMenuLoc = targetMenuLoc;
context.hasTargetMenu = (targetMenuName) && (targetMenuLoc);
