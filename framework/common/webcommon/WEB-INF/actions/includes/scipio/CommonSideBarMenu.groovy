/**
 * SCIPIO: Default Common SideBar Menu logic.
 */

import org.ofbiz.base.util.Debug;
import org.ofbiz.widget.model.AbstractModelCondition.IfServicePermission;

final module = "CommonSideBarMenuGroovy";
 
csbmArgs = context.commonSideBarMenu ?: [:];
 
userLogin = context.userLogin;

perm = csbmArgs.perm ?: null; // WARN: gets overridden in the script
permAction = csbmArgs.permAction ?: null; // WARN: gets overridden in the script
permList = csbmArgs.permList ?: null;
permServ = csbmArgs.permServ ?: null; // WARN: gets overridden in the script
permServList = csbmArgs.permServList ?: null;

cond = csbmArgs.cond;
if (cond == null) {
    cond = true;
}
condList = csbmArgs.condList ?: [];

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
    if (condList) {
        for(cond in condList) {
            showTargetMenu = (Boolean) cond;
            if (!showTargetMenu) {
                break;
            }
        }
    }
    
    if (showTargetMenu && (perm)) {
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
    if (showTargetMenu && (permServ)) {
        showTargetMenu = IfServicePermission.checkServicePermission(context, 
            permServ.name, permServ.mainAction, permServ.servCtx, permServ.resource, userLogin);
        Debug.logInfo("Checking single perm service: " + permServ + ": " + showTargetMenu, module);
    }
    if (showTargetMenu && (permServList)) {
        for(permServ in permServList) {
            showTargetMenu = IfServicePermission.checkServicePermission(context,
                permServ.name, permServ.mainAction, permServ.servCtx, permServ.resource, userLogin);
            Debug.logInfo("Checking perm service list entry: " + permServ + ": " + showTargetMenu, module);
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
