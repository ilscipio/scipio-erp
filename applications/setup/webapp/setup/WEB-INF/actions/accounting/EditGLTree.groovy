/**
 * SCIPIO: SETUP interactive glAccount tree data prep.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;

import com.ilscipio.scipio.treeMenu.jsTree.JsTreeHelper
 
final module = "EditGlTree.groovy";

//glAccountUrls = [
//    "addGlAccountUrl" : "setupAddGlAccount",
//    "createGlAccountUrl" : "setupCreateGlAccount",
//    "editGlAccountUrl" : "setupEditGlAccount",
//    "updateGlAccountUrl" : "setupUpdateGlAccount",
//    "deleteGlAccountUrl" : "setupDeleteGlAccount",
//    "assignGlAccountUrl" : "setupAssignGlAccount",
//    "importGlAccountUrl" : "setupImportGlAccounts"
//];
//context.glAccountUrls = glAccountUrls;
context.organizationPartyId = context.orgPartyId;

isEventError = context.egltIsEventError;
if (isEventError == null) isEventError = context.isError;
if (isEventError == null) isEventError = false;
context.egltIsEventError = isEventError;

getSetStringParam = { paramName ->
    def value = context[paramName];
    if (value == null) value = parameters[paramName] as String;
    context[paramName] = value;
    return value;
};
targetNodePath = getSetStringParam("egltTargetNodePath");
newTargetNodePath = getSetStringParam("egltNewTargetNodePath");
submittedFormId = getSetStringParam("egltSubmittedFormId");

treeMenuHelper = new JsTreeHelper();
treeMenuData = [];
result = dispatcher.runSync("buildGlAccountTree", ["glAccountId": context.topGlAccountId]);
if (result?.treeList) {
    treeMenuData = treeMenuData + result.treeList;
}
treeMenuHelper.addAll(treeMenuData);
context.treeMenuData = treeMenuHelper;

context.treeOptionsFixedParams=["orgPartyId": context.orgPartyId, "topGlAccountId" : context.topGlAccountId];
