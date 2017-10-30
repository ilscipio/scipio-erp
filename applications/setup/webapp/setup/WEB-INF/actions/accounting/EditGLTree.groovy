/**
 * SCIPIO: SETUP interactive catalog tree data prep.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;

import com.ilscipio.scipio.treeMenu.jsTree.JsTreeHelper
 
final module = "EditGlTree.groovy";

glAccountUrls = [
    "addGlAccountUrl" : "setupAddGlAccount",
    "createGlAccountUrl" : "setupCreateGlAccount",
    "editGlAccountUrl" : "setupEditGlAccount",
    "updateGlAccountUrl" : "setupUpdateGlAccount",
    "deleteGlAccountUrl" : "setupDeleteGlAccount",
    "assignGlAccountUrl" : "setupAssignGlAccount",
    "importGlAccountUrl" : "setupImportGlAccounts",
];
context.glAccountUrls = glAccountUrls;

context.organizationPartyId = context.orgPartyId;
context.accountMaps = context.glAccountList;

treeMenuHelper = new JsTreeHelper();
treeMenuData = [];
result = dispatcher.runSync("buildGlAccountTree", ["glAccountId": context.topGlAccountId]);

if (result?.treeList) {
    treeMenuData = treeMenuData + result.treeList;
}
treeMenuHelper.addAll(treeMenuData)

for (data in treeMenuData) {
    Debug.log("data ==============> " + data);
}

context.treeMenuData = treeMenuHelper;


context.treeOptionsFixedParams=["orgPartyId": context.orgPartyId, "topGlAccountId" : context.topGlAccountId];
