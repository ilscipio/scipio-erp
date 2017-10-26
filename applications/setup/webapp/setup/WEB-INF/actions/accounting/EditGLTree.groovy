/**
 * SCIPIO: SETUP interactive catalog tree data prep.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
 
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

context.treeOptionsFixedParams=["orgPartyId": context.orgPartyId, "topGlAccountId" : context.topGlAccountId];
