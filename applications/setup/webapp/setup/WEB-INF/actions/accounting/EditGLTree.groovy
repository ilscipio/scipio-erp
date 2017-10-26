/**
 * SCIPIO: SETUP interactive catalog tree data prep.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
 
final module = "EditGlTree.groovy";

context.addGlAccountUrl = "setupAddGlAccount";
context.createGlAccountUrl = "setupCreateGlAccount";
context.editGlAccountUrl = "setupEditGlAccount";
context.updateGlAccountUrl = "setupUpdateGlAccount";
context.deleteGlAccountUrl = "setupDeleteGlAccount";
context.assignGlAccountUrl = "setupAssignGlAccount";
context.importGlAccountsUrl = "setupImportGlAccounts";

context.organizationPartyId = context.orgPartyId;
context.accountMaps = context.glAccountList;

context.treeOptionsFixedParams=["orgPartyId": context.orgPartyId, "topGlAccountId" : context.topGlAccountId];
