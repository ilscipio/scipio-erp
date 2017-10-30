import javax.servlet.*;
import javax.servlet.http.*;

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator
import org.ofbiz.entity.util.EntityQuery

import com.ilscipio.scipio.treeMenu.jsTree.JsTreeHelper;

final String module = "GetAccountsList.groovy";

glAccountUrls = [
    "addGlAccountUrl" : "AddGlAccount",
    "createGlAccountUrl" : "createGlAccount",
    "editGlAccountUrl" : "EditGlobalGlAccount",
    "assignGlAccountUrl" : "AssignGlAccount",
    "accountTransactionBaseUrl" : "FindAcctgTrans",
    "accountTransactionBaseUrl" : "FindAcctgTrans",
    "importGlAccountUrl" : "ImportGlAccounts"
];
context.glAccountUrls = glAccountUrls;

/*Create Accounts Tree*/

treeMenuHelper = new JsTreeHelper();
accountingGLs = EntityQuery.use(delegator).select("glAccountId").from("GlAccount").where(EntityCondition.makeCondition("parentGlAccountId", EntityOperator.EQUALS, null)).queryList();
for (GenericValue topGlAccount : accountingGLs) {
    result = dispatcher.runSync("buildGlAccountTree", ["glAccountId": topGlAccount.glAccountId]);
    if (result?.treeList) {
        treeMenuHelper.addAll(result.treeList);
    }
}
context.treeMenuData = treeMenuHelper;
