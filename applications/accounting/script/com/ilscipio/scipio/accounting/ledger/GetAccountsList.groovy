import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

import javax.servlet.*;
import javax.servlet.http.*;

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.LocalDispatcher;

import com.ilscipio.scipio.treeMenu.jsTree.JsTreeCore;
import com.ilscipio.scipio.treeMenu.jsTree.JsTreeHelper;

final String module = "GetAccountsList.groovy";

glAccountUrls = [
    "addGlAccountUrl" : "AddGlAccount",
    "createGlAccountUrl" : "createGlAccount",
    "editGlAccountUrl" : "EditGlobalGlAccount",
    "assignGlAccountUrl" : "AssignGlAccount",
    "accountTransactionBaseUrl" : "FindAcctgTrans"    
];
context.glAccountUrls = glAccountUrls;


/*Create Accounts Tree*/
accountMaps = delegator.findAll("GlAccount", true);
context.accountMaps = accountMaps;

