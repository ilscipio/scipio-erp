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
import org.ofbiz.webapp.control.*;
import org.ofbiz.base.component.ComponentConfig;

import com.ilscipio.scipio.treeMenu.jsTree.JsTreeCore;
import com.ilscipio.scipio.treeMenu.jsTree.JsTreeHelper;
import com.ilscipio.scipio.cms.webapp.ComponentUtil;
import com.ilscipio.scipio.cms.control.CmsWebSiteInfo;

final String module = "CmsMenuTree.groovy";
// Fetch menus from db

menuResult = dispatcher.runSync("cmsGetMenus", ["request":request, "response":response, 
    "userLogin":context.userLogin, "webSiteId":null, "locale": context.locale]);
if (ServiceUtil.isSuccess(menuResult)) {
    context.menus=menuResult.menuJson;
} else {
    context.cmsErrorHandler.addContextReadErrorFromServiceResult(context, menuResult);
}
