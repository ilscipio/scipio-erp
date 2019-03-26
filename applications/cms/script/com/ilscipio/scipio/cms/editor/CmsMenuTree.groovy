/**
 * Scipio CMS Menu Tree - script
 */
import org.ofbiz.base.util.*;

final String module = "CmsMenuTree.groovy";

// Fetch menus from db

/*menuResult = dispatcher.runSync("cmsGetMenus", ["request":request, "response":response, 
    "userLogin":context.userLogin, "locale": context.locale]);
if (ServiceUtil.isSuccess(menuResult)) {
    context.cmsMenus=menuResult.menuJson;
} else {
    context.cmsErrorHandler.addContextReadErrorFromServiceResult(context, menuResult);
}*/

context.cmsMenus = select("menuId", "menuName").from("CmsMenu").cache(false).queryList();