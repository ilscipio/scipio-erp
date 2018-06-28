/**
 * Scipio CMS Get Page List - script
 * Fetches all available pages and puts the information into context
 */

import javax.servlet.*;
import javax.servlet.http.*;

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.service.ServiceUtil;

import org.ofbiz.service.LocalDispatcher;

final String module = "CmsGetPageList.groovy";

/* Initialize */
userLogin = context.userLogin;

/* Parameters */
int viewSize = ((context.viewSize) ? context.viewSize : 0) as Integer;
int viewIndex = ((context.viewIndex) ? context.viewIndex : 0) as Integer;

webSiteId = parameters.webSiteId ?: null;
context.webSiteId = webSiteId;
resultMap = dispatcher.runSync("cmsGetPages", ["request":request, "response": response, 
    "userLogin":context.userLogin, "locale": context.locale, "webSiteId":webSiteId]);
if (ServiceUtil.isSuccess(resultMap)) {
    List pageList = resultMap.pages;
    
    listSize = pageList ? pageList.size() : 0;
    lowIndex = viewIndex * viewSize;
    highIndex = (viewIndex + 1) * viewSize;
    highIndex = highIndex > listSize ? listSize : highIndex;
    lowIndex = lowIndex > highIndex ? highIndex : lowIndex;
    
    List returnList = pageList.subList(lowIndex,highIndex);
    
    context.viewSize = viewSize;
    context.viewIndex = viewIndex;
    context.listSize = listSize;
    context.pages = returnList;
} else {
}
