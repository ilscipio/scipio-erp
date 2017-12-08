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

final String module = "CmsContentTree.groovy";

/* Create Content Tree */
// Fetch controller entries
// NOTE: 2016: this is now filtered by cmsWebSiteIdSet meaning sites registered with process filter or screen view handler

// DEV NOTE: MOST OF OUR CODE CURRENTLY ASSUMES primaryPathFromContextRoot(Default)=Y
// This means by default we display /control prefix everywhere so ALL paths
// are relative to webapp context root, consistently.
// This is the simplest/best default because it allows the greatest flexibility with
// the simplest syntax (only a single path needed to express, no matter if page or request,
// always from webapp context root).

// FIXME?: running addWebSiteSettingsToMap twice before/after for now... oh well...
webSiteExtraConfigs = [:];
for(webSiteId in context.cmsWebSiteIdSet) {
    settingsMap = [:];
    context.cmsContentTreeUtil.addWebSiteSettingsToMap(settingsMap, webSiteId);
    webSiteExtraConfigs[webSiteId] = settingsMap;
}
requestMaps = ComponentUtil.getWebappRequestMaps(context.cmsWebSiteIdSet, webSiteExtraConfigs);
context.requestMaps = requestMaps;

for(requestMap in requestMaps.values()) {
    context.cmsContentTreeUtil.addWebSiteSettingsToMap(requestMap, requestMap.webSiteId);
}


// Fetch content from db

pagesResult = dispatcher.runSync("cmsGetPages", ["request":request, "response":response, 
    "userLogin":context.userLogin, "webSiteId":null, "locale": context.locale]);
if (ServiceUtil.isSuccess(pagesResult)) {
    // Merge DB content with regular fallback (only list by Website)
    if (pagesResult.pages) {
        pages = pagesResult.pages;
        pages.each { page ->
            if (page.primaryPath != null) {
                String webSiteId = page.webSiteId;
                String primaryPath = page.primaryPath;
                if (requestMaps.containsKey(webSiteId)) {
                    Map currMap = requestMaps.get(webSiteId);
                    List currPages = currMap.pages;
                    if (currPages == null) {
                        currPages = new ArrayList();
                        ComponentUtil.generateMapFromRequestUri(primaryPath, "page", currPages, webSiteId, webSiteId);
                        currMap.put("pages",currPages);
                    } else {
                        ComponentUtil.generateMapFromRequestUri(primaryPath, "page", currPages, webSiteId, webSiteId);
                    }
                } else {
                    Map currMap = new HashMap();
                    List currPages = new ArrayList();
                    ComponentUtil.generateMapFromRequestUri(primaryPath, "page", currPages, webSiteId, webSiteId);
                    currMap.put("pages", currPages);
                    requestMaps.put(webSiteId,currMap);
                }
            }   
        }
    }
} else {
    context.cmsErrorHandler.addContextReadErrorFromServiceResult(context, pagesResult);
}

cmsCtrlRootAliasMsgs = context.cmsWebSiteHelper.getWebSitesControlRootAliasMsgs(context);
context.cmsCtrlRootAliasMsgs = cmsCtrlRootAliasMsgs;
