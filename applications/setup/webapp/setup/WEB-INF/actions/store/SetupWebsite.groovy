

import org.ofbiz.base.component.ComponentConfig
import org.ofbiz.base.component.ComponentConfig.WebappInfo
import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.webapp.WebAppUtil

import com.ilscipio.scipio.setup.*;

final module = "SetupWebsite.groovy";

// SCIPIO (12/03/2018): Builds a map for webapp->websiteId used to show only the websiteIds available
Map<Object, String> webappWebsiteMap = UtilMisc.newMap();
for (WebappInfo webappInfo in ComponentConfig.getAllWebappResourceInfos()) {
    String websiteId = WebAppUtil.getWebSiteId(webappInfo);
    if (Debug.isOn(Debug.VERBOSE)) {
        Debug.log("webappInfo[" + webappInfo.getName() + "]:               [" + webappInfo.getTitle() + "] > websiteId: " + websiteId);
    }
    if (UtilValidate.isNotEmpty(websiteId)) {
        webappWebsiteMap.put(webappInfo, websiteId);
    }
}
context.webappWebsiteMap = webappWebsiteMap;

final defaultInitialWebSiteId = UtilProperties.getPropertyValue("scipiosetup", "website.defaultInitialWebSiteId");

websiteData = context.websiteData ?: [:];

context.webSite = websiteData.webSite;
context.webSiteId = websiteData.webSiteId;
context.webSiteList = websiteData.webSiteList;
context.webSiteCount = websiteData.webSiteCount;

defaultWebSiteId = null;
if (!delegator.findOne("WebSite", [webSiteId:defaultInitialWebSiteId], false)) {
    defaultWebSiteId = defaultInitialWebSiteId;
}

context.defaultWebSiteId = defaultWebSiteId;
context.defaultInitialWebSiteId = defaultInitialWebSiteId;

context.defaultVisualThemeSetId = UtilProperties.getPropertyValue("scipiosetup", "website.visualThemeSetId", "ECOMMERCE");

context.defaultVisualThemeSelectorScript = UtilProperties.getPropertyValue("scipiosetup", "website.visualThemeSelectorScript");
