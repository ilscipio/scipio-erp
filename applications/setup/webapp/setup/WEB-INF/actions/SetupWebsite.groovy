import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;

final defaultInitialWebSiteId = UtilProperties.getPropertyValue("scipiosetup", "website.defaultInitialWebSiteId");

websiteData = context.websiteData ?: [:];

context.webSite = websiteData.webSite;
context.webSiteId = websiteData.webSiteId;

defaultWebSiteId = null;
if (!delegator.findOne("WebSite", [webSiteId:defaultInitialWebSiteId], false)) {
    defaultWebSiteId = defaultInitialWebSiteId;
}

context.defaultWebSiteId = defaultWebSiteId;
context.defaultInitialWebSiteId = defaultInitialWebSiteId;

context.defaultVisualThemeSetId = UtilProperties.getPropertyValue("scipiosetup", "website.visualThemeSetId", "ECOMMERCE");
