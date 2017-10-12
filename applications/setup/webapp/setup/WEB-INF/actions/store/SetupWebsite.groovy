import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import com.ilscipio.scipio.setup.*;

final module = "SetupWebsite.groovy";

final defaultInitialWebSiteId = UtilProperties.getPropertyValue("scipiosetup", "website.defaultInitialWebSiteId");

websiteData = context.websiteData ?: [:];

context.webSite = websiteData.webSite;
context.webSiteId = websiteData.webSiteId;
context.webSiteCount = websiteData.webSiteCount;

defaultWebSiteId = null;
if (!delegator.findOne("WebSite", [webSiteId:defaultInitialWebSiteId], false)) {
    defaultWebSiteId = defaultInitialWebSiteId;
}

context.defaultWebSiteId = defaultWebSiteId;
context.defaultInitialWebSiteId = defaultInitialWebSiteId;

context.defaultVisualThemeSetId = UtilProperties.getPropertyValue("scipiosetup", "website.visualThemeSetId", "ECOMMERCE");

context.defaultVisualThemeSelectorScript = UtilProperties.getPropertyValue("scipiosetup", "website.visualThemeSelectorScript");
