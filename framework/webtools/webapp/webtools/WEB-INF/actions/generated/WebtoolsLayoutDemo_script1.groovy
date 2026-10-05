import org.ofbiz.base.util.*;
                    import org.ofbiz.entity.condition.*;
                    import org.ofbiz.entity.util.*;
                    shopInfo = [:];
                    try {
                        def shopWebSite = EntityQuery.use(delegator).from("WebSite").where("webSiteId", "ScipioWebStore").cache().queryOne();
                        if (!shopWebSite) {
                            // FIXME: missing logic to find most-appropriate default website in this case
                            shopWebSite = EntityQuery.use(delegator).from("WebSite")
                                .where(EntityCondition.makeCondition("productStoreId", EntityOperator.NOT_EQUAL, null)).cache().queryFirst();
                        }
                        Debug.logInfo("Using website for shop tests: " + (shopWebSite ? shopWebSite.webSiteId : "[missing]"), "InlineDemoScript.groovy");
                        
                        if (shopWebSite) {
                            shopInfo.webSiteId = shopWebSite.webSiteId;
                            shopInfo.mountPoint = org.ofbiz.webapp.WebAppUtil.getWebappInfoFromWebsiteId(shopWebSite.webSiteId).getContextRoot();
                        }
                    } catch(Exception e) {
                        Debug.logError(e, "Cannot get WebSite for shop tests", "InlineDemoScript.groovy");
                    }
                    context.shopInfo = shopInfo;