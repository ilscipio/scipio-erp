/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */


import org.ofbiz.base.component.ComponentConfig
import org.ofbiz.base.component.ComponentConfig.WebappInfo
import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityJoinOperator
import org.ofbiz.entity.util.*;
import org.ofbiz.webapp.WebAppUtil

import com.ilscipio.scipio.setup.*;

final module = "SetupWebsite.groovy";

// SCIPIO (12/03/2018): Builds a map for webapp->websiteId used to show only the websiteIds available
Map<Object, String> webappWebsiteMap = UtilMisc.newMap();
webappWebsiteIdList = UtilMisc.newList();
for (WebappInfo webappInfo in ComponentConfig.getAllWebappResourceInfos()) {
    String websiteId = WebAppUtil.getWebSiteId(webappInfo);
    if (UtilValidate.isNotEmpty(websiteId)) {
        webappWebsiteIdList.add(websiteId);

        if (Debug.isOn(Debug.VERBOSE)) {
            Debug.log("webappInfo[" + webappInfo.getName() + "]:               [" + webappInfo.getTitle() + "] > websiteId: " + websiteId);
        }

        existingWebsite = delegator.findOne("WebSite", [webSiteId:websiteId], false);

        // SCIPIO (2019-03-21): Filtering the ones that already exist, they cannot be used anymore
        if (!existingWebsite) {
            webappWebsiteMap.put(webappInfo, websiteId);
        }
    }
}

List<EntityCondition> conds = UtilMisc.newList();
conds.add(EntityCondition.makeCondition("productStoreId", EntityJoinOperator.EQUALS, null));
conds.add(EntityCondition.makeCondition("webSiteId", EntityJoinOperator.NOT_IN, webappWebsiteIdList));
context.existingDBWebsiteIds = EntityUtil.getFieldListFromEntityList(
       EntityQuery.use(delegator).select("webSiteId").from("WebSite").where(conds).queryList(), "webSiteId", true);

context.webappWebsiteMap = webappWebsiteMap;

final defaultInitialWebSiteId = UtilProperties.getPropertyValue("scipiosetup", "website.defaultInitialWebSiteId");

websiteData = context.websiteData ?: [:];

context.webSite = websiteData.webSite;
context.webSiteId = websiteData.webSiteId;
context.webSiteList = websiteData.webSiteList;
context.webSiteCount = websiteData.webSiteCount;

defaultSetupWebSiteId = null;
if (defaultInitialWebSiteId && !delegator.findOne("WebSite", [webSiteId:defaultInitialWebSiteId], false)) {
    defaultSetupWebSiteId = defaultInitialWebSiteId;
}

context.defaultSetupWebSiteId = defaultSetupWebSiteId;
context.defaultInitialWebSiteId = defaultInitialWebSiteId;

context.defaultVisualThemeSetId = UtilProperties.getPropertyValue("scipiosetup", "website.visualThemeSetId", "ECOMMERCE");

context.defaultVisualThemeSelectorScript = UtilProperties.getPropertyValue("scipiosetup", "website.visualThemeSelectorScript");
