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
package com.ilscipio.scipio.marketing.widget;

import com.ilscipio.scipio.widget.def.menu.*;
import com.ilscipio.scipio.widget.def.screen.*;
import com.ilscipio.scipio.widget.def.condition.Condition;
import com.ilscipio.scipio.widget.def.condition.ConditionNode;
import com.ilscipio.scipio.widget.def.condition.NestedCondition;
import com.ilscipio.scipio.widget.def.condition.NestedCondition2;
import com.ilscipio.scipio.widget.def.condition.impl.*;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class MarketingMenus {

    @Menu(
        name = "MarketingAppBar",
        location = "component://marketing/widget/MarketingMenus.xml",
        title = "${uiLabelMap.MarketingManager}",
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "Tracking", title = "${uiLabelMap.MarketingTracking}", link = @MenuLink(target = "FindTrackingCode")),
            @MenuItem(name = "Segment", title = "${uiLabelMap.MarketingSegments}", link = @MenuLink(target = "FindSegmentGroup")),
            @MenuItem(name = "Promotions", title = "${uiLabelMap.ProductPromotions}", link = @MenuLink(target = "FindProductPromo")),
            @MenuItem(name = "MarketingCampaign", title = "${uiLabelMap.MarketingCampaigns}", link = @MenuLink(target = "FindMarketingCampaign"))
        }
    )
    public interface MarketingAppBar {}

    @Menu(
        name = "MarketingAppSideBar",
        location = "component://marketing/widget/MarketingMenus.xml",
        title = "${uiLabelMap.MarketingManager}",
        extendsMenu = "CommonAppSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "MarketingAppBar")
        },
        alwaysExpandSelectedOrAncestor = "true"
    )
    public interface MarketingAppSideBar {}

    @Menu(
        name = "MarketingSideBar",
        location = "component://marketing/widget/MarketingMenus.xml",
        title = "${uiLabelMap.MarketingManager}",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "MarketingAppBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        items = {
            @MenuItem(name = "Tracking", subMenus = {@SubMenu(name = "TrackingCode", include = "component://marketing/widget/TrackingCodeMenus.xml#TrackingCodeSideBar")}),
            @MenuItem(name = "Segment", subMenus = {@SubMenu(name = "SegmentGroup", include = "component://marketing/widget/SegmentMenus.xml#SegmentGroupSideBar")}),
            @MenuItem(name = "Promotions", subMenus = {@SubMenu(name = "Promo", include = "component://product/widget/catalog/CatalogMenus.xml#PromoSideBar")}),
            @MenuItem(name = "MarketingCampaign", subMenus = {@SubMenu(name = "MarketingCampaign", include = "component://marketing/widget/MarketingCampaignMenus.xml#MarketingCampaignSideBar")})
        }
    )
    public interface MarketingSideBar {}

}
