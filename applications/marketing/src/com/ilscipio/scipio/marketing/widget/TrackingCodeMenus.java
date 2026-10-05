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
public class TrackingCodeMenus {

    @Menu(
        name = "TrackingCodeTabBar",
        location = "component://marketing/widget/TrackingCodeMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "TrackingCode",
        items = {
            @MenuItem(name = "TrackingCode", title = "${uiLabelMap.MarketingTrackingCode}", link = @MenuLink(target = "FindTrackingCode")),
            @MenuItem(name = "TrackingCodeOrder", title = "${uiLabelMap.MarketingTrackingCodeOrder}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"trackingCodeId"})}), link = @MenuLink(target = "FindTrackingCodeOrders", parameters = {@MenuParameter(paramName = "trackingCodeId", fromField = "trackingCodeId")})),
            @MenuItem(name = "TrackingCodeVisit", title = "${uiLabelMap.MarketingTrackingCodeVisit}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"trackingCodeId"})}), link = @MenuLink(target = "FindTrackingCodeVisits", parameters = {@MenuParameter(paramName = "trackingCodeId", fromField = "trackingCodeId")})),
            @MenuItem(name = "TrackingCodeType", title = "${uiLabelMap.MarketingTrackingCodeType}", link = @MenuLink(target = "FindTrackingCodeType"))
        }
    )
    public interface TrackingCodeTabBar {}

    @Menu(
        name = "TrackingCodeSideBar",
        location = "component://marketing/widget/TrackingCodeMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "TrackingCodeTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "TrackingCode",
        selectedMenuItemContextFieldName = "activeTrackingCodeSubMenuItem"
    )
    public interface TrackingCodeSideBar {}

}
