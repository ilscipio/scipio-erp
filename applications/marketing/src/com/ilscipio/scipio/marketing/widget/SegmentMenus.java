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
public class SegmentMenus {

    @Menu(
        name = "SegmentGroupTabBar",
        location = "component://marketing/widget/SegmentMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "SegmentGroup",
        items = {
            @MenuItem(name = "SegmentGroup", title = "${uiLabelMap.MarketingSegmentGroupSegmentGroup}", link = @MenuLink(target = "viewSegmentGroup", parameters = {@MenuParameter(paramName = "segmentGroupId", fromField = "segmentGroupId")})),
            @MenuItem(name = "SegmentGroupClassification", title = "${uiLabelMap.MarketingSegmentGroupSegmentGroupClassification}", link = @MenuLink(target = "listSegmentGroupClass", parameters = {@MenuParameter(paramName = "segmentGroupId", fromField = "segmentGroupId")})),
            @MenuItem(name = "SegmentGroupGeo", title = "${uiLabelMap.MarketingSegmentGroupSegmentGroupGeo}", link = @MenuLink(target = "listSegmentGroupGeo", parameters = {@MenuParameter(paramName = "segmentGroupId", fromField = "segmentGroupId")})),
            @MenuItem(name = "SegmentGroupRole", title = "${uiLabelMap.MarketingSegmentGroupSegmentGroupRole}", link = @MenuLink(target = "listSegmentGroupRole", parameters = {@MenuParameter(paramName = "segmentGroupId", fromField = "segmentGroupId")}))
        }
    )
    public interface SegmentGroupTabBar {}

    @Menu(
        name = "SegmentGroupSideBar",
        location = "component://marketing/widget/SegmentMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "SegmentGroupTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "PARENT-NOSUB"
    )
    public interface SegmentGroupSideBar {}

}
