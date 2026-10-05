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
package com.ilscipio.scipio.product.widget;

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
public class CatalogSubscriptionMenus {

    @Menu(
        name = "EditSubscription",
        location = "component://product/widget/catalog/SubscriptionMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditSubscription", title = "${uiLabelMap.ProductSubscription}", link = @MenuLink(target = "EditSubscription", parameters = {@MenuParameter(paramName = "subscriptionId", fromField = "subscriptionId")})),
            @MenuItem(name = "EditSubscriptionAttributes", title = "${uiLabelMap.ProductSubscriptionAttributes}", link = @MenuLink(target = "EditSubscriptionAttributes", parameters = {@MenuParameter(paramName = "subscriptionId", fromField = "subscriptionId")})),
            @MenuItem(name = "EditSubscriptionCommEvent", title = "${uiLabelMap.ProductSubscriptionCommEvent}", link = @MenuLink(target = "EditSubscriptionCommEvent", parameters = {@MenuParameter(paramName = "subscriptionId", fromField = "subscriptionId")}))
        }
    )
    public interface EditSubscription {}

    @Menu(
        name = "EditSubscriptionResource",
        location = "component://product/widget/catalog/SubscriptionMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditSubscriptionResource", title = "${uiLabelMap.ProductSubscriptionResource}", link = @MenuLink(target = "EditSubscriptionResource", parameters = {@MenuParameter(paramName = "subscriptionResourceId", fromField = "subscriptionResourceId")})),
            @MenuItem(name = "EditSubscriptionResourceProducts", title = "${uiLabelMap.ProductProducts}", link = @MenuLink(target = "EditSubscriptionResourceProducts", parameters = {@MenuParameter(paramName = "subscriptionResourceId", fromField = "subscriptionResourceId")}))
        }
    )
    public interface EditSubscriptionResource {}

}
