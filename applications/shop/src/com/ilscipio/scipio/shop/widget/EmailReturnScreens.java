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
package com.ilscipio.scipio.shop.widget;

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
public class EmailReturnScreens {

    @Screen(name = "ReturnAccept", location = "component://shop/widget/EmailReturnScreens.xml")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/returnaccept.ftl", platform = "email"
            )})
        }
    )
    public interface ReturnAccept {}

    @Screen(name = "ReturnCancel", location = "component://shop/widget/EmailReturnScreens.xml")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/returncancel.ftl", platform = "email"
            )})
        }
    )
    public interface ReturnCancel {}

    @Screen(name = "ReturnComplete", location = "component://shop/widget/EmailReturnScreens.xml")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/returncomplete.ftl", platform = "email"
            )})
        }
    )
    public interface ReturnComplete {}

}
