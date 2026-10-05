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
package com.ilscipio.scipio.order.widget;

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
public class OrdermgrOrderHeaderScreens {

    @Screen(name = "CommonOrderHeaderDecorator", location = "component://order/widget/ordermgr/OrderHeaderScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "findorders")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${orderHeader.orderId}", style = "heading"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonOrderHeaderDecorator {}

    @Screen(name = "EditOrderHeader", location = "component://order/widget/ordermgr/OrderHeaderScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditOrderHeader")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditOrderHeader")
    @Action(type = ActionType.SET, field = "orderId", fromField = "parameters.orderId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "OrderHeader", valueField = "orderHeader")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap")
    @DecoratorScreen(
        name = "CommonOrderHeaderDecorator",
        location = "component://order/widget/ordermgr/OrderHeaderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditOrderHeader", location = "component://order/widget/ordermgr/OrderForms.xml"
                )})})
        }
    )
    public interface EditOrderHeader {}

    @Screen(name = "ListOrderHeaders", location = "component://order/widget/ordermgr/OrderHeaderScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListOrderHeaders")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditOrderHeader")
    @Action(type = ActionType.SET, field = "orderId", fromField = "parameters.orderId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "OrderHeader", valueField = "OrderHeader")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap")
    @DecoratorScreen(
        name = "CommonOrderHeaderDecorator",
        location = "component://order/widget/ordermgr/OrderHeaderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListOrderHeaders", location = "component://order/widget/ordermgr/OrderForms.xml"
                )})})
        }
    )
    public interface ListOrderHeaders {}

}
