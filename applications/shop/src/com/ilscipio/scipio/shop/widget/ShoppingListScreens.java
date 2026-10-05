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
public class ShoppingListScreens {

    @Screen(name = "editShoppingList", location = "component://shop/widget/ShoppingListScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleShoppingList")
    @Action(type = ActionType.SET, field = "activeMainMenuItem", value = "Shopping List")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shoppinglist/EditShoppingList.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userHasAccount"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/shoppinglist/editShoppingList.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface editShoppingList {}

    @Screen(name = "showShoppingList", location = "component://shop/widget/ShoppingListScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"parameters.mainSubmitted"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shoppinglist/EditShoppingList.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = True.class, params = {"userHasAccount"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/shoppinglist/editShoppingList.ftl")}), failWidgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "p")}))
    public interface showShoppingList {}

}
