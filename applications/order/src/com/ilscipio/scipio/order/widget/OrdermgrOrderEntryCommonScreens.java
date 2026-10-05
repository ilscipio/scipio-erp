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
public class OrdermgrOrderEntryCommonScreens {

    @Screen(name = "CommonOrderEntryBaseDecorator", location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "orderentry")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonOrderEntryBaseDecorator {}

    @Screen(name = "CommonOrderEntryDecorator", location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/SetShoppingCart.groovy")
    @DecoratorScreen(
        name = "CommonOrderEntryBaseDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}12 ${styles.grid_cell}", htmlTemplates = {
                        @HtmlTemplate(location = "component://order/webapp/ordermgr/entry/OrderEntryTabBar.ftl"
                    )})}),
                    @Container(style = "${styles.grid_row}", containers = {
                        @Container2(style = "${styles.grid_large}12 ${styles.grid_cell}", decoratorSectionIncludes = {
                            @DecoratorSectionInclude(name = "body")})})})
        }
    )
    public interface CommonOrderEntryDecorator {}

    @Screen(name = "CommonOrderCatalogDecorator", location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/SetShoppingCart.groovy")
    @DecoratorScreen(
        name = "CommonOrderEntryBaseDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "screenlet", htmlTemplates = {
                    @HtmlTemplate(location = "component://order/webapp/ordermgr/entry/OrderEntryCatalogTabBar.ftl"
                )}, containers = {
                    @Container2(style = "screenlet-body", decoratorSectionIncludes = {
                        @DecoratorSectionInclude(name = "body")})})})
        }
    )
    public interface CommonOrderCatalogDecorator {}

    @Screen(name = "leftbar", location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "orderHeaderInfo", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "orderShortcuts", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "choosecatalog", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "keywordsearchbox", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "sidedeepcategory", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "compareproductslist", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")}))
    public interface leftbar {}

    @Screen(name = "leftbarCatalog", location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "orderHeaderInfo", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "minicart", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "sidedeepcategory", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "compareproductslist", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")}))
    public interface leftbarCatalog {}

}
