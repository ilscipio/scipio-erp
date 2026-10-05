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
public class OrdermgrOrderReturnScreens {

    @Screen(name = "OrderFindReturn", location = "component://order/widget/ordermgr/OrderReturnScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindReturn")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "OrderFindReturn")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonReturnDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderCreateNewReturn}", style = "${styles.link_nav} ${styles.action_add}", target = "returnMain"
                )}, position = 0)}, screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "FindReturns", location = "component://order/widget/ordermgr/ReturnForms.xml"
                    ),
                    @IncludeForm(name = "ListReturns", location = "component://order/widget/ordermgr/ReturnForms.xml"
                )}, position = 1)})
        }
    )
    public interface OrderFindReturn {}

    @Screen(name = "OrderQuickReturn", location = "component://order/widget/ordermgr/OrderReturnScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindQuickReturn")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "OrderQuickReturn")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/return/QuickReturn.groovy")
    @DecoratorScreen(
        name = "CommonReturnDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/return/quickReturn.ftl"
            )})
        }
    )
    public interface OrderQuickReturn {}

    @Screen(name = "OrderReturnHeader", location = "component://order/widget/ordermgr/OrderReturnScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleReturnHeader")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "OrderReturnHeader")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/ordermgr-js/return.js", global = true)
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/return/ReturnHeader.groovy")
    @DecoratorScreen(
        name = "CommonReturnDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditReturn", location = "component://order/widget/ordermgr/ReturnForms.xml"
            )})
        }
    )
    public interface OrderReturnHeader {}

    @Screen(name = "OrderReturnList", location = "component://order/widget/ordermgr/OrderReturnScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleReturnList")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "OrderReturnList")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ReturnHeader", list = "returnList")
    @DecoratorScreen(
        name = "CommonReturnDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://order/webapp/ordermgr/return/returnList.ftl"
                )})})
        }
    )
    public interface OrderReturnList {}

    @Screen(name = "OrderReturnItems", location = "component://order/widget/ordermgr/OrderReturnScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleReturnItems")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "OrderReturnItems")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/return/ReturnItems.groovy")
    @DecoratorScreen(
        name = "CommonReturnDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/return/returnItems.ftl"
            )})
        }
    )
    public interface OrderReturnItems {}

    @Screen(name = "OrderReturnHistory", location = "component://order/widget/ordermgr/OrderReturnScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderReturnHistory")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "OrderReturnHistory")
    @Action(type = ActionType.SET, field = "returnId", fromField = "parameters.returnId")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/return/ReturnItems.groovy")
    @DecoratorScreen(
        name = "CommonReturnDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ReturnStatusHistory", location = "component://order/widget/ordermgr/OrderReturnScreens.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ReturnTypeHistory", location = "component://order/widget/ordermgr/OrderReturnScreens.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ReturnReasonHistory", location = "component://order/widget/ordermgr/OrderReturnScreens.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ReturnQuantityHistory", location = "component://order/widget/ordermgr/OrderReturnScreens.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ReceivedQuantityHistory", location = "component://order/widget/ordermgr/OrderReturnScreens.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ReturnPriceHistory", location = "component://order/widget/ordermgr/OrderReturnScreens.xml"
            )})
        }
    )
    public interface OrderReturnHistory {}

    @Screen(name = "ReturnStatusHistory", location = "component://order/widget/ordermgr/OrderReturnScreens.xml")
    @Action(type = ActionType.ENTITY_AND, entityName = "ReturnStatus", list = "orderReturnStatusHistories", fieldMaps = {@FieldMap(fieldName = "returnId")})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"orderReturnStatusHistories"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderOrderReturn} ${uiLabelMap.CommonStatusHistory}", name = "ReturnStatusHistoryPanel", collapsible = true, includeForms = {@IncludeForm(name = "ReturnStatusHistory", location = "component://order/widget/ordermgr/ReturnForms.xml")})}), failWidgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderOrderReturn} ${uiLabelMap.CommonStatusHistory}", labels = {@Label(text = "${uiLabelMap.OrderHistoryNotAvailable}")})}))
    public interface ReturnStatusHistory {}

    @Screen(name = "ReturnTypeHistory", location = "component://order/widget/ordermgr/OrderReturnScreens.xml")
    @Action(type = ActionType.SET, field = "entityField", value = "returnTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/return/ReturnHistory.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"orderReturnItemHistories"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderReturnTypeHistory}", name = "ReturnTypeHistoryPanel", collapsible = true, includeForms = {@IncludeForm(name = "ReturnTypeHistory", location = "component://order/widget/ordermgr/ReturnForms.xml")})}), failWidgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderReturnTypeHistory}", labels = {@Label(text = "${uiLabelMap.OrderHistoryNotAvailable}")})}))
    public interface ReturnTypeHistory {}

    @Screen(name = "ReturnReasonHistory", location = "component://order/widget/ordermgr/OrderReturnScreens.xml")
    @Action(type = ActionType.SET, field = "entityField", value = "returnReasonId")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/return/ReturnHistory.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"orderReturnItemHistories"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderReturnReasonHistory}", name = "ReturnReasonHistoryPanel", collapsible = true, includeForms = {@IncludeForm(name = "ReturnReasonHistory", location = "component://order/widget/ordermgr/ReturnForms.xml")})}), failWidgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderReturnReasonHistory}", labels = {@Label(text = "${uiLabelMap.OrderHistoryNotAvailable}")})}))
    public interface ReturnReasonHistory {}

    @Screen(name = "ReturnQuantityHistory", location = "component://order/widget/ordermgr/OrderReturnScreens.xml")
    @Action(type = ActionType.SET, field = "entityField", value = "returnQuantity")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/return/ReturnHistory.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"orderReturnItemHistories"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderReturnQtyHistory}", name = "ReturnQuantityHistoryPanel", collapsible = true, includeForms = {@IncludeForm(name = "ReturnAndReceivedQuantityHistory", location = "component://order/widget/ordermgr/ReturnForms.xml")})}), failWidgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderReturnQtyHistory}", labels = {@Label(text = "${uiLabelMap.OrderHistoryNotAvailable}")})}))
    public interface ReturnQuantityHistory {}

    @Screen(name = "ReceivedQuantityHistory", location = "component://order/widget/ordermgr/OrderReturnScreens.xml")
    @Action(type = ActionType.SET, field = "entityField", value = "receivedQuantity")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/return/ReturnHistory.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"orderReturnItemHistories"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderReceivedQtyHistory}", name = "ReceivedQuantityHistoryPanel", collapsible = true, includeForms = {@IncludeForm(name = "ReturnAndReceivedQuantityHistory", location = "component://order/widget/ordermgr/ReturnForms.xml")})}), failWidgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderReceivedQtyHistory}", labels = {@Label(text = "${uiLabelMap.OrderHistoryNotAvailable}")})}))
    public interface ReceivedQuantityHistory {}

    @Screen(name = "ReturnPriceHistory", location = "component://order/widget/ordermgr/OrderReturnScreens.xml")
    @Action(type = ActionType.SET, field = "entityField", value = "returnPrice")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/return/ReturnHistory.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"orderReturnItemHistories"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderReturnPriceHistory}", name = "ReturnPriceHistoryPanel", collapsible = true, includeForms = {@IncludeForm(name = "ReturnPriceHistory", location = "component://order/widget/ordermgr/ReturnForms.xml")})}), failWidgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderReturnPriceHistory}", labels = {@Label(text = "${uiLabelMap.OrderHistoryNotAvailable}")})}))
    public interface ReturnPriceHistory {}

}
