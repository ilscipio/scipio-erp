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
public class OrdermgrOrderViewScreens {

    @Screen(name = "Main", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderManager")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "main")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}4 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "DashboardStatsOrderTotalDay", location = "component://order/widget/ordermgr/CommonWidgets.xml"
                    )}),
                    @Container2(style = "${styles.grid_large}4 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "DashboardStatsOrderTotalMonth", location = "component://order/widget/ordermgr/CommonWidgets.xml"
                    )}),
                    @Container2(style = "${styles.grid_large}4 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "DashboardStatsReturnReasonWeek", location = "component://order/widget/ordermgr/CommonWidgets.xml"
                    )})}),
                    @Container(style = "${styles.grid_row}", containers = {
                        @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                            @IncludeScreen(name = "ListSalesOrders", location = "component://order/widget/ordermgr/OrderViewScreens.xml"
                        )}),
                        @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                            @IncludeScreen(name = "BestSellingProducts", location = "component://product/widget/catalog/ProductScreens.xml"
                        )})})})
        }
    )
    public interface Main {}

    @Screen(name = "OrderHeaderView", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.OrderOrder}: ${parameters.orderId}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Summary")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/ordermgr-js/order.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/ordermgr-js/OrderShippingInfo.js", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/OrderView.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/OrderViewWebSecure.groovy")
    @DecoratorScreen(
        name = "CommonOrderDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"orderHeader"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_SCREEN, name = "orderHeader", position = 0
                    ),
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "orderFooter", position = 2
                )}, containers = {
                    @Container(style = "${styles.grid_row}", containers = {
                        @Container2(style = "${styles.grid_large}12 ${styles.grid_cell}", htmlTemplates = {
                            @HtmlTemplate(location = "component://order/webapp/ordermgr/order/orderitems.ftl"
                        )})}, position = 1)}), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.OrderNoOrderFound}: ${parameters.orderId}", style = "common-msg-error"
                        )}))})
        }
    )
    public interface OrderHeaderView {}

    @Screen(name = "orderHeader", location = "component://order/widget/ordermgr/OrderViewScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"orderHeader"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.OrderNoOrderFound}: [${orderId}]", style = "common-msg-error")}))
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "orderactions")}, containers = {@Container(style = "${styles.grid_row}", containers = {@Container2(style = "${styles.grid_large}4 ${styles.grid_cell}", includeScreens = {@IncludeScreen(name = "orderinfo", location = "component://order/widget/ordermgr/OrderViewScreens.xml"), @IncludeScreen(name = "orderterms", location = "component://order/widget/ordermgr/OrderViewScreens.xml")}), @Container2(style = "${styles.grid_large}4 ${styles.grid_cell}", includeScreens = {@IncludeScreen(name = "ordercontactinfo", location = "component://order/widget/ordermgr/OrderViewScreens.xml")}), @Container2(style = "${styles.grid_large}4 ${styles.grid_cell}", includeScreens = {@IncludeScreen(name = "orderpaymentinfo", location = "component://order/widget/ordermgr/OrderViewScreens.xml"), @IncludeScreen(name = "ordercustomerinfo", location = "component://order/widget/ordermgr/OrderViewScreens.xml"), @IncludeScreen(name = "projectAssoOrder", location = "component://order/widget/ordermgr/OrderViewScreens.xml")})})}))
    public interface orderHeader {}

    @Screen(name = "orderFooter", location = "component://order/widget/ordermgr/OrderViewScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"orderHeader"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.OrderNoOrderFound}: [${orderId}]", style = "common-msg-error")}))
    @Section(widgets = @Widgets(containers = {@Container(style = "${styles.grid_row}", containers = {@Container2(style = "${styles.grid_large}12 ${styles.grid_cell}", includeScreens = {@IncludeScreen(name = "OrderSalesReps", location = "component://order/widget/ordermgr/OrderViewScreens.xml", position = 2)}, htmlTemplates = {@HtmlTemplate(location = "component://order/webapp/ordermgr/order/ordernotes.ftl", position = 0), @HtmlTemplate(location = "component://order/webapp/ordermgr/order/transitions.ftl", position = 1)})})}))
    public interface orderFooter {}

    @Screen(name = "orderinfo", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/orderinfo.ftl")}))
    public interface orderinfo {}

    @Screen(name = "orderterms", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/orderterms.ftl")}))
    public interface orderterms {}

    @Screen(name = "orderpaymentinfo", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/orderpaymentinfo.ftl")}))
    public interface orderpaymentinfo {}

    @Screen(name = "ordercustomerinfo", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/OrderCustomerView.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/ordercustomerinfo.ftl")}))
    public interface ordercustomerinfo {}

    @Screen(name = "projectAssoOrder", location = "component://order/widget/ordermgr/OrderViewScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"PROJECTMGR", "_VIEW"})}))
    @Action(type = ActionType.SET, field = "orderId", fromField = "parameters.orderId")
    @Action(type = ActionType.ENTITY_AND, entityName = "OrderHeaderAndWorkEffort", list = "listProjectAssoOrder", fieldMaps = {@FieldMap(fieldName = "orderId", fromField = "orderId"), @FieldMap(fieldName = "workEffortTypeId", value = "PROJECT")})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"listProjectAssoOrder"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PageTitleProjectInformation}", includeForms = {@IncludeForm(name = "projectAssoOrder", location = "component://projectmgr/widget/forms/ProjectForms.xml")})}))
    public interface projectAssoOrder {}

    @Screen(name = "ordercontactinfo", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/ordercontactinfo.ftl")}))
    public interface ordercontactinfo {}

    @Screen(name = "ordershippinginfo", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/ordershippinginfo.ftl")}))
    public interface ordershippinginfo {}

    @Screen(name = "orderactions", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/orderactions.ftl")}))
    public interface orderactions {}

    @Screen(name = "OrderSalesReps", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/OrderSalesReps.ftl")}))
    public interface OrderSalesReps {}

    @Screen(name = "OrderHeaderListView", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderLookupOrder")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "orderlist")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.filterDate", valueType = "Timestamp")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/OrderList.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/FilterOrderList.groovy")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/orderlist.ftl"
            )})
        }
    )
    public interface OrderHeaderListView {}

    @Screen(name = "OrderItemEdit", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderEditItems")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Summary")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/OrderView.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/OrderViewWebSecure.groovy")
    @DecoratorScreen(
        name = "CommonOrderDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "orderHeader", location = "component://order/widget/ordermgr/OrderViewScreens.xml"
            ),
            @Widget(type = WidgetType.CONTAINER, style = "clear"),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/editorderitems.ftl"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/appendorderitem.ftl"
            )}, containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderBackToOrder}", style = "${styles.link_nav_cancel}", target = "orderview?orderId=${orderId}"
                )}, position = 0)})
        }
    )
    public interface OrderItemEdit {}

    @Screen(name = "OrderFindOrder", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderFindOrder")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "OrderFindOrder")
    @Action(type = ActionType.SET, field = "rsleArgs", valueType = "NewMap")
    @Action(type = ActionType.SET, field = "rsleArgs.serviceName", value = "findOrders")
    @Action(type = ActionType.SET, field = "rsleArgs.doExec", value = "${groovy: (parameters.doFindQuery == 'Y' || (parameters.doFindQuery != 'Y' && context.requestMethod == 'POST'))}", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/runServiceLikeEvent.groovy")
    @Action(type = ActionType.SET, field = "isFindQueryExec", fromField = "rsleRes.isExec", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "isFindQueryError", fromField = "rsleRes.isError", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "isFindQueryInputError", fromField = "rsleRes.isServiceFailure", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/FindOrders.groovy")
    @Action(type = ActionType.SET, field = "asm_multipleSelectForm", value = "lookuporder")
    @Action(type = ActionType.SET, field = "asm_multipleSelect", value = "roleTypeId")
    @Action(type = ActionType.SET, field = "asm_formSize", value = "1000")
    @Action(type = ActionType.SET, field = "asm_asmListItemPercentOfForm", value = "95")
    @Action(type = ActionType.SET, field = "asm_sortable", value = "false")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "asm_title", value = "${uiLabelMap.OrderPartySelectRoleForParty}")
    @DecoratorScreen(
        name = "CommonOrderDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/setMultipleSelectJs.ftl"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/findOrders.ftl"
            )})
        }
    )
    public interface OrderFindOrder {}

    @Screen(name = "OrderNewNote", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderAddNote")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "OrderNewNote")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/NewNote.groovy")
    @DecoratorScreen(
        name = "CommonOrderDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderBackToOrder}", style = "${styles.link_nav_cancel}", target = "orderview?orderId=${orderId}"
                )}, position = 0)}, screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "OrderNewNote", location = "component://order/widget/ordermgr/OrderForms.xml"
                    )}, position = 1)})
        }
    )
    public interface OrderNewNote {}

    @Screen(name = "OrderDeliveryScheduleInfo", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderViewEditDeliveryScheduleInfo")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "OrderDeliveryScheduleInfo")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/OrderDeliveryScheduleInfo.groovy")
    @DecoratorScreen(
        name = "CommonOrderDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/OrderDeliveryScheduleInfo.ftl"
            )})
        }
    )
    public interface OrderDeliveryScheduleInfo {}

    @Screen(name = "OrderStats", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderStatisticsPage")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "stats")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/OrderStats.groovy")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "DashboardStatsOrderTotal", location = "component://order/widget/ordermgr/CommonWidgets.xml"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/orderstats.ftl"
            )})
        }
    )
    public interface OrderStats {}

    @Screen(name = "OrderReceivePayment", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderReceiveOfflinePayments")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "OrderReceivePayment")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/ReceivePayment.groovy")
    @Action(type = ActionType.ENTITY_AND, entityName = "OrderRole", list = "orderRoles", fieldMaps = {@FieldMap(fieldName = "orderId", value = "${parameters.orderId}"), @FieldMap(fieldName = "roleTypeId", value = "BILL_FROM_VENDOR")})
    @Action(type = ActionType.ENTITY_AND, entityName = "PaymentMethod", list = "paymentMethods", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "orderRoles[0].partyId")})
    @DecoratorScreen(
        name = "CommonOrderDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/receivepayment.ftl"
            )})
        }
    )
    public interface OrderReceivePayment {}

    @Screen(name = "ViewImage", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderViewImage")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/ViewImage.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/viewimage.ftl")}))
    public interface ViewImage {}

    @Screen(name = "SendOrderConfirmation", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderSendConfirmationEmail")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "SendOrderConfirmation")
    @Action(type = ActionType.SET, field = "emailType", value = "PRDS_ODR_CONFIRM")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/SendConfirmationEmail.groovy")
    @DecoratorScreen(
        name = "CommonOrderDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/sendconfirmationemail.ftl"
            )})
        }
    )
    public interface SendOrderConfirmation {}

    @Screen(name = "SendOrderCompletion", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderSendConfirmationEmail")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "SendOrderCompletion")
    @Action(type = ActionType.SET, field = "emailType", value = "PRDS_ODR_COMPLETE")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/SendConfirmationEmail.groovy")
    @DecoratorScreen(
        name = "CommonOrderDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/sendconfirmationemail.ftl"
            )})
        }
    )
    public interface SendOrderCompletion {}

    @Screen(name = "ListOrderTerms", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListOrderTerms")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderTerms")
    @Action(type = ActionType.ENTITY_AND, entityName = "OrderTerm", list = "orderTerms", fieldMaps = {@FieldMap(fieldName = "orderId", fromField = "parameters.orderId")})
    @DecoratorScreen(
        name = "CommonOrderDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListOrderTerms", location = "component://order/widget/ordermgr/OrderForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonAdd} ${uiLabelMap.OrderOrderTerms}", name = "AddOrderTermPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddOrderTerm", location = "component://order/widget/ordermgr/OrderForms.xml"
                )}, position = 0)})
        }
    )
    public interface ListOrderTerms {}

    @Screen(name = "OrderHistory", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "OrderHistory")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/OrderHistory.groovy")
    @DecoratorScreen(
        name = "CommonOrderDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.OrderOrderHistory} #${orderId}", style = "heading"
            )}, screenlets = {
                @Screenlet(includeScreens = {
                    @IncludeScreen(name = "OrderShipmentMethodHistory", location = "component://order/widget/ordermgr/OrderViewScreens.xml", position = 1
                ),
                @IncludeScreen(name = "OrderUnitPriceHistory", location = "component://order/widget/ordermgr/OrderViewScreens.xml", position = 2
            ),
            @IncludeScreen(name = "OrderQuantityHistory", location = "component://order/widget/ordermgr/OrderViewScreens.xml", position = 3
            )}, containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderBackToOrder}", style = "${styles.link_nav_cancel}", target = "orderview?orderId=${orderId}"
                )}, position = 0)})})
        }
    )
    public interface OrderHistory {}

    @Screen(name = "OrderShipping", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "OrderShipping")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.OrderShipmentInformation} ${orderId}")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/OrderView.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/OrderViewWebSecure.groovy")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScriptsFooter[]", value = "/ordermgr-js/OrderShippingInfo.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScriptsFooter[]", value = "/ordermgr-js/geoAutoCompleter.js", global = true)
    @DecoratorScreen(
        name = "CommonOrderDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "OrderShippingSubTabBar", location = "component://order/widget/ordermgr/OrderMenus.xml"
            )}, screenlets = {
                @Screenlet(includeScreens = {
                    @IncludeScreen(name = "ordershippinginfo", location = "component://order/widget/ordermgr/OrderViewScreens.xml"
                )})})
        }
    )
    public interface OrderShipping {}

    @Screen(name = "OrderShipmentMethodHistory", location = "component://order/widget/ordermgr/OrderViewScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"orderShipmentHistories"})}))
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderShipmentMethodHistory}", includeForms = {@IncludeForm(name = "OrderShipmentMethodHistory", location = "component://order/widget/ordermgr/OrderForms.xml")})}))
    public interface OrderShipmentMethodHistory {}

    @Screen(name = "OrderUnitPriceHistory", location = "component://order/widget/ordermgr/OrderViewScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"orderUnitPriceHistories"})}))
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderUnitPriceHistory}", includeForms = {@IncludeForm(name = "OrderUnitPriceHistory", location = "component://order/widget/ordermgr/OrderForms.xml")})}))
    public interface OrderUnitPriceHistory {}

    @Screen(name = "OrderQuantityHistory", location = "component://order/widget/ordermgr/OrderViewScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"orderQuantityHistories"})}))
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderQuantityHistory}", includeForms = {@IncludeForm(name = "OrderQuantityHistory", location = "component://order/widget/ordermgr/OrderForms.xml")})}))
    public interface OrderQuantityHistory {}

    @Screen(name = "ListCustomerOrders", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "MyPortalUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "partyId", fromField = "userLogin.partyId")
    @Action(type = ActionType.SET, field = "statusId", fromField = "statusId")
    @Action(type = ActionType.SET, field = "roleTypeId", fromField = "roleTypeId")
    @Action(type = ActionType.SET, field = "screenletTitle", fromField = "uiLabelMap.MyPortalMyOrders")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${screenletTitle} ${partyId} ${statusId}", includeForms = {@IncludeForm(name = "ListCustomerOrders", location = "component://order/widget/ordermgr/OrderForms.xml")})}))
    public interface ListCustomerOrders {}

    @Screen(name = "ListSalesOrders", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "fromDate", value = "${nowTimestamp}", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "intervalPeriod", value = "month", valueType = "String")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/OrderList.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ORDERMGR", "_VIEW"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/OrderListByDate.ftl")}), failWidgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.OrderViewPermissionError}", style = "common-msg-error-perm")}))
    public interface ListSalesOrders {}

    @Screen(name = "ListPurchaseOrders", location = "component://order/widget/ordermgr/OrderViewScreens.xml")
    @Action(type = ActionType.SET, field = "partyId", fromField = "userLogin.partyId")
    @Action(type = ActionType.SET, field = "roleTypeId", value = "SUPPLIER_AGENT")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ORDERMGR", "_VIEW"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderPurchaseOrder}", includeForms = {@IncludeForm(name = "ListPurchaseOrders", location = "component://order/widget/ordermgr/OrderForms.xml")})}), failWidgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.OrderViewPermissionError}", style = "common-msg-error-perm")}))
    public interface ListPurchaseOrders {}

}
