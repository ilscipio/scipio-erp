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
public class OrdermgrOrderPrintScreens {

    @Screen(name = "OrderPDF", location = "component://order/widget/ordermgr/OrderPrintScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrder")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/OrderView.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/CompanyHeader.groovy")
    @DecoratorScreen(
        name = "FoReportDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "topLeft", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/orderReportContactMechs.fo.ftl", platform = "xsl-fo"
            )}),
            @DecoratorSection(name = "topRight", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CompanyLogo", location = "component://order/widget/ordermgr/OrderPrintScreens.xml"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/orderReportHeaderInfo.fo.ftl", platform = "xsl-fo"
            )}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/orderReportBody.fo.ftl", platform = "xsl-fo"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/orderReportConditions.fo.ftl", platform = "xsl-fo"
            )}),
            @DecoratorSection(name = "footer", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/pdf/ScipioOrderFooter.fo.ftl", platform = "xsl-fo"
            )})
        }
    )
    public interface OrderPDF {}

    @Screen(name = "CompanyLogo", location = "component://order/widget/ordermgr/OrderPrintScreens.xml")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://order/webapp/ordermgr/order/companyHeader.fo.ftl", platform = "xsl-fo")}))
    public interface CompanyLogo {}

    @Screen(name = "ReturnPDF", location = "component://order/widget/ordermgr/OrderPrintScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderReturn")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/return/ReturnHeader.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/return/ReturnItems.groovy")
    @DecoratorScreen(
        name = "FoReportDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "topLeft", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CompanyLogo", location = "component://order/widget/ordermgr/OrderPrintScreens.xml"
            )}),
            @DecoratorSection(name = "topRight", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/return/returnReportHeaderInfo.fo.ftl", platform = "xsl-fo"
            )}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/return/returnReportContactMechs.fo.ftl", platform = "xsl-fo"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/return/returnReportBody.fo.ftl", platform = "xsl-fo"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/return/returnReportConditions.fo.ftl", platform = "xsl-fo"
            )})
        }
    )
    public interface ReturnPDF {}

    @Screen(name = "ShipGroupsPDF", location = "component://order/widget/ordermgr/OrderPrintScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderShipGroups")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/ShipGroups.groovy")
    @DecoratorScreen(
        name = "FoReportDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "topLeft", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CompanyLogo", location = "component://order/widget/ordermgr/OrderPrintScreens.xml"
            )}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/shipGroups.fo.ftl", platform = "xsl-fo"
            )})
        }
    )
    public interface ShipGroupsPDF {}

}
