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
package com.ilscipio.scipio.accounting.widget;

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
public class AccountingPrintScreens {

    @Screen(name = "InvoicePDF", location = "component://accounting/widget/AccountingPrintScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingInvoice")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/invoice/EditInvoice.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/GetMyCompany.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/order/CompanyHeader.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Or.class, tree = {@ConditionNode(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"}), @ConditionNode(type = Compare.class, params = {"invoice.partyIdFrom", "equals", "${userLogin.partyId}"}), @ConditionNode(type = Compare.class, params = {"invoice.partyId", "equals", "${userLogin.partyId}"}), @ConditionNode(type = Compare.class, params = {"invoice.partyIdFrom", "equals", "${myCompanyId}"}), @ConditionNode(type = Compare.class, params = {"invoice.partyId", "equals", "${myCompanyId}"})})}), widgets = @Widgets(decorator = @DecoratorScreen(
        name = "FoReportDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "topLeft", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/invoice/invoiceReportContactMechs.fo.ftl", platform = "xsl-fo"
            )}),
            @DecoratorSection(name = "topRight", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CompanyLogo", location = "component://accounting/widget/AccountingPrintScreens.xml"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/invoice/invoiceReportHeaderInfo.fo.ftl", platform = "xsl-fo"
            )}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/invoice/invoiceReportItems.fo.ftl", platform = "xsl-fo"
            )}),
            @DecoratorSection(name = "footer", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/invoice/pdf/ScipioInvoiceFooter.fo.ftl", platform = "xsl-fo"
            )})
        }
    )), failWidgets = @Widgets(decorator = @DecoratorScreen(
        name = "FoReportDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/invoice/NoAccountingView.fo.ftl", platform = "xsl-fo"
            )})
        }
    )))
    public interface InvoicePDF {}

    @Screen(name = "PrintCheckPDF", location = "component://accounting/widget/AccountingPrintScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingPrintChecksPermissionError}", style = "common-msg-error-perm")}))
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingPrintChecks")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonNotImplementedSentence}")}))
    public interface PrintCheckPDF {}

    @Screen(name = "PrintInvoices", location = "component://accounting/widget/AccountingPrintScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "invoiceIds", fromField = "parameters.invoiceIds", valueType = "List")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/invoice/PrintInvoices.groovy")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://accounting/webapp/accounting/invoice/PrintInvoices.fo.ftl", platform = "xsl-fo")}))
    public interface PrintInvoices {}

    @Screen(name = "CommissionReportPdf", location = "component://accounting/widget/AccountingPrintScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/ap/WEB-INF/actions/invoices/CommissionReport.groovy")
    @DecoratorScreen(
        name = "FoReportDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "topLeft", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CompanyLogo", location = "component://order/widget/ordermgr/OrderPrintScreens.xml"
            )}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/ap/reports/CommissionReport.fo.ftl", platform = "xsl-fo"
            )})
        }
    )
    public interface CommissionReportPdf {}

    @Screen(name = "CompanyLogo", location = "component://accounting/widget/AccountingPrintScreens.xml")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://accounting/webapp/accounting/invoice/pdf/ScipioCompanyHeader.fo.ftl", platform = "xsl-fo")}))
    public interface CompanyLogo {}

}
