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
public class ArArScreens {

    @Screen(name = "ARDashboard", location = "component://accounting/widget/ar/ArScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "armain")
    @Action(type = ActionType.SET, field = "invoiceTypeId", value = "SALES_INVOICE")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CommonOverview")
    @DecoratorScreen(
        name = "CommonArDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "organizationPartyId", defaultValue = "${defaultOrganizationPartyId}"
                ),
                @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/invoice/InvoiceReport.groovy"
            )}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ArInvoicesDueSoon", location = "component://accounting/widget/ar/ArScreens.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ArPastDueInvoices", location = "component://accounting/widget/ar/ArScreens.xml"
            )}))})
        }
    )
    public interface ARDashboard {}

    @Screen(name = "ArPastDueInvoices", location = "component://accounting/widget/ar/ArScreens.xml")
    @Action(type = ActionType.SET, field = "invoiceTypeId", value = "SALES_INVOICE")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "organizationPartyId", defaultValue = "${defaultOrganizationPartyId}", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/invoice/InvoiceReport.groovy")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "invoices", fromField = "PastDueInvoices")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"invoices"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.AccountingAccountsReceivable}", includeScreens = {@IncludeScreen(name = "ScipioInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml")})}))
    public interface ArPastDueInvoices {}

    @Screen(name = "ArInvoicesDueSoon", location = "component://accounting/widget/ar/ArScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "invoices", fromField = "InvoicesDueSoon")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"invoices"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.AccountingInvoicesDueSoon}: (${InvoicesDueSoonTotalAmount})", includeScreens = {@IncludeScreen(name = "ScipioInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml")})}))
    public interface ArInvoicesDueSoon {}

}
