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
public class ApApScreens {

    @Screen(name = "APDashboard", location = "component://accounting/widget/ap/ApScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "apmain")
    @Action(type = ActionType.SET, field = "invoiceTypeId", value = "PURCHASE_INVOICE")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CommonOverview")
    @DecoratorScreen(
        name = "CommonApDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "organizationPartyId", defaultValue = "${defaultOrganizationPartyId}"
                ),
                @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/invoice/InvoiceReport.groovy"
            )}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ApInvoicesDueSoon", location = "component://accounting/widget/ap/ApScreens.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ApPastDueInvoices", location = "component://accounting/widget/ap/ApScreens.xml"
            )}))})
        }
    )
    public interface APDashboard {}

    @Screen(name = "ApPastDueInvoices", location = "component://accounting/widget/ap/ApScreens.xml")
    @Action(type = ActionType.SET, field = "invoiceTypeId", value = "PURCHASE_INVOICE")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "organizationPartyId", defaultValue = "${defaultOrganizationPartyId}", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/invoice/InvoiceReport.groovy")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "invoices", fromField = "PastDueInvoices")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"invoices"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.AccountingAccountsPayable}", includeScreens = {@IncludeScreen(name = "ScipioInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml")})}))
    public interface ApPastDueInvoices {}

    @Screen(name = "ApInvoicesDueSoon", location = "component://accounting/widget/ap/ApScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "invoices", fromField = "InvoicesDueSoon")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"invoices"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.AccountingInvoicesDueSoon}: (${InvoicesDueSoonTotalAmount})", includeScreens = {@IncludeScreen(name = "ScipioInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml")})}))
    public interface ApInvoicesDueSoon {}

    @Screen(name = "FindApPayments", location = "component://accounting/widget/ap/ApScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFindApPayments")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findApPayments")
    @DecoratorScreen(
        name = "CommonApPaymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonNew} ${uiLabelMap.CommonPayment}", style = "${styles.link_nav} ${styles.action_add}", target = "newPayment"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindApPayments", location = "component://accounting/widget/ap/VendorForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPayments", location = "component://accounting/widget/payments/PaymentForms.xml"
                        )}))})})
        }
    )
    public interface FindApPayments {}

    @Screen(name = "NewOutgoingPayment", location = "component://accounting/widget/ap/ApScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingNewPaymentOutgoing")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "newApPayment")
    @DecoratorScreen(
        name = "CommonApPaymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "NewPaymentOut", location = "component://accounting/widget/payments/PaymentForms.xml"
                )})})
        }
    )
    public interface NewOutgoingPayment {}

}
