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
public class PaymentsPaymentGroupScreens {

    @Screen(name = "FindPaymentGroup", location = "component://accounting/widget/payments/PaymentGroupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFindPaymentGroup")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PaymentGroup")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PaymentGroup", list = "paymentGroupList", conditions = {@ConditionExpr(fieldName = "paymentGroupId", fromField = "parameters.paymentGroupId", ignoreIfEmpty = true)})
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingCreateNewPaymentGroup}", style = "${styles.link_nav} ${styles.action_add}", target = "EditPaymentGroup"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindPaymentGroup", location = "component://accounting/widget/payments/PaymentGroupForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPaymentGroup", location = "component://accounting/widget/payments/PaymentGroupForms.xml"
                        )}))})})
        }
    )
    public interface FindPaymentGroup {}

    @Screen(name = "EditPaymentGroup", location = "component://accounting/widget/payments/PaymentGroupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingEditPaymentGroup")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPaymentGroup")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGroup", valueField = "paymentGroup")
    @Action(type = ActionType.SET, field = "display", value = "false", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonPaymentGroupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"paymentGroup"})}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.AccountingPaymentGroup}", includeForms = {
                            @IncludeForm(name = "EditPaymentGroup", location = "component://accounting/widget/payments/PaymentGroupForms.xml"
                        )})}), failWidgets = @InlineWidgets(screenlets = {
                            @Screenlet(title = "${uiLabelMap.AccountingNewPaymentGroup}", includeForms = {
                                @IncludeForm(name = "AddPaymentGroup", location = "component://accounting/widget/payments/PaymentGroupForms.xml"
                            )})}))})
        }
    )
    public interface EditPaymentGroup {}

    @Screen(name = "EditPaymentGroupMember", location = "component://accounting/widget/payments/PaymentGroupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingEditPaymentGroupMember")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPaymentGroupMember")
    @Action(type = ActionType.SET, field = "paymentGroupId", fromField = "parameters.paymentGroupId")
    @Action(type = ActionType.ENTITY_AND, entityName = "PaymentGroupMember", list = "paymentGroupMembers", fieldMaps = {@FieldMap(fieldName = "paymentGroupId")})
    @DecoratorScreen(
        name = "CommonPaymentGroupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAddPaymentGroupMember}", name = "addPaymentGroupMember", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPaymentGroupMember", location = "component://accounting/widget/payments/PaymentGroupForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.AccountingPaymentGroupMembers}", name = "listPaymentGroupMember", collapsible = true, includeForms = {
                    @IncludeForm(name = "ListPaymentGroupMember", location = "component://accounting/widget/payments/PaymentGroupForms.xml"
                )})})
        }
    )
    public interface EditPaymentGroupMember {}

    @Screen(name = "PaymentGroupOverview", location = "component://accounting/widget/payments/PaymentGroupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingPaymentGroupOverview")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PaymentGroupOverview")
    @Action(type = ActionType.SET, field = "paymentGroupId", fromField = "parameters.paymentGroupId")
    @Action(type = ActionType.SET, field = "display", value = "true", valueType = "Boolean")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGroup", valueField = "paymentGroup")
    @Action(type = ActionType.ENTITY_AND, entityName = "PaymentGroupMember", list = "paymentGroupMembers", fieldMaps = {@FieldMap(fieldName = "paymentGroupId")})
    @Action(type = ActionType.SERVICE, serviceName = "getPaymentGroupReconciliationId", resultMapName = "resultMap", fieldMaps = {@FieldMap(fieldName = "paymentGroupId")})
    @Action(type = ActionType.SET, field = "glReconciliationId", fromField = "resultMap.glReconciliationId")
    @DecoratorScreen(
        name = "CommonPaymentGroupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingPaymentGroupHeader} [${paymentGroupId}]", name = "editPaymentGroup", includeForms = {
                    @IncludeForm(name = "EditPaymentGroup", location = "component://accounting/widget/payments/PaymentGroupForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.AccountingPaymentGroupMembers}", name = "paymentGroupMembers", includeForms = {
                    @IncludeForm(name = "PaymentGroupMembers", location = "component://accounting/widget/payments/PaymentGroupForms.xml"
                )})})
        }
    )
    public interface PaymentGroupOverview {}

    @Screen(name = "DepositSlipPdf", location = "component://accounting/widget/payments/PaymentGroupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGroup", valueField = "paymentGroup")
    @Action(type = ActionType.SERVICE, serviceName = "getPayments", resultMapName = "getPaymentsMap")
    @Action(type = ActionType.SET, field = "payments", fromField = "getPaymentsMap.payments")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/reports/DepositSlip.fo.ftl", platform = "xsl-fo"
            )})
        }
    )
    public interface DepositSlipPdf {}

}
