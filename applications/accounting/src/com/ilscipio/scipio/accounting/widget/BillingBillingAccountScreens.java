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
public class BillingBillingAccountScreens {

    @Screen(name = "FindBillingAccount", location = "component://accounting/widget/billing/BillingAccountScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindBillingAccount")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "billingaccount")
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Empty.class, params = {"parameters.partyId"
                })}), widgets = @InlineWidgets(decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonNew} ${uiLabelMap.CommonAccount}", style = "${styles.link_nav} ${styles.action_add}", target = "EditBillingAccount"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindBillingAccounts", location = "component://accounting/widget/billing/BillingAccountForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListBillingAccounts", location = "component://accounting/widget/billing/BillingAccountForms.xml"
                        )}))})), failWidgets = @InlineWidgets(sections = {
                            @SectionNested(actions = @Actions(value = {
                                @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/payment/BillingAccounts.groovy"
                            ),
                            @Action(type = ActionType.SET, field = "roleTypeId", value = "BILL_TO_CUSTOMER"
                        )}), widgets = @WidgetsForContainer(screenlets = {
                            @ScreenletNested(includeForms = {
                    @IncludeForm(name = "ListBillingAccountsByParty", location = "component://accounting/widget/billing/BillingAccountForms.xml"
                
                        )}, widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonNew} ", style = "${styles.link_nav} ${styles.action_add}", target = "EditBillingAccount"
                
                    )})}))}))})
        }
    )
    public interface FindBillingAccount {}

    @Screen(name = "EditBillingAccount", location = "component://accounting/widget/billing/BillingAccountScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditBillingAccount")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.SET, field = "billingAccountId", fromField = "parameters.billingAccountId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "BillingAccount", valueField = "billingAccount")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "roleTypeId", fromField = "parameters.roleTypeId")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.billingAccount ? 'PageTitleEditBillingAccount' : 'AccountingNewBillingAccount'}")
    @DecoratorScreen(
        name = "CommonBillingAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditBillingAccount", location = "component://accounting/widget/billing/BillingAccountForms.xml"
                )})})
        }
    )
    public interface EditBillingAccount {}

    @Screen(name = "EditBillingAccountRoles", location = "component://accounting/widget/billing/BillingAccountScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditBillingAccountRoles")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditBillingAccountRoles")
    @Action(type = ActionType.SET, field = "billingAccountId", fromField = "parameters.billingAccountId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "BillingAccount", valueField = "billingAccount")
    @DecoratorScreen(
        name = "CommonBillingAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddBillingAccountRoles}", includeForms = {
                    @IncludeForm(name = "AddBillingAccountRole", location = "component://accounting/widget/billing/BillingAccountForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleListBillingAccountRoles} - ${uiLabelMap.AccountingAccountId} ${billingAccount.billingAccountId}", includeForms = {
                    @IncludeForm(name = "ListBillingAccountRoles", location = "component://accounting/widget/billing/BillingAccountForms.xml"
                )})})
        }
    )
    public interface EditBillingAccountRoles {}

    @Screen(name = "EditBillingAccountTerms", location = "component://accounting/widget/billing/BillingAccountScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditBillingAccountTerms")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditBillingAccountTerms")
    @Action(type = ActionType.SET, field = "billingAccountId", fromField = "parameters.billingAccountId")
    @Action(type = ActionType.SET, field = "billingAccountTermId", fromField = "parameters.billingAccountTermId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "BillingAccount", valueField = "billingAccount")
    @DecoratorScreen(
        name = "CommonBillingAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddBillingAccountTerms}", includeForms = {
                    @IncludeForm(name = "EditBillingAccountTerms", location = "component://accounting/widget/billing/BillingAccountForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleListBillingAccountTerms} - ${uiLabelMap.AccountingAccountId} ${billingAccount.billingAccountId}", includeForms = {
                    @IncludeForm(name = "ListBillingAccountTerms", location = "component://accounting/widget/billing/BillingAccountForms.xml"
                )})})
        }
    )
    public interface EditBillingAccountTerms {}

    @Screen(name = "BillingAccountInvoices", location = "component://accounting/widget/billing/BillingAccountScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditBillingAccountInvoices")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "BillingAccountInvoices")
    @Action(type = ActionType.SET, field = "billingAccountId", fromField = "parameters.billingAccountId")
    @Action(type = ActionType.SET, field = "billingAccountTermId", fromField = "parameters.billingAccountTermId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "BillingAccount", valueField = "billingAccount")
    @DecoratorScreen(
        name = "CommonBillingAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingFindInvoices}", includeForms = {
                    @IncludeForm(name = "lookupInvoicesStatus", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleListBillingAccountInvoices} - ${uiLabelMap.AccountingAccountId} ${billingAccount.billingAccountId}", includeForms = {
                    @IncludeForm(name = "ListBillingAccountInvoices", location = "component://accounting/widget/billing/BillingAccountForms.xml"
                )})})
        }
    )
    public interface BillingAccountInvoices {}

    @Screen(name = "BillingAccountOrders", location = "component://accounting/widget/billing/BillingAccountScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditBillingAccountOrders")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "BillingAccountOrders")
    @Action(type = ActionType.SET, field = "billingAccountId", fromField = "parameters.billingAccountId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "BillingAccount", valueField = "billingAccount")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/order/BillingAccountOrders.groovy")
    @DecoratorScreen(
        name = "CommonBillingAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleListBillingAccountOrders} - ${uiLabelMap.AccountingAccountId} ${billingAccount.billingAccountId}", includeForms = {
                    @IncludeForm(name = "ListBillingAccountOrders", location = "component://accounting/widget/billing/BillingAccountForms.xml"
                )})})
        }
    )
    public interface BillingAccountOrders {}

    @Screen(name = "BillingAccountPayments", location = "component://accounting/widget/billing/BillingAccountScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditBillingAccountPayments")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "BillingAccountPayments")
    @Action(type = ActionType.SET, field = "billingAccountId", fromField = "parameters.billingAccountId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "BillingAccount", valueField = "billingAccount")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "BillingAccountAndRole", list = "billToCustomers", filterByDate = true, conditions = {@ConditionExpr(fieldName = "billingAccountId", fromField = "billingAccountId"), @ConditionExpr(fieldName = "roleTypeId", value = "BILL_TO_CUSTOMER")})
    @Action(type = ActionType.SET, field = "billToCustomer", fromField = "billToCustomers[0]")
    @DecoratorScreen(
        name = "CommonBillingAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddBillingAccountPayments}", includeForms = {
                    @IncludeForm(name = "CreateIncomingBillingAccountPayment", location = "component://accounting/widget/billing/BillingAccountForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleListBillingAccountPayments} - ${uiLabelMap.AccountingAccountId} ${billingAccount.billingAccountId}", includeForms = {
                    @IncludeForm(name = "ListBillingAccountPayments", location = "component://accounting/widget/billing/BillingAccountForms.xml"
                )})})
        }
    )
    public interface BillingAccountPayments {}

}
