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
public class FinanceFinAccountScreens {

    @Screen(name = "FindFinAccount", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindFinAccount")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindFinAccount")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "displayAdvancedSearch", fromField = "parameters.displayAdvancedSearch")
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", includeMenus = {
                            @IncludeMenu(name = "FinAccountSubTabBar", location = "component://accounting/widget/AccountingMenus.xml"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(sections = {
                            @SectionLeaf(condition = @Condition(type = Compare.class, params = {"displayAdvancedSearch", "equals", "true"
                        }), widgets = @WidgetsLeaf(includeForms = {
                            @IncludeForm(name = "FindFinAccounts", location = "component://accounting/widget/finance/FinAccountForms.xml"
                        )}), failWidgets = @WidgetsLeaf(includeForms = {
                            @IncludeForm(name = "QuickFindFinAccounts", location = "component://accounting/widget/finance/FinAccountForms.xml"
                        )}))})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFinAccounts", location = "component://accounting/widget/finance/FinAccountForms.xml"
                        )}))})})
        }
    )
    public interface FindFinAccount {}

    @Screen(name = "EditFinAccount", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditFinAccount")
    @Action(type = ActionType.SET, field = "finAccountId", fromField = "parameters.finAccountId")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccount", valueField = "finAccount")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.finAccount ? 'PageTitleEditFinAccount' : 'AccountingCreateNewFinAccount'}")
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"finAccountId"})}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(name = "EditFinAccountPanel", includeForms = {
                            @IncludeForm(name = "EditFinAccount", location = "component://accounting/widget/finance/FinAccountForms.xml"
                        )}, position = 1)}, containers = {
                            @Container(style = "button-bar", widgets = {
                                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingCreateNewFinAccount}", style = "${styles.link_nav} ${styles.action_add}", target = "EditFinAccount"
                            )}, position = 0)}), failWidgets = @InlineWidgets(screenlets = {
                                @Screenlet(name = "CreateFinAccountPanel", includeForms = {
                                    @IncludeForm(name = "EditFinAccount", location = "component://accounting/widget/finance/FinAccountForms.xml"
                                )})}))})
        }
    )
    public interface EditFinAccount {}

    @Screen(name = "EditFinAccountRoles", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFinAccountRole")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditFinAccountRoles")
    @Action(type = ActionType.SET, field = "finAccountId", fromField = "parameters.finAccountId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccount", valueField = "finAccount")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "roleTypeId", fromField = "parameters.roleTypeId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate")
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingEditFinAccountRoleFor}", style = "heading"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFinAccountRoles", location = "component://accounting/widget/finance/FinAccountForms.xml"
            )}, screenlets = {
                @Screenlet(name = "FinAccountRolePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddFinAccountRole", location = "component://accounting/widget/finance/FinAccountForms.xml"
                )}, position = 1)})
        }
    )
    public interface EditFinAccountRoles {}

    @Screen(name = "EditFinAccountTrans", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFinAccountTrans")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FinAccountTrans")
    @Action(type = ActionType.SET, field = "finAccountId", fromField = "parameters.finAccountId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccount", valueField = "finAccount")
    @Action(type = ActionType.SET, field = "finAccountTransId", fromField = "parameters.finAccountTransId")
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(name = "FinAccountTransPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddFinAccountTrans", location = "component://accounting/widget/finance/FinAccountForms.xml"
                )})})
        }
    )
    public interface EditFinAccountTrans {}

    @Screen(name = "EditFinAccountAuths", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFinAccountAuths")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditFinAccountAuths")
    @Action(type = ActionType.SET, field = "finAccountId", fromField = "parameters.finAccountId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccount", valueField = "finAccount")
    @Action(type = ActionType.SET, field = "finAccountAuthId", fromField = "parameters.finAccountAuthId")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFinAccountAuths", location = "component://accounting/widget/finance/FinAccountForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingEditFinAccountAuthorityFor}]", name = "FinAccountAuthsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddFinAccountAuth", location = "component://accounting/widget/finance/FinAccountForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditFinAccountAuths {}

    @Screen(name = "PaymentsDepositWithdraw", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingDepositOrWithdrawPayments")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "depositWithdraw")
    @Action(type = ActionType.SET, field = "finAccountId", fromField = "parameters.finAccountId")
    @Action(type = ActionType.SET, field = "paymentMethodTypeId", fromField = "parameters.paymentMethodTypeId")
    @Action(type = ActionType.SET, field = "cardType", fromField = "parameters.cardType")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "parameters.partyIdFrom")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccount", valueField = "finAccount")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/payment/DepositWithdrawPayments.groovy")
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingCreateNewDepositPayment}", style = "${styles.link_nav} ${styles.action_add}", target = "NewDepositPayment"
                ),
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingCreateNewWithdrawalPayment}", style = "${styles.link_nav} ${styles.action_add}", target = "NewWithdrawalPayment"
            )})}, decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "PaymentsDepositWithdraw", location = "component://accounting/widget/finance/FinAccountForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/payment/depositWithdrawPayments.ftl"
                    )}))})})
        }
    )
    public interface PaymentsDepositWithdraw {}

    @Screen(name = "FinAccountMain", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "finAccountMain")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFinAccounts")
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ListBankAccount", location = "component://accounting/widget/finance/FinAccountScreens.xml"
            )}, containers = {
                @Container(style = "button-bar", includeMenus = {
                    @IncludeMenu(name = "FinAccountSubTabBar", location = "component://accounting/widget/AccountingMenus.xml"
                )}, position = 0)})
        }
    )
    public interface FinAccountMain {}

    @Screen(name = "ListBankAccount", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "parameters.finAccountTypeId", value = "BANK_ACCOUNT")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.CommonList} ${uiLabelMap.AccountingBankAccount}", includeScreens = {@IncludeScreen(name = "FinAccountPortlets", location = "component://accounting/widget/finance/FinAccountScreens.xml")})}))
    public interface ListBankAccount {}

    @Screen(name = "FindDepositSlips", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFindDepositSlip")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findDepositSlips")
    @Action(type = ActionType.SET, field = "finAccountId", fromField = "parameters.finAccountId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccount", valueField = "finAccount")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PmtGrpMembrPaymentAndFinAcctTrans", list = "pmtGrpMembrPaymentAndFinAcctTransList", conditions = {@ConditionExpr(fieldName = "paymentGroupId", fromField = "parameters.paymentGroupId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "finAccountId", fromField = "parameters.finAccountId")})
    @Action(type = ActionType.SET, field = "paymentGroupIds", value = "${groovy:org.ofbiz.entity.util.EntityUtil.getFieldListFromEntityList(pmtGrpMembrPaymentAndFinAcctTransList, 'paymentGroupId', true);}", valueType = "List")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PaymentGroup", list = "paymentGroupList", conditions = {@ConditionExpr(fieldName = "paymentGroupId", operator = "in", fromField = "paymentGroupIds", ignoreIfEmpty = true), @ConditionExpr(fieldName = "paymentGroupTypeId", value = "BATCH_PAYMENT")})
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingCreateNewDepositSlip}", style = "${styles.link_nav} ${styles.action_add}", target = "NewDepositSlip"
                )})}, decorators = {
                    @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindDepositSlips", location = "component://accounting/widget/finance/FinAccountForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListDepositSlips", location = "component://accounting/widget/finance/FinAccountForms.xml"
                        )}))})})
        }
    )
    public interface FindDepositSlips {}

    @Screen(name = "EditDepositSlipAndMembers", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFindDepositSlip")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findDepositSlips")
    @Action(type = ActionType.SET, field = "finAccountId", fromField = "parameters.finAccountId")
    @Action(type = ActionType.SET, field = "paymentGroupId", fromField = "parameters.paymentGroupId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccount", valueField = "finAccount")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGroup", valueField = "paymentGroup")
    @Action(type = ActionType.ENTITY_AND, entityName = "PaymentGroupMember", list = "paymentGroupMemberList", filterByDate = true, fieldMaps = {@FieldMap(fieldName = "paymentGroupId")})
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingEditPaymentGroupFor}", includeForms = {
                    @IncludeForm(name = "EditDepositSlip", location = "component://accounting/widget/finance/FinAccountForms.xml"
                )}, position = 1),
                @Screenlet(title = "${uiLabelMap.AccountingEditPaymentGroupMemberFor}", includeForms = {
                    @IncludeForm(name = "ListDepositSlipMember", location = "component://accounting/widget/finance/FinAccountForms.xml"
                )}, position = 2)}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"paymentGroupMemberList"
                    })}), widgets = @InlineWidgets(containers = {
                        @Container(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingPrintDepositSlip}", style = "${styles.link_run_sys} ${styles.action_export}", target = "DepositSlip.pdf", targetWindow = "_BLANK"
                        )})}), position = 0)})
        }
    )
    public interface EditDepositSlipAndMembers {}

    @Screen(name = "NewDepositSlip", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingCreateNewDepositSlipForFinancialAccount")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findDepositSlips")
    @Action(type = ActionType.SET, field = "finAccountId", fromField = "parameters.finAccountId")
    @Action(type = ActionType.SET, field = "paymentMethodTypeId", fromField = "parameters.paymentMethodTypeId")
    @Action(type = ActionType.SET, field = "cardType", fromField = "parameters.cardType")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "parameters.partyIdFrom")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccount", valueField = "finAccount")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/ar/WEB-INF/actions/BatchPayments.groovy")
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindBatchPaymentsForDepositSlip", location = "component://accounting/widget/payments/PaymentForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/ar/payment/batchPayments.ftl"
                    )}))})})
        }
    )
    public interface NewDepositSlip {}

    @Screen(name = "FindFinAccountTrans", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindFinAccountTrans")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FinAccountTrans")
    @Action(type = ActionType.SERVICE, serviceName = "getFinAccountTransListAndTotals", resultMapName = "finAccountTransListAndTotals")
    @Action(type = ActionType.SET, field = "finAccountTransList", fromField = "finAccountTransListAndTotals.finAccountTransList", valueType = "List")
    @Action(type = ActionType.SET, field = "searchedNumberOfRecords", fromField = "finAccountTransListAndTotals.searchedNumberOfRecords", valueType = "Integer")
    @Action(type = ActionType.SET, field = "grandTotal", fromField = "finAccountTransListAndTotals.grandTotal", valueType = "BigDecimal")
    @Action(type = ActionType.SET, field = "createdGrandTotal", fromField = "finAccountTransListAndTotals.createdGrandTotal", valueType = "BigDecimal")
    @Action(type = ActionType.SET, field = "totalCreatedTransactions", fromField = "finAccountTransListAndTotals.totalCreatedTransactions", valueType = "Long")
    @Action(type = ActionType.SET, field = "approvedGrandTotal", fromField = "finAccountTransListAndTotals.approvedGrandTotal", valueType = "BigDecimal")
    @Action(type = ActionType.SET, field = "totalApprovedTransactions", fromField = "finAccountTransListAndTotals.totalApprovedTransactions", valueType = "Long")
    @Action(type = ActionType.SET, field = "createdApprovedGrandTotal", fromField = "finAccountTransListAndTotals.createdApprovedGrandTotal", valueType = "BigDecimal")
    @Action(type = ActionType.SET, field = "totalCreatedApprovedTransactions", fromField = "finAccountTransListAndTotals.totalCreatedApprovedTransactions", valueType = "Long")
    @Action(type = ActionType.SET, field = "finAccountId", fromField = "parameters.finAccountId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccount", valueField = "finAccount")
    @Action(type = ActionType.SET, field = "glReconciliationId", fromField = "parameters.glReconciliationId")
    @Action(type = ActionType.SET, field = "finAccountTransId", fromField = "parameters.finAccountTransId")
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonNew} ${uiLabelMap.CommonTransaction}", style = "${styles.link_nav} ${styles.action_add}", target = "EditFinAccountTrans"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"finAccount.finAccountTypeId", "equals", "BANK_ACCOUNT"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingBankReconciliation}", style = "${styles.link_nav}", target = "BankReconciliation"
                )}))}, decorators = {
                    @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindFinAccountTransactions", location = "component://accounting/widget/finance/FinAccountForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/finaccounttrans/FinAccountTrans.ftl"
                        )}))})})
        }
    )
    public interface FindFinAccountTrans {}

    @Screen(name = "BankReconciliation", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingBankReconciliation")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FinAccountTrans")
    @Action(type = ActionType.SET, field = "finAccountId", fromField = "parameters.finAccountId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccount", valueField = "finAccount")
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonNew} ${uiLabelMap.CommonTransaction}", style = "${styles.link_nav} ${styles.action_add}", target = "EditFinAccountTrans"
            ),
            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingReconcileFinAccountTransFor}", style = "heading"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"parameters.glReconciliationId"
                })}), actions = @Actions(value = {
                    @Action(type = ActionType.ENTITY_ONE, entityName = "GlReconciliation", valueField = "glReconciliation"
                ),
                @Action(type = ActionType.SET, field = "parameters.openingBalance", fromField = "glReconciliation.openingBalance"
            ),
            @Action(type = ActionType.SERVICE, serviceName = "getFinAccountTransListAndTotals", resultMapName = "finAccountTransListAndTotals"
            ),
            @Action(type = ActionType.SET, field = "finAccountTransList", fromField = "finAccountTransListAndTotals.finAccountTransList", valueType = "List"
            ),
            @Action(type = ActionType.SET, field = "createdApprovedGrandTotal", fromField = "finAccountTransListAndTotals.createdApprovedGrandTotal", valueType = "BigDecimal"
            ),
            @Action(type = ActionType.SET, field = "glReconciliationApprovedGrandTotal", fromField = "finAccountTransListAndTotals.glReconciliationApprovedGrandTotal", valueType = "BigDecimal"
            )}), widgets = @InlineWidgets(sections = {
                @SectionNested(actions = @Actions(value = {
                    @Action(type = ActionType.ENTITY_CONDITION, entityName = "GlAccountOrganizationAndClass", list = "glAccountOrgAndClassList", conditions = {
                        @ConditionExpr(fieldName = "organizationPartyId", fromField = "defaultOrganizationPartyId"
                    )}, orderBy = {"glAccountId"})}), widgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/finaccounttrans/FinAccountTrans.ftl", position = 1
                    )}, screenlets = {
                        @ScreenletNested(id = "FinAccountTransPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "FindBankReconciliationFinAcctTrans", location = "component://accounting/widget/finance/FinAccountForms.xml"
                
                    )}, position = 0)}))}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/finaccounttrans/FinAccountTrans.ftl", position = 1
                    )}, screenlets = {
                        @Screenlet(name = "FinAccountTransPanel", collapsible = true, includeForms = {
                            @IncludeForm(name = "FindBankReconciliationFinAcctTrans", location = "component://accounting/widget/finance/FinAccountForms.xml"
                        )}, position = 0)}))})
        }
    )
    public interface BankReconciliation {}

    @Screen(name = "FinAccountPortlets", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ListFinAccounts", location = "component://accounting/widget/finance/FinAccountForms.xml")}))
    public interface FinAccountPortlets {}

    @Screen(name = "NewDepositPayment", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "depositWithdraw")
    @Action(type = ActionType.SET, field = "finAccountId", fromField = "parameters.finAccountId")
    @Action(type = ActionType.SET, field = "statusId", value = "PMNT_RECEIVED")
    @Action(type = ActionType.SET, field = "parentTypeId", value = "RECEIPT")
    @Action(type = ActionType.SET, field = "finAccountTransTypeId", value = "DEPOSIT")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccount", valueField = "finAccount")
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingCreateNewDepositPaymentFor}", style = "heading"
            )}, screenlets = {
                @Screenlet(name = "EditDepositPaymentPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditDepositPayment", location = "component://accounting/widget/finance/FinAccountForms.xml"
                )})})
        }
    )
    public interface NewDepositPayment {}

    @Screen(name = "NewWithdrawalPayment", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "depositWithdraw")
    @Action(type = ActionType.SET, field = "finAccountId", fromField = "parameters.finAccountId")
    @Action(type = ActionType.SET, field = "statusId", value = "PMNT_SENT")
    @Action(type = ActionType.SET, field = "parentTypeId", value = "DISBURSEMENT")
    @Action(type = ActionType.SET, field = "finAccountTransTypeId", value = "WITHDRAWAL")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccount", valueField = "finAccount")
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingCreateNewWithdrawalPaymentFor}", name = "EditWithdrawalPaymentPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditWithdrawalPayment", location = "component://accounting/widget/finance/FinAccountForms.xml"
                )})})
        }
    )
    public interface NewWithdrawalPayment {}

    @Screen(name = "EditFinAccountReconciliations", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingEditFinAccountReconciliations")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindFinAccountReconciliations")
    @Action(type = ActionType.SET, field = "finAccountId", fromField = "parameters.finAccountId")
    @Action(type = ActionType.SET, field = "glReconciliationId", fromField = "parameters.glReconciliationId")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "${groovy:glReconciliationId==null?'NewFinAccountReconciliations':'EditFinAccountReconciliations'}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccount", valueField = "finAccount")
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "FinAccountReconciliationsTabBar", location = "component://accounting/widget/AccountingMenus.xml"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Empty.class, params = {"glReconciliationId"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(title = "${uiLabelMap.AccountingAddFinAccountReconciliations}", name = "AddFinAccountReconciliation", includeForms = {
                        @IncludeForm(name = "EditFinAccountReconciliation", location = "component://accounting/widget/finance/FinAccountForms.xml"
                    )})}), failWidgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.AccountingEditFinAccountReconciliations}", name = "EditFinAccountReconciliation", includeForms = {
                            @IncludeForm(name = "EditFinAccountReconciliation", location = "component://accounting/widget/finance/FinAccountForms.xml"
                        )})}))})
        }
    )
    public interface EditFinAccountReconciliations {}

    @Screen(name = "ViewGlReconciliationWithTransaction", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindFinAccountReconciliations")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingEditFinAccountReconciliations")
    @Action(type = ActionType.SET, field = "finAccountId", fromField = "parameters.finAccountId")
    @Action(type = ActionType.SET, field = "glReconciliationId", fromField = "parameters.glReconciliationId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccount", valueField = "finAccount")
    @Action(type = ActionType.ENTITY_ONE, entityName = "GlReconciliation", valueField = "currentGlReconciliation")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "GlReconciliation", list = "glReconciliationList", conditions = {@ConditionExpr(fieldName = "reconciledDate", operator = "less", fromField = "currentGlReconciliation.reconciledDate", ignoreIfEmpty = true), @ConditionExpr(fieldName = "glAccountId", operator = "equals", fromField = "finAccount.postToGlAccountId")}, orderBy = {"reconciledDate DESC"})
    @Action(type = ActionType.SET, field = "previousGlReconciliation", fromField = "glReconciliationList[0]")
    @Action(type = ActionType.SERVICE, serviceName = "getFinAccountTransListAndTotals", resultMapName = "transactionTotalAmount")
    @Action(type = ActionType.SET, field = "finAccountTransList", fromField = "transactionTotalAmount.finAccountTransList", valueType = "List")
    @Action(type = ActionType.SET, field = "finAccountTransIds", value = "${groovy:org.ofbiz.entity.util.EntityUtil.getFieldListFromEntityList(finAccountTransList, 'finAccountTransId', true);}", valueType = "List")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "FinAccountTrans", list = "finAccountTransactions", conditions = {@ConditionExpr(fieldName = "finAccountTransId", operator = "in", fromField = "finAccountTransIds"), @ConditionExpr(fieldName = "statusId", value = "FINACT_TRNS_CREATED")})
    @Action(type = ActionType.SERVICE, serviceName = "isGlReconciliationReconciled", resultMapName = "reconciledMap")
    @Action(type = ActionType.SET, field = "isReconciled", fromField = "reconciledMap.isReconciled")
    @Action(type = ActionType.SERVICE, serviceName = "getReconciliationClosingBalance", resultMapName = "currentRecnciliationClosingBalance")
    @Action(type = ActionType.SET, field = "currentClosingBalance", fromField = "currentRecnciliationClosingBalance.closingBalance")
    @Action(type = ActionType.SET, field = "previousGlReconciliationId", fromField = "previousGlReconciliation.glReconciliationId", defaultValue = "${glReconciliationId}")
    @Action(type = ActionType.SERVICE, serviceName = "getReconciliationClosingBalance", resultMapName = "previousReconciliationClosingBalance", fieldMaps = {@FieldMap(fieldName = "glReconciliationId", fromField = "previousGlReconciliationId")})
    @Action(type = ActionType.SET, field = "previousClosingBalance", fromField = "previousReconciliationClosingBalance.closingBalance")
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "FinAccountReconciliationsTabBar", location = "component://accounting/widget/AccountingMenus.xml"
            ),
            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingGlReconciliationFor}", style = "heading"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/finaccounttrans/GlReconciledFinAccountTrans.ftl"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingCurrentBankReconciliation}", includeForms = {
                    @IncludeForm(name = "FinAccountReconciliationBalance", location = "component://accounting/widget/finance/FinAccountForms.xml"
                )}, position = 1)})
        }
    )
    public interface ViewGlReconciliationWithTransaction {}

    @Screen(name = "FindFinAccountReconciliations", location = "component://accounting/widget/finance/FinAccountScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindFinAccountReconciliations")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindFinAccountReconciliations")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "Find")
    @Action(type = ActionType.SET, field = "finAccountId", fromField = "parameters.finAccountId")
    @Action(type = ActionType.SET, field = "glReconciliationId", fromField = "parameters.glReconciliationId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccount", valueField = "finAccount")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "FinAccountTrans", list = "finAccountTransList", conditions = {@ConditionExpr(fieldName = "finAccountId", operator = "equals", fromField = "finAccountId"), @ConditionExpr(fieldName = "glReconciliationId", operator = "not-equals", fromField = "nullField", ignoreIfEmpty = true), @ConditionExpr(fieldName = "glReconciliationId", operator = "equals", fromField = "glReconciliationId", ignoreIfEmpty = true)})
    @Action(type = ActionType.SET, field = "glReconciliationIds", value = "${groovy:org.ofbiz.entity.util.EntityUtil.getFieldListFromEntityList(finAccountTransList, 'glReconciliationId', true);}", valueType = "List")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "GlReconciliation", list = "glReconciliations", conditions = {@ConditionExpr(fieldName = "glReconciliationId", operator = "in", fromField = "glReconciliationIds"), @ConditionExpr(fieldName = "glAccountId", operator = "equals", fromField = "finAccount.postToGlAccountId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "glReconciliationName", operator = "equals", fromField = "parameters.glReconciliationName", ignoreIfEmpty = true), @ConditionExpr(fieldName = "description", operator = "equals", fromField = "parameters.description", ignoreIfEmpty = true), @ConditionExpr(fieldName = "statusId", operator = "equals", fromField = "parameters.statusId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "organizationPartyId", operator = "equals", fromField = "parameters.organizationPartyId", ignoreIfEmpty = true)})
    @DecoratorScreen(
        name = "CommonFinAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "FinAccountReconciliationsTabBar", location = "component://accounting/widget/AccountingMenus.xml"
            )}, decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindBankReconciliation", location = "component://accounting/widget/finance/FinAccountForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFinAccountReconciliations", location = "component://accounting/widget/finance/FinAccountForms.xml"
                    )}))})})
        }
    )
    public interface FindFinAccountReconciliations {}

}
