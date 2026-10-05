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
public class PaymentsPaymentScreens {

    @Screen(name = "FindPayments", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindPayment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findPayments")
    @DecoratorScreen(
        name = "CommonPaymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "PaymentsSubTabBar", location = "component://accounting/widget/AccountingMenus.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindPayments", location = "component://accounting/widget/payments/PaymentForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPayments", location = "component://accounting/widget/payments/PaymentForms.xml"
                    )}))})})
        }
    )
    public interface FindPayments {}

    @Screen(name = "ListPayments", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "noConditionFind", value = "Y")
    @Action(type = ActionType.SET, field = "parameters.statusId", fromField = "statusId")
    @Action(type = ActionType.SET, field = "isReduced", fromField = "isReduced", valueType = "Boolean", defaultValue = "false")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.AccountingRecentPayments}", includeForms = {@IncludeForm(name = "ListPayments", location = "component://accounting/widget/payments/PaymentForms.xml")})}))
    public interface ListPayments {}

    @Screen(name = "NewPayment", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingNewPayment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "newPayment")
    @Action(type = ActionType.SET, field = "paymentId", fromField = "parameters.paymentId")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @DecoratorScreen(
        name = "CommonPaymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingNewPaymentIncoming}", includeForms = {
                    @IncludeForm(name = "NewPaymentIn", location = "component://accounting/widget/payments/PaymentForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.AccountingNewPaymentOutgoing}", includeForms = {
                    @IncludeForm(name = "NewPaymentOut", location = "component://accounting/widget/payments/PaymentForms.xml"
                )})})
        }
    )
    public interface NewPayment {}

    @Screen(name = "EditPayment", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditPayment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editPayment")
    @Action(type = ActionType.SET, field = "paymentId", fromField = "parameters.paymentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Payment", valueField = "payment", fieldMaps = {@FieldMap(fieldName = "paymentId", fromField = "parameters.paymentId")})
    @DecoratorScreen(
        name = "CommonPaymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"payment"})}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.AccountingInvoiceHeaderDetailedInformation}", includeForms = {
                            @IncludeForm(name = "EditPayment", location = "component://accounting/widget/payments/PaymentForms.xml"
                        )})}), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingPaymentNotFound}", style = "common-msg-error"
                        )}))})
        }
    )
    public interface EditPayment {}

    @Screen(name = "EditPaymentApplications", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListPaymentApplications")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editPaymentApplications")
    @Action(type = ActionType.SET, field = "paymentId", fromField = "parameters.paymentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Payment", valueField = "payment")
    @Action(type = ActionType.SET, field = "appliedAmount", value = "${groovy:                     import java.text.NumberFormat;                     return(NumberFormat.getNumberInstance(context.get(\"locale\")).format(org.ofbiz.accounting.payment.PaymentWorker.getPaymentApplied(payment)));}", valueType = "String")
    @Action(type = ActionType.SET, field = "notAppliedAmount", value = "${groovy:org.ofbiz.accounting.payment.PaymentWorker.getPaymentNotApplied(payment)}", valueType = "BigDecimal")
    @Action(type = ActionType.SET, field = "notAppliedAmountStr", value = "${groovy:                     import java.text.NumberFormat;                     return(NumberFormat.getCurrencyInstance(context.get(\"locale\")).format(org.ofbiz.accounting.payment.PaymentWorker.getPaymentNotApplied(payment)));}", valueType = "String")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/payment/ListNotAppliedInvoices.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/payment/ListNotAppliedPayments.groovy")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "partyNameViewTo", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "payment.partyIdTo")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "partyNameViewFrom", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "payment.partyIdFrom")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PaymentApplication", list = "paymentApplications", conditions = {@ConditionExpr(fieldName = "paymentId", operator = "equals", value = "${paymentId}")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PaymentApplication", list = "paymentApplicationsInv", conditions = {@ConditionExpr(fieldName = "paymentId", operator = "equals", value = "${paymentId}"), @ConditionExpr(fieldName = "invoiceId", operator = "not-equals", fromField = "null")}, orderBy = {"invoiceId", "invoiceItemSeqId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PaymentApplication", list = "paymentApplicationsPay", conditions = {@ConditionExpr(fieldName = "paymentId", operator = "equals", fromField = "paymentId"), @ConditionExpr(fieldName = "toPaymentId", operator = "not-equals", fromField = "nullField")}, orderBy = {"toPaymentId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PaymentApplication", list = "paymentApplicationsBil", conditions = {@ConditionExpr(fieldName = "paymentId", fromField = "paymentId"), @ConditionExpr(fieldName = "billingAccountId", operator = "not-equals", fromField = "nullField")}, orderBy = {"billingAccountId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PaymentApplication", list = "paymentApplicationsTax", conditions = {@ConditionExpr(fieldName = "paymentId", fromField = "paymentId"), @ConditionExpr(fieldName = "taxAuthGeoId", operator = "not-equals", fromField = "nullField")}, orderBy = {"taxAuthGeoId"})
    @DecoratorScreen(
        name = "CommonPaymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Empty.class, params = {"paymentApplications"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingPayment} ${uiLabelMap.AccountingApplications}", style = "heading"
                )}, containers = {
                    @Container(labels = {
                        @Label(text = "${uiLabelMap.CommonAmount} ${uiLabelMap.CommonTotal} ${payment.amount?currency(${payment.currencyUomId})} ${uiLabelMap.AccountingAmountNotApplied} ${notAppliedAmount?currency(${payment.currencyUomId})}"
                    )}),
                    @Container(labels = {
                        @Label(text = "${uiLabelMap.AccountingNoPaymentsApplicationsfound}"
                    )})}), failWidgets = @InlineWidgets(sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                            @Condition(type = Empty.class, params = {"paymentApplicationsInv"
                        })}), widgets = @WidgetsForContainer(screenlets = {
                            @ScreenletNested(title = "${uiLabelMap.AccountingPayment} ${uiLabelMap.AccountingApplications}", includeForms = {
                    @IncludeForm(name = "editPaymentApplicationsInv", location = "component://accounting/widget/payments/PaymentForms.xml"
                
                        )})})),
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                            @OrCondition(ifNotEmpty = {"paymentApplicationsPay", "paymentApplicationsBil", "paymentApplicationsTax"
                        })}), widgets = @WidgetsForContainer(screenlets = {
                            @ScreenletNested(title = "${uiLabelMap.CommonAmount} ${uiLabelMap.CommonTotal} ${payment.amount?currency(${payment.currencyUomId})} ${uiLabelMap.AccountingAmountNotApplied} ${notAppliedAmount?currency(${payment.currencyUomId})}", sections = {
                    @SectionLeaf(condition = @Condition(type = NotEmpty.class, params = {"paymentApplicationsPay"
                
                        }), widgets = @WidgetsLeaf(includeForms = {
                    @IncludeForm(name = "editPaymentApplicationsPay", location = "component://accounting/widget/payments/PaymentForms.xml"
                
                    )})),
                @SectionLeaf(condition = @Condition(type = NotEmpty.class, params = {"paymentApplicationsBil"
                
                }), widgets = @WidgetsLeaf(includeForms = {
                    @IncludeForm(name = "editPaymentApplicationsBil", location = "component://accounting/widget/payments/PaymentForms.xml"
                
            )})),
                @SectionLeaf(condition = @Condition(type = NotEmpty.class, params = {"paymentApplicationsTax"
                
            }), widgets = @WidgetsLeaf(includeForms = {
                    @IncludeForm(name = "editPaymentApplicationsTax", location = "component://accounting/widget/payments/PaymentForms.xml"
                
            )}))})}))})),
            @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                @Condition(type = Compare.class, params = {"notAppliedAmount", "greater", "0.00", "BigDecimal"
            })}), widgets = @InlineWidgets(screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingApplyPaymentoTo}", includeForms = {
                    @IncludeForm(name = "addPaymentApplication", location = "component://accounting/widget/payments/PaymentForms.xml"
                )}, position = 2)}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                        @OrCondition(ifNotEmpty = {"invoices", "invoicesOtherCurrency"
                    })}), widgets = @WidgetsForContainer(screenlets = {
                        @ScreenletNested(title = "${uiLabelMap.AccountingListInvoicesNotYetApplied}", containers = {
                    @ContainerInScreenlet(labels = {
                        @Label(text = "${uiLabelMap.CommonFrom} ${partyNameViewTo.groupName}${partyNameViewTo.lastName},${partyNameViewTo.firstName} ${partyNameViewTo.middleName}[${payment.partyIdTo}]", style = "p"
                    
                    ),
                    @Label(text = "${uiLabelMap.CommonTo} ${partyNameViewFrom.groupName}${partyNameViewFrom.lastName},${partyNameViewFrom.firstName} ${partyNameViewFrom.middleName} [${payment.partyIdFrom}]", style = "p"
                
                )}, position = 0)}, sections = {
                    @SectionLeaf(condition = @Condition(type = NotEmpty.class, params = {"invoices"
                
            }), widgets = @WidgetsLeaf(includeForms = {
                    @IncludeForm(name = "listInvoicesNotApplied", location = "component://accounting/widget/payments/PaymentForms.xml"
                
            )}), position = 1),
                @SectionLeaf(condition = @Condition(type = NotEmpty.class, params = {"invoicesOtherCurrency"
                
            }), widgets = @WidgetsLeaf(includeForms = {
                    @IncludeForm(name = "listInvoicesNotAppliedOtherCurrency", location = "component://accounting/widget/payments/PaymentForms.xml", position = 1
                
            )}, labels = {
                    @Label(text = "${uiLabelMap.FormFieldTitle_otherCurrency}", style = "heading", position = 0
                
            )}), position = 2)})}), position = 0),
            @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                @Condition(type = Empty.class, params = {"payments"})}), widgets = @WidgetsForContainer(screenlets = {
                    @ScreenletNested(title = "${uiLabelMap.AccountingListPaymentsNotYetApplied}", containers = {
                    @ContainerInScreenlet(labels = {
                        @Label(text = "${uiLabelMap.CommonFrom}: ${partyNameViewTo.groupName}${partyNameViewTo.lastName},${partyNameViewTo.firstName} ${partyNameViewTo.middleName}[${payment.partyIdTo}]", style = "p"
                    
                ),
                    @Label(text = "${uiLabelMap.CommonTo}: ${partyNameViewFrom.groupName}${partyNameViewFrom.lastName},${partyNameViewFrom.firstName} ${partyNameViewFrom.middleName} [${payment.partyIdFrom}]", style = "p"
                
            )})}, includeForms = {
                    @IncludeForm(name = "listPaymentsNotApplied", location = "component://accounting/widget/payments/PaymentForms.xml"
                
            )})}), position = 1)}))})
        }
    )
    public interface EditPaymentApplications {}

    @Screen(name = "PaymentOverview", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.AccountingPayment}: ${parameters.paymentId}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "paymentOverview")
    @Action(type = ActionType.SET, field = "paymentId", fromField = "parameters.paymentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Payment", valueField = "payment")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "AcctgTransAndEntries", list = "AcctgTransAndEntries", conditions = {@ConditionExpr(fieldName = "paymentId", fromField = "paymentId")}, orderBy = {"acctgTransId", "acctgTransEntrySeqId"})
    @DecoratorScreen(
        name = "CommonPaymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"payment.statusId", "equals", "PMNT_NOT_PAID"
                })}), widgets = @InlineWidgets(containers = {
                    @Container(style = "${styles.grid_row}", containers = {
                        @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                            @IncludeScreen(name = "ScipioPaymentInfo", location = "component://accounting/widget/payments/PaymentScreens.xml"
                        )}),
                        @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                            @IncludeScreen(name = "ScipioPaymentAppliedPayment", location = "component://accounting/widget/payments/PaymentScreens.xml"
                        )})}),
                        @Container(style = "${styles.grid_row}", containers = {
                            @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                                @IncludeScreen(name = "ScipioPaymentFinancialTrans", location = "component://accounting/widget/payments/PaymentScreens.xml"
                            )}),
                            @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}"
                        )})}), failWidgets = @InlineWidgets(containers = {
                            @Container(style = "${styles.grid_row}", containers = {
                                @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                                    @IncludeScreen(name = "ScipioPaymentInfo", location = "component://accounting/widget/payments/PaymentScreens.xml"
                                )}),
                                @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                                    @IncludeScreen(name = "ScipioPaymentAppliedPayment", location = "component://accounting/widget/payments/PaymentScreens.xml"
                                )})}),
                                @Container(style = "${styles.grid_row}", containers = {
                                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                                        @IncludeScreen(name = "ScipioPaymentFinancialTrans", location = "component://accounting/widget/payments/PaymentScreens.xml"
                                    )}),
                                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}"
                                )})}))})
        }
    )
    public interface PaymentOverview {}

    @Screen(name = "ManualTransaction", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingManualTransaction")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "manualTransaction")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/payment/ManualTx.groovy")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @DecoratorScreen(
        name = "CommonPaymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/payment/manualTx.ftl"
            )})
        }
    )
    public interface ManualTransaction {}

    @Screen(name = "manualCCTx", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/payment/manualCCTx.ftl")}))
    public interface manualCCTx {}

    @Screen(name = "manualGCTx", location = "component://accounting/widget/payments/PaymentScreens.xml")
    public interface manualGCTx {}

    @Screen(name = "PrintChecks", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "paymentId", fromField = "parameters.paymentId")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/payment/PrintChecks.groovy")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "InvoiceItemType", list = "PayrolGroup", conditions = {@ConditionExpr(fieldName = "parentTypeId", value = "PAYROL")})
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://accounting/webapp/accounting/payment/PrintChecks.fo.ftl", platform = "xsl-fo")}))
    public interface PrintChecks {}

    @Screen(name = "FindSalesInvoicesByDueDate", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFindSalesInvoicesByDueDate")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "payments")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/payment/FindInvoicesByDueDate.groovy")
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonSearchOptions}", includeForms = {
                    @IncludeForm(name = "FindSalesInvoicesByDueDate", location = "component://accounting/widget/payments/PaymentForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"invoicePaymentInfoList"
                    })}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.CommonSearchResults}", includeForms = {
                            @IncludeForm(name = "ListInvoicesByDueDate", location = "component://accounting/widget/payments/PaymentForms.xml"
                        )})}))})
        }
    )
    public interface FindSalesInvoicesByDueDate {}

    @Screen(name = "FindPurchaseInvoicesByDueDate", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFindPurchaseInvoicesByDueDate")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "payments")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/payment/FindInvoicesByDueDate.groovy")
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonSearchOptions}", includeForms = {
                    @IncludeForm(name = "FindPurchaseInvoicesByDueDate", location = "component://accounting/widget/payments/PaymentForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"invoicePaymentInfoList"
                    })}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.CommonSearchResults}", includeForms = {
                            @IncludeForm(name = "ListInvoicesByDueDate", location = "component://accounting/widget/payments/PaymentForms.xml"
                        )})}))})
        }
    )
    public interface FindPurchaseInvoicesByDueDate {}

    @Screen(name = "FindApPaymentGroups", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFindApPaymentGroups")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "apPaymentGroups")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PaymentGroup", list = "paymentGroupList", conditions = {@ConditionExpr(fieldName = "paymentGroupId", fromField = "parameters.paymentGroupId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "paymentGroupTypeId", value = "CHECK_RUN")})
    @DecoratorScreen(
        name = "CommonApDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", includeMenus = {
                            @IncludeMenu(name = "PaymentGroupSubTabBar", location = "component://accounting/widget/AccountingMenus.xml"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindApPaymentGroups", location = "component://accounting/widget/ap/VendorForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPaymentGroup", location = "component://accounting/widget/payments/PaymentGroupForms.xml"
                        )}))})})
        }
    )
    public interface FindApPaymentGroups {}

    @Screen(name = "FindApPayments", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFindApPayments")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findApPayments")
    @DecoratorScreen(
        name = "CommonApDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingNewPayment}", style = "${styles.link_nav} ${styles.action_add}", target = "newPayment"
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

    @Screen(name = "FindArPayments", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFindArPayments")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findArPayments")
    @DecoratorScreen(
        name = "CommonArDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingNewPayment}", style = "${styles.link_nav} ${styles.action_add}", target = "newPayment"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindArPayments", location = "component://accounting/widget/payments/PaymentForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPayments", location = "component://accounting/widget/payments/PaymentForms.xml"
                        )}))})})
        }
    )
    public interface FindArPayments {}

    @Screen(name = "BatchPayments", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleDepositPaymentsAndCreateBatch")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "batchPayments")
    @Action(type = ActionType.SET, field = "paymentMethodTypeId", fromField = "parameters.paymentMethodTypeId")
    @Action(type = ActionType.SET, field = "cardType", fromField = "parameters.cardType")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "parameters.partyIdFrom")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/ar/WEB-INF/actions/BatchPayments.groovy")
    @Action(type = ActionType.ENTITY_AND, entityName = "FinAccount", list = "finAccounts", fieldMaps = {@FieldMap(fieldName = "finAccountTypeId", value = "BANK_ACCOUNT")})
    @DecoratorScreen(
        name = "CommonPaymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindBatchPayments", location = "component://accounting/widget/payments/PaymentForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/ar/payment/batchPayments.ftl"
                    )}))})})
        }
    )
    public interface BatchPayments {}

    @Screen(name = "NewIncomingPayment", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingNewPaymentIncoming")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "newPayment")
    @DecoratorScreen(
        name = "CommonArDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "NewPaymentIn", location = "component://accounting/widget/payments/PaymentForms.xml"
                )})})
        }
    )
    public interface NewIncomingPayment {}

    @Screen(name = "FindArPaymentGroups", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFindArPaymentGroups")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "arPaymentGroups")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PaymentGroup", list = "paymentGroupList", conditions = {@ConditionExpr(fieldName = "paymentGroupId", fromField = "parameters.paymentGroupId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "paymentGroupTypeId", value = "BATCH_PAYMENT")})
    @DecoratorScreen(
        name = "CommonArDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", includeMenus = {
                            @IncludeMenu(name = "PaymentGroupSubTabBar", location = "component://accounting/widget/AccountingMenus.xml"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindArPaymentGroups", location = "component://accounting/widget/payments/PaymentForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPaymentGroup", location = "component://accounting/widget/payments/PaymentGroupForms.xml"
                        )}))})})
        }
    )
    public interface FindArPaymentGroups {}

    @Screen(name = "ScipioPaymentInfo", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SERVICE, serviceName = "getPartyNameForDate", resultMapName = "partyNameResultFrom", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "payment.partyIdFrom"), @FieldMap(fieldName = "compareDate", fromField = "payment.effectiveDate"), @FieldMap(fieldName = "lastNameFirst", value = "Y")})
    @Action(type = ActionType.SERVICE, serviceName = "getPartyNameForDate", resultMapName = "partyNameResultTo", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "payment.partyIdTo"), @FieldMap(fieldName = "compareDate", fromField = "payment.effectiveDate"), @FieldMap(fieldName = "lastNameFirst", value = "Y")})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"payment"})}), widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://accounting/webapp/accounting/payment/ScipioPaymentInfo.ftl")})}))
    public interface ScipioPaymentInfo {}

    @Screen(name = "ScipioPaymentAppliedPayment", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "appliedAmount", value = "${groovy:org.ofbiz.accounting.payment.PaymentWorker.getPaymentApplied(payment).toString()}")
    @Action(type = ActionType.SET, field = "notAppliedAmount", value = "${groovy:org.ofbiz.accounting.payment.PaymentWorker.getPaymentNotApplied(payment).toString()}")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PaymentApplication", list = "paymentList", conditions = {@ConditionExpr(fieldName = "paymentId", fromField = "paymentId"), @ConditionExpr(fieldName = "toPaymentId", fromField = "paymentId")}, orderBy = {"invoiceId", "invoiceItemSeqId"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = NotEmpty.class, params = {"parameters.paymentId"})}), widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://accounting/webapp/accounting/payment/ScipioPaymentAppliedPayment.ftl")})}))
    public interface ScipioPaymentAppliedPayment {}

    @Screen(name = "ScipioPaymentFinancialTrans", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "finAccountTransId", fromField = "payment.finAccountTransId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccountTrans", valueField = "finAccountTrans")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://accounting/webapp/accounting/payment/ScipioPaymentFinancialTrans.ftl")})}))
    public interface ScipioPaymentFinancialTrans {}

    @Screen(name = "ViewGatewayResponse", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewGatewayResponse")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "gatewayResponses")
    @Action(type = ActionType.SET, field = "paymentGatewayResponseId", fromField = "parameters.paymentGatewayResponseId")
    @Action(type = ActionType.SET, field = "orderPaymentPreferenceId", fromField = "parameters.orderPaymentPreferenceId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGatewayResponse", valueField = "paymentGatewayResponse")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/transaction/ViewGatewayResponse.groovy")
    @DecoratorScreen(
        name = "CommonPaymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleViewGatewayResponse}", includeForms = {
                    @IncludeForm(name = "ViewGatewayResponseRelations", location = "component://accounting/widget/payments/PaymentForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.AccountingPaymentsMenu}", includeForms = {
                    @IncludeForm(name = "ViewGatewayResponsePayments", location = "component://accounting/widget/payments/PaymentForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleViewGatewayResponse}", includeForms = {
                    @IncludeForm(name = "ViewGatewayResponse", location = "component://accounting/widget/payments/PaymentForms.xml"
                )})})
        }
    )
    public interface ViewGatewayResponse {}

    @Screen(name = "FindGatewayResponses", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindGatewayResponses")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "gatewayResponses")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonPaymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(widgets = @InlineWidgets(decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindGatewayResponses", location = "component://accounting/widget/payments/PaymentForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListGatewayResponses", location = "component://accounting/widget/payments/PaymentForms.xml"
                    )}))})))})
        }
    )
    public interface FindGatewayResponses {}

    @Screen(name = "AuthorizeTransaction", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingAuthorize")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "authorizeTransaction")
    @Action(type = ActionType.SET, field = "orderId", fromField = "parameters.orderId")
    @Action(type = ActionType.SET, field = "orderPaymentPreferenceId", fromField = "parameters.orderPaymentPreferenceId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "OrderPaymentPreference", valueField = "opp", fieldMaps = {@FieldMap(fieldName = "orderPaymentPreferenceId", fromField = "orderPaymentPreferenceId")})
    @Action(type = ActionType.SET, field = "paymentMethodTypeId", fromField = "opp.paymentMethodTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/transaction/AuthorizeTransaction.groovy")
    @DecoratorScreen(
        name = "CommonPaymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "AuthorizeTransaction", location = "component://accounting/widget/payments/PaymentForms.xml"
                )})})
        }
    )
    public interface AuthorizeTransaction {}

    @Screen(name = "CaptureTransaction", location = "component://accounting/widget/payments/PaymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingCapture")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "captureTransaction")
    @Action(type = ActionType.SET, field = "orderId", fromField = "parameters.orderId")
    @Action(type = ActionType.SET, field = "orderPaymentPreferenceId", fromField = "parameters.orderPaymentPreferenceId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "OrderPaymentPreference", valueField = "opp", fieldMaps = {@FieldMap(fieldName = "orderPaymentPreferenceId", fromField = "orderPaymentPreferenceId")})
    @Action(type = ActionType.SET, field = "paymentMethodTypeId", fromField = "opp.paymentMethodTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/transaction/CaptureTransaction.groovy")
    @DecoratorScreen(
        name = "CommonPaymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "CaptureTransaction", location = "component://accounting/widget/payments/PaymentForms.xml"
                )})})
        }
    )
    public interface CaptureTransaction {}

}
