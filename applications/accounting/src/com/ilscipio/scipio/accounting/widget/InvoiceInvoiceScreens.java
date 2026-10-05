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
public class InvoiceInvoiceScreens {

    @Screen(name = "FindInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Invoices")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindInvoice")
    @DecoratorScreen(
        name = "CommonInvoicesDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "InvoiceSubTabBar", location = "component://accounting/widget/AccountingMenus.xml"
                    )}, containers = {
                        @Container4(style = "clear")})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindInvoices", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListInvoices", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                        )}))})})
        }
    )
    public interface FindInvoices {}

    @Screen(name = "NewInvoice", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingCreateNewInvoice")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Invoices")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Invoice", valueField = "invoice")
    @DecoratorScreen(
        name = "CommonInvoicesDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingCreateNewSalesInvoice}", includeForms = {
                    @IncludeForm(name = "NewSalesInvoice", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.AccountingCreateNewPurchaseInvoice}", includeForms = {
                    @IncludeForm(name = "NewPurchaseInvoice", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                )})})
        }
    )
    public interface NewInvoice {}

    @Screen(name = "EditInvoice", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditInvoice")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editInvoice")
    @Action(type = ActionType.SET, field = "invoiceId", fromField = "parameters.invoiceId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Invoice", valueField = "invoice")
    @Action(type = ActionType.ENTITY_ONE, entityName = "InvoiceType", valueField = "invoiceType", fieldMaps = {@FieldMap(fieldName = "invoiceTypeId", fromField = "invoice.invoiceTypeId")})
    @DecoratorScreen(
        name = "InvoiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"invoice.statusId", "equals", "INVOICE_IN_PROCESS"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "EditInvoice", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                    )})}))})
        }
    )
    public interface EditInvoice {}

    @Screen(name = "invoiceOverview", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.AccountingInvoice}: ${parameters.invoiceId}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "invoiceOverview")
    @Action(type = ActionType.SET, field = "invoiceId", fromField = "parameters.invoiceId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Invoice", valueField = "invoice")
    @Action(type = ActionType.ENTITY_AND, entityName = "InvoiceRole", list = "invoiceRoles", fieldMaps = {@FieldMap(fieldName = "invoiceId", fromField = "parameters.invoiceId")}, orderBy = {"partyId"})
    @Action(type = ActionType.ENTITY_AND, entityName = "InvoiceStatus", list = "invoiceStatus", fieldMaps = {@FieldMap(fieldName = "invoiceId", fromField = "parameters.invoiceId")}, orderBy = {"statusDate"})
    @Action(type = ActionType.ENTITY_AND, entityName = "InvoiceTerm", list = "invoiceTerms", fieldMaps = {@FieldMap(fieldName = "invoiceId", fromField = "parameters.invoiceId")}, orderBy = {"invoiceTermId"})
    @Action(type = ActionType.ENTITY_AND, entityName = "TimeEntry", list = "timeEntries", fieldMaps = {@FieldMap(fieldName = "invoiceId", fromField = "parameters.invoiceId")}, orderBy = {"invoiceItemSeqId"})
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/invoice/CreateApplicationList.groovy")
    @Action(type = ActionType.SET, field = "invoiceAmount", value = "${groovy:org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceTotal(invoice)}", valueType = "BigDecimal")
    @Action(type = ActionType.SET, field = "notAppliedAmount", value = "${groovy:org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceNotApplied(invoice)}", valueType = "BigDecimal")
    @Action(type = ActionType.SET, field = "appliedAmount", value = "${groovy:org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceApplied(invoice)}", valueType = "BigDecimal")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "InvItemAndOrdItem", list = "invItemAndOrdItems", conditions = {@ConditionExpr(fieldName = "invoiceId", operator = "equals", fromField = "invoiceId")}, orderBy = {"invoiceItemSeqId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "AcctgTransAndEntries", list = "acctgTransAndEntries", conditions = {@ConditionExpr(fieldName = "invoiceId", operator = "equals", fromField = "invoiceId")}, orderBy = {"acctgTransId", "acctgTransEntrySeqId"})
    @DecoratorScreen(
        name = "InvoiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"invoice"})}), widgets = @InlineWidgets(sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = Compare.class, params = {"invoice.statusId", "equals", "INVOICE_IN_PROCESS"
                        })}), widgets = @WidgetsForContainer(containers = {
                            @Container2(style = "${styles.grid_row}", containers = {
                                @Container3(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                                    @IncludeScreen(name = "ScipioInvoiceInfo", location = "component://accounting/widget/invoice/InvoiceScreens.xml"
                                )}),
                                @Container3(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                                    @IncludeScreen(name = "ScipioInvoiceTerms", location = "component://accounting/widget/invoice/InvoiceScreens.xml"
                                ),
                                @IncludeScreen(name = "ScipioInvoicePaymentInfo", location = "component://accounting/widget/invoice/InvoiceScreens.xml"
                            ),
                            @IncludeScreen(name = "ScipioInvoiceAppliedPayment", location = "component://accounting/widget/invoice/InvoiceScreens.xml"
                        )})}),
                        @Container2(style = "${styles.grid_row}", containers = {
                            @Container3(style = "${styles.grid_large}12 ${styles.grid_cell}", includeScreens = {
                                @IncludeScreen(name = "ScipioInvoiceItems", location = "component://accounting/widget/invoice/InvoiceScreens.xml"
                            )})})}), failWidgets = @WidgetsForContainer(containers = {
                                @Container2(style = "${styles.grid_row}", containers = {
                                    @Container3(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                                        @IncludeScreen(name = "ScipioInvoiceInfo", location = "component://accounting/widget/invoice/InvoiceScreens.xml"
                                    )}),
                                    @Container3(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                                        @IncludeScreen(name = "ScipioInvoiceTerms", location = "component://accounting/widget/invoice/InvoiceScreens.xml"
                                    ),
                                    @IncludeScreen(name = "ScipioInvoicePaymentInfo", location = "component://accounting/widget/invoice/InvoiceScreens.xml"
                                ),
                                @IncludeScreen(name = "ScipioInvoiceAppliedPayment", location = "component://accounting/widget/invoice/InvoiceScreens.xml"
                            )})}),
                            @Container2(style = "${styles.grid_row}", containers = {
                                @Container3(style = "${styles.grid_large}12 ${styles.grid_cell}", includeScreens = {
                                    @IncludeScreen(name = "ScipioInvoiceItems", location = "component://accounting/widget/invoice/InvoiceScreens.xml"
                                )})})}))}), failWidgets = @InlineWidgets(value = {
                                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingInvoiceDoesNotExists}"
                                )}))})
        }
    )
    public interface invoiceOverview {}

    @Screen(name = "EditInvoiceApplications", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListEditInvoiceApplications")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editInvoiceApplications")
    @Action(type = ActionType.SET, field = "invoiceId", fromField = "parameters.invoiceId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Invoice", valueField = "invoice")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/invoice/CreateApplicationList.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/invoice/ListNotAppliedPayments.groovy")
    @Action(type = ActionType.SET, field = "invoiceAmount", value = "${groovy:org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceTotal(invoice)}", valueType = "BigDecimal")
    @Action(type = ActionType.SET, field = "notAppliedAmount", value = "${groovy:org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceNotApplied(invoice)}", valueType = "BigDecimal")
    @Action(type = ActionType.SET, field = "appliedAmount", value = "${groovy:org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceApplied(invoice)}", valueType = "BigDecimal")
    @DecoratorScreen(
        name = "InvoiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"notAppliedAmount", "greater", "0"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(title = "${uiLabelMap.AccountingAssignPaymentToInvoice}", includeForms = {
                        @IncludeForm(name = "AddPayment", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                    )}, position = 0),
                    @Screenlet(title = "${uiLabelMap.AccountingPaymentsApplied} ${appliedAmount?currency(${invoice.currencyUomId})} ${uiLabelMap.AccountingOpenPayments} ${notAppliedAmount?currency(${invoice.currencyUomId})}", includeForms = {
                        @IncludeForm(name = "EditInvoiceApplications", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                    )}, position = 1)}, sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                            @OrCondition(ifNotEmpty = {"payments", "paymentsActualCurrency"
                        })}), widgets = @WidgetsForContainer(screenlets = {
                            @ScreenletNested(title = "${uiLabelMap.AccountingListPaymentsNotYetApplied} [${invoice.partyIdFrom}] ${uiLabelMap.AccountingPaymentSentForm} [${invoice.partyId}]", sections = {
                    @SectionLeaf(condition = @Condition(type = NotEmpty.class, params = {"payments"
                
                        }), widgets = @WidgetsLeaf(includeForms = {
                    @IncludeForm(name = "ListPaymentsNotApplied", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                
                    )})),
                @SectionLeaf(condition = @Condition(type = NotEmpty.class, params = {"paymentsActualCurrency"
                
                }), widgets = @WidgetsLeaf(includeForms = {
                    @IncludeForm(name = "ListPaymentsNotAppliedForeignCurrency", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                
            )}))})}), position = 2)}), failWidgets = @InlineWidgets(screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingPaymentsApplied} ${appliedAmount?currency(${invoice.currencyUomId})}  ${uiLabelMap.AccountingOpenPayments} ${notAppliedAmount?currency(${invoice.currencyUomId})}", includeForms = {
                    @IncludeForm(name = "EditInvoiceApplications", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                )})}))})
        }
    )
    public interface EditInvoiceApplications {}

    @Screen(name = "EditInvoiceItems", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.viewIndex")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.viewSize")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListInvoices")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "listInvoiceItems")
    @Action(type = ActionType.SET, field = "invoiceId", fromField = "parameters.invoiceId")
    @Action(type = ActionType.SET, field = "invoiceItemSeqd", fromField = "parameters.invoiceItemSeqId")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Invoice", valueField = "invoice")
    @Action(type = ActionType.ENTITY_ONE, entityName = "InvoiceItem", valueField = "invoiceItem")
    @Action(type = ActionType.ENTITY_AND, entityName = "InvoiceItem", list = "invoiceItems", fieldMaps = {@FieldMap(fieldName = "invoiceId", fromField = "parameters.invoiceId")}, orderBy = {"invoiceItemSeqId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "InvoiceItemType", list = "PayrolGroup", conditions = {@ConditionExpr(fieldName = "parentTypeId", value = "PAYROL")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "InvoiceItemType", list = "PayrolList")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/invoice/GetAccountOrganizationAndClass.groovy")
    @DecoratorScreen(
        name = "InvoiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingInvoiceItemAdd} - ${uiLabelMap.AccountingInvoice}: ${invoiceId}", sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Compare.class, params = {"invoice.invoiceTypeId", "equals", "PAYROL_INVOICE"
                    })}), widgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "EditInvoiceItem", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                    )}), failWidgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/invoice/InvoiceItemsPayrol.ftl"
                    )}))}),
                    @Screenlet(title = "${uiLabelMap.AccountingInvoiceItems}", includeForms = {
                        @IncludeForm(name = "EditInvoiceItems", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                    )})})
        }
    )
    public interface EditInvoiceItems {}

    @Screen(name = "EditInvoiceTimeEntries", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.viewIndex")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.viewSize")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListInvoiceTimeEntries")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditInvoiceTimeEntries")
    @Action(type = ActionType.SET, field = "invoiceId", fromField = "parameters.invoiceId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Invoice", valueField = "invoice")
    @Action(type = ActionType.ENTITY_AND, entityName = "TimeEntry", list = "timeEntries", fieldMaps = {@FieldMap(fieldName = "invoiceId", fromField = "parameters.invoiceId")}, orderBy = {"timeEntryId"})
    @DecoratorScreen(
        name = "InvoiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingInvoiceTimeEntries}", includeForms = {
                    @IncludeForm(name = "EditTimeEntries", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                )})})
        }
    )
    public interface EditInvoiceTimeEntries {}

    @Screen(name = "InvoiceRoles", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListInvoiceRoles")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "invoiceRoles")
    @Action(type = ActionType.SET, field = "invoiceId", fromField = "parameters.invoiceId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Invoice", valueField = "invoice")
    @Action(type = ActionType.ENTITY_AND, entityName = "InvoiceRole", list = "invoiceRoles", fieldMaps = {@FieldMap(fieldName = "invoiceId", fromField = "invoiceId")}, orderBy = {"partyId"})
    @DecoratorScreen(
        name = "InvoiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListInvoiceRoles", location = "component://accounting/widget/invoice/InvoiceForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingPartyRoleAdd}", name = "PartyInvoiceRolePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditInvoiceRole", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                )}, position = 0)})
        }
    )
    public interface InvoiceRoles {}

    @Screen(name = "InvoiceTerms", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListEditInvoiceTerms")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "invoiceTerms")
    @Action(type = ActionType.SET, field = "invoiceId", fromField = "parameters.invoiceId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Invoice", valueField = "invoice")
    @Action(type = ActionType.ENTITY_AND, entityName = "InvoiceTerm", list = "invoiceTerms", fieldMaps = {@FieldMap(fieldName = "invoiceId", fromField = "invoiceId")}, orderBy = {"invoiceTermId"})
    @DecoratorScreen(
        name = "InvoiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleNewInvoiceTerm}", name = "PartyInvoiceTermPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditInvoiceTerm", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.CommonTerms}", includeForms = {
                    @IncludeForm(name = "ListInvoiceTerms", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                )})})
        }
    )
    public interface InvoiceTerms {}

    @Screen(name = "SendPerEmail", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleSendInvoicePerEmail")
    @Action(type = ActionType.SET, field = "invoiceId", fromField = "parameters.invoiceId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Invoice", valueField = "invoice")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "sendPerEmail")
    @DecoratorScreen(
        name = "CommonInvoicesDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonSendPerEmail}", includeForms = {
                    @IncludeForm(name = "SendPerEmail", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                )})})
        }
    )
    public interface SendPerEmail {}

    @Screen(name = "sendPerEmailBody", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonNotImplementedSentence}")}))
    public interface sendPerEmailBody {}

    @Screen(name = "ListCustomerInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/GetMyCompany.groovy")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Invoice", list = "invoices", conditions = {@ConditionExpr(fieldName = "partyId", operator = "equals", fromField = "myCompanyId")}, orderBy = {"invoiceDate DESC"})
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PageTitleListInvoices}", includeForms = {@IncludeForm(name = "ListCustomerInvoices", location = "component://accounting/widget/invoice/InvoiceForms.xml")})}))
    public interface ListCustomerInvoices {}

    @Screen(name = "ListSupplierInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "myCompanyId", fromField = "userLogin.partyId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Invoice", list = "invoiceslistexternal", conditions = {@ConditionExpr(fieldName = "partyIdFrom", operator = "equals", fromField = "myCompanyId"), @ConditionExpr(fieldName = "invoiceTypeId", operator = "equals", value = "PURCHASE_INVOICE")}, orderBy = {"invoiceDate DESC"})
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PageTitleListInvoices}", includeForms = {@IncludeForm(name = "ListSupplierInvoices", location = "component://accounting/widget/invoice/InvoiceForms.xml")})}))
    public interface ListSupplierInvoices {}

    @Screen(name = "DownloadInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/GetMyCompany.groovy")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "StatusItem", list = "invoiceStatuses", conditions = {@ConditionExpr(fieldName = "statusTypeId", operator = "equals", value = "INVOICE_STATUS")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "InvoiceType", list = "invoiceTypes", conditions = {@ConditionExpr(fieldName = "parentTypeId", operator = "equals", value = "INVOICE")})
    @DecoratorScreen(
        name = "CommonInvoicesDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleDownloadInvoices}", htmlTemplates = {
                    @HtmlTemplate(location = "component://accounting/webapp/accounting/invoice/ScipioDownloadInvoices.ftl"
                )})})
        }
    )
    public interface DownloadInvoices {}

    @Screen(name = "CommissionRun", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFindSalesInvoicesForCommissionRun")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "commissionRun")
    @Action(type = ActionType.SET, field = "salesRepPartyList", fromField = "parameters.partyIds", valueType = "List")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/ap/WEB-INF/actions/invoices/CommissionRun.groovy")
    @Action(type = ActionType.SET, field = "asm_multipleSelectForm", value = "CommissionRun")
    @Action(type = ActionType.SET, field = "asm_multipleSelect", value = "CommissionRun_partyId")
    @Action(type = ActionType.SET, field = "asm_formSize", value = "700")
    @Action(type = ActionType.SET, field = "asm_listItemPercentOfForm", value = "95")
    @Action(type = ActionType.SET, field = "asm_sortable", value = "false")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "asm_title", value = "${uiLabelMap.AccountingSelectPartiesForCommissionInvoice}")
    @DecoratorScreen(
        name = "CommonInvoicesDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/setMultipleSelectJs.ftl"
            )}, decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "CommissionRun", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/ap/invoices/CommissionRun.ftl"
                    )}))})})
        }
    )
    public interface CommissionRun {}

    @Screen(name = "CommissionReport", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingCommissionReport")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "commissionReport")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/ap/WEB-INF/actions/invoices/CommissionReport.groovy")
    @DecoratorScreen(
        name = "CommonInvoicesDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "CommissionReport", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/ap/invoices/CommissionReport.ftl"
                    )}))})})
        }
    )
    public interface CommissionReport {}

    @Screen(name = "ListAPReports", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingArPageTitleListReports")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "apReports")
    @Action(type = ActionType.SET, field = "invoiceTypeId", value = "PURCHASE_INVOICE")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "userPreferences.ORGANIZATION_PARTY", defaultValue = "${defaultOrganizationPartyId}")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/invoice/InvoiceReport.groovy")
    @DecoratorScreen(
        name = "CommonApDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingPastDueInvoices}: (${PastDueInvoicestotalAmount})", actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "invoices", fromField = "PastDueInvoices"
                )}), includeScreens = {
                    @IncludeScreen(name = "ScipioInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.AccountingInvoicesDueSoon}: (${InvoicesDueSoonTotalAmount})", actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "invoices", fromField = "InvoicesDueSoon"
                )}), includeScreens = {
                    @IncludeScreen(name = "ScipioInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml"
                )})})
        }
    )
    public interface ListAPReports {}

    @Screen(name = "FindApInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml", finallyActions = @Actions(value = {@Action(type = ActionType.CLOSE_OBJECT, field = "result.listIt")}))
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFindApInvoices")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findApInvoices")
    @Action(type = ActionType.SERVICE, serviceName = "performFind", resultMapName = "result", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "InvoiceAndType"), @FieldMap(fieldName = "orderBy", value = "invoiceDate DESC")})
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/generated/FindApInvoices_script1.groovy")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", defaultValue = "${defaultOrganizationPartyId}")
    @Action(type = ActionType.SET, field = "paymentPartyId", fromField = "parameters.partyIdFrom", defaultValue = "${parameters.organizationPartyId}")
    @DecoratorScreen(
        name = "CommonApDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonNew} ${uiLabelMap.CommonInvoice}", style = "${styles.link_nav} ${styles.action_add}", target = "NewPurchaseInvoice"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindApInvoices", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/ap/invoices/PurchaseInvoices.ftl"
                        )}))})})
        }
    )
    public interface FindApInvoices {}

    @Screen(name = "NewPurchaseInvoice", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingCreateNewPurchaseInvoice")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "newInvoice")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Invoice", valueField = "invoice")
    @DecoratorScreen(
        name = "CommonInvoicesDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "NewPurchaseInvoice", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                )})})
        }
    )
    public interface NewPurchaseInvoice {}

    @Screen(name = "ListARReports", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "arReports")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingArPageTitleListReports")
    @Action(type = ActionType.SET, field = "invoiceTypeId", value = "SALES_INVOICE")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "userPreferences.ORGANIZATION_PARTY", defaultValue = "${defaultOrganizationPartyId}")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/invoice/InvoiceReport.groovy")
    @DecoratorScreen(
        name = "CommonArDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingPastDueInvoices}: (${PastDueInvoicestotalAmount})", actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "invoices", fromField = "PastDueInvoices"
                )}), includeScreens = {
                    @IncludeScreen(name = "ScipioInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.AccountingInvoicesDueSoon}: (${InvoicesDueSoonTotalAmount})", actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "invoices", fromField = "InvoicesDueSoon"
                )}), includeScreens = {
                    @IncludeScreen(name = "ScipioInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml"
                )})})
        }
    )
    public interface ListARReports {}

    @Screen(name = "FindArInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml", finallyActions = @Actions(value = {@Action(type = ActionType.CLOSE_OBJECT, field = "result.listIt")}))
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findArInvoices")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFindArInvoices")
    @Action(type = ActionType.SERVICE, serviceName = "performFind", resultMapName = "result", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "InvoiceAndType"), @FieldMap(fieldName = "orderBy", value = "invoiceDate DESC")})
    @Action(type = ActionType.SET, field = "invoices", fromField = "result.listIt")
    @DecoratorScreen(
        name = "CommonArDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingCreateNewInvoice}", style = "${styles.link_nav} ${styles.action_add}", target = "newInvoice"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindArInvoices", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/ar/invoice/ListInvoices.ftl"
                        )}))})})
        }
    )
    public interface FindArInvoices {}

    @Screen(name = "NewSalesInvoice", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingCreateNewSalesInvoice")
    @DecoratorScreen(
        name = "CommonInvoicesDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "NewSalesInvoice", location = "component://accounting/widget/invoice/InvoiceForms.xml"
                )})})
        }
    )
    public interface NewSalesInvoice {}

    @Screen(name = "ScipioInvoiceInfo", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "total", value = "${groovy:return(org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceTotal(invoice));}")
    @Action(type = ActionType.SERVICE, serviceName = "getPartyNameForDate", resultMapName = "partyNameResultFrom", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "invoice.partyIdFrom"), @FieldMap(fieldName = "compareDate", fromField = "invoice.invoiceDate"), @FieldMap(fieldName = "lastNameFirst", value = "Y")})
    @Action(type = ActionType.SERVICE, serviceName = "getPartyNameForDate", resultMapName = "partyNameResultTo", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "invoice.partyId"), @FieldMap(fieldName = "compareDate", fromField = "invoice.invoiceDate"), @FieldMap(fieldName = "lastNameFirst", value = "Y")})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"parameters.invoiceId"})}), widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://accounting/webapp/accounting/invoice/ScipioInvoiceInfo.ftl")})}))
    public interface ScipioInvoiceInfo {}

    @Screen(name = "ScipioInvoiceAppliedPayment", location = "component://accounting/widget/invoice/InvoiceScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"invoiceApplications"})}))
    @Section(widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://accounting/webapp/accounting/invoice/ScipioInvoiceAppliedPayment.ftl")})}))
    public interface ScipioInvoiceAppliedPayment {}

    @Screen(name = "ScipioInvoiceItems", location = "component://accounting/widget/invoice/InvoiceScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"invItemAndOrdItems"})}))
    @Section(widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://accounting/webapp/accounting/invoice/ScipioInvoiceItems.ftl")})}))
    public interface ScipioInvoiceItems {}

    @Screen(name = "ScipioInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "organizationPartyId", defaultValue = "${defaultOrganizationPartyId}")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://accounting/webapp/accounting/invoice/ScipioInvoices.ftl")})}))
    public interface ScipioInvoices {}

    @Screen(name = "ScipioInvoiceTerms", location = "component://accounting/widget/invoice/InvoiceScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"invoiceTerms"})}))
    @Section(widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://accounting/webapp/accounting/invoice/ScipioInvoiceTerms.ftl")})}))
    public interface ScipioInvoiceTerms {}

    @Screen(name = "ScipioInvoicePaymentInfo", location = "component://accounting/widget/invoice/InvoiceScreens.xml")
    @Action(type = ActionType.SERVICE, serviceName = "getInvoicePaymentInfoList", resultMapName = "result", fieldMaps = {@FieldMap(fieldName = "invoiceId", fromField = "parameters.invoiceId")})
    @Action(type = ActionType.SET, field = "invoicePaymentInfoList", fromField = "result.invoicePaymentInfoList")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"invoicePaymentInfoList"})}), widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://accounting/webapp/accounting/invoice/ScipioInvoicePaymentInfo.ftl")})}))
    public interface ScipioInvoicePaymentInfo {}

}
