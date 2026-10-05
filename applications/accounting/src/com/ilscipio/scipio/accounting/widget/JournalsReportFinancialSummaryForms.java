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

import com.ilscipio.scipio.widget.def.form.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class JournalsReportFinancialSummaryForms {

    @Form(
        name = "BaseSummaryOptions",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "month", title = "${uiLabelMap.CommonMonth}", requiredField = true, dropDown = @DropDownField(options = {@Option(key = "01", description = "${uiLabelMap.CommonJanuary}"), @Option(key = "02", description = "${uiLabelMap.CommonFebruary}"), @Option(key = "03", description = "${uiLabelMap.CommonMarch}"), @Option(key = "04", description = "${uiLabelMap.CommonApril}"), @Option(key = "05", description = "${uiLabelMap.CommonMay}"), @Option(key = "06", description = "${uiLabelMap.CommonJune}"), @Option(key = "07", description = "${uiLabelMap.CommonJuly}"), @Option(key = "08", description = "${uiLabelMap.CommonAugust}"), @Option(key = "09", description = "${uiLabelMap.CommonSeptember}"), @Option(key = "10", description = "${uiLabelMap.CommonOctober}"), @Option(key = "11", description = "${uiLabelMap.CommonNovember}"), @Option(key = "12", description = "${uiLabelMap.CommonDecember}")})),
            @FormField(name = "year", title = "${uiLabelMap.CommonYear}", requiredField = true, text = @TextField(size = 4)),
            @FormField(name = "organizationPartyId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyRoleNameDetail", description = "${groupName} [${partyId}]", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}, orderBy = {@EntityOrderBy(fieldName = "groupName")}))),
            @FormField(name = "currencyUomId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")})))
        }
    )
    public interface BaseSummaryOptions {}

    @Form(
        name = "SalesInvoiceByProductCategorySummaryOptions",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        target = "SalesInvoiceByProductCategorySummary",
        extendsForm = "BaseSummaryOptions",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "rootProductCategoryId", title = "${uiLabelMap.ProductCategoryId}", requiredField = true, lookup = @LookupField(targetFormName = "LookupProductCategory")),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_view}", submit = @SubmitField)
        }
    )
    public interface SalesInvoiceByProductCategorySummaryOptions {}

    @Form(
        name = "SalesInvoiceByProductGlAccountSummaryOptions",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        target = "SalesInvoiceByProductGlAccountSummary",
        extendsForm = "BaseSummaryOptions",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_view}", submit = @SubmitField)
        }
    )
    public interface SalesInvoiceByProductGlAccountSummaryOptions {}

    @Form(
        name = "PaymentByMethodSummaryOptions",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        target = "PaymentByMethodSummary",
        extendsForm = "BaseSummaryOptions",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_view}", submit = @SubmitField)
        }
    )
    public interface PaymentByMethodSummaryOptions {}

    @Form(
        name = "InventoryIssueSummaryOptions",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        target = "InventoryIssueSummary",
        extendsForm = "BaseSummaryOptions",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_view}", submit = @SubmitField)
        }
    )
    public interface InventoryIssueSummaryOptions {}

    @Form(
        name = "FinancialAccountSummaryOptions",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        target = "FinancialAccountSummary",
        extendsForm = "BaseSummaryOptions",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_view}", submit = @SubmitField)
        }
    )
    public interface FinancialAccountSummaryOptions {}

    @Form(
        name = "TransactionSelectionForm",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "selectedMonth", title = "${uiLabelMap.CommonMonth}", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "monthList", keyName = "value", description = "${description}"))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", tooltip = "Please enter From and Thru date in fields above", widgetStyle = "${styles.link_run_sys} ${styles.action_select}", submit = @SubmitField)
        }
    )
    public interface TransactionSelectionForm {}

    @Form(
        name = "IncomeStatementRevenues",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "revenueAccountBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", title = "${uiLabelMap.CommonName}", display = @DisplayField(description = "${uiLabelMap.AccountingRevenues}")),
            @FormField(name = "balance", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface IncomeStatementRevenues {}

    @Form(
        name = "IncomeStatementExpenses",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "expenseAccountBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", title = "${uiLabelMap.CommonName}", display = @DisplayField(description = "${uiLabelMap.AccountingExpenses}")),
            @FormField(name = "balance", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface IncomeStatementExpenses {}

    @Form(
        name = "IncomeStatementIncome",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "incomeAccountBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", title = "${uiLabelMap.CommonName}", display = @DisplayField(description = "${uiLabelMap.AccountingIncome}")),
            @FormField(name = "balance", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface IncomeStatementIncome {}

    @Form(
        name = "ComparativeIncomeStatementParameters",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        target = "ComparativeIncomeStatement",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "period1FromDate", title = "${uiLabelMap.FormFieldTitle_period1FromDate}", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "period2FromDate", title = "${uiLabelMap.FormFieldTitle_period2FromDate}", position = 2, requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "period1ThruDate", title = "${uiLabelMap.FormFieldTitle_period1ThruDate}", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "period2ThruDate", title = "${uiLabelMap.FormFieldTitle_period2ThruDate}", position = 2, requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "period1GlFiscalTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", keyFieldName = "glFiscalTypeId", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "period2GlFiscalTypeId", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", keyFieldName = "glFiscalTypeId", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_select}", submit = @SubmitField)
        }
    )
    public interface ComparativeIncomeStatementParameters {}

    @Form(
        name = "ComparativeIncomeStatementParametersOneColumn",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        target = "ComparativeIncomeStatement",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "period1FromDate", title = "${uiLabelMap.FormFieldTitle_period1FromDate}", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "period1ThruDate", title = "${uiLabelMap.FormFieldTitle_period1ThruDate}", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "period1GlFiscalTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", keyFieldName = "glFiscalTypeId", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "period2FromDate", title = "${uiLabelMap.FormFieldTitle_period2FromDate}", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "period2ThruDate", title = "${uiLabelMap.FormFieldTitle_period2ThruDate}", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "period2GlFiscalTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", keyFieldName = "glFiscalTypeId", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_select}", submit = @SubmitField)
        }
    )
    public interface ComparativeIncomeStatementParametersOneColumn {}

    @Form(
        name = "ComparativeIncomeStatementRevenues",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "revenueAccountBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${uiLabelMap.AccountingTotalCapital} (${uiLabelMap.AccountingRevenues})")),
            @FormField(name = "balance1", title = "${uiLabelMap.AccountingBalance} 1", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "balance2", title = "${uiLabelMap.AccountingBalance} 2", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface ComparativeIncomeStatementRevenues {}

    @Form(
        name = "ComparativeIncomeStatementExpenses",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "expenseAccountBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${uiLabelMap.AccountingTotalCapital} (${uiLabelMap.AccountingExpenses})")),
            @FormField(name = "balance1", title = "${uiLabelMap.AccountingBalance} 1", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "balance2", title = "${uiLabelMap.AccountingBalance} 2", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface ComparativeIncomeStatementExpenses {}

    @Form(
        name = "ComparativeIncomeStatementIncome",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "incomeAccountBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${uiLabelMap.AccountingTotalCapital} (${uiLabelMap.AccountingIncome})")),
            @FormField(name = "balance1", title = "${uiLabelMap.AccountingBalance} 1", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "balance2", title = "${uiLabelMap.AccountingBalance} 2", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface ComparativeIncomeStatementIncome {}

    @Form(
        name = "BalanceSheetParameters",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        target = "BalanceSheet",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(defaultValue = "${nowTimestamp}")),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_select}", submit = @SubmitField)
        }
    )
    public interface BalanceSheetParameters {}

    @Form(
        name = "BalanceSheetAssets",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "assetAccountBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${uiLabelMap.AccountingTotalCapital} (${uiLabelMap.AccountingAssets})")),
            @FormField(name = "balance", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface BalanceSheetAssets {}

    @Form(
        name = "BalanceSheetLiabilities",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "liabilityAccountBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${uiLabelMap.AccountingTotalCapital} (${uiLabelMap.AccountingLiabilities})")),
            @FormField(name = "balance", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface BalanceSheetLiabilities {}

    @Form(
        name = "BalanceSheetEquities",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "equityAccountBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${uiLabelMap.AccountingTotalCapital} (${uiLabelMap.AccountingEquities})")),
            @FormField(name = "balance", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface BalanceSheetEquities {}

    @Form(
        name = "BalanceTotals",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "balanceTotalList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "totalName", title = "${uiLabelMap.CommonTotal}", titleAreaStyle = "tableheadhuge", display = @DisplayField(description = "${totalName}")),
            @FormField(name = "balance", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        },
        rowActions = @RowActions(set = {@SetAction(field = "totalName", value = "${groovy: uiLabelMap.get(totalName)}")})
    )
    public interface BalanceTotals {}

    @Form(
        name = "ComparativeBalanceSheetParameters",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        target = "ComparativeBalanceSheet",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "period1ThruDate", dateTime = @DateTimeField),
            @FormField(name = "period2ThruDate", position = 2, dateTime = @DateTimeField),
            @FormField(name = "period1GlFiscalTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", keyFieldName = "glFiscalTypeId", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "period2GlFiscalTypeId", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", keyFieldName = "glFiscalTypeId", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_select}", submit = @SubmitField)
        }
    )
    public interface ComparativeBalanceSheetParameters {}

    @Form(
        name = "ComparativeBalanceSheetParametersOneColumn",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        target = "ComparativeBalanceSheet",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "period1ThruDate", dateTime = @DateTimeField),
            @FormField(name = "period1GlFiscalTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", keyFieldName = "glFiscalTypeId", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "period2ThruDate", dateTime = @DateTimeField),
            @FormField(name = "period2GlFiscalTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", keyFieldName = "glFiscalTypeId", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_select}", submit = @SubmitField)
        }
    )
    public interface ComparativeBalanceSheetParametersOneColumn {}

    @Form(
        name = "ComparativeBalanceSheetAssets",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "assetAccountBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${uiLabelMap.AccountingTotalCapital} (${uiLabelMap.AccountingAssets})")),
            @FormField(name = "balance1", title = "${uiLabelMap.AccountingBalance} 1", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "balance2", title = "${uiLabelMap.AccountingBalance} 2", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface ComparativeBalanceSheetAssets {}

    @Form(
        name = "ComparativeBalanceSheetLiabilities",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "liabilityAccountBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${uiLabelMap.AccountingTotalCapital} (${uiLabelMap.AccountingLiabilities})")),
            @FormField(name = "balance1", title = "${uiLabelMap.AccountingBalance} 1", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "balance2", title = "${uiLabelMap.AccountingBalance} 2", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface ComparativeBalanceSheetLiabilities {}

    @Form(
        name = "ComparativeBalanceSheetEquities",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "equityAccountBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${uiLabelMap.AccountingTotalCapital} (${uiLabelMap.AccountingRevenues})")),
            @FormField(name = "balance1", title = "${uiLabelMap.AccountingBalance} 1", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "balance2", title = "${uiLabelMap.AccountingBalance} 2", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface ComparativeBalanceSheetEquities {}

    @Form(
        name = "ComparativeBalanceTotals",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "balanceTotalList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "totalName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${totalName}")),
            @FormField(name = "balance1", title = "${uiLabelMap.AccountingBalance} 1", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "balance2", title = "${uiLabelMap.AccountingBalance} 2", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        },
        rowActions = @RowActions(set = {@SetAction(field = "totalName", value = "${groovy: uiLabelMap.get(totalName)}")})
    )
    public interface ComparativeBalanceTotals {}

    @Form(
        name = "SelectAcctReportPeriod",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        title = "Select period for report",
        fields = {
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_select}", submit = @SubmitField)
        }
    )
    public interface SelectAcctReportPeriod {}

    @Form(
        name = "FindTransactionTotals",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        target = "TransactionTotals",
        title = "Find list of transaction totals",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "fromDate", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindTransactionTotals {}

    @Form(
        name = "PostedTransactionTotalList",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "postedTransactionTotals",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", title = "${uiLabelMap.CommonCode}", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", title = "${uiLabelMap.CommonName}", display = @DisplayField(description = "${accountName}")),
            @FormField(name = "openingD", title = "${uiLabelMap.AccountingOpeningDebit}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "openingC", title = "${uiLabelMap.AccountingOpeningCredit}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "D", title = "${uiLabelMap.AccountingDebitFlag}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "C", title = "${uiLabelMap.AccountingCreditFlag}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "closingD", title = "${uiLabelMap.AccountingClosingDebit}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "closingC", title = "${uiLabelMap.AccountingClosingDebit}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        },
        rowActions = @RowActions(set = {@SetAction(field = "closingD", value = "${openingD + D}", type = "BigDecimal"), @SetAction(field = "closingC", value = "${openingC + C}", type = "BigDecimal")})
    )
    public interface PostedTransactionTotalList {}

    @Form(
        name = "UnpostedTransactionTotalList",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "unpostedTransactionTotals",
        extendsForm = "PostedTransactionTotalList",
        oddRowStyle = "alternate-row"
    )
    public interface UnpostedTransactionTotalList {}

    @Form(
        name = "PostedAndUnpostedTransactionTotalList",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "allTransactionTotals",
        extendsForm = "PostedTransactionTotalList",
        oddRowStyle = "alternate-row"
    )
    public interface PostedAndUnpostedTransactionTotalList {}

    @Form(
        name = "IncomeStatementListCsv",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "glAccountIncomeList",
        viewSize = 99999,
        fields = {
            @FormField(name = "glAccountId", display = @DisplayField(description = "${glAccountId}")),
            @FormField(name = "glAccountName", display = @DisplayField(description = "${glAccount.accountName}")),
            @FormField(name = "totalAmount", display = @DisplayField(type = "currency")),
            @FormField(name = "totalOfCurrentFiscalPeriod", display = @DisplayField(type = "currency"))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "GlAccount", valueField = "glAccount")})
    )
    public interface IncomeStatementListCsv {}

    @Form(
        name = "ExpenseStatementListCsv",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "glAccountExpenseList",
        viewSize = 99999,
        fields = {
            @FormField(name = "glAccountId", display = @DisplayField(description = "${glAccountId}")),
            @FormField(name = "glAccountName", display = @DisplayField(description = "${glAccount.accountName}")),
            @FormField(name = "totalAmount", display = @DisplayField(type = "currency")),
            @FormField(name = "totalOfCurrentFiscalPeriod", display = @DisplayField(type = "currency"))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "GlAccount", valueField = "glAccount")})
    )
    public interface ExpenseStatementListCsv {}

    @Form(
        name = "BalanceSheetAssetListCsv",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "assetBalancesList",
        viewSize = 99999,
        fields = {
            @FormField(name = "glAccountId", display = @DisplayField(description = "[${glAccountId}] [${glAccount.accountCode}] ${glAccount.accountName}")),
            @FormField(name = "totalAmount", display = @DisplayField(type = "currency"))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "GlAccount", valueField = "glAccount")})
    )
    public interface BalanceSheetAssetListCsv {}

    @Form(
        name = "BalanceSheetLiabilityListCsv",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "liabilityBalancesList",
        viewSize = 99999,
        fields = {
            @FormField(name = "glAccountId", display = @DisplayField(description = "[${glAccountId}] [${glAccount.accountCode}] ${glAccount.accountName}")),
            @FormField(name = "totalAmount", display = @DisplayField(type = "currency"))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "GlAccount", valueField = "glAccount")})
    )
    public interface BalanceSheetLiabilityListCsv {}

    @Form(
        name = "BalanceSheetEquityListCsv",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "equityBalancesList",
        viewSize = 99999,
        fields = {
            @FormField(name = "glAccountId", display = @DisplayField(description = "[${glAccountId}] [${glAccount.accountCode}] ${glAccount.accountName}")),
            @FormField(name = "totalAmount", display = @DisplayField(type = "currency"))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "GlAccount", valueField = "glAccount")})
    )
    public interface BalanceSheetEquityListCsv {}

    @Form(
        name = "GlAccountTrialBalance",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        target = "GlAccountTrialBalance",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "glAccountId", title = "${uiLabelMap.AccountingGlAccountId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${glAccountId} ${accountName}", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "glAccountId")}))),
            @FormField(name = "timePeriod", dropDown = @DropDownField(listOptions = @ListOptions(listName = "customTimePeriods", keyName = "customTimePeriodId", description = "${fromDate} - ${thruDate}"))),
            @FormField(name = "isPosted", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}"), @Option(key = "ALL", description = "${uiLabelMap.CommonAll}")})),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface GlAccountTrialBalance {}

    @Form(
        name = "InventoryValuation",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        target = "InventoryValuation",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "facilityId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName}", constraints = {@EntityConstraint(name = "ownerPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "facilityId")}))),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(defaultValue = "${nowTimestamp}", type = "date")),
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "showSearchResults", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface InventoryValuation {}

    @Form(
        name = "InventoryValuationList",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "inventoryValuationList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", displayEntity = @DisplayEntityField(entityName = "Product", description = "${productId} - ${internalName}")),
            @FormField(name = "unitCost", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "accountingQuantitySum", title = "${uiLabelMap.CommonQuantity}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(description = "${accountingQuantitySum} ${quantityUom}")),
            @FormField(name = "value", title = "${uiLabelMap.CommonValue}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(description = "${accountingQuantitySum * unitCost}", type = "currency"))
        },
        rowActions = @RowActions(set = {@SetAction(field = "quantityUom", fromField = "uom.abbreviation", defaultValue = "${uom.uomId}")}, entityOne = {@EntityOneAction(entityName = "Product", valueField = "product")})
    )
    public interface InventoryValuationList {}

    @Form(
        name = "TrialBalanceFinancialTimePeriodSelection",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        target = "TrialBalance",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "customTimePeriodId", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "CustomTimePeriod", description = "${periodName}: ${fromDate} - ${thruDate}", keyFieldName = "customTimePeriodId", filterByDate = "false", constraints = {@EntityConstraint(name = "periodTypeId", value = "FISCAL_%", operator = "like"), @EntityConstraint(name = "organizationPartyId", value = "${organizationPartyId}")}, orderBy = {@EntityOrderBy(fieldName = "-thruDate"), @EntityOrderBy(fieldName = "periodNum")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_select}", submit = @SubmitField)
        }
    )
    public interface TrialBalanceFinancialTimePeriodSelection {}

    @Form(
        name = "TrialBalanceReport",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "accountBalances",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${accountName}")),
            @FormField(name = "openingBalance", title = "${uiLabelMap.AccountingOpeningBalance}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(description = "${openingBalance}", type = "currency")),
            @FormField(name = "postedDebits", title = "${uiLabelMap.AccountingDebitFlag}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(description = "${postedDebits}", type = "currency")),
            @FormField(name = "postedCredits", title = "${uiLabelMap.AccountingCreditFlag}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(description = "${postedCredits}", type = "currency")),
            @FormField(name = "endingBalance", title = "${uiLabelMap.AccountingEndingBalance}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(description = "${endingBalance}", type = "currency"))
        }
    )
    public interface TrialBalanceReport {}

    @Form(
        name = "CashFlowStatementParameters",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "selectedMonth", title = "${uiLabelMap.CommonMonth}", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "monthList", keyName = "value", description = "${description}"))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", tooltip = "Please enter From and Thru date in fields above", widgetStyle = "${styles.link_run_sys} ${styles.action_select}", submit = @SubmitField)
        }
    )
    public interface CashFlowStatementParameters {}

    @Form(
        name = "CashFlowStatementOpeningCashBalance",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "openingCashBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${accountName}")),
            @FormField(name = "balance", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface CashFlowStatementOpeningCashBalance {}

    @Form(
        name = "CashFlowStatementPeriodCashBalance",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "periodCashBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${accountName}")),
            @FormField(name = "D", title = "${uiLabelMap.AccountingTotalDebit_Receipts}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "C", title = "${uiLabelMap.AccountingTotalCredit_Disbursement}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "balance", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface CashFlowStatementPeriodCashBalance {}

    @Form(
        name = "CashFlowStatementClosingCashBalance",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "closingCashBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${accountName}")),
            @FormField(name = "balance", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface CashFlowStatementClosingCashBalance {}

    @Form(
        name = "CashFlowBalanceTotals",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "cashFlowBalanceTotalList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "totalName", title = "${uiLabelMap.CommonTotal}", titleAreaStyle = "tableheadhuge", display = @DisplayField(description = "${totalName}")),
            @FormField(name = "balance", title = "_", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        },
        rowActions = @RowActions(set = {@SetAction(field = "totalName", value = "${groovy: uiLabelMap.get(totalName)}")})
    )
    public interface CashFlowBalanceTotals {}

    @Form(
        name = "FindCashFlowTotals",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        target = "TransactionTotals",
        title = "Find list of cash flow totals",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindCashFlowTotals {}

    @Form(
        name = "ComparativeCashFlowStatementParameters",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        target = "ComparativeCashFlowStatement",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "period1FromDate", title = "${uiLabelMap.FormFieldTitle_period1FromDate}", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "period2FromDate", title = "${uiLabelMap.FormFieldTitle_period2FromDate}", position = 2, requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "period1ThruDate", title = "${uiLabelMap.FormFieldTitle_period1ThruDate}", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "period2ThruDate", title = "${uiLabelMap.FormFieldTitle_period2ThruDate}", position = 2, requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "period1GlFiscalTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", keyFieldName = "glFiscalTypeId", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "period2GlFiscalTypeId", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", keyFieldName = "glFiscalTypeId", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_select}", submit = @SubmitField)
        }
    )
    public interface ComparativeCashFlowStatementParameters {}

    @Form(
        name = "ComparativeCashFlowStatementOpeningCashBalance",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "openingCashBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${accountName}")),
            @FormField(name = "balance1", title = "${uiLabelMap.AccountingBalance} 1", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "balance2", title = "${uiLabelMap.AccountingBalance} 2", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface ComparativeCashFlowStatementOpeningCashBalance {}

    @Form(
        name = "ComparativeCashFlowStatementPeriodCashBalance",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "periodCashBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${accountName}")),
            @FormField(name = "D1", title = "${uiLabelMap.AccountingPeriod1Debit_Receipts}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "C1", title = "${uiLabelMap.AccountingPeriod1Credit_Disbursement}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "balance1", title = "${uiLabelMap.AccountingBalance} 1", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "D2", title = "${uiLabelMap.AccountingPeriod2Debit_Receipts}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "C2", title = "${uiLabelMap.AccountingPeriod2Credit_Disbursement}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "balance2", title = "${uiLabelMap.AccountingBalance} 2", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface ComparativeCashFlowStatementPeriodCashBalance {}

    @Form(
        name = "ComparativeCashFlowStatementClosingCashBalance",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "closingCashBalanceList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${accountName}")),
            @FormField(name = "balance1", title = "${uiLabelMap.AccountingBalance} 1", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "balance2", title = "${uiLabelMap.AccountingBalance} 2", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface ComparativeCashFlowStatementClosingCashBalance {}

    @Form(
        name = "ComparativeCashFlowBalanceTotals",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        type = FormType.LIST,
        listName = "cashFlowBalanceTotalList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "totalName", titleAreaStyle = "tableheadwide", display = @DisplayField(description = "${totalName}")),
            @FormField(name = "balance1", title = "${uiLabelMap.AccountingBalance} 1", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "balance2", title = "${uiLabelMap.AccountingBalance} 2", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        },
        rowActions = @RowActions(set = {@SetAction(field = "totalName", value = "${groovy: uiLabelMap.get(totalName)}")})
    )
    public interface ComparativeCashFlowBalanceTotals {}

    @Form(
        name = "ComparativeCashFlowStatementParametersOneColumn",
        location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "period1FromDate", title = "${uiLabelMap.FormFieldTitle_period1FromDate}", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "period1ThruDate", title = "${uiLabelMap.FormFieldTitle_period1ThruDate}", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "period1GlFiscalTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", keyFieldName = "glFiscalTypeId", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "period2FromDate", title = "${uiLabelMap.FormFieldTitle_period2FromDate}", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "period2ThruDate", title = "${uiLabelMap.FormFieldTitle_period2ThruDate}", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "period2GlFiscalTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", keyFieldName = "glFiscalTypeId", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")})))
        }
    )
    public interface ComparativeCashFlowStatementParametersOneColumn {}

}
