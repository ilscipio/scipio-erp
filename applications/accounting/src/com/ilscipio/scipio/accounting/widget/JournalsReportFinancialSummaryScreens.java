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
public class JournalsReportFinancialSummaryScreens {

    @Screen(name = "FinancialSummaryReportOptions", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFinancialSummaryReportOptions")
    @Action(type = ActionType.SET, field = "activeSubMenu2Item", value = "FindFinAccount")
    @Action(type = ActionType.SET, field = "month", fromField = "parameters.month", defaultValue = "${groovy:org.ofbiz.base.util.UtilDateTime.nowDateString(\"MM\")}")
    @Action(type = ActionType.SET, field = "year", fromField = "parameters.year", defaultValue = "${groovy:org.ofbiz.base.util.UtilDateTime.nowDateString(\"yyyy\")}")
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PageTitleFinancialSummaryReportOptions}", style = "heading"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleSalesInvoiceByProductCategorySummary}", includeForms = {
                    @IncludeForm(name = "SalesInvoiceByProductCategorySummaryOptions", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                )}),
                @Screenlet(includeForms = {
                    @IncludeForm(name = "SalesInvoiceByProductGlAccountSummaryOptions", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitlePaymentByMethodSummary}", includeForms = {
                    @IncludeForm(name = "PaymentByMethodSummaryOptions", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleInventoryIssueSummary}", includeForms = {
                    @IncludeForm(name = "InventoryIssueSummaryOptions", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleFinancialAccountSummary}", includeForms = {
                    @IncludeForm(name = "FinancialAccountSummaryOptions", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                )})})
        }
    )
    public interface FinancialSummaryReportOptions {}

    @Screen(name = "FinancialSummaryDataPrep", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "month", fromField = "parameters.month", valueType = "Integer", defaultValue = "${groovy:org.ofbiz.base.util.UtilDateTime.nowDateString(\"MM\")}")
    @Action(type = ActionType.SET, field = "year", fromField = "parameters.year", valueType = "Integer", defaultValue = "${groovy:org.ofbiz.base.util.UtilDateTime.nowDateString(\"yyyy\")}")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "currencyUomId", fromField = "parameters.currencyUomId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "organizationPartyName", autoFieldMap = false, useCache = true, fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "organizationPartyId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "Uom", valueField = "currencyUom", autoFieldMap = false, useCache = true, fieldMaps = {@FieldMap(fieldName = "uomId", fromField = "currencyUomId")})
    public interface FinancialSummaryDataPrep {}

    @Screen(name = "SalesInvoiceByProductCategorySummary", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "FinancialSummaryDataPrep")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleSalesInvoiceByProductCategorySummary")
    @Action(type = ActionType.SET, field = "rootProductCategoryId", fromField = "parameters.rootProductCategoryId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductCategory", valueField = "rootProductCategory", autoFieldMap = false, useCache = true, fieldMaps = {@FieldMap(fieldName = "productCategoryId", fromField = "rootProductCategoryId")})
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/SalesInvoiceByProductCategorySummary.groovy")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "SalesInvoiceByProductCategorySummary")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://accounting/webapp/accounting/reports/SalesInvoiceByProductCategorySummary.ftl"
                )})})
        }
    )
    public interface SalesInvoiceByProductCategorySummary {}

    @Screen(name = "SalesInvoiceByProductGlAccountSummary", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "FinancialSummaryDataPrep")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleSalesInvoiceByProductGlAccountSummary")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(labels = {
                    @Label(text = "${uiLabelMap.CommonNotImplementedSentence}")})
                })
        }
    )
    public interface SalesInvoiceByProductGlAccountSummary {}

    @Screen(name = "PaymentByMethodSummary", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "FinancialSummaryDataPrep")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitlePaymentByMethodSummary")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(labels = {
                    @Label(text = "${uiLabelMap.CommonNotImplementedSentence}")})
                })
        }
    )
    public interface PaymentByMethodSummary {}

    @Screen(name = "InventoryIssueSummary", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "FinancialSummaryDataPrep")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleInventoryIssueSummary")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(labels = {
                    @Label(text = "${uiLabelMap.CommonNotImplementedSentence}")})
                })
        }
    )
    public interface InventoryIssueSummary {}

    @Screen(name = "FinancialAccountSummary", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "FinancialSummaryDataPrep")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFinancialAccountSummary")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(labels = {
                    @Label(text = "${uiLabelMap.CommonNotImplementedSentence}")})
                })
        }
    )
    public interface FinancialAccountSummary {}

    @Screen(name = "TrialBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingTrialBalance")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TrialBalance")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "partyIds", value = "${groovy:org.ofbiz.party.party.PartyWorker.getAssociatedPartyIdsByRelationshipType(delegator, organizationPartyId, 'GROUP_ROLLUP')}", valueType = "List")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Party", list = "parties", conditions = {@ConditionExpr(fieldName = "partyId", operator = "in", fromField = "partyIds")})
    @Action(type = ActionType.SET, field = "partyIds[]", fromField = "organizationPartyId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/TrialBalance.groovy")
    @DecoratorScreen(
        name = "CommonOrganizationAccountingReportsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "TrialBalanceFinancialTimePeriodSelection", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
            )}, containers = {
                @Container(labels = {
                    @Label(text = "${uiLabelMap.AccountingConsolidatedDataFromDivisions}"
                )}, position = 0)}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"parameters.customTimePeriodId"
                    })}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.AccountingTrialBalance}", includeForms = {
                            @IncludeForm(name = "TrialBalanceReport", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                        )}, containers = {
                            @Container(labels = {
                                @Label(text = "${uiLabelMap.AccountingDebitFlag}: ${postedDebitsTotal}"
                            )}),
                            @Container(labels = {
                                @Label(text = "${uiLabelMap.AccountingCreditFlag}: ${postedCreditsTotal}"
                            )})}, widgets = {
                                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsCsv}", style = "${styles.link_run_sys} ${styles.action_export}", target = "TrialBalanceSearchResultsCsv.csv"
                            ),
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsPdf}", style = "${styles.link_run_sys} ${styles.action_export}", target = "TrialBalanceSearchResultsPdf.pdf", targetWindow = "_BLANK"
                        )})}), position = 2)})
        }
    )
    public interface TrialBalance {}

    @Screen(name = "TrialBalanceSearchResultsCsv", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "partyIds", value = "${groovy:org.ofbiz.party.party.PartyWorker.getAssociatedPartyIdsByRelationshipType(delegator, organizationPartyId, 'GROUP_ROLLUP')}", valueType = "List")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Party", list = "parties", conditions = {@ConditionExpr(fieldName = "partyId", operator = "in", fromField = "partyIds")})
    @Action(type = ActionType.SET, field = "partyIds[]", fromField = "organizationPartyId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/TrialBalance.groovy")
    @Section(widgets = @Widgets(containers = {@Container(includeForms = {@IncludeForm(name = "TrialBalanceReport", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml")})}))
    public interface TrialBalanceSearchResultsCsv {}

    @Screen(name = "TrialBalanceSearchResultsPdf", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "partyIds", value = "${groovy:org.ofbiz.party.party.PartyWorker.getAssociatedPartyIdsByRelationshipType(delegator, organizationPartyId, 'GROUP_ROLLUP')}", valueType = "List")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Party", list = "parties", conditions = {@ConditionExpr(fieldName = "partyId", operator = "in", fromField = "partyIds")})
    @Action(type = ActionType.SET, field = "partyIds[]", fromField = "organizationPartyId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/TrialBalance.groovy")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "TrialBalanceFinancialTimePeriodSelection", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "TrialBalanceReport", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
            )}, containers = {
                @Container(labels = {
                    @Label(text = "${uiLabelMap.AccountingTrialBalance}")}, position = 0
                ),
                @Container(labels = {
                    @Label(text = "${uiLabelMap.AccountingConsolidatedDataFromDivisions}"
                )}, position = 1),
                @Container(labels = {
                    @Label(text = "${uiLabelMap.AccountingDebitFlag}: ${postedDebitsTotal}"
                )}, position = 4),
                @Container(labels = {
                    @Label(text = "${uiLabelMap.AccountingCreditFlag}: ${postedCreditsTotal}"
                )}, position = 5)})
        }
    )
    public interface TrialBalanceSearchResultsPdf {}

    @Screen(name = "BalanceSheet", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingBalanceSheet")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "BalanceSheet")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "parameters.glFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/BalanceSheet.groovy")
    @DecoratorScreen(
        name = "CommonOrganizationAccountingReportsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsCsv}", style = "${styles.link_run_sys} ${styles.action_export}", target = "BalanceSheet.csv"
            ),
            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsPdf}", style = "${styles.link_run_sys} ${styles.action_export}", target = "BalanceSheet.pdf", targetWindow = "_BLANK"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "BalanceSheetParameters", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                )}, position = 0),
                @Screenlet(includeForms = {
                    @IncludeForm(name = "BalanceSheetAssets", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 1
                ),
                @IncludeForm(name = "BalanceSheetLiabilities", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 3
            ),
            @IncludeForm(name = "BalanceSheetEquities", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 5
            ),
            @IncludeForm(name = "BalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 7
            )}, labels = {
                @Label(text = "${uiLabelMap.AccountingAssets}", position = 0),
                @Label(text = "${uiLabelMap.AccountingLiabilities}", position = 2
            ),
            @Label(text = "${uiLabelMap.AccountingEquities}", position = 4
            ),
            @Label(text = "${uiLabelMap.CommonTotal}", position = 6)}, position = 3
            )})
        }
    )
    public interface BalanceSheet {}

    @Screen(name = "BalanceSheetPdf", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "parameters.glFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/BalanceSheet.groovy")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(includeForms = {
                    @IncludeForm(name = "BalanceSheetParameters", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 1
                ),
                @IncludeForm(name = "BalanceSheetAssets", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 3
            ),
            @IncludeForm(name = "BalanceSheetLiabilities", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 5
            ),
            @IncludeForm(name = "BalanceSheetEquities", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 7
            ),
            @IncludeForm(name = "BalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 9
            )}, labels = {
                @Label(text = "${uiLabelMap.AccountingBalanceSheet}", style = "heading", position = 0
            ),
            @Label(text = "${uiLabelMap.AccountingAssets}", position = 2),
            @Label(text = "${uiLabelMap.AccountingLiabilities}", position = 4
            ),
            @Label(text = "${uiLabelMap.AccountingEquities}", position = 6
            ),
            @Label(text = "${uiLabelMap.CommonTotal}", position = 8)})})
        }
    )
    public interface BalanceSheetPdf {}

    @Screen(name = "BalanceSheetCsv", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "parameters.glFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/BalanceSheet.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingAssets}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "BalanceSheetAssets", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingLiabilities}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "BalanceSheetLiabilities", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingEquities}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "BalanceSheetEquities", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml")}))
    public interface BalanceSheetCsv {}

    @Screen(name = "ComparativeBalanceSheet", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingComparativeBalanceSheet")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ComparativeBalanceSheet")
    @Action(type = ActionType.SET, field = "period1ThruDate", fromField = "parameters.period1ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period1GlFiscalTypeId", fromField = "parameters.period1GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period1ThruDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period1GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/BalanceSheet.groovy")
    @Action(type = ActionType.SET, field = "assetAccountBalanceList1", fromField = "assetAccountBalanceList")
    @Action(type = ActionType.SET, field = "liabilityAccountBalanceList1", fromField = "liabilityAccountBalanceList")
    @Action(type = ActionType.SET, field = "equityAccountBalanceList1", fromField = "equityAccountBalanceList")
    @Action(type = ActionType.SET, field = "assetBalanceTotal1", fromField = "assetBalanceTotal")
    @Action(type = ActionType.SET, field = "currentAssetBalanceTotal1", fromField = "currentAssetBalanceTotal")
    @Action(type = ActionType.SET, field = "longtermAssetBalanceTotal1", fromField = "longtermAssetBalanceTotal")
    @Action(type = ActionType.SET, field = "liabilityBalanceTotal1", fromField = "liabilityBalanceTotal")
    @Action(type = ActionType.SET, field = "currentLiabilityBalanceTotal1", fromField = "currentLiabilityBalanceTotal")
    @Action(type = ActionType.SET, field = "equityBalanceTotal1", fromField = "equityBalanceTotal")
    @Action(type = ActionType.SET, field = "liabilityEquityBalanceTotal1", fromField = "liabilityEquityBalanceTotal")
    @Action(type = ActionType.SET, field = "balanceTotalList1", fromField = "balanceTotalList")
    @Action(type = ActionType.SET, field = "period2ThruDate", fromField = "parameters.period2ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period2GlFiscalTypeId", fromField = "parameters.period2GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period2ThruDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period2GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/BalanceSheet.groovy")
    @Action(type = ActionType.SET, field = "assetAccountBalanceList2", fromField = "assetAccountBalanceList")
    @Action(type = ActionType.SET, field = "liabilityAccountBalanceList2", fromField = "liabilityAccountBalanceList")
    @Action(type = ActionType.SET, field = "equityAccountBalanceList2", fromField = "equityAccountBalanceList")
    @Action(type = ActionType.SET, field = "assetBalanceTotal2", fromField = "assetBalanceTotal")
    @Action(type = ActionType.SET, field = "currentAssetBalanceTotal2", fromField = "currentAssetBalanceTotal")
    @Action(type = ActionType.SET, field = "longtermAssetBalanceTotal2", fromField = "longtermAssetBalanceTotal")
    @Action(type = ActionType.SET, field = "liabilityBalanceTotal2", fromField = "liabilityBalanceTotal")
    @Action(type = ActionType.SET, field = "currentLiabilityBalanceTotal2", fromField = "currentLiabilityBalanceTotal")
    @Action(type = ActionType.SET, field = "equityBalanceTotal2", fromField = "equityBalanceTotal")
    @Action(type = ActionType.SET, field = "liabilityEquityBalanceTotal2", fromField = "liabilityEquityBalanceTotal")
    @Action(type = ActionType.SET, field = "balanceTotalList2", fromField = "balanceTotalList")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/ComparativeBalanceSheet.groovy")
    @DecoratorScreen(
        name = "CommonOrganizationAccountingReportsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsCsv}", style = "${styles.link_run_sys} ${styles.action_export}", target = "ComparativeBalanceSheet.csv"
            ),
            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsPdf}", style = "${styles.link_run_sys} ${styles.action_export}", target = "ComparativeBalanceSheet.pdf", targetWindow = "_BLANK"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ComparativeBalanceSheetParameters", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                )}, position = 0),
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ComparativeBalanceSheetAssets", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 1
                ),
                @IncludeForm(name = "ComparativeBalanceSheetLiabilities", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 3
            ),
            @IncludeForm(name = "ComparativeBalanceSheetEquities", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 5
            ),
            @IncludeForm(name = "ComparativeBalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 7
            )}, labels = {
                @Label(text = "${uiLabelMap.AccountingAssets}", position = 0),
                @Label(text = "${uiLabelMap.AccountingLiabilities}", position = 2
            ),
            @Label(text = "${uiLabelMap.AccountingEquities}", position = 4
            ),
            @Label(text = "${uiLabelMap.CommonTotal}", position = 6)}, position = 3
            )})
        }
    )
    public interface ComparativeBalanceSheet {}

    @Screen(name = "ComparativeBalanceSheetPdf", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingComparativeBalanceSheet")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ComparativeBalanceSheet")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "period1ThruDate", fromField = "parameters.period1ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period1GlFiscalTypeId", fromField = "parameters.period1GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period1ThruDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period1GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/BalanceSheet.groovy")
    @Action(type = ActionType.SET, field = "assetAccountBalanceList1", fromField = "assetAccountBalanceList")
    @Action(type = ActionType.SET, field = "liabilityAccountBalanceList1", fromField = "liabilityAccountBalanceList")
    @Action(type = ActionType.SET, field = "equityAccountBalanceList1", fromField = "equityAccountBalanceList")
    @Action(type = ActionType.SET, field = "assetBalanceTotal1", fromField = "assetBalanceTotal")
    @Action(type = ActionType.SET, field = "currentAssetBalanceTotal1", fromField = "currentAssetBalanceTotal")
    @Action(type = ActionType.SET, field = "longtermAssetBalanceTotal1", fromField = "longtermAssetBalanceTotal")
    @Action(type = ActionType.SET, field = "liabilityBalanceTotal1", fromField = "liabilityBalanceTotal")
    @Action(type = ActionType.SET, field = "currentLiabilityBalanceTotal1", fromField = "currentLiabilityBalanceTotal")
    @Action(type = ActionType.SET, field = "equityBalanceTotal1", fromField = "equityBalanceTotal")
    @Action(type = ActionType.SET, field = "liabilityEquityBalanceTotal1", fromField = "liabilityEquityBalanceTotal")
    @Action(type = ActionType.SET, field = "balanceTotalList1", fromField = "balanceTotalList")
    @Action(type = ActionType.SET, field = "period2ThruDate", fromField = "parameters.period2ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period2GlFiscalTypeId", fromField = "parameters.period2GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period2ThruDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period2GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/BalanceSheet.groovy")
    @Action(type = ActionType.SET, field = "assetAccountBalanceList2", fromField = "assetAccountBalanceList")
    @Action(type = ActionType.SET, field = "liabilityAccountBalanceList2", fromField = "liabilityAccountBalanceList")
    @Action(type = ActionType.SET, field = "equityAccountBalanceList2", fromField = "equityAccountBalanceList")
    @Action(type = ActionType.SET, field = "assetBalanceTotal2", fromField = "assetBalanceTotal")
    @Action(type = ActionType.SET, field = "currentAssetBalanceTotal2", fromField = "currentAssetBalanceTotal")
    @Action(type = ActionType.SET, field = "longtermAssetBalanceTotal2", fromField = "longtermAssetBalanceTotal")
    @Action(type = ActionType.SET, field = "liabilityBalanceTotal2", fromField = "liabilityBalanceTotal")
    @Action(type = ActionType.SET, field = "currentLiabilityBalanceTotal2", fromField = "currentLiabilityBalanceTotal")
    @Action(type = ActionType.SET, field = "equityBalanceTotal2", fromField = "equityBalanceTotal")
    @Action(type = ActionType.SET, field = "liabilityEquityBalanceTotal2", fromField = "liabilityEquityBalanceTotal")
    @Action(type = ActionType.SET, field = "balanceTotalList2", fromField = "balanceTotalList")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/ComparativeBalanceSheet.groovy")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(includeForms = {
                    @IncludeForm(name = "ComparativeBalanceSheetParametersOneColumn", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 1
                ),
                @IncludeForm(name = "ComparativeBalanceSheetAssets", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 3
            ),
            @IncludeForm(name = "ComparativeBalanceSheetLiabilities", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 5
            ),
            @IncludeForm(name = "ComparativeBalanceSheetEquities", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 7
            ),
            @IncludeForm(name = "ComparativeBalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 9
            )}, labels = {
                @Label(text = "${uiLabelMap.AccountingComparativeBalanceSheet}", style = "heading", position = 0
            ),
            @Label(text = "${uiLabelMap.AccountingAssets}", position = 2),
            @Label(text = "${uiLabelMap.AccountingLiabilities}", position = 4
            ),
            @Label(text = "${uiLabelMap.AccountingEquities}", position = 6
            ),
            @Label(text = "${uiLabelMap.CommonTotal}", position = 8)})})
        }
    )
    public interface ComparativeBalanceSheetPdf {}

    @Screen(name = "ComparativeBalanceSheetCsv", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingComparativeBalanceSheet")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ComparativeBalanceSheet")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "period1ThruDate", fromField = "parameters.period1ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period1GlFiscalTypeId", fromField = "parameters.period1GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period1ThruDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period1GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/BalanceSheet.groovy")
    @Action(type = ActionType.SET, field = "assetAccountBalanceList1", fromField = "assetAccountBalanceList")
    @Action(type = ActionType.SET, field = "liabilityAccountBalanceList1", fromField = "liabilityAccountBalanceList")
    @Action(type = ActionType.SET, field = "equityAccountBalanceList1", fromField = "equityAccountBalanceList")
    @Action(type = ActionType.SET, field = "assetBalanceTotal1", fromField = "assetBalanceTotal")
    @Action(type = ActionType.SET, field = "currentAssetBalanceTotal1", fromField = "currentAssetBalanceTotal")
    @Action(type = ActionType.SET, field = "longtermAssetBalanceTotal1", fromField = "longtermAssetBalanceTotal")
    @Action(type = ActionType.SET, field = "liabilityBalanceTotal1", fromField = "liabilityBalanceTotal")
    @Action(type = ActionType.SET, field = "currentLiabilityBalanceTotal1", fromField = "currentLiabilityBalanceTotal")
    @Action(type = ActionType.SET, field = "equityBalanceTotal1", fromField = "equityBalanceTotal")
    @Action(type = ActionType.SET, field = "liabilityEquityBalanceTotal1", fromField = "liabilityEquityBalanceTotal")
    @Action(type = ActionType.SET, field = "balanceTotalList1", fromField = "balanceTotalList")
    @Action(type = ActionType.SET, field = "period2ThruDate", fromField = "parameters.period2ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period2GlFiscalTypeId", fromField = "parameters.period2GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period2ThruDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period2GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/BalanceSheet.groovy")
    @Action(type = ActionType.SET, field = "assetAccountBalanceList2", fromField = "assetAccountBalanceList")
    @Action(type = ActionType.SET, field = "liabilityAccountBalanceList2", fromField = "liabilityAccountBalanceList")
    @Action(type = ActionType.SET, field = "equityAccountBalanceList2", fromField = "equityAccountBalanceList")
    @Action(type = ActionType.SET, field = "assetBalanceTotal2", fromField = "assetBalanceTotal")
    @Action(type = ActionType.SET, field = "currentAssetBalanceTotal2", fromField = "currentAssetBalanceTotal")
    @Action(type = ActionType.SET, field = "longtermAssetBalanceTotal2", fromField = "longtermAssetBalanceTotal")
    @Action(type = ActionType.SET, field = "liabilityBalanceTotal2", fromField = "liabilityBalanceTotal")
    @Action(type = ActionType.SET, field = "currentLiabilityBalanceTotal2", fromField = "currentLiabilityBalanceTotal")
    @Action(type = ActionType.SET, field = "equityBalanceTotal2", fromField = "equityBalanceTotal")
    @Action(type = ActionType.SET, field = "liabilityEquityBalanceTotal2", fromField = "liabilityEquityBalanceTotal")
    @Action(type = ActionType.SET, field = "balanceTotalList2", fromField = "balanceTotalList")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/ComparativeBalanceSheet.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingAssets}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ComparativeBalanceSheetAssets", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingLiabilities}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ComparativeBalanceSheetLiabilities", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingEquities}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ComparativeBalanceSheetEquities", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonTotal}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ComparativeBalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml")}))
    public interface ComparativeBalanceSheetCsv {}

    @Screen(name = "TransactionTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingTransactionTotals")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "AccountingTransactionTotals")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TransactionTotals")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "partyIds", value = "${groovy:org.ofbiz.party.party.PartyWorker.getAssociatedPartyIdsByRelationshipType(delegator, organizationPartyId, 'GROUP_ROLLUP')}", valueType = "List")
    @Action(type = ActionType.SET, field = "partyIds[]", fromField = "organizationPartyId")
    @Action(type = ActionType.SERVICE, serviceName = "findLastClosedDate", resultMapName = "findLastClosedDateOutMap", fieldMaps = {@FieldMap(fieldName = "organizationPartyId", fromField = "organizationPartyId")})
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "parameters.glFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/MonthSelection.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/TransactionTotals.groovy")
    @DecoratorScreen(
        name = "CommonOrganizationAccountingReportsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "TransactionSelectionForm", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = NotEmpty.class, params = {"fromDate"}),
                        @Condition(type = NotEmpty.class, params = {"thruDate"}),
                        @Condition(type = NotEmpty.class, params = {"organizationPartyId"
                    })}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsCsv}", style = "${styles.link_run_sys} ${styles.action_export}", target = "TransactionTotalsCsv.csv"
                    ),
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsPdf}", style = "${styles.link_run_sys} ${styles.action_export}", target = "TransactionTotalsPdf.pdf", targetWindow = "_BLANK"
                )}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.AccountingPostedTransactionTotals}", includeForms = {
                        @IncludeForm(name = "PostedTransactionTotalList", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                    )}),
                    @Screenlet(title = "${uiLabelMap.AccountingUnPostedTransactionTotals}", includeForms = {
                        @IncludeForm(name = "UnpostedTransactionTotalList", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                    )}),
                    @Screenlet(title = "${uiLabelMap.AccountingPostedAndUnpostedTransactionTotals}", includeForms = {
                        @IncludeForm(name = "PostedAndUnpostedTransactionTotalList", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                    )})}))})
        }
    )
    public interface TransactionTotals {}

    @Screen(name = "TransactionTotalsPdf", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingTransactionTotals")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TransactionTotals")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "partyIds", value = "${groovy:org.ofbiz.party.party.PartyWorker.getAssociatedPartyIdsByRelationshipType(delegator, organizationPartyId, 'GROUP_ROLLUP')}", valueType = "List")
    @Action(type = ActionType.SET, field = "partyIds[]", fromField = "organizationPartyId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "parameters.glFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/TransactionTotals.groovy")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(includeForms = {
                    @IncludeForm(name = "FindTransactionTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 0
                ),
                @IncludeForm(name = "PostedTransactionTotalList", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 2
            ),
            @IncludeForm(name = "UnpostedTransactionTotalList", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 4
            ),
            @IncludeForm(name = "PostedAndUnpostedTransactionTotalList", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 6
            )}, labels = {
                @Label(text = "${uiLabelMap.AccountingPostedTransactionTotals}", position = 1
            ),
            @Label(text = "${uiLabelMap.AccountingUnPostedTransactionTotals}", position = 3
            ),
            @Label(text = "${uiLabelMap.AccountingPostedAndUnpostedTransactionTotals}", position = 5
            )})})
        }
    )
    public interface TransactionTotalsPdf {}

    @Screen(name = "TransactionTotalsCsv", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingTransactionTotals")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "AccountingTransactionTotals")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TransactionTotals")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "partyIds", value = "${groovy:org.ofbiz.party.party.PartyWorker.getAssociatedPartyIdsByRelationshipType(delegator, organizationPartyId, 'GROUP_ROLLUP')}", valueType = "List")
    @Action(type = ActionType.SET, field = "partyIds[]", fromField = "organizationPartyId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "parameters.glFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/TransactionTotals.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingPostedTransactionTotals}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "PostedTransactionTotalList", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingPostedTransactionTotals}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "UnpostedTransactionTotalList", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingPostedTransactionTotals}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "PostedAndUnpostedTransactionTotalList", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml")}))
    public interface TransactionTotalsCsv {}

    @Screen(name = "IncomeStatement", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingIncomeStatement")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "IncomeStatement")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SERVICE, serviceName = "findLastClosedDate", resultMapName = "findLastClosedDateOutMap", fieldMaps = {@FieldMap(fieldName = "organizationPartyId", fromField = "organizationPartyId")})
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "parameters.glFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/MonthSelection.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/IncomeStatement.groovy")
    @DecoratorScreen(
        name = "CommonOrganizationAccountingReportsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "TransactionSelectionForm", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                )}),
                @Screenlet(includeForms = {
                    @IncludeForm(name = "IncomeStatementRevenues", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 3
                ),
                @IncludeForm(name = "IncomeStatementExpenses", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 5
            ),
            @IncludeForm(name = "IncomeStatementIncome", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 7
            ),
            @IncludeForm(name = "BalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 9
            )}, labels = {
                @Label(text = "${uiLabelMap.AccountingRevenues}", position = 2
            ),
            @Label(text = "${uiLabelMap.AccountingExpenses}", position = 4
            ),
            @Label(text = "${uiLabelMap.AccountingIncome}", position = 6),
            @Label(text = "${uiLabelMap.CommonTotal}", position = 8)}, widgets = {
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsCsv}", style = "${styles.link_run_sys} ${styles.action_export}", target = "IncomeStatementListCsv.csv", position = 0
            ),
            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsPdf}", style = "${styles.link_run_sys} ${styles.action_export}", target = "IncomeStatementListPdf.pdf", targetWindow = "_BLANK", position = 1
            )})})
        }
    )
    public interface IncomeStatement {}

    @Screen(name = "IncomeStatementListPdf", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "parameters.glFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/IncomeStatement.groovy")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(includeForms = {
                    @IncludeForm(name = "FindTransactionTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 1
                ),
                @IncludeForm(name = "IncomeStatementRevenues", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 3
            ),
            @IncludeForm(name = "IncomeStatementExpenses", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 5
            ),
            @IncludeForm(name = "IncomeStatementIncome", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 7
            ),
            @IncludeForm(name = "BalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 9
            )}, labels = {
                @Label(text = "${uiLabelMap.AccountingIncomeStatement}", style = "heading", position = 0
            ),
            @Label(text = "${uiLabelMap.AccountingRevenues}", position = 2
            ),
            @Label(text = "${uiLabelMap.AccountingExpenses}", position = 4
            ),
            @Label(text = "${uiLabelMap.AccountingIncome}", position = 6),
            @Label(text = "${uiLabelMap.CommonTotal}", position = 8)})})
        }
    )
    public interface IncomeStatementListPdf {}

    @Screen(name = "IncomeStatementListCsv", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "parameters.glFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/IncomeStatement.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingRevenues}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "IncomeStatementRevenues", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingExpenses}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "IncomeStatementExpenses", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingIncome}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "IncomeStatementIncome", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonTotal}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "BalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml")}))
    public interface IncomeStatementListCsv {}

    @Screen(name = "ComparativeIncomeStatement", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingComparativeIncomeStatement")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ComparativeIncomeStatement")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", valueType = "String")
    @Action(type = ActionType.SERVICE, serviceName = "findLastClosedDate", resultMapName = "findLastClosedDateOutMap", fieldMaps = {@FieldMap(fieldName = "organizationPartyId", fromField = "organizationPartyId")})
    @Action(type = ActionType.SET, field = "period1FromDate", fromField = "parameters.period1FromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "period1ThruDate", fromField = "parameters.period1ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period1GlFiscalTypeId", fromField = "parameters.period1GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "period1FromDate")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period1ThruDate")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period1GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/IncomeStatement.groovy")
    @Action(type = ActionType.SET, field = "revenueAccountBalanceList1", fromField = "revenueAccountBalanceList")
    @Action(type = ActionType.SET, field = "revenueAccountBalanceTotal1", fromField = "revenueAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "expenseAccountBalanceList1", fromField = "expenseAccountBalanceList")
    @Action(type = ActionType.SET, field = "expenseAccountBalanceTotal1", fromField = "expenseAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "cogsExpense1", fromField = "cogsExpense")
    @Action(type = ActionType.SET, field = "incomeAccountBalanceList1", fromField = "incomeAccountBalanceList")
    @Action(type = ActionType.SET, field = "incomeAccountBalanceTotal1", fromField = "incomeAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "grossMargin1", fromField = "grossMargin")
    @Action(type = ActionType.SET, field = "sgaExpense1", fromField = "sgaExpense")
    @Action(type = ActionType.SET, field = "incomeFromOperations1", fromField = "incomeFromOperations")
    @Action(type = ActionType.SET, field = "netIncome1", fromField = "netIncome")
    @Action(type = ActionType.SET, field = "balanceTotalList1", fromField = "balanceTotalList")
    @Action(type = ActionType.SET, field = "period2FromDate", fromField = "parameters.period2FromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "period2ThruDate", fromField = "parameters.period2ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period2GlFiscalTypeId", fromField = "parameters.period2GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "period2FromDate")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period2ThruDate")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period2GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/IncomeStatement.groovy")
    @Action(type = ActionType.SET, field = "revenueAccountBalanceList2", fromField = "revenueAccountBalanceList")
    @Action(type = ActionType.SET, field = "revenueAccountBalanceTotal2", fromField = "revenueAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "expenseAccountBalanceList2", fromField = "expenseAccountBalanceList")
    @Action(type = ActionType.SET, field = "expenseAccountBalanceTotal2", fromField = "expenseAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "cogsExpense2", fromField = "cogsExpense")
    @Action(type = ActionType.SET, field = "incomeAccountBalanceList2", fromField = "incomeAccountBalanceList")
    @Action(type = ActionType.SET, field = "incomeAccountBalanceTotal2", fromField = "incomeAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "grossMargin2", fromField = "grossMargin")
    @Action(type = ActionType.SET, field = "sgaExpense2", fromField = "sgaExpense")
    @Action(type = ActionType.SET, field = "incomeFromOperations2", fromField = "incomeFromOperations")
    @Action(type = ActionType.SET, field = "netIncome2", fromField = "netIncome")
    @Action(type = ActionType.SET, field = "balanceTotalList2", fromField = "balanceTotalList")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/ComparativeIncomeStatement.groovy")
    @DecoratorScreen(
        name = "CommonOrganizationAccountingReportsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ComparativeIncomeStatementParameters", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                )}),
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ComparativeIncomeStatementRevenues", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 3
                ),
                @IncludeForm(name = "ComparativeIncomeStatementExpenses", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 5
            ),
            @IncludeForm(name = "ComparativeIncomeStatementIncome", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 7
            ),
            @IncludeForm(name = "ComparativeBalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 9
            )}, labels = {
                @Label(text = "${uiLabelMap.AccountingRevenues}", position = 2
            ),
            @Label(text = "${uiLabelMap.AccountingExpenses}", position = 4
            ),
            @Label(text = "${uiLabelMap.AccountingIncome}", position = 6),
            @Label(text = "${uiLabelMap.CommonTotal}", position = 8)}, widgets = {
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsCsv}", style = "${styles.link_run_sys} ${styles.action_export}", target = "ComparativeIncomeStatements.csv", position = 0
            ),
            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsPdf}", style = "${styles.link_run_sys} ${styles.action_export}", target = "ComparativeIncomeStatements.pdf", targetWindow = "_BLANK", position = 1
            )})})
        }
    )
    public interface ComparativeIncomeStatement {}

    @Screen(name = "ComparativeIncomeStatementsPdf", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingComparativeIncomeStatement")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ComparativeIncomeStatement")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", valueType = "String")
    @Action(type = ActionType.SERVICE, serviceName = "findLastClosedDate", resultMapName = "findLastClosedDateOutMap", fieldMaps = {@FieldMap(fieldName = "organizationPartyId", fromField = "organizationPartyId")})
    @Action(type = ActionType.SET, field = "period1FromDate", fromField = "parameters.period1FromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "period1ThruDate", fromField = "parameters.period1ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period1GlFiscalTypeId", fromField = "parameters.period1GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "period1FromDate")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period1ThruDate")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period1GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/IncomeStatement.groovy")
    @Action(type = ActionType.SET, field = "revenueAccountBalanceList1", fromField = "revenueAccountBalanceList")
    @Action(type = ActionType.SET, field = "revenueAccountBalanceTotal1", fromField = "revenueAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "expenseAccountBalanceList1", fromField = "expenseAccountBalanceList")
    @Action(type = ActionType.SET, field = "expenseAccountBalanceTotal1", fromField = "expenseAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "cogsExpense1", fromField = "cogsExpense")
    @Action(type = ActionType.SET, field = "incomeAccountBalanceList1", fromField = "incomeAccountBalanceList")
    @Action(type = ActionType.SET, field = "incomeAccountBalanceTotal1", fromField = "incomeAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "grossMargin1", fromField = "grossMargin")
    @Action(type = ActionType.SET, field = "sgaExpense1", fromField = "sgaExpense")
    @Action(type = ActionType.SET, field = "incomeFromOperations1", fromField = "incomeFromOperations")
    @Action(type = ActionType.SET, field = "netIncome1", fromField = "netIncome")
    @Action(type = ActionType.SET, field = "balanceTotalList1", fromField = "balanceTotalList")
    @Action(type = ActionType.SET, field = "period2FromDate", fromField = "parameters.period2FromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "period2ThruDate", fromField = "parameters.period2ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period2GlFiscalTypeId", fromField = "parameters.period2GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "period2FromDate")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period2ThruDate")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period2GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/IncomeStatement.groovy")
    @Action(type = ActionType.SET, field = "revenueAccountBalanceList2", fromField = "revenueAccountBalanceList")
    @Action(type = ActionType.SET, field = "revenueAccountBalanceTotal2", fromField = "revenueAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "expenseAccountBalanceList2", fromField = "expenseAccountBalanceList")
    @Action(type = ActionType.SET, field = "expenseAccountBalanceTotal2", fromField = "expenseAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "cogsExpense2", fromField = "cogsExpense")
    @Action(type = ActionType.SET, field = "incomeAccountBalanceList2", fromField = "incomeAccountBalanceList")
    @Action(type = ActionType.SET, field = "incomeAccountBalanceTotal2", fromField = "incomeAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "grossMargin2", fromField = "grossMargin")
    @Action(type = ActionType.SET, field = "sgaExpense2", fromField = "sgaExpense")
    @Action(type = ActionType.SET, field = "incomeFromOperations2", fromField = "incomeFromOperations")
    @Action(type = ActionType.SET, field = "netIncome2", fromField = "netIncome")
    @Action(type = ActionType.SET, field = "balanceTotalList2", fromField = "balanceTotalList")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/ComparativeIncomeStatement.groovy")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(includeForms = {
                    @IncludeForm(name = "ComparativeIncomeStatementParametersOneColumn", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 1
                ),
                @IncludeForm(name = "ComparativeIncomeStatementRevenues", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 3
            ),
            @IncludeForm(name = "ComparativeIncomeStatementExpenses", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 5
            ),
            @IncludeForm(name = "ComparativeIncomeStatementIncome", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 7
            ),
            @IncludeForm(name = "ComparativeBalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 9
            )}, labels = {
                @Label(text = "${uiLabelMap.AccountingComparativeIncomeStatement}", style = "heading", position = 0
            ),
            @Label(text = "${uiLabelMap.AccountingRevenues}", position = 2
            ),
            @Label(text = "${uiLabelMap.AccountingExpenses}", position = 4
            ),
            @Label(text = "${uiLabelMap.AccountingIncome}", position = 6),
            @Label(text = "${uiLabelMap.CommonTotal}", position = 8)})})
        }
    )
    public interface ComparativeIncomeStatementsPdf {}

    @Screen(name = "ComparativeIncomeStatementsCsv", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingComparativeIncomeStatement")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ComparativeIncomeStatement")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", valueType = "String")
    @Action(type = ActionType.SERVICE, serviceName = "findLastClosedDate", resultMapName = "findLastClosedDateOutMap", fieldMaps = {@FieldMap(fieldName = "organizationPartyId", fromField = "organizationPartyId")})
    @Action(type = ActionType.SET, field = "period1FromDate", fromField = "parameters.period1FromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "period1ThruDate", fromField = "parameters.period1ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period1GlFiscalTypeId", fromField = "parameters.period1GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "period1FromDate")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period1ThruDate")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period1GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/IncomeStatement.groovy")
    @Action(type = ActionType.SET, field = "revenueAccountBalanceList1", fromField = "revenueAccountBalanceList")
    @Action(type = ActionType.SET, field = "revenueAccountBalanceTotal1", fromField = "revenueAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "expenseAccountBalanceList1", fromField = "expenseAccountBalanceList")
    @Action(type = ActionType.SET, field = "expenseAccountBalanceTotal1", fromField = "expenseAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "cogsExpense1", fromField = "cogsExpense")
    @Action(type = ActionType.SET, field = "incomeAccountBalanceList1", fromField = "incomeAccountBalanceList")
    @Action(type = ActionType.SET, field = "incomeAccountBalanceTotal1", fromField = "incomeAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "grossMargin1", fromField = "grossMargin")
    @Action(type = ActionType.SET, field = "sgaExpense1", fromField = "sgaExpense")
    @Action(type = ActionType.SET, field = "incomeFromOperations1", fromField = "incomeFromOperations")
    @Action(type = ActionType.SET, field = "netIncome1", fromField = "netIncome")
    @Action(type = ActionType.SET, field = "balanceTotalList1", fromField = "balanceTotalList")
    @Action(type = ActionType.SET, field = "period2FromDate", fromField = "parameters.period2FromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "period2ThruDate", fromField = "parameters.period2ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period2GlFiscalTypeId", fromField = "parameters.period2GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "period2FromDate")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period2ThruDate")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period2GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/IncomeStatement.groovy")
    @Action(type = ActionType.SET, field = "revenueAccountBalanceList2", fromField = "revenueAccountBalanceList")
    @Action(type = ActionType.SET, field = "revenueAccountBalanceTotal2", fromField = "revenueAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "expenseAccountBalanceList2", fromField = "expenseAccountBalanceList")
    @Action(type = ActionType.SET, field = "expenseAccountBalanceTotal2", fromField = "expenseAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "cogsExpense2", fromField = "cogsExpense")
    @Action(type = ActionType.SET, field = "incomeAccountBalanceList2", fromField = "incomeAccountBalanceList")
    @Action(type = ActionType.SET, field = "incomeAccountBalanceTotal2", fromField = "incomeAccountBalanceTotal")
    @Action(type = ActionType.SET, field = "grossMargin2", fromField = "grossMargin")
    @Action(type = ActionType.SET, field = "sgaExpense2", fromField = "sgaExpense")
    @Action(type = ActionType.SET, field = "incomeFromOperations2", fromField = "incomeFromOperations")
    @Action(type = ActionType.SET, field = "netIncome2", fromField = "netIncome")
    @Action(type = ActionType.SET, field = "balanceTotalList2", fromField = "balanceTotalList")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/ComparativeIncomeStatement.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingRevenues}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ComparativeIncomeStatementRevenues", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingExpenses}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ComparativeIncomeStatementExpenses", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingIncome}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ComparativeIncomeStatementIncome", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonTotal}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ComparativeBalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml")}))
    public interface ComparativeIncomeStatementsCsv {}

    @Screen(name = "GlAccountTrialBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingGlAccountTrialBalance")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "AccountingGlAccountTrialBalance")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountTrialBalance")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/GlAccountTrialBalance.groovy")
    @DecoratorScreen(
        name = "CommonOrganizationAccountingReportsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingGlAccountTrialBalance}", includeForms = {
                    @IncludeForm(name = "GlAccountTrialBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                        @OrCondition(ifNotEmpty = {"parameters.glAccountId", "parameters.timePeriod", "parameters.isPosted"
                    })}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.AccountingGlAccountTrialBalance}", htmlTemplates = {
                            @HtmlTemplate(location = "component://accounting/webapp/accounting/reports/GlAccountTrialBalanceReport.ftl"
                        )})}))})
        }
    )
    public interface GlAccountTrialBalance {}

    @Screen(name = "GlAccountTrialBalanceReportPdf", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/GlAccountTrialBalance.groovy")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/reports/GlAccountTrialBalanceReport.fo.ftl", platform = "xsl-fo"
            )})
        }
    )
    public interface GlAccountTrialBalanceReportPdf {}

    @Screen(name = "GlAccountBalanceByCostCenter", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.SET, field = "titleProperty", value = "FormFieldTitle_costCenters")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "FormFieldTitle_costCenters")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountBalanceByCostCenter")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "partyIds", value = "${groovy:org.ofbiz.party.party.PartyWorker.getAssociatedPartyIdsByRelationshipType(delegator, organizationPartyId, 'GROUP_ROLLUP')}", valueType = "List")
    @Action(type = ActionType.SET, field = "partyIds[]", fromField = "organizationPartyId")
    @Action(type = ActionType.SERVICE, serviceName = "findLastClosedDate", resultMapName = "findLastClosedDateOutMap", fieldMaps = {@FieldMap(fieldName = "organizationPartyId", fromField = "organizationPartyId")})
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/CostCenters.groovy")
    @DecoratorScreen(
        name = "CommonOrganizationAccountingReportsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.FormFieldTitle_costCenters}", includeForms = {
                    @IncludeForm(name = "SelectAcctReportPeriod", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = NotEmpty.class, params = {"fromDate"}),
                        @Condition(type = NotEmpty.class, params = {"thruDate"}),
                        @Condition(type = NotEmpty.class, params = {"organizationPartyId"
                    })}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsPdf}", style = "${styles.link_run_sys} ${styles.action_export}", target = "GlAccountBalanceByCostCenter.pdf", targetWindow = "_BLANK"
                    )}, screenlets = {
                        @Screenlet(title = "${uiLabelMap.FormFieldTitle_costCenters}", htmlTemplates = {
                            @HtmlTemplate(location = "component://accounting/webapp/accounting/reports/CostCentersReport.ftl"
                        )})}))})
        }
    )
    public interface GlAccountBalanceByCostCenter {}

    @Screen(name = "GlAccountBalanceByCostCenterPdf", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "partyIds", value = "${groovy:org.ofbiz.party.party.PartyWorker.getAssociatedPartyIdsByRelationshipType(delegator, organizationPartyId, 'GROUP_ROLLUP')}", valueType = "List")
    @Action(type = ActionType.SET, field = "partyIds[]", fromField = "organizationPartyId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/CostCenters.groovy")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/reports/CostCentersReport.fo.ftl", platform = "xsl-fo"
            )})
        }
    )
    public interface GlAccountBalanceByCostCenterPdf {}

    @Screen(name = "InventoryValuation", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "AccountingInventoryValuation")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "InventoryValuation")
    @Action(type = ActionType.SET, field = "parameters.thruDate", fromField = "parameters.thruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @DecoratorScreen(
        name = "CommonOrganizationAccountingReportsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "InventoryValuation", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(sections = {
                        @SectionLeaf(condition = @Condition(type = Compare.class, params = {"parameters.showSearchResults", "equals", "Y"
                    }), widgets = @WidgetsLeaf(value = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsPdf}", style = "${styles.link_run_sys} ${styles.action_export}", target = "InventoryValuation.pdf", targetWindow = "_BLANK"
                    ),
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsCsv}", style = "${styles.link_run_sys} ${styles.action_export}", target = "InventoryValuation.csv"
                ),
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "InventoryValuation-part1", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml", shareScope = true
            )}))}))})})
        }
    )
    public interface InventoryValuation {}

    @Screen(name = "InventoryValuation-part1", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.AccountingInventoryValuationList}", includeForms = {@IncludeForm(name = "InventoryValuationList", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml")})}))
    public interface InventoryValuation_part1 {}

    @Screen(name = "InventoryValuationPdf", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(includeForms = {
                    @IncludeForm(name = "InventoryValuationList", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 1
                )}, labels = {
                    @Label(text = "${uiLabelMap.AccountingInventoryValuation}", style = "heading", position = 0
                )})})
        }
    )
    public interface InventoryValuationPdf {}

    @Screen(name = "InventoryValuationCsv", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Section(widgets = @Widgets(containers = {@Container(includeForms = {@IncludeForm(name = "InventoryValuationList", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml")})}))
    public interface InventoryValuationCsv {}

    @Screen(name = "CashFlowStatement", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingCashFlowStatement")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "CashFlowStatement")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SERVICE, serviceName = "findLastClosedDate", resultMapName = "findLastClosedDateOutMap", fieldMaps = {@FieldMap(fieldName = "organizationPartyId", fromField = "organizationPartyId")})
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "parameters.glFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/MonthSelection.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/CashFlowStatement.groovy")
    @DecoratorScreen(
        name = "CommonOrganizationAccountingReportsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "CashFlowStatementParameters", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                )}),
                @Screenlet(includeForms = {
                    @IncludeForm(name = "CashFlowStatementOpeningCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 3
                ),
                @IncludeForm(name = "CashFlowStatementPeriodCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 5
            ),
            @IncludeForm(name = "CashFlowStatementClosingCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 7
            ),
            @IncludeForm(name = "CashFlowBalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 9
            )}, labels = {
                @Label(text = "${uiLabelMap.AccountingOpeningCashBalance}", position = 2
            ),
            @Label(text = "${uiLabelMap.AccountingPeriodCashBalance}", position = 4
            ),
            @Label(text = "${uiLabelMap.AccountingClosingCashBalance}", position = 6
            ),
            @Label(text = "${uiLabelMap.CommonTotal}", position = 8)}, widgets = {
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsCsv}", style = "${styles.link_run_sys} ${styles.action_export}", target = "CashFlowStatementListCsv.csv", position = 0
            ),
            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsPdf}", style = "${styles.link_run_sys} ${styles.action_export}", target = "CashFlowStatementListPdf.pdf", targetWindow = "_BLANK", position = 1
            )})})
        }
    )
    public interface CashFlowStatement {}

    @Screen(name = "CashFlowStatementListPdf", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "parameters.glFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/CashFlowStatement.groovy")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(includeForms = {
                    @IncludeForm(name = "FindCashFlowTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 1
                ),
                @IncludeForm(name = "CashFlowStatementOpeningCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 3
            ),
            @IncludeForm(name = "CashFlowStatementPeriodCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 5
            ),
            @IncludeForm(name = "CashFlowStatementClosingCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 7
            ),
            @IncludeForm(name = "CashFlowBalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 9
            )}, labels = {
                @Label(text = "${uiLabelMap.AccountingCashFlowStatement}", style = "heading", position = 0
            ),
            @Label(text = "${uiLabelMap.AccountingOpeningCashBalance}", position = 2
            ),
            @Label(text = "${uiLabelMap.AccountingPeriodCashBalance}", position = 4
            ),
            @Label(text = "${uiLabelMap.AccountingClosingCashBalance}", position = 6
            ),
            @Label(text = "${uiLabelMap.CommonTotal}", position = 8)})})
        }
    )
    public interface CashFlowStatementListPdf {}

    @Screen(name = "CashFlowStatementListCsv", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "parameters.glFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/CashFlowStatement.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingOpeningCashBalance}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "CashFlowStatementOpeningCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingPeriodCashBalance}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "CashFlowStatementPeriodCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingClosingCashBalance}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "CashFlowStatementClosingCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonTotal}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "CashFlowBalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml")}))
    public interface CashFlowStatementListCsv {}

    @Screen(name = "ComparativeCashFlowStatement", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingComparativeCashFlowStatement")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ComparativeCashFlowStatement")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", valueType = "String")
    @Action(type = ActionType.SERVICE, serviceName = "findLastClosedDate", resultMapName = "findLastClosedDateOutMap", fieldMaps = {@FieldMap(fieldName = "organizationPartyId", fromField = "organizationPartyId")})
    @Action(type = ActionType.SET, field = "period1FromDate", fromField = "parameters.period1FromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "period1ThruDate", fromField = "parameters.period1ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period1GlFiscalTypeId", fromField = "parameters.period1GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "period1FromDate")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period1ThruDate")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period1GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/CashFlowStatement.groovy")
    @Action(type = ActionType.SET, field = "openingCashBalanceList1", fromField = "openingCashBalanceList")
    @Action(type = ActionType.SET, field = "periodCashBalanceList1", fromField = "periodCashBalanceList")
    @Action(type = ActionType.SET, field = "closingCashBalanceList1", fromField = "closingCashBalanceList")
    @Action(type = ActionType.SET, field = "cashFlowBalanceTotalList1", fromField = "cashFlowBalanceTotalList")
    @Action(type = ActionType.SET, field = "period2FromDate", fromField = "parameters.period2FromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "period2ThruDate", fromField = "parameters.period2ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period2GlFiscalTypeId", fromField = "parameters.period2GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "period2FromDate")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period2ThruDate")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period2GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/CashFlowStatement.groovy")
    @Action(type = ActionType.SET, field = "openingCashBalanceList2", fromField = "openingCashBalanceList")
    @Action(type = ActionType.SET, field = "periodCashBalanceList2", fromField = "periodCashBalanceList")
    @Action(type = ActionType.SET, field = "closingCashBalanceList2", fromField = "closingCashBalanceList")
    @Action(type = ActionType.SET, field = "cashFlowBalanceTotalList2", fromField = "cashFlowBalanceTotalList")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/ComparativeCashFlowStatement.groovy")
    @DecoratorScreen(
        name = "CommonOrganizationAccountingReportsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ComparativeCashFlowStatementParameters", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"
                )}),
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ComparativeCashFlowStatementOpeningCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 3
                ),
                @IncludeForm(name = "ComparativeCashFlowStatementPeriodCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 5
            ),
            @IncludeForm(name = "ComparativeCashFlowStatementClosingCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 7
            ),
            @IncludeForm(name = "ComparativeCashFlowBalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 9
            )}, labels = {
                @Label(text = "${uiLabelMap.AccountingOpeningCashBalance}", position = 2
            ),
            @Label(text = "${uiLabelMap.AccountingPeriodCashBalance}", position = 4
            ),
            @Label(text = "${uiLabelMap.AccountingClosingCashBalance}", position = 6
            ),
            @Label(text = "${uiLabelMap.CommonTotal}", position = 8)}, widgets = {
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsCsv}", style = "${styles.link_run_sys} ${styles.action_export}", target = "ComparativeCashFlowStatement.csv", position = 0
            ),
            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsPdf}", style = "${styles.link_run_sys} ${styles.action_export}", target = "ComparativeCashFlowStatement.pdf", targetWindow = "_BLANK", position = 1
            )})})
        }
    )
    public interface ComparativeCashFlowStatement {}

    @Screen(name = "ComparativeCashFlowStatementPdf", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingComparativeCashFlowStatement")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ComparativeCashFlowStatement")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", valueType = "String")
    @Action(type = ActionType.SERVICE, serviceName = "findLastClosedDate", resultMapName = "findLastClosedDateOutMap", fieldMaps = {@FieldMap(fieldName = "organizationPartyId", fromField = "organizationPartyId")})
    @Action(type = ActionType.SET, field = "period1FromDate", fromField = "parameters.period1FromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "period1ThruDate", fromField = "parameters.period1ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period1GlFiscalTypeId", fromField = "parameters.period1GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "period1FromDate")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period1ThruDate")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period1GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/CashFlowStatement.groovy")
    @Action(type = ActionType.SET, field = "openingCashBalanceList1", fromField = "openingCashBalanceList")
    @Action(type = ActionType.SET, field = "periodCashBalanceList1", fromField = "periodCashBalanceList")
    @Action(type = ActionType.SET, field = "closingCashBalanceList1", fromField = "closingCashBalanceList")
    @Action(type = ActionType.SET, field = "cashFlowBalanceTotalList1", fromField = "cashFlowBalanceTotalList")
    @Action(type = ActionType.SET, field = "period2FromDate", fromField = "parameters.period2FromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "period2ThruDate", fromField = "parameters.period2ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period2GlFiscalTypeId", fromField = "parameters.period2GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "period2FromDate")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period2ThruDate")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period2GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/CashFlowStatement.groovy")
    @Action(type = ActionType.SET, field = "openingCashBalanceList2", fromField = "openingCashBalanceList")
    @Action(type = ActionType.SET, field = "periodCashBalanceList2", fromField = "periodCashBalanceList")
    @Action(type = ActionType.SET, field = "closingCashBalanceList2", fromField = "closingCashBalanceList")
    @Action(type = ActionType.SET, field = "cashFlowBalanceTotalList2", fromField = "cashFlowBalanceTotalList")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/ComparativeCashFlowStatement.groovy")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(includeForms = {
                    @IncludeForm(name = "ComparativeCashFlowStatementParametersOneColumn", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 1
                ),
                @IncludeForm(name = "ComparativeCashFlowStatementOpeningCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 3
            ),
            @IncludeForm(name = "ComparativeCashFlowStatementPeriodCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 5
            ),
            @IncludeForm(name = "ComparativeCashFlowStatementClosingCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 7
            ),
            @IncludeForm(name = "ComparativeCashFlowBalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml", position = 9
            )}, labels = {
                @Label(text = "${uiLabelMap.AccountingComparativeCashFlowStatement}", style = "heading", position = 0
            ),
            @Label(text = "${uiLabelMap.AccountingOpeningCashBalance}", position = 2
            ),
            @Label(text = "${uiLabelMap.AccountingPeriodCashBalance}", position = 4
            ),
            @Label(text = "${uiLabelMap.AccountingClosingCashBalance}", position = 6
            ),
            @Label(text = "${uiLabelMap.CommonTotal}", position = 8)})})
        }
    )
    public interface ComparativeCashFlowStatementPdf {}

    @Screen(name = "ComparativeCashFlowStatementCsv", location = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "viewSize", value = "99999")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingComparativeCashFlowStatement")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ComparativeCashFlowStatement")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", valueType = "String")
    @Action(type = ActionType.SERVICE, serviceName = "findLastClosedDate", resultMapName = "findLastClosedDateOutMap", fieldMaps = {@FieldMap(fieldName = "organizationPartyId", fromField = "organizationPartyId")})
    @Action(type = ActionType.SET, field = "period1FromDate", fromField = "parameters.period1FromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "period1ThruDate", fromField = "parameters.period1ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period1GlFiscalTypeId", fromField = "parameters.period1GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "period1FromDate")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period1ThruDate")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period1GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/CashFlowStatement.groovy")
    @Action(type = ActionType.SET, field = "openingCashBalanceList1", fromField = "openingCashBalanceList")
    @Action(type = ActionType.SET, field = "periodCashBalanceList1", fromField = "periodCashBalanceList")
    @Action(type = ActionType.SET, field = "closingCashBalanceList1", fromField = "closingCashBalanceList")
    @Action(type = ActionType.SET, field = "cashFlowBalanceTotalList1", fromField = "cashFlowBalanceTotalList")
    @Action(type = ActionType.SET, field = "period2FromDate", fromField = "parameters.period2FromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")
    @Action(type = ActionType.SET, field = "period2ThruDate", fromField = "parameters.period2ThruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(type = ActionType.SET, field = "period2GlFiscalTypeId", fromField = "parameters.period2GlFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "period2FromDate")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "period2ThruDate")
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "period2GlFiscalTypeId")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/CashFlowStatement.groovy")
    @Action(type = ActionType.SET, field = "openingCashBalanceList2", fromField = "openingCashBalanceList")
    @Action(type = ActionType.SET, field = "periodCashBalanceList2", fromField = "periodCashBalanceList")
    @Action(type = ActionType.SET, field = "closingCashBalanceList2", fromField = "closingCashBalanceList")
    @Action(type = ActionType.SET, field = "cashFlowBalanceTotalList2", fromField = "cashFlowBalanceTotalList")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/reports/ComparativeCashFlowStatement.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingOpeningCashBalance}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ComparativeCashFlowStatementOpeningCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingPeriodCashBalance}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ComparativeCashFlowStatementPeriodCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingClosingCashBalance}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ComparativeCashFlowStatementClosingCashBalance", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonTotal}"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ComparativeCashFlowBalanceTotals", location = "component://accounting/widget/journals/ReportFinancialSummaryForms.xml")}))
    public interface ComparativeCashFlowStatementCsv {}

}
