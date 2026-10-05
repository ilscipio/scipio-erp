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
public class LedgerGlScreens {

    @Screen(name = "PartyAccountsSummary", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingPartyAccountsSummary")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PartyAccountsSummary")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "AccountingPartyAccountsSummary")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", global = true)
    @Action(type = ActionType.SET, field = "partyIds[]", fromField = "organizationPartyId")
    @DecoratorScreen(
        name = "CommonPartyGlDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", includeScreens = {
                    @IncludeScreen(name = "StatsTransactions", location = "component://accounting/widget/ledger/GlScreens.xml"
                )})})
        }
    )
    public interface PartyAccountsSummary {}

    @Screen(name = "StatsTransactions", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(order = 0, type = ActionType.SET, field = "chartType", value = "bar")
    @Action(order = 1, type = ActionType.SET, field = "chartValue", value = "total")
    @Action(order = 2, type = ActionType.SET, field = "chartLibrary", value = "chart")
    @Action(order = 3, type = ActionType.SET, field = "xlabel")
    @Action(order = 4, type = ActionType.SET, field = "ylabel")
    @Action(order = 5, type = ActionType.SET, field = "label1", value = "${uiLabelMap.CommonTotal}")
    @Action(order = 6, type = ActionType.SET, field = "title", value = "Total Transactions per Type")
    @Action(order = 7, type = ActionType.SET, field = "glFiscalTypeId", fromField = "parameters.glFiscalTypeId", defaultValue = "ACTUAL")
    @Action(order = 8, type = ActionType.SERVICE, serviceName = "findLastClosedDate", resultMapName = "findLastClosedDateOutMap", fieldMaps = {@FieldMap(fieldName = "organizationPartyId", fromField = "organizationPartyId")})
    @IfAction(order = 9, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"findLastClosedDateOutMap.lastClosedDate"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp", defaultValue = "${findLastClosedDateOutMap.lastClosedDate}")}), elseActions = @Actions(value = {@Action(type = ActionType.SET, field = "fromDate", value = "${nowTimestamp}", valueType = "Timestamp")}))
    @Action(order = 10, type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp", defaultValue = "${nowTimestamp}")
    @Action(order = 11, type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/accountSummary/StatsAccountSummaryTotal.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/accountSummary/statsTransactionsTotals.ftl")}))
    public interface StatsTransactions {}

    @Screen(name = "FindAcctgTrans", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingAcctgTrans")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindAcctgTrans")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "AccountingAcctgTrans")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", global = true)
    @DecoratorScreen(
        name = "CommonPartyGlDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonSearchOptions}", includeForms = {
                    @IncludeForm(name = "FindAcctgTrans", location = "component://accounting/widget/ledger/GlForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.CommonSearchResults}", includeForms = {
                    @IncludeForm(name = "ListAcctgTrans", location = "component://accounting/widget/ledger/GlForms.xml", position = 1
                )}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Compare.class, params = {"parameters.performSearch", "equals", "Y"
                    })}), widgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsCsv}", style = "${styles.link_run_sys} ${styles.action_export}", target = "AcctgTransSearchResultsCsv.csv"
                    ),
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsPdf}", style = "${styles.link_run_sys} ${styles.action_export}", target = "AcctgTransSearchResultPdf.pdf", targetWindow = "_BLANK"
                )}), position = 0)})})
        }
    )
    public interface FindAcctgTrans {}

    @Screen(name = "FindAcctgTransEntries", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingAcctgTransEntries")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindAcctgTransEntries")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "AccountingAcctgTransEntries")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", global = true)
    @DecoratorScreen(
        name = "CommonPartyGlDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAcctgTransEntries}", includeForms = {
                    @IncludeForm(name = "FindAcctgTransEntries", location = "component://accounting/widget/ledger/GlForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Compare.class, params = {"parameters.performSearch", "equals", "Y"
                    }),
                    @Condition(type = Compare.class, params = {"parameters.reportType", "equals", "byAccount"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(title = "${uiLabelMap.AccountingAcctgTransEntries} ${uiLabelMap.AccountingByAccount}", includeForms = {
                        @IncludeForm(name = "ListFindAcctgTransEntriesByAccount", location = "component://accounting/widget/ledger/GlForms.xml"
                    )}, widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsCsv}", style = "${styles.link_run_sys} ${styles.action_export}", target = "AcctgTransEntriesSearchResultsCsv.csv"
                    ),
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsPdf}", style = "${styles.link_run_sys} ${styles.action_export}", target = "AcctgTransEntriesSearchResultsPdf.pdf", targetWindow = "_BLANK"
                )})})),
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"parameters.performSearch", "equals", "Y"
                }),
                @Condition(type = Compare.class, params = {"parameters.reportType", "equals", "byDate"
            })}), widgets = @InlineWidgets(screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAcctgTransEntries} ${uiLabelMap.AccountingByDate}", includeForms = {
                    @IncludeForm(name = "ListFindAcctgTransEntriesByDate", location = "component://accounting/widget/ledger/GlForms.xml"
                )}, widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsCsv}", style = "${styles.link_run_sys} ${styles.action_export}", target = "AcctgTransEntriesSearchResultsCsv.csv"
                ),
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsPdf}", style = "${styles.link_run_sys} ${styles.action_export}", target = "AcctgTransEntriesSearchResultsPdf.pdf", targetWindow = "_BLANK"
            )})}))})
        }
    )
    public interface FindAcctgTransEntries {}

    @Screen(name = "CreateAcctgTransAndEntries", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingCreateAcctgTransAndEntries")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindAcctgTrans")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", global = true)
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "AccountingCreateAcctgTransAndEntries")
    @DecoratorScreen(
        name = "CommonPartyGlDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "CreateAcctgTransAndEntries", location = "component://accounting/widget/ledger/GlForms.xml"
                )})})
        }
    )
    public interface CreateAcctgTransAndEntries {}

    @Screen(name = "EditAcctgTrans", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditTransaction")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindAcctgTrans")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", global = true)
    @Action(type = ActionType.SET, field = "acctgTransId", fromField = "parameters.acctgTransId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AcctgTrans", valueField = "acctgTrans")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AcctgTransEntry", valueField = "acctgTransEntry", fieldMaps = {@FieldMap(fieldName = "acctgTransId"), @FieldMap(fieldName = "acctgTransEntrySeqId", fromField = "parameters.editAcctgTransEntrySeqId")})
    @Action(type = ActionType.ENTITY_AND, entityName = "AcctgTransEntry", list = "acctgTransEntries", fieldMaps = {@FieldMap(fieldName = "acctgTransId")}, orderBy = {"acctgTransEntrySeqId"})
    @DecoratorScreen(
        name = "CommonPartyGlDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"acctgTransId"})}), widgets = @InlineWidgets(sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = Compare.class, params = {"acctgTrans.isPosted", "equals", "Y"
                        })}), widgets = @WidgetsForContainer(screenlets = {
                            @ScreenletNested(title = "${uiLabelMap.AccountingTransactionHeader}", includeForms = {
                    @IncludeForm(name = "ViewAcctgTrans", location = "component://accounting/widget/ledger/GlForms.xml"
                
                        )}),
                        @ScreenletNested(title = "${uiLabelMap.PageTitleViewTransactionEntries}", includeForms = {
                    @IncludeForm(name = "ViewAcctgTransEntries", location = "component://accounting/widget/ledger/GlForms.xml"
                
                    )})}), failWidgets = @WidgetsForContainer(screenlets = {
                        @ScreenletNested(title = "${uiLabelMap.AccountingTransactionHeader}", includeForms = {
                    @IncludeForm(name = "EditAcctgTrans", location = "component://accounting/widget/ledger/GlForms.xml"
                
                    )}),
                    @ScreenletNested(title = "${uiLabelMap.PageTitleAddTransactionEntry}", includeForms = {
                    @IncludeForm(name = "EditAcctgTransEntry", location = "component://accounting/widget/ledger/GlForms.xml"
                
                )}),
                @ScreenletNested(title = "${uiLabelMap.PageTitleEditTransactionEntries}", includeForms = {
                    @IncludeForm(name = "ListAcctgTransEntries", location = "component://accounting/widget/ledger/GlForms.xml"
                
            )})}))}))})
        }
    )
    public interface EditAcctgTrans {}

    @Screen(name = "ListUnpostedAcctgTrans", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleUnpostedTransactions")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindAcctgTrans")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", global = true)
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.PageTitleUnpostedTransactions}")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "AcctgTrans", list = "transactions", conditions = {@ConditionExpr(fieldName = "isPosted", operator = "not-equals", value = "Y")}, orderBy = {"transactionDate"})
    @DecoratorScreen(
        name = "CommonPartyGlDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListUnpostedAcctgTrans", location = "component://accounting/widget/ledger/GlForms.xml"
                )})})
        }
    )
    public interface ListUnpostedAcctgTrans {}

    @Screen(name = "ListChecksToPrint", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingPrintChecks")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ChecksTabButton")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "PrintChecksTabButton")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.AccountingPrintChecks}")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Payment", list = "payments", conditions = {@ConditionExpr(fieldName = "partyIdFrom", operator = "equals", fromField = "organizationPartyId"), @ConditionExpr(fieldName = "statusId", operator = "equals", value = "PMNT_NOT_PAID")}, orderBy = {"effectiveDate"})
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/admin/FilterOutReceipts.groovy")
    @DecoratorScreen(
        name = "CommonAdminChecksDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "checks-body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"ACCOUNTING", "_PRINT_CHECKS"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "ListChecksToPrint", location = "component://accounting/widget/payments/PaymentForms.xml"
                    )})}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingPrintChecksPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface ListChecksToPrint {}

    @Screen(name = "ListChecksToSend", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingSendChecks")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ChecksTabButton")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "SendChecksTabButton")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Payment", list = "payments", conditions = {@ConditionExpr(fieldName = "partyIdFrom", operator = "equals", fromField = "organizationPartyId"), @ConditionExpr(fieldName = "statusId", operator = "equals", value = "PMNT_NOT_PAID")}, orderBy = {"effectiveDate"})
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/admin/FilterOutReceipts.groovy")
    @DecoratorScreen(
        name = "CommonAdminChecksDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "checks-body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                    @OrCondition(ifHasPermission = {
                        @IfHasPermission(permission = "ACCOUNTING", action = "_UPDATE"
                    ),
                    @IfHasPermission(permission = "PAY_INFO", action = "_UPDATE")
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "ListChecksToSend", location = "component://accounting/widget/payments/PaymentForms.xml"
                    )})}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingUpdatePaymentPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface ListChecksToSend {}

    @Screen(name = "NewAcctgTrans", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingCreateAnAccountingTransaction")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindAcctgTrans")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", global = true)
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "AccountingCreateAnAccountingTransaction")
    @DecoratorScreen(
        name = "CommonPartyGlDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "CreateAcctgTrans", location = "component://accounting/widget/ledger/GlForms.xml"
                )})})
        }
    )
    public interface NewAcctgTrans {}

    @Screen(name = "FindGlAccountReconciliation", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingAcctTransEntryRecon")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "AccountReconciliation")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "AccountingAcctRecon")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", global = true)
    @Action(type = ActionType.SET, field = "glAccountId", fromField = "parameters.glAccountId")
    @DecoratorScreen(
        name = "CommonPartyGlDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonSearchOptions}", includeForms = {
                    @IncludeForm(name = "FindGlAccountReconciliation", location = "component://accounting/widget/ledger/GlForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.CommonSearchResults}", includeForms = {
                    @IncludeForm(name = "ListGlAccountReconciliation", location = "component://accounting/widget/ledger/GlForms.xml"
                )})})
        }
    )
    public interface FindGlAccountReconciliation {}

    @Screen(name = "EditGlReconciliation", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingAcctGlRecon")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "parameters.activeSubMenuItem", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", global = true)
    @Action(type = ActionType.SET, field = "glReconciliationId", fromField = "parameters.glReconciliationId", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "GlReconciliation", valueField = "glReconciliation")
    @DecoratorScreen(
        name = "CommonPartyGlDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingEditAcctRecon}", includeForms = {
                    @IncludeForm(name = "EditGlReconciliation", location = "component://accounting/widget/ledger/GlForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.AccountingEditAcctRecon}", includeForms = {
                    @IncludeForm(name = "ListGlReconciliationEntries", location = "component://accounting/widget/ledger/GlForms.xml"
                )})})
        }
    )
    public interface EditGlReconciliation {}

    @Screen(name = "FindGlAccountReconciliations", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingAcctGlRecon")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "AccountReconciliations")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "AccountingAcctRecons")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", global = true)
    @Action(type = ActionType.SET, field = "glAccountId", fromField = "parameters.glAccountId")
    @DecoratorScreen(
        name = "CommonPartyGlDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonSearchOptions}", includeForms = {
                    @IncludeForm(name = "FindGlAccountReconciliations", location = "component://accounting/widget/ledger/GlForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.CommonSearchResults}", includeForms = {
                    @IncludeForm(name = "ListGlAccountReconciliations", location = "component://accounting/widget/ledger/GlForms.xml"
                )})})
        }
    )
    public interface FindGlAccountReconciliations {}

    @Screen(name = "AcctgTransSearchResultsCsv", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Section(widgets = @Widgets(containers = {@Container(includeForms = {@IncludeForm(name = "AcctgTransSearchResultsCsv", location = "component://accounting/widget/ledger/GlForms.xml")})}))
    public interface AcctgTransSearchResultsCsv {}

    @Screen(name = "AcctgTransEntriesSearchResultsCsv", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Section(widgets = @Widgets(containers = {@Container(includeForms = {@IncludeForm(name = "AcctgTransEntriesSearchResultsCsv", location = "component://accounting/widget/ledger/GlForms.xml")})}))
    public interface AcctgTransEntriesSearchResultsCsv {}

    @Screen(name = "AcctgTransEntriesSearchResultsPdf", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", global = true)
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "AcctgTransAndEntries", list = "acctgTransEntryList", conditions = {@ConditionExpr(fieldName = "organizationPartyId", operator = "equals", fromField = "parameters.organizationPartyId"), @ConditionExpr(fieldName = "glAccountId", operator = "equals", fromField = "parameters.glAccountId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "acctgTransTypeId", operator = "equals", fromField = "parameters.acctgTransTypeId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "glFiscalTypeId", operator = "equals", fromField = "parameters.glFiscalTypeId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "glJournalId", operator = "equals", fromField = "parameters.glJournalId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "isPosted", operator = "equals", fromField = "parameters.isPosted", ignoreIfEmpty = true), @ConditionExpr(fieldName = "partyId", operator = "equals", fromField = "parameters.partyId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "invoiceId", operator = "equals", fromField = "parameters.invoiceId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "paymentId", operator = "equals", fromField = "parameters.paymentId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "productId", operator = "equals", fromField = "parameters.productId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "workEffortId", operator = "equals", fromField = "parameters.workEffortId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "shipmentId", operator = "equals", fromField = "parameters.shipmentId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "acctgTransId", operator = "equals", fromField = "parameters.acctgTransId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "transactionDate", operator = "greater-equals", fromField = "parameters.fromDate", ignoreIfEmpty = true), @ConditionExpr(fieldName = "transactionDate", operator = "less", fromField = "parameters.thruDate", ignoreIfEmpty = true)}, orderBy = {"-transactionDate"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/reports/AcctgTransEntriesSearchResult.fo.ftl", platform = "xsl-fo")}))
    public interface AcctgTransEntriesSearchResultsPdf {}

    @Screen(name = "AcctgTransSearchResultPdf", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", global = true)
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "AcctgTransAndEntries", list = "acctgTransList", distinct = true, conditions = {@ConditionExpr(fieldName = "organizationPartyId", operator = "equals", fromField = "organizationPartyId"), @ConditionExpr(fieldName = "acctgTransTypeId", operator = "equals", fromField = "parameters.acctgTransTypeId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "glFiscalTypeId", operator = "equals", fromField = "parameters.glFiscalTypeId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "glJournalId", operator = "equals", fromField = "parameters.glJournalId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "isPosted", operator = "equals", fromField = "parameters.isPosted", ignoreIfEmpty = true), @ConditionExpr(fieldName = "invoiceId", operator = "equals", fromField = "parameters.invoiceId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "paymentId", operator = "equals", fromField = "parameters.paymentId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "productId", operator = "equals", fromField = "parameters.productId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "workEffortId", operator = "equals", fromField = "parameters.workEffortId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "shipmentId", operator = "equals", fromField = "parameters.shipmentId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "acctgTransId", operator = "equals", fromField = "parameters.acctgTransId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "transactionDate", operator = "greater-equals", fromField = "parameters.fromDate", ignoreIfEmpty = true), @ConditionExpr(fieldName = "transactionDate", operator = "less", fromField = "parameters.thruDate", ignoreIfEmpty = true)}, orderBy = {"-transactionDate"}, selectFields = {"acctgTransId", "transactionDate", "acctgTransTypeId", "glFiscalTypeId", "invoiceId", "paymentId", "workEffortId", "shipmentId", "isPosted", "postedDate"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/reports/AcctgTransSearchResult.fo.ftl", platform = "xsl-fo")}))
    public interface AcctgTransSearchResultPdf {}

    @Screen(name = "AcctgTransDetailReportPdf", location = "component://accounting/widget/ledger/GlScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "acctgTransId", fromField = "parameters.acctgTransId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AcctgTrans", valueField = "acctgTrans")
    @Action(type = ActionType.ENTITY_AND, entityName = "AcctgTransEntry", list = "acctgTransEntries", fieldMaps = {@FieldMap(fieldName = "acctgTransId")}, orderBy = {"acctgTransEntrySeqId"})
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "AcctgTransDetailReportPdf", location = "component://accounting/widget/ledger/GlForms.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "AcctgTransEntriesDetailReportPdf", location = "component://accounting/widget/ledger/GlForms.xml"
            )})
        }
    )
    public interface AcctgTransDetailReportPdf {}

}
