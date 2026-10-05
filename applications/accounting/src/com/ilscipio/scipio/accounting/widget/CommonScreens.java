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
public class CommonScreens {

    @Screen(name = "webapp-common-actions", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "MainSideBarMenu")
    @Action(type = ActionType.SET, field = "mainSideBarMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "mainComplexMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "menuCfg")
    public interface webapp_common_actions {}

    @Screen(name = "main-decorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.AccountingCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.AccountingCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "activeApp", value = "accounting", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuName", value = "AccountingAppBar", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuLocation", value = "component://accounting/widget/AccountingMenus.xml", global = true)
    @Action(type = ActionType.SET, field = "applicationTitle", value = "${uiLabelMap.AccountingManagerApplication}", global = true)
    @Action(type = ActionType.SET, field = "menuCfg", fromField = "mainComplexMenuCfg")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "DeriveComplexSideBarMenuItems", location = "component://common/widget/CommonScreens.xml")
    @DecoratorScreen(
        name = "ApplicationDecorator",
        location = "component://commonext/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = EmptySection.class, params = {"left-column"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "left-column"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "DefMainSideBarMenu", location = "component://accounting/widget/CommonScreens.xml"
                )}))}),
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface main_decorator {}

    @Screen(name = "CommonAccountingAppDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonAccountingAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}))
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonAccountingAppSideBarMenu", location = "component://accounting/widget/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"commonAccountingAppBasePermCond"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonAccountingAppDecorator {}

    @Screen(name = "CommonApDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://accounting/widget/AccountingMenus.xml#AccountsPayableSideBar")
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonApDecorator {}

    @Screen(name = "CommonArDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://accounting/widget/AccountingMenus.xml#AccountsReceivableSideBar")
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonArDecorator {}

    @Screen(name = "CommonBillingAccountDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "billingaccount")
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonBillingAccountDecorator {}

    @Screen(name = "CommonGLDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "glAccountId", fromField = "parameters.glAccountId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "GlAccount", valueField = "glAccount")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://accounting/widget/AccountingMenus.xml#CommonGLSideBar")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "chartofaccounts")
    @Action(type = ActionType.SET, field = "activeSubMenuItemTop", fromField = "activeSubMenuItemTop", defaultValue = "chartofaccounts")
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonGLDecorator {}

    @Screen(name = "CommonPartyGlDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "currentOrganization", autoFieldMap = false, fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "organizationPartyId")})
    @DecoratorScreen(
        name = "CommonGLDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface CommonPartyGlDecorator {}

    @Screen(name = "CommonAgreementDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://accounting/widget/AccountingMenus.xml#ContractsSideBar")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgAgreementPermissionCheck", "VIEW"}), @Condition(type = NotEmpty.class, params = {"agreement"})}))
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"acctgAgreementPermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body", position = 1
                )}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"agreement"})}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.LABEL, text = "${agreement.description} [${agreement.agreementId}]", style = "heading"
                        )}, sections = {
                            @SectionNested2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                                @Condition(type = Empty.class, params = {"agreementItem"})}), widgets = @WidgetsForContainer2(containers = {
                                    @Container3(style = "button-bar", widgets = {
                                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingNewAgreement}", style = "${styles.link_nav} ${styles.action_add}", target = "EditAgreement"
                                    )}, includeMenus = {
                                        @IncludeMenu(name = "AgreementItemSubTabBar", location = "component://accounting/widget/AccountingMenus.xml"
                                    )})}), failWidgets = @WidgetsForContainer2(containers = {
                                        @Container3(style = "button-bar", widgets = {
                                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingNewAgreement}", style = "${styles.link_nav} ${styles.action_add}", target = "EditAgreement"
                                        )}, includeMenus = {
                                            @IncludeMenu(name = "AgreementTabBar", location = "component://accounting/widget/AccountingMenus.xml"
                                        )})}))}), position = 0)}), failWidgets = @InlineWidgets(value = {
                                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingViewPermissionError}\n                                ", style = "common-msg-error-perm"
                                        )}))})
        }
    )
    public interface CommonAgreementDecorator {}

    @Screen(name = "AgreementSubDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "AgreementItem")
    @DecoratorScreen(
        name = "CommonAgreementDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface AgreementSubDecorator {}

    @Screen(name = "CommonControllingDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://accounting/widget/AccountingMenus.xml#ControllingSideBar")
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonControllingDecorator {}

    @Screen(name = "CommonBudgetDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListBudgets")
    @Action(type = ActionType.SET, field = "budgetId", fromField = "budget.budgetId", defaultValue = "${parameters.budgetId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Budget", valueField = "budget")
    @Action(type = ActionType.ENTITY_AND, entityName = "BudgetStatus", list = "budgetStatus", fieldMaps = {@FieldMap(fieldName = "budgetId", fromField = "budgetId")}, orderBy = {"-statusDate"})
    @Action(type = ActionType.SET, field = "statusId", fromField = "budgetStatus[0].statusId")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.budgetId}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonControllingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"budgetId"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "BudgetTabBar", location = "component://accounting/widget/AccountingMenus.xml"
                    ),
                    @Widget(type = WidgetType.INCLUDE_MENU, name = "BudgetSubTabBar", location = "component://accounting/widget/AccountingMenus.xml"
                ),
                @Widget(type = WidgetType.LABEL, text = "Budget : [${budgetId}]", style = "heading"
            )}), position = 0)})
        }
    )
    public interface CommonBudgetDecorator {}

    @Screen(name = "CommonFinAccountDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://accounting/widget/AccountingMenus.xml#FinAccount")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccount", valueField = "finAccount")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.finAccountId}", valueType = "Boolean")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgFinAcctPermissionCheck", "VIEW"})}))
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"acctgFinAcctPermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonFinAccountDecorator {}

    @Screen(name = "CommonFixedAssetsDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItemTop", fromField = "activeSubMenuItemTop", defaultValue = "FixedAsset")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://accounting/widget/AccountingMenus.xml#FixedAssetsSideBar")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", defaultValue = "${defaultOrganizationPartyId}", global = true)
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonFixedAssetsDecorator {}

    @Screen(name = "FixedAssetDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://accounting/widget/AccountingMenus.xml#FixedAssetsSideBar")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "fixedAsset.fixedAssetId", defaultValue = "${parameters.fixedAssetId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAsset", valueField = "fixedAsset")
    @Action(type = ActionType.SET, field = "titleFormat", value = "\\${finalTitle}${groovy: context.fixedAsset ? (': ' + context.fixedAsset.fixedAssetName + ' [' + context.fixedAssetId + ']') : ''}")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.fixedAssetId}", valueType = "Boolean")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"fixedAssetPermissionCheck", "VIEW"})}))
    @DecoratorScreen(
        name = "CommonFixedAssetsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"fixedAssetPermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface FixedAssetDecorator {}

    @Screen(name = "CommonOrganizationAccountingReportsDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(order = 0, type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", defaultValue = "${defaultOrganizationPartyId}", global = true)
    @IfAction(order = 1, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"organizationPartyId"})}), then = @Actions(value = {@Action(type = ActionType.SERVICE, serviceName = "getPartyAccountingPreferences", resultMapName = "result", fieldMaps = {@FieldMap(fieldName = "organizationPartyId")}), @Action(type = ActionType.SET, field = "partyAcctgPreference", fromField = "result.partyAccountingPreference"), @Action(type = ActionType.SET, field = "currencyUomId", fromField = "partyAcctgPreference.baseCurrencyUomId"), @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "currentOrganization", autoFieldMap = false, fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "organizationPartyId")}), @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.organizationPartyId}", valueType = "Boolean")}), elseActions = @Actions(value = {@Action(type = ActionType.SET, field = "titleProperty", value = "AccountingAvailableInternalOrganizations"), @Action(type = ActionType.SET, field = "labelTitleProperty"), @Action(type = ActionType.ENTITY_CONDITION, entityName = "PartyAcctgPreference", list = "parties")}))
    @DecoratorScreen(
        name = "CommonReportsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"organizationPartyId"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_MENU, name = "OrganizationTabBar", location = "component://accounting/widget/AccountingMenus.xml"
                ),
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap[labelTitleProperty]} ${uiLabelMap.CommonFor}: ${currentOrganization.groupName} [${organizationPartyId}]", style = "heading"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}), failWidgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap[labelTitleProperty]} Select organization", style = "heading"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/reports/SelectOrganization.ftl"
            )}))})
        }
    )
    public interface CommonOrganizationAccountingReportsDecorator {}

    @Screen(name = "CommonInvoicesDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItemTop", fromField = "activeSubMenuItemTop", defaultValue = "ListInvoices")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://accounting/widget/AccountingMenus.xml#InvoicesSideBar")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgInvoicePermissionCheck", "VIEW"})}))
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"acctgInvoicePermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body", position = 1
                )}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"parameters.invoiceId"
                    })}), widgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "InvoiceSubTabBar", location = "component://accounting/widget/AccountingMenus.xml"
                    )}), position = 0)}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface CommonInvoicesDecorator {}

    @Screen(name = "InvoiceDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeMainMenu", fromField = "activeMainMenu", defaultValue = "InvoiceSideBar")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "InvoiceSideBar")
    @Action(type = ActionType.SET, field = "invoiceId", fromField = "invoice.invoiceId", defaultValue = "${parameters.invoiceId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Invoice", valueField = "invoice")
    @Action(type = ActionType.SET, field = "titleFormat", value = "\\${finalTitle}${groovy: context.invoiceId ? ('[' + context.invoiceId + ']') : ''}")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.invoiceId}", valueType = "Boolean")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgInvoicePermissionCheck", "VIEW"})}))
    @DecoratorScreen(
        name = "CommonInvoicesDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"acctgInvoicePermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface InvoiceDecorator {}

    @Screen(name = "CommonImportExportDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://accounting/widget/AccountingMenus.xml#ImportExportSideBar")
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonImportExportDecorator {}

    @Screen(name = "CommonTransactionsDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://accounting/widget/AccountingMenus.xml#TransactionsSideBar")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", defaultValue = "${defaultOrganizationPartyId}", global = true)
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonTransactionsDecorator {}

    @Screen(name = "CommonReportsDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://accounting/widget/AccountingMenus.xml#TransactionReportsSideBar")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", defaultValue = "${defaultOrganizationPartyId}", global = true)
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonReportsDecorator {}

    @Screen(name = "CommonPartyDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItemTop", fromField = "activeSubMenuItemTop", defaultValue = "Journals")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", defaultValue = "${defaultOrganizationPartyId}", global = true)
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.organizationPartyId}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonReportsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"organizationPartyId"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingUserOrgNotSpecified}", style = "common-msg-error"
                )}))})
        }
    )
    public interface CommonPartyDecorator {}

    @Screen(name = "CommonAdminChecksDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItemTop", fromField = "activeSubMenuItemTop", defaultValue = "PartyAccounts")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "currentOrganization", autoFieldMap = false, fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "organizationPartyId")})
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "OrganizationTabBar", location = "component://accounting/widget/AccountingMenus.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_MENU, name = "PartyAccountingChecksTabBar", location = "component://accounting/widget/AccountingMenus.xml"
            ),
            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonFor}: ${currentOrganization.groupName} [${organizationPartyId}]", style = "heading"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "checks-body"
            )})
        }
    )
    public interface CommonAdminChecksDecorator {}

    @Screen(name = "CommonPaymentDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://accounting/widget/AccountingMenus.xml#PaymentSideBar")
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "PaymentSubTabBar", location = "component://accounting/widget/AccountingMenus.xml"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonPaymentDecorator {}

    @Screen(name = "CommonPaymentGroupDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://accounting/widget/AccountingMenus.xml#PaymentGroup")
    @Action(type = ActionType.SET, field = "paymentGroupId", fromField = "parameters.paymentGroupId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGroup", valueField = "paymentGroup")
    @Action(type = ActionType.ENTITY_AND, entityName = "PaymentGroupMember", list = "paymentGroupMembers", filterByDate = true, fieldMaps = {@FieldMap(fieldName = "paymentGroupId")})
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.paymentGroup}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "PaymentGroupSubTabBar", location = "component://accounting/widget/AccountingMenus.xml"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonPaymentGroupDecorator {}

    @Screen(name = "CommonGlSetupDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://accounting/widget/AccountingMenus.xml#PartyAdmin")
    @Action(type = ActionType.SET, field = "activeSubMenuItemTop", fromField = "activeSubMenuItemTop", defaultValue = "Admin")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "currentOrganization", autoFieldMap = false, fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "organizationPartyId")})
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${labelTitleProperty} ${uiLabelMap.CommonFor}: ${currentOrganization.groupName} [${organizationPartyId}]", style = "heading"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonGlSetupDecorator {}

    @Screen(name = "CommonSettingsDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://accounting/widget/AccountingMenus.xml#SettingsSideBar")
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonSettingsDecorator {}

    @Screen(name = "CommonTaxAuthorityDecorator", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "TaxAuthority")
    @Action(type = ActionType.SET, field = "taxAuthPartyId", fromField = "parameters.taxAuthPartyId")
    @Action(type = ActionType.SET, field = "taxAuthGeoId", fromField = "parameters.taxAuthGeoId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "TaxAuthority", valueField = "taxAuthority")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "taxAuthPartyName", autoFieldMap = false, fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "taxAuthority.taxAuthPartyId")})
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "taxAuthority", relationName = "TaxAuthGeo", toValueField = "taxAuthGeo", useCache = true)
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"taxAuthority"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonFor}: ${taxAuthPartyName.firstName} ${taxAuthPartyName.middleName} ${taxAuthPartyName.lastName} ${taxAuthPartyName.groupName} [${taxAuthority.taxAuthPartyId}] / ${taxAuthGeo.geoName} [${taxAuthority.taxAuthGeoId}]", style = "heading+2"
                    )}), position = 0)})
        }
    )
    public interface CommonTaxAuthorityDecorator {}

    @Screen(name = "MainSideBarMenu", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "menuCfg.location", value = "component://accounting/widget/AccountingMenus.xml")
    @Action(type = ActionType.SET, field = "menuCfg.name", value = "AccountingAppSideBar")
    @Action(type = ActionType.SET, field = "menuCfg.defLocation", value = "component://accounting/widget/AccountingMenus.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ComplexSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface MainSideBarMenu {}

    @Screen(name = "DefMainSideBarMenu", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/PrepareDefComplexSideBarMenu.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "MainSideBarMenu")}))
    public interface DefMainSideBarMenu {}

    @Screen(name = "CommonAccountingAppSideBarMenu", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonAccountingAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.SET, field = "commonSideBarMenu.cond", fromField = "commonSideBarMenu.cond", valueType = "Boolean", defaultValue = "${commonAccountingAppBasePermCond}")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.menuLoc", value = "component://accounting/widget/CommonScreens.xml", valueType = "String")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface CommonAccountingAppSideBarMenu {}

    @Screen(name = "ScipioIncomesExpenses", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "chartType", value = "bar")
    @Action(type = ActionType.SET, field = "chartLibrary", value = "chart")
    @Action(type = ActionType.SET, field = "chartIntervalScope", value = "month")
    @Action(type = ActionType.SET, field = "chartIntervalCount", value = "6")
    @Action(type = ActionType.SET, field = "chartDatasets", value = "2")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", defaultValue = "${defaultOrganizationPartyId}", global = true)
    @Action(type = ActionType.SET, field = "glFiscalTypeId", fromField = "parameters.glFiscalTypeId", defaultValue = "ACTUAL")
    @Action(type = ActionType.SET, field = "xlabel")
    @Action(type = ActionType.SET, field = "ylabel")
    @Action(type = ActionType.SET, field = "label1", value = "${uiLabelMap.AccountingIncome}")
    @Action(type = ActionType.SET, field = "label2", value = "${uiLabelMap.AccountingExpenses}")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/script/com/ilscipio/scipio/accounting/dashboard/IncomeExpenses.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"totalMap"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.AccountingIncomesExpenses}", htmlTemplates = {@HtmlTemplate(location = "component://accounting/webapp/accounting/dashboard/incomeExpenses.ftl")})}))
    public interface ScipioIncomesExpenses {}

    @Screen(name = "creditCardFields", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "fieldNamePrefix", fromField = "ccfFieldNamePrefix", defaultValue = "${''}")
    @Action(type = ActionType.SET, field = "showSecurityCodeField", fromField = "ccfShowSecurityCodeField", valueType = "Boolean", defaultValue = "false")
    @Action(type = ActionType.SET, field = "ccfTemplateLocation", fromField = "ccfTemplateLocation", defaultValue = "component://accounting/webapp/accounting/common/creditcardfields.ftl")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "${ccfTemplateLocation}")}))
    public interface creditCardFields {}

    @Screen(name = "ApPastDueInvoices", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "invoiceTypeId", value = "PURCHASE_INVOICE")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "organizationPartyId", defaultValue = "${defaultOrganizationPartyId}", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/invoice/InvoiceReport.groovy")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "invoices", fromField = "PastDueInvoices")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"invoices"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.AccountingAccountsPayable}", includeScreens = {@IncludeScreen(name = "ScipioInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml")})}))
    public interface ApPastDueInvoices {}

    @Screen(name = "ApInvoicesDueSoon", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "invoices", fromField = "InvoicesDueSoon")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"invoices"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.AccountingInvoicesDueSoon}: (${InvoicesDueSoonTotalAmount})", includeScreens = {@IncludeScreen(name = "ScipioInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml")})}))
    public interface ApInvoicesDueSoon {}

    @Screen(name = "ArPastDueInvoices", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "invoiceTypeId", value = "SALES_INVOICE")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "organizationPartyId", defaultValue = "${defaultOrganizationPartyId}", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/invoice/InvoiceReport.groovy")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "invoices", fromField = "PastDueInvoices")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"invoices"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.AccountingAccountsReceivable}", includeScreens = {@IncludeScreen(name = "ScipioInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml")})}))
    public interface ArPastDueInvoices {}

    @Screen(name = "ArInvoicesDueSoon", location = "component://accounting/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "invoices", fromField = "InvoicesDueSoon")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"invoices"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.AccountingInvoicesDueSoon}: (${InvoicesDueSoonTotalAmount})", includeScreens = {@IncludeScreen(name = "ScipioInvoices", location = "component://accounting/widget/invoice/InvoiceScreens.xml")})}))
    public interface ArInvoicesDueSoon {}

}
