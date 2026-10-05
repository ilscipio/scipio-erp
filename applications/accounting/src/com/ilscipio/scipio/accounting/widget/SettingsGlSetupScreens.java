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
public class SettingsGlSetupScreens {

    @Screen(name = "ListCompanies", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingAvailableInternalOrganizations")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "settings")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.AccountingAvailableInternalOrganizations}")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PartyAcctgPreference", list = "parties")
    @Action(type = ActionType.SERVICE, serviceName = "acctgPrefPermissionCheck", resultMapName = "permResult", fieldMaps = {@FieldMap(fieldName = "mainAction", value = "UPDATE")})
    @Action(type = ActionType.SET, field = "hasPrefPermission", fromField = "permResult.hasPermission")
    @Action(type = ActionType.SERVICE, serviceName = "basicGeneralLedgerPermissionCheck", resultMapName = "permResult", fieldMaps = {@FieldMap(fieldName = "mainAction", value = "VIEW")})
    @Action(type = ActionType.SET, field = "hasBasicPermission", fromField = "permResult.hasPermission")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListCompanies", location = "component://accounting/widget/settings/GlSetupForms.xml", position = 1
                )}, includeMenus = {
                    @IncludeMenu(name = "OrganizationSubTabBar", location = "component://accounting/widget/AccountingMenus.xml", position = 0
                )})})
        }
    )
    public interface ListCompanies {}

    @Screen(name = "AddCompany", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingNewCompany")
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "AddCompany", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )})})
        }
    )
    public interface AddCompany {}

    @Screen(name = "ListGlAccountOrganization", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingChartOfAcctsMenu")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListGlAccountOrganization")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.AccountingChartOfAcctsMenu}")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsCsv}", style = "${styles.link_run_sys} ${styles.action_export}", target = "ListGlAccountOrgCsv.csv"
            ),
            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingExportAsPdf}", style = "${styles.link_run_sys} ${styles.action_export}", target = "ListGlAccountOrgPdf.pdf", targetWindow = "_BLANK"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListGlAccountOrganization", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "GlAccountOrganizationPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AssignGlAccount", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface ListGlAccountOrganization {}

    @Screen(name = "PartyAcctgPreference", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingPreference")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.AccountingPreference}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "settings")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "PartyAcctgPreference")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyAcctgPreference", valueField = "partyAcctgPreference")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "PartyAcctgPreference", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )})})
        }
    )
    public interface PartyAcctgPreference {}

    @Screen(name = "SetupGlJournals", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingGlJournals")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.AccountingGlJournals}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "SetupGlJournals")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "GlJournal", valueField = "glJournal")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListGlJournals", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "GlJournalPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditGlJournal", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface SetupGlJournals {}

    @Screen(name = "GlAccountTypeDefaults", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingGlAccountTypeDefaults")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.AccountingGlAccountTypeDefaults}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountAssignment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "GlAccountTypeDefaults")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListGlAccountTypeDefaults", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonAdd}", name = "GlAccountTypeDefaultPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditGlAccountTypeDefault", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface GlAccountTypeDefaults {}

    @Screen(name = "GlAccountSalInvoice", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingInvoiceSales")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.AccountingInvoiceSales}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountAssignment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "GlAccountSalInvoice")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListSalInvoiceItemTypeGlAssignments", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAssignSalesInvoiceToRevenue}", name = "SalInvoiceItemTypeGlAsigmtPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddSalInvoiceItemTypeGlAssignment", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface GlAccountSalInvoice {}

    @Screen(name = "GlAccountPurInvoice", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingInvoicePurchase")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountAssignment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "GlAccountPurInvoice")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPurInvoiceItemTypeGlAssignments", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAssignPurchaseInvoiceToRevenue}", name = "PurInvoiceItemTypeGlAsigmtPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPurInvoiceItemTypeGlAssignment", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface GlAccountPurInvoice {}

    @Screen(name = "GlAccountTypePaymentType", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.AccountingPaymentType}/${uiLabelMap.FormFieldTitle_glAccountTypeId}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountAssignment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "GlAccountTypePaymentType")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPaymentTypeGlAssignments", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingPaymentTypeAssignAccountType}", name = "PaymentTypeGlAsigmtPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPaymentTypeGlAssignment", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface GlAccountTypePaymentType {}

    @Screen(name = "GlAccountNrPaymentMethod", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.AccountingPaymentMethodId}/${uiLabelMap.AccountingGlAccountId}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountAssignment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "GlAccountNrPaymentMethod")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPaymentMethodTypeGlAssignments", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingPaymentMethodAssignAccountType}", name = "PaymentMethodTypeGlAsigmtPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPaymentMethodTypeGlAssignment", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface GlAccountNrPaymentMethod {}

    @Screen(name = "EditProductGlAccounts", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingProductGlAccount")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.AccountingProductGlAccount}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountAssignment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "ProductGlAccounts")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductGlAccount", list = "productGlAccounts", conditions = {@ConditionExpr(fieldName = "organizationPartyId", fromField = "organizationPartyId")}, orderBy = {"glAccountTypeId"})
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListProductGlAccounts", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductAddGlAccount}", name = "ProductGlAccountPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddProductGlAccount", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditProductGlAccounts {}

    @Screen(name = "EditFinAccountTypeGlAccounts", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFinAccountTypeGlAccount")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.AccountingFinAccountTypeGlAccount}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountAssignment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "FinAccountTypeGlAccounts")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "FinAccountTypeGlAccount", list = "finAccountTypeGlAccounts", conditions = {@ConditionExpr(fieldName = "organizationPartyId", operator = "equals", fromField = "organizationPartyId")}, orderBy = {"finAccountTypeId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "FinAccountType", list = "finAccountTypes", useCache = true, orderBy = {"finAccountTypeId"})
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFinAccountTypeGlAccounts", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAddFinAccountTypeGlAccount}", name = "FinAccountTypeGlAccountPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddFinAccountTypeGlAccount", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditFinAccountTypeGlAccounts {}

    @Screen(name = "EditProductCategoryGlAccounts", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingProductCategoryGlAccount")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.AccountingProductCategoryGlAccount}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountAssignment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "ProductCategoryGlAccounts")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductCategoryGlAccount", list = "productCategoryGlAccounts", conditions = {@ConditionExpr(fieldName = "organizationPartyId", fromField = "organizationPartyId")}, orderBy = {"glAccountTypeId"})
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListProductCategoryGlAccounts", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductAddCategoryGlAccount}", name = "ProductCategoryGlAccountPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddProductCategoryGlAccount", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditProductCategoryGlAccounts {}

    @Screen(name = "EditVarianceReasonGlAccounts", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingVarianceReasonGlAccounts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountAssignment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "VarianceReasonGlAccounts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.AccountingVarianceReasonGlAccounts}")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "VarianceReasonGlAccount", list = "varianceReasonGlAccounts", conditions = {@ConditionExpr(fieldName = "organizationPartyId", fromField = "organizationPartyId")}, orderBy = {"glAccountId"})
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListVarianceReasonGlAccounts", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingVarianceReasonGlAccounts}", name = "VarianceReasonGlAccountPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddVarianceReasonGlAccount", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditVarianceReasonGlAccounts {}

    @Screen(name = "EditCreditCardTypeGlAccounts", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingCreditCardTypeGlAccount")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountAssignment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "CreditCardTypeGlAccount")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.AccountingCreditCardTypeGlAccount}")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CreditCardTypeGlAccount", list = "creditCardTypeGlAccounts", conditions = {@ConditionExpr(fieldName = "organizationPartyId", fromField = "organizationPartyId")})
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListCreditCardTypeGlAccounts", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingCreditCardTypeGlAccount}", name = "CreditCardTypeGlAccountPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddCreditCardTypeGlAccount", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditCreditCardTypeGlAccounts {}

    @Screen(name = "EditOrganizationTaxAuthorityGlAccounts", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditTaxAuthorityGlAccounts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountAssignment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "TaxAuthorityGlAccounts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.PageTitleEditTaxAuthorityGlAccounts}")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "TaxAuthorityGlAccount", list = "taxAuthorityGlAccounts", conditions = {@ConditionExpr(fieldName = "organizationPartyId", fromField = "organizationPartyId")}, orderBy = {"taxAuthGeoId", "taxAuthPartyId"})
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/chartofaccounts/TaxAuthorityGlAccounts.groovy")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListTaxAuthorityGlAccounts", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAddTaxAuthorityGlAccount}", name = "OrgTaxAuthorityGlAccountPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddTaxAuthorityGlAccount", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditOrganizationTaxAuthorityGlAccounts {}

    @Screen(name = "EditPartyGlAccount", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditPartyGlAccounts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.PageTitleEditPartyGlAccounts}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountAssignment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "PartyGlAccounts")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PartyGlAccount", list = "partyGlAccounts", conditions = {@ConditionExpr(fieldName = "organizationPartyId", fromField = "organizationPartyId")}, orderBy = {"partyId"})
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartyGlAccounts", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAddPartyGlAccount}", name = "PartyGlAccountPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPartyGlAccount", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditPartyGlAccount {}

    @Screen(name = "FixedAssetTypeGlAccounts", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "FixedAssetTypeGlAccounts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.FixedAssetTypeGlAccounts}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountAssignment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "FixedAssetTypeGlAccounts")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFixedAssetTypeGlAccounts", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonAdd}", name = "FixedAssetTypeGlAccountPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddFixedAssetTypeGlAccount", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface FixedAssetTypeGlAccounts {}

    @Screen(name = "ListGlAccountOrgPdf", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "GlAccountOrganizationAndClass", list = "glAccountOrgAndClassList", conditions = {@ConditionExpr(fieldName = "organizationPartyId", fromField = "organizationPartyId")}, orderBy = {"glAccountId"})
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://accounting/webapp/accounting/reports/ChartOfAccount.fo.ftl", platform = "xsl-fo")}))
    public interface ListGlAccountOrgPdf {}

    @Screen(name = "ListGlAccountOrgCsv", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ListGlAccountOrgCsv", location = "component://accounting/widget/settings/GlSetupForms.xml")}))
    public interface ListGlAccountOrgCsv {}

    @Screen(name = "FindGlAccountCategory", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "FormFieldTitle_findGlAccountCategory")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindGlAccountCategory")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.FormFieldTitle_newGlAccountCategory}", style = "${styles.link_nav} ${styles.action_add}", target = "EditGlAccountCategory"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindGlAccountCategory", location = "component://accounting/widget/settings/GlSetupForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListGlAccountCategory", location = "component://accounting/widget/settings/GlSetupForms.xml"
                        )}))})), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingViewPermissionError}", style = "common-msg-error-perm"
                        )}))})
        }
    )
    public interface FindGlAccountCategory {}

    @Screen(name = "EditGlAccountCategory", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindGlAccountCategory")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "EditGlAccountCategory")
    @Action(type = ActionType.SET, field = "glAccountCategoryId", fromField = "parameters.glAccountCategoryId")
    @Action(type = ActionType.SET, field = "glAccountCategoryTypeId", fromField = "parameters.glAccountCategoryTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "GlAccountCategory", valueField = "glAccountCategory")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.glAccountCategory ? 'FormFieldTitle_editGlAccountCategory' : 'FormFieldTitle_newGlAccountCategory'}")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditGlAccountCategory", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"glAccountCategory"}
                )}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_MENU, name = "GlAccountCategoryTabBar", location = "component://accounting/widget/AccountingMenus.xml"
                )}), position = 0)})
        }
    )
    public interface EditGlAccountCategory {}

    @Screen(name = "EditGlAccountCategoryMember", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "FormFieldTitle_editGlAccountCategoryMember")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindGlAccountCategory")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "EditGlAccountCategoryMember")
    @Action(type = ActionType.SET, field = "glAccountCategoryId", fromField = "parameters.glAccountCategoryId")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListGlAccountCategoryMember", location = "component://accounting/widget/settings/GlSetupForms.xml", position = 1
                ),
                @IncludeForm(name = "AddGlAccountCategoryMember", location = "component://accounting/widget/settings/GlSetupForms.xml", position = 2
            )}, includeMenus = {
                @IncludeMenu(name = "GlAccountCategoryTabBar", location = "component://accounting/widget/AccountingMenus.xml", position = 0
            )})})
        }
    )
    public interface EditGlAccountCategoryMember {}

    @Screen(name = "ViewRateAmounts", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingRateAmounts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.AccountingRateAmounts}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewRateAmounts")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListRateAmounts", location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingUpdateRateAmount}", includeForms = {
                    @IncludeForm(name = "updateRateAmount", location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml"
                )}, position = 0)})
        }
    )
    public interface ViewRateAmounts {}

    @Screen(name = "ViewFXConversions", location = "component://accounting/widget/settings/GlSetupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFX")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.AccountingFX}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewFXConversions")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "UomConversionDated", list = "conversions", orderBy = {"uomId", "uomIdTo", "fromDate"})
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListConversions", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingUpdateFX}", name = "FxConversionPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "updateFXConversion", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface ViewFXConversions {}

}
