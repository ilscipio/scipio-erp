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

import com.ilscipio.scipio.widget.def.menu.*;
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
public class AccountingMenus {

    @Menu(
        name = "AccountingAppBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        title = "${uiLabelMap.AccountingAccounting}",
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "payable", title = "${uiLabelMap.AccountingAccountsPayable}", link = @MenuLink(target = "apmain")),
            @MenuItem(name = "receivable", title = "${uiLabelMap.AccountingAccountsReceivable}", link = @MenuLink(target = "armain")),
            @MenuItem(name = "chartofaccounts", title = "${uiLabelMap.AcctgChartOfAcctsTabMenu}", link = @MenuLink(target = "GlAccountNavigate")),
            @MenuItem(name = "payments", title = "${uiLabelMap.AccountingPaymentsMenu}", link = @MenuLink(target = "findPayments")),
            @MenuItem(name = "ListFixedAssets", title = "${uiLabelMap.AccountingFixedAssets}", link = @MenuLink(target = "ListFixedAssets")),
            @MenuItem(name = "billingaccount", title = "${uiLabelMap.AccountingBillingMenu}", link = @MenuLink(target = "FindBillingAccount")),
            @MenuItem(name = "FinAccount", title = "${uiLabelMap.AccountingFinAccount}", link = @MenuLink(target = "FinAccountMain")),
            @MenuItem(name = "ListInvoices", title = "${uiLabelMap.AccountingInvoices}", link = @MenuLink(target = "findInvoices")),
            @MenuItem(name = "Contracts", title = "${uiLabelMap.AccountingContracts}", link = @MenuLink(target = "FindAgreement")),
            @MenuItem(name = "Transactions", title = "${uiLabelMap.AccountingTransactions}", link = @MenuLink(target = "Transactions")),
            @MenuItem(name = "TransactionReports", title = "${uiLabelMap.AccountingReports}", link = @MenuLink(target = "TransactionReports")),
            @MenuItem(name = "controlling", title = "${uiLabelMap.AccountingControlling}", link = @MenuLink(target = "ListBudgets")),
            @MenuItem(name = "ImportExport", title = "${uiLabelMap.CommonImportExport}", link = @MenuLink(target = "ExportTransactions")),
            @MenuItem(name = "settings", title = "${uiLabelMap.CommonSettings}", link = @MenuLink(target = "settings"))
        }
    )
    public interface AccountingAppBar {}

    @Menu(
        name = "AccountingAppSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        title = "${uiLabelMap.AccountingManager}",
        extendsMenu = "CommonAppSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "AccountingAppBar")
        },
        items = {
            @MenuItem(name = "payable", subMenus = {@SubMenu(name = "AccountsPayable", include = "component://accounting/widget/AccountingMenus.xml#AccountsPayableSideBar")}),
            @MenuItem(name = "receivable", subMenus = {@SubMenu(name = "AccountsReceivable", include = "component://accounting/widget/AccountingMenus.xml#AccountsReceivableSideBar")}),
            @MenuItem(name = "billingaccount", subMenus = {@SubMenu(name = "BillingAccount", include = "component://accounting/widget/AccountingMenus.xml#BillingAccountSideBar")}),
            @MenuItem(name = "chartofaccounts", subMenus = {@SubMenu(name = "CommonGL", include = "component://accounting/widget/AccountingMenus.xml#CommonGLSideBar")}),
            @MenuItem(name = "Contracts", subMenus = {@SubMenu(name = "Contracts", include = "component://accounting/widget/AccountingMenus.xml#ContractsSideBar")}),
            @MenuItem(name = "controlling", subMenus = {@SubMenu(name = "Controlling", include = "component://accounting/widget/AccountingMenus.xml#ControllingSideBar")}),
            @MenuItem(name = "FinAccount", subMenus = {@SubMenu(name = "FinAccount", include = "component://accounting/widget/AccountingMenus.xml#FinAccountSideBar")}),
            @MenuItem(name = "ListFixedAssets", subMenus = {@SubMenu(name = "FixedAssets", include = "component://accounting/widget/AccountingMenus.xml#FixedAssetsSideBar")}),
            @MenuItem(name = "ListInvoices", subMenus = {@SubMenu(name = "Invoices", include = "component://accounting/widget/AccountingMenus.xml#InvoicesSideBar")}),
            @MenuItem(name = "ImportExport", subMenus = {@SubMenu(name = "ImportExport", include = "component://accounting/widget/AccountingMenus.xml#ImportExportSideBar")}),
            @MenuItem(name = "Transactions", subMenus = {@SubMenu(name = "Transactions", include = "component://accounting/widget/AccountingMenus.xml#TransactionsSideBar")}),
            @MenuItem(name = "TransactionReports", subMenus = {@SubMenu(name = "TransactionReports", include = "component://accounting/widget/AccountingMenus.xml#TransactionReportsSideBar")}),
            @MenuItem(name = "payments", subMenus = {@SubMenu(name = "Payment", include = "component://accounting/widget/AccountingMenus.xml#PaymentSideBar")}),
            @MenuItem(name = "settings", subMenus = {@SubMenu(name = "Settings", include = "component://accounting/widget/AccountingMenus.xml#SettingsSideBar")})
        }
    )
    public interface AccountingAppSideBar {}

    @Menu(
        name = "AccountsPayableSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "apmain", title = "${uiLabelMap.CommonOverview}", sortMode = "off", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"userLogin"})}), link = @MenuLink(target = "apmain", linkType = LinkType.ANCHOR)),
            @MenuItem(name = "findApInvoices", title = "${uiLabelMap.AccountingInvoicesMenu}", link = @MenuLink(target = "FindApInvoices")),
            @MenuItem(name = "findApPayments", title = "${uiLabelMap.AccountingPaymentsMenu}", link = @MenuLink(target = "FindApPayments")),
            @MenuItem(name = "apPaymentGroups", title = "${uiLabelMap.AccountingApPaymentGroupMenu}", link = @MenuLink(target = "FindApPaymentGroups"), subMenus = {@SubMenu(name = "PaymentGroup", include = "component://accounting/widget/AccountingMenus.xml#PaymentGroupSideBar")}),
            @MenuItem(name = "apReports", title = "${uiLabelMap.AccountingReports}", link = @MenuLink(target = "listAPReports"))
        }
    )
    public interface AccountsPayableSideBar {}

    @Menu(
        name = "AccountsReceivableSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "armain", title = "${uiLabelMap.CommonOverview}", overrideMode = "remove-replace", sortMode = "off", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"userLogin"})}), link = @MenuLink(target = "armain", linkType = LinkType.ANCHOR)),
            @MenuItem(name = "findArInvoices", title = "${uiLabelMap.AccountingInvoicesMenu}", link = @MenuLink(target = "findArInvoices")),
            @MenuItem(name = "findArPayments", title = "${uiLabelMap.AccountingPaymentsMenu}", link = @MenuLink(target = "findArPayments")),
            @MenuItem(name = "arPaymentGroups", title = "${uiLabelMap.AccountingArPaymentGroupMenu}", link = @MenuLink(target = "FindArPaymentGroups"), subMenus = {@SubMenu(name = "ArPaymentGroup", include = "component://accounting/widget/AccountingMenus.xml#PaymentGroupSideBar")}),
            @MenuItem(name = "arReports", title = "${uiLabelMap.AccountingReports}", link = @MenuLink(target = "ListARReports"))
        }
    )
    public interface AccountsReceivableSideBar {}

    @Menu(
        name = "BillingAccountSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml"
    )
    public interface BillingAccountSideBar {}

    @Menu(
        name = "BillingAccountTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditBillingAccount", title = "${uiLabelMap.CommonAccount}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"billingAccount.billingAccountId"})}), link = @MenuLink(target = "EditBillingAccount", parameters = {@MenuParameter(paramName = "billingAccountId", fromField = "billingAccount.billingAccountId")})),
            @MenuItem(name = "EditBillingAccountRoles", title = "${uiLabelMap.CommonRoles}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"billingAccount.billingAccountId"})}), link = @MenuLink(target = "EditBillingAccountRoles", parameters = {@MenuParameter(paramName = "billingAccountId", fromField = "billingAccount.billingAccountId")})),
            @MenuItem(name = "EditBillingAccountTerms", title = "${uiLabelMap.CommonTerms}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"billingAccount.billingAccountId"})}), link = @MenuLink(target = "EditBillingAccountTerms", parameters = {@MenuParameter(paramName = "billingAccountId", fromField = "billingAccount.billingAccountId")})),
            @MenuItem(name = "BillingAccountInvoices", title = "${uiLabelMap.CommonInvoices}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"billingAccount.billingAccountId"})}), link = @MenuLink(target = "BillingAccountInvoices", parameters = {@MenuParameter(paramName = "billingAccountId", fromField = "billingAccount.billingAccountId")})),
            @MenuItem(name = "BillingAccountPayments", title = "${uiLabelMap.CommonPayments}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"billingAccount.billingAccountId"})}), link = @MenuLink(target = "BillingAccountPayments", parameters = {@MenuParameter(paramName = "billingAccountId", fromField = "billingAccount.billingAccountId")})),
            @MenuItem(name = "BillingAccountOrders", title = "${uiLabelMap.CommonOrders}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"billingAccount.billingAccountId"})}), link = @MenuLink(target = "BillingAccountOrders", parameters = {@MenuParameter(paramName = "billingAccountId", fromField = "billingAccount.billingAccountId")}))
        }
    )
    public interface BillingAccountTabBar {}

    @Menu(
        name = "CommonGLSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "GlAccountNavigate",
        actions = @MenuActions(set = {@SetAction(field = "organizationPartyId", fromField = "parameters.organizationPartyId", defaultValue = "${defaultOrganizationPartyId}")}),
        items = {
            @MenuItem(name = "Chartofaccounts", title = "${uiLabelMap.AccountingChartOfAcctsMenu}", link = @MenuLink(target = "FindGlobalGlAccount")),
            @MenuItem(name = "GlAccountNavigate", title = "${uiLabelMap.AcctgNavigateAccts}", link = @MenuLink(target = "GlAccountNavigate")),
            @MenuItem(name = "FindAcctgTrans", title = "${uiLabelMap.AccountingAcctgTrans}", link = @MenuLink(target = "FindAcctgTrans", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")}))
        }
    )
    public interface CommonGLSideBar {}

    @Menu(
        name = "GlAccountTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "FindGlobalGlAccount", title = "${uiLabelMap.AcctgChartOfAcctsTabMenu}", link = @MenuLink(target = "FindGlobalGlAccount")),
            @MenuItem(name = "AssignGlAccount", title = "${uiLabelMap.AcctgAssignGlAccount}", link = @MenuLink(target = "AssignGlAccount"))
        }
    )
    public interface GlAccountTabBar {}

    @Menu(
        name = "GlSettingTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        selectedMenuItemContextFieldName = "activeSubMenuItem2",
        items = {
            @MenuItem(name = "GlAccountNavigate", title = "${uiLabelMap.AcctgNavigateAccts}", link = @MenuLink(target = "GlAccountNavigate", parameters = {@MenuParameter(paramName = "trail", value = "null")})),
            @MenuItem(name = "AssignGlAccount", title = "${uiLabelMap.AcctgAssignGlAccount}", link = @MenuLink(target = "AssignGlAccount"))
        }
    )
    public interface GlSettingTabBar {}

    @Menu(
        name = "GlAccountCategoryTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        selectedMenuItemContextFieldName = "activeSubMenuItem2",
        items = {
            @MenuItem(name = "EditGlAccountCategory", title = "${uiLabelMap.FormFieldTitle_glAccountCategory}", link = @MenuLink(target = "EditGlAccountCategory", parameters = {@MenuParameter(paramName = "glAccountCategoryId", fromField = "glAccountCategoryId"), @MenuParameter(paramName = "glAccountCategoryTypeId", fromField = "glAccountCategoryTypeId")})),
            @MenuItem(name = "EditGlAccountCategoryMember", title = "${uiLabelMap.FormFieldTitle_glAccountCategoryMember}", link = @MenuLink(target = "EditGlAccountCategoryMember", parameters = {@MenuParameter(paramName = "glAccountCategoryId", fromField = "glAccountCategoryId")}))
        }
    )
    public interface GlAccountCategoryTabBar {}

    @Menu(
        name = "GlAccountListSubTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "ListGlAccountsReport", title = "${uiLabelMap.CommonPrint}", widgetStyle = "+${styles.action_run_sys} ${styles.action_export}", link = @MenuLink(target = "ListGlAccountsReport")),
            @MenuItem(name = "ListGlAccountsExport", title = "${uiLabelMap.CommonExport}", widgetStyle = "+${styles.action_run_sys} ${styles.action_export}", link = @MenuLink(target = "ListGlAccountsExport"))
        }
    )
    public interface GlAccountListSubTabBar {}

    @Menu(
        name = "GlAccountListCombinedTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        itemsSortMode = "off"
    )
    public interface GlAccountListCombinedTabBar {}

    @Menu(
        name = "ContractsSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "EditAgreement",
        items = {
            @MenuItem(name = "EditAgreement", title = "${uiLabelMap.AccountingAgreement}", link = @MenuLink(target = "EditAgreement", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId")})),
            @MenuItem(name = "AgreementItems", title = "${uiLabelMap.CommonItems}", link = @MenuLink(target = "ListAgreementItems", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId")}), subMenus = {@SubMenu(name = "AgreementItem", include = "component://accounting/widget/AccountingMenus.xml#AgreementItemSideBar")}),
            @MenuItem(name = "AgreementRoles", title = "${uiLabelMap.CommonParties}", link = @MenuLink(target = "EditAgreementRoles", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId")})),
            @MenuItem(name = "AgreementTerms", title = "${uiLabelMap.AccountingAgreementTerms}", link = @MenuLink(target = "EditAgreementTerms", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId")})),
            @MenuItem(name = "AgreementWorkEffort", title = "${uiLabelMap.WorkEffort}", link = @MenuLink(target = "EditAgreementWorkEffortApplics", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId")}))
        }
    )
    public interface ContractsSideBar {}

    @Menu(
        name = "AgreementTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "EditAgreementItem",
        items = {
            @MenuItem(name = "EditAgreementItem", title = "${uiLabelMap.CommonNew} ${uiLabelMap.CommonItem}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditAgreementItem", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId")})),
            @MenuItem(name = "NewRole", title = "${uiLabelMap.CommonNew} ${uiLabelMap.CommonParty}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "EditAgreementRoles", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId")})),
            @MenuItem(name = "NewTerm", title = "${uiLabelMap.CommonNew} ${uiLabelMap.CommonTerm}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "EditAgreementTerms", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId")})),
            @MenuItem(name = "NewWorkEffort", title = "${uiLabelMap.CommonNew} ${uiLabelMap.WorkEffort}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "EditAgreementWorkEffortApplics", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId")})),
            @MenuItem(name = "NewGeo", title = "${uiLabelMap.CommonNew} ${uiLabelMap.CommonGeo}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "EditAgreementGeographicalApplic", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "NewFacility", title = "${uiLabelMap.ProductNewFacility}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "EditAgreementItemFacility", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "NewParty", title = "${uiLabelMap.CommonNew} ${uiLabelMap.CommonParty}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "EditAgreementItemParty", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "NewProduct", title = "${uiLabelMap.ProductNewProduct}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "EditAgreementItemProduct", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "NewPromo", title = "${uiLabelMap.AccountingNewAgreementPromoAppl}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "EditAgreementPromoAppl", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "NewItemTerm", title = "${uiLabelMap.CommonNew} ${uiLabelMap.CommonTerm}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "EditAgreementItemTerm", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")}))
        }
    )
    public interface AgreementTabBar {}

    @Menu(
        name = "AgreementItemTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "EditAgreementItem",
        items = {
            @MenuItem(name = "EditAgreementItem", title = "${uiLabelMap.AccountingAgreementItem}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "EditAgreementItem", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreementItem.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "ListAgreementGeographicalApplic", title = "${uiLabelMap.CommonGeo}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "ListAgreementGeographicalApplic", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreementItem.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "ListAgreementItemWarehouses", title = "${uiLabelMap.ProductFacilities}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"agreementItem.agreementItemSeqId"}), @Condition(type = Compare.class, params = {"agreement.agreementTypeId", "not-equals", "PURCHASE_AGREEMENT"})}), link = @MenuLink(target = "ListAgreementItemFacilities", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreementItem.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "ListAgreementItemParties", title = "${uiLabelMap.PartyParties}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "ListAgreementItemParties", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreementItem.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "ListAgreementItemProducts", title = "${uiLabelMap.ProductProducts}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"agreementItem.agreementItemSeqId"}), @Condition(type = Compare.class, params = {"agreement.agreementTypeId", "not-equals", "PURCHASE_AGREEMENT"})}), link = @MenuLink(target = "ListAgreementItemProducts", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreementItem.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "ListAgreementPromoAppls", title = "${uiLabelMap.AccountingAgreementPromoAppls}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "ListAgreementPromoAppls", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreementItem.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "ListAgreementItemSupplierProducts", title = "${uiLabelMap.ProductProducts}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"agreement.agreementTypeId", "equals", "PURCHASE_AGREEMENT"}), @Condition(type = NotEmpty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "ListAgreementItemSupplierProducts", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreementItem.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "ListAgreementItemTerms", title = "${uiLabelMap.AccountingAgreementItemTerms}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "ListAgreementItemTerms", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreementItem.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "FacilitiesReport", title = "${uiLabelMap.Facilities}: ${uiLabelMap.CommonPdf}", widgetStyle = "+${styles.action_run_sys} ${styles.action_export}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "ListAgreementItemFacilitiesReport", targetWindow = "_blank", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "ProductsReport", title = "${uiLabelMap.Products}: ${uiLabelMap.CommonPdf}", widgetStyle = "+${styles.action_run_sys} ${styles.action_export}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "ListAgreementItemProductsReport", targetWindow = "_blank", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")}))
        }
    )
    public interface AgreementItemTabBar {}

    @Menu(
        name = "AgreementItemSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "AgreementItemTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "EditAgreementItem"
    )
    public interface AgreementItemSideBar {}

    @Menu(
        name = "AgreementItemSubTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "NewItem", title = "${uiLabelMap.CommonNew} ${uiLabelMap.CommonItem}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditAgreementItem", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId")})),
            @MenuItem(name = "NewGeo", title = "${uiLabelMap.CommonNew} ${uiLabelMap.CommonGeo}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "EditAgreementGeographicalApplic", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "NewFacility", title = "${uiLabelMap.ProductNewFacility}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "EditAgreementItemFacility", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "NewParty", title = "${uiLabelMap.CommonNew} ${uiLabelMap.CommonParty}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "EditAgreementItemParty", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "NewProduct", title = "${uiLabelMap.ProductNewProduct}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "EditAgreementItemProduct", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "NewPromo", title = "${uiLabelMap.AccountingNewAgreementPromoAppl}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "EditAgreementPromoAppl", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})),
            @MenuItem(name = "NewTerm", title = "${uiLabelMap.AccountingNewAgreementItemTerm}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"agreementItem.agreementItemSeqId"})}), link = @MenuLink(target = "EditAgreementItemTerm", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreement.agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")}))
        }
    )
    public interface AgreementItemSubTabBar {}

    @Menu(
        name = "ControllingSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "ListBudgets", title = "${uiLabelMap.AccountingBudgets}", link = @MenuLink(target = "ListBudgets")),
            @MenuItem(name = "CostCenters", title = "${uiLabelMap.FormFieldTitle_costCenters}", link = @MenuLink(target = "CostCenters"))
        }
    )
    public interface ControllingSideBar {}

    @Menu(
        name = "BudgetTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "ListBudgets", title = "${uiLabelMap.AccountingBudgetFind}", link = @MenuLink(target = "ListBudgets")),
            @MenuItem(name = "EditBudget", title = "${uiLabelMap.AccountingBudget}", link = @MenuLink(target = "EditBudget", parameters = {@MenuParameter(paramName = "budgetId", fromField = "budgetId")})),
            @MenuItem(name = "BudgetOverview", title = "${uiLabelMap.AccountingBudgetOverview}", link = @MenuLink(target = "BudgetOverview", parameters = {@MenuParameter(paramName = "budgetId", fromField = "budgetId")})),
            @MenuItem(name = "BudgetItem", title = "${uiLabelMap.CommonItems}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "statusId", operator = "equals", value = "BG_CREATED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "statusId", operator = "equals", value = "BG_REVIEWED")})}, conditions = {@Condition(type = NotEmpty.class, params = {"budgetId"})}), link = @MenuLink(target = "EditBudgetItems", parameters = {@MenuParameter(paramName = "budgetId", fromField = "budgetId")})),
            @MenuItem(name = "BudgetRoles", title = "${uiLabelMap.CommonParties}", link = @MenuLink(target = "BudgetRoles", parameters = {@MenuParameter(paramName = "budgetId", fromField = "budgetId")})),
            @MenuItem(name = "BudgetReviews", title = "${uiLabelMap.AccountingBudgetReviews}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"budgetId"}), @Condition(type = Compare.class, params = {"statusId", "not-equals", "BG_REJECTED"})}), link = @MenuLink(target = "BudgetReviews", parameters = {@MenuParameter(paramName = "budgetId", fromField = "budgetId")}))
        }
    )
    public interface BudgetTabBar {}

    @Menu(
        name = "BudgetSubTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditBudget", title = "${uiLabelMap.CommonEdit}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "statusId", operator = "equals", value = "BG_CREATED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "statusId", operator = "equals", value = "BG_REVIEWED")})}, conditions = {@Condition(type = NotEmpty.class, params = {"budgetId"})}), link = @MenuLink(target = "EditBudget", parameters = {@MenuParameter(paramName = "budgetId", fromField = "budgetId")})),
            @MenuItem(name = "statusToApproved", title = "${uiLabelMap.AccountingBudgetStatusToApproved}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "statusId", operator = "equals", value = "BG_CREATED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "statusId", operator = "equals", value = "BG_REVIEWED")})}, conditions = {@Condition(type = NotEmpty.class, params = {"budgetId"})}), link = @MenuLink(target = "updateBudgetStatus", parameters = {@MenuParameter(paramName = "budgetId", fromField = "budgetId"), @MenuParameter(paramName = "statusId", value = "BG_APPROVED")})),
            @MenuItem(name = "statusToReview", title = "${uiLabelMap.AccountingBudgetStatusToReviewed}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"budgetId"}), @Condition(type = Compare.class, params = {"statusId", "equals", "BG_CREATED"})}), link = @MenuLink(target = "updateBudgetStatus", parameters = {@MenuParameter(paramName = "budgetId", fromField = "budgetId"), @MenuParameter(paramName = "statusId", value = "BG_REVIEWED")})),
            @MenuItem(name = "statusToReject", title = "${uiLabelMap.AccountingBudgetStatusToRejected}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "statusId", operator = "equals", value = "BG_CREATED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "statusId", operator = "equals", value = "BG_REVIEWED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "statusId", operator = "equals", value = "BG_APPROVED")})}, conditions = {@Condition(type = NotEmpty.class, params = {"budgetId"})}), link = @MenuLink(target = "updateBudgetStatus", parameters = {@MenuParameter(paramName = "budgetId", fromField = "budgetId"), @MenuParameter(paramName = "statusId", value = "BG_REJECTED")}))
        }
    )
    public interface BudgetSubTabBar {}

    @Menu(
        name = "FinAccountTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "EditFinAccount",
        items = {
            @MenuItem(name = "EditFinAccount", title = "${uiLabelMap.AccountingFinAccount}", link = @MenuLink(target = "EditFinAccount", parameters = {@MenuParameter(paramName = "finAccountId", fromField = "finAccountId")})),
            @MenuItem(name = "EditFinAccountRoles", title = "${uiLabelMap.CommonParties}", link = @MenuLink(target = "EditFinAccountRoles", parameters = {@MenuParameter(paramName = "finAccountId", fromField = "finAccountId")})),
            @MenuItem(name = "FinAccountTrans", title = "${uiLabelMap.AccountingFinAccountTransations}", link = @MenuLink(target = "FindFinAccountTrans", parameters = {@MenuParameter(paramName = "finAccountId", fromField = "finAccountId")})),
            @MenuItem(name = "EditFinAccountAuths", title = "${uiLabelMap.AccountingFinAccountAuth}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"finAccount.finAccountTypeId", "not-equals", "BANK_ACCOUNT"})}), link = @MenuLink(target = "EditFinAccountAuths", parameters = {@MenuParameter(paramName = "finAccountId", fromField = "finAccountId")})),
            @MenuItem(name = "depositWithdraw", title = "${uiLabelMap.AccountingDepositWithdraw}", link = @MenuLink(target = "FindPaymentsForDepositOrWithdraw", parameters = {@MenuParameter(paramName = "finAccountId", fromField = "finAccountId"), @MenuParameter(paramName = "organizationPartyId", fromField = "finAccount.ownerPartyId")})),
            @MenuItem(name = "findDepositSlips", title = "${uiLabelMap.AccountingDepositSlips}", link = @MenuLink(target = "FindDepositSlips", parameters = {@MenuParameter(paramName = "finAccountId", fromField = "finAccountId"), @MenuParameter(paramName = "organizationPartyId", fromField = "finAccount.ownerPartyId")})),
            @MenuItem(name = "FindFinAccountReconciliations", title = "${uiLabelMap.AccountingReconciliation}", link = @MenuLink(target = "FindFinAccountReconciliations", parameters = {@MenuParameter(paramName = "finAccountId", fromField = "finAccountId")}))
        }
    )
    public interface FinAccountTabBar {}

    @Menu(
        name = "FinAccountSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "FinAccountTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "EditFinAccount"
    )
    public interface FinAccountSideBar {}

    @Menu(
        name = "FinAccountSubTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "createNew", title = "${uiLabelMap.CommonNew} ${uiLabelMap.AccountingFinAccount}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditFinAccount")),
            @MenuItem(name = "advancedFinAccountSearch", title = "${uiLabelMap.CommonAdvancedSearch}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"displayAdvancedSearch", "not-equals", "true"})}), link = @MenuLink(target = "FindFinAccount", parameters = {@MenuParameter(paramName = "displayAdvancedSearch", value = "true")})),
            @MenuItem(name = "quickFinAccountSearch", title = "${uiLabelMap.AccountingQuickSearch}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"displayAdvancedSearch", "equals", "true"})}), link = @MenuLink(target = "FindFinAccount"))
        }
    )
    public interface FinAccountSubTabBar {}

    @Menu(
        name = "FinAccountMainTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "finAccountMain", title = "${uiLabelMap.CommonMain}", link = @MenuLink(target = "FinAccountMain")),
            @MenuItem(name = "FindFinAccount", title = "${uiLabelMap.PageTitleFindFinAccount}", link = @MenuLink(target = "FindFinAccount"))
        }
    )
    public interface FinAccountMainTabBar {}

    @Menu(
        name = "FinAccountMainSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "FinAccountSideBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface FinAccountMainSideBar {}

    @Menu(
        name = "FinAccountReconciliationsTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "Find", title = "${uiLabelMap.AccountingFindFinAccountReconciliations}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"finAccountId"}), @Condition(type = NotEmpty.class, params = {"glReconciliationId"}), @Condition(type = Compare.class, params = {"activeSubMenuItem2", "not-equals", "Find"})}), link = @MenuLink(target = "FindFinAccountReconciliations", parameters = {@MenuParameter(paramName = "finAccountId", fromField = "finAccountId")})),
            @MenuItem(name = "NewFinAccountReconciliations", title = "${uiLabelMap.CommonNew}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifNotEmpty = {"glReconciliationId"}, ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "activeSubMenuItem2", operator = "equals", value = "Find")})}, conditions = {@Condition(type = Compare.class, params = {"finAccount.statusId", "not-equals", "FNACT_MANFROZEN"}), @Condition(type = Compare.class, params = {"finAccount.statusId", "not-equals", "FNACT_CANCELLED"})}), link = @MenuLink(target = "EditFinAccountReconciliations", parameters = {@MenuParameter(paramName = "finAccountId", fromField = "finAccountId")}))
        }
    )
    public interface FinAccountReconciliationsTabBar {}

    @Menu(
        name = "FixedAssetsSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "FixedAssetTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "ListFixedAssets"
    )
    public interface FixedAssetsSideBar {}

    @Menu(
        name = "FixedAssetTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "EditFixedAsset",
        items = {
            @MenuItem(name = "EditFixedAsset", title = "${uiLabelMap.AccountingFixedAsset}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"fixedAssetId"})}), link = @MenuLink(target = "EditFixedAsset", parameters = {@MenuParameter(paramName = "fixedAssetId", fromField = "fixedAssetId")})),
            @MenuItem(name = "FixedAssetChildren", title = "${uiLabelMap.CommonEntityChildren}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"fixedAssetId"})}), link = @MenuLink(target = "FixedAssetChildren", parameters = {@MenuParameter(paramName = "fixedAssetId", fromField = "fixedAssetId"), @MenuParameter(paramName = "trail", fromField = "fixedAssetId")})),
            @MenuItem(name = "ListFixedAssetProducts", title = "${uiLabelMap.AccountingFixedAssetProducts}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"fixedAssetId"})}), link = @MenuLink(target = "ListFixedAssetProducts", parameters = {@MenuParameter(paramName = "fixedAssetId", fromField = "fixedAssetId")})),
            @MenuItem(name = "ListFixedAssetCalendar", title = "${uiLabelMap.AccountingFixedAssetCalendar}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"fixedAssetId"})}), link = @MenuLink(target = "calendar", parameters = {@MenuParameter(paramName = "fixedAssetId", fromField = "fixedAssetId")})),
            @MenuItem(name = "EditFixedAssetStdCosts", title = "${uiLabelMap.AccountingFixedAssetStdCosts}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"fixedAssetId"})}), link = @MenuLink(target = "EditFixedAssetStdCosts", parameters = {@MenuParameter(paramName = "fixedAssetId", fromField = "fixedAssetId")})),
            @MenuItem(name = "EditFixedAssetIdents", title = "${uiLabelMap.AccountingFixedAssetIdents}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"fixedAssetId"})}), link = @MenuLink(target = "EditFixedAssetIdents", parameters = {@MenuParameter(paramName = "fixedAssetId", fromField = "fixedAssetId")})),
            @MenuItem(name = "EditFixedAssetRegistrations", title = "${uiLabelMap.AccountingFixedAssetRegistrations}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"fixedAssetId"})}), link = @MenuLink(target = "EditFixedAssetRegistrations", parameters = {@MenuParameter(paramName = "fixedAssetId", fromField = "fixedAssetId")})),
            @MenuItem(name = "ListFixedAssetMaints", title = "${uiLabelMap.AccountingFixedAssetMaints}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"fixedAssetId"})}), link = @MenuLink(target = "ListFixedAssetMaints", parameters = {@MenuParameter(paramName = "fixedAssetId", fromField = "fixedAssetId")})),
            @MenuItem(name = "FixedAssetDepreciation", title = "${uiLabelMap.AccountingDepreciation}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"fixedAssetId"})}), link = @MenuLink(target = "showFixedAssetDepreciation", parameters = {@MenuParameter(paramName = "fixedAssetId", fromField = "fixedAssetId")})),
            @MenuItem(name = "FixedAssetGeoLocation", title = "${uiLabelMap.CommonGeoLocation}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"fixedAssetId"})}), link = @MenuLink(target = "FixedAssetGeoLocation", parameters = {@MenuParameter(paramName = "fixedAssetId", fromField = "fixedAssetId")}))
        }
    )
    public interface FixedAssetTabBar {}

    @Menu(
        name = "InvoicesSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "Invoices", title = "${uiLabelMap.AccountingInvoices}", link = @MenuLink(target = "findInvoices")),
            @MenuItem(name = "commissionRun", title = "${uiLabelMap.AccountingCommissionRun}", link = @MenuLink(target = "CommissionRun")),
            @MenuItem(name = "commissionReport", title = "${uiLabelMap.AccountingCommissionReport}", link = @MenuLink(target = "FindCommissions")),
            @MenuItem(name = "massDownloadInvoices", title = "${uiLabelMap.AccountingDownloadInvoices}", link = @MenuLink(target = "downloadInvoices"))
        }
    )
    public interface InvoicesSideBar {}

    @Menu(
        name = "InvoiceSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "invoiceOverview", title = "${uiLabelMap.AccountingInvoiceOverview}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"invoice.invoiceId"})}), link = @MenuLink(target = "invoiceOverview", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId")})),
            @MenuItem(name = "editInvoice", title = "${uiLabelMap.AccountingInvoiceHeader}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_IN_PROCESS"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_RECEIVED")})}, conditions = {@Condition(type = NotEmpty.class, params = {"invoice.invoiceId"})}), link = @MenuLink(target = "editInvoice", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId")})),
            @MenuItem(name = "listInvoiceItems", title = "${uiLabelMap.AccountingInvoiceItems}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_IN_PROCESS"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_RECEIVED")})}, conditions = {@Condition(type = NotEmpty.class, params = {"invoice.invoiceId"})}), link = @MenuLink(target = "listInvoiceItems", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId")})),
            @MenuItem(name = "EditInvoiceTimeEntries", title = "${uiLabelMap.AccountingInvoiceTimeEntries}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_IN_PROCESS"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_RECEIVED")})}, conditions = {@Condition(type = NotEmpty.class, params = {"invoice.invoiceId"})}), link = @MenuLink(target = "editInvoiceTimeEntries", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId")})),
            @MenuItem(name = "invoiceTerms", title = "${uiLabelMap.PartyTerms}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_IN_PROCESS"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_RECEIVED")})}, conditions = {@Condition(type = NotEmpty.class, params = {"invoice.invoiceId"})}), link = @MenuLink(target = "invoiceTerms", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId")})),
            @MenuItem(name = "editInvoiceApplications", title = "${uiLabelMap.AccountingApplications}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_IN_PROCESS"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_SENT"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_RECEIVED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_APPROVED")})}, conditions = {@Condition(type = NotEmpty.class, params = {"invoice.invoiceId"})}), link = @MenuLink(target = "editInvoiceApplications", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId")}))
        }
    )
    public interface InvoiceSideBar {}

    @Menu(
        name = "InvoiceSubTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "createNew", title = "${uiLabelMap.AccountingCreateNewInvoice}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "newInvoice")),
            @MenuItem(name = "copyInvoice", title = "${uiLabelMap.CommonCopy}", widgetStyle = "+${styles.action_run_sys} ${styles.action_copy}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"invoice.invoiceId"})}), link = @MenuLink(target = "copyInvoice", parameters = {@MenuParameter(paramName = "invoiceIdToCopyFrom", fromField = "invoiceId")})),
            @MenuItem(name = "statusToApproved", title = "${uiLabelMap.AccountingInvoiceStatusToApproved}", widgetStyle = "+${styles.action_run_sys} ${styles.action_updatestatus}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_IN_PROCESS"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_SENT"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_RECEIVED")})}, conditions = {@Condition(type = NotEmpty.class, params = {"invoice.invoiceId"})}), link = @MenuLink(target = "setInvoiceStatus", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId"), @MenuParameter(paramName = "statusId", value = "INVOICE_APPROVED")})),
            @MenuItem(name = "statusToReceived", title = "${uiLabelMap.AccountingInvoiceStatusToReceived}", widgetStyle = "+${styles.action_run_sys} ${styles.action_updatestatus}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.invoiceTypeId", operator = "equals", value = "PURCHASE_INVOICE"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.invoiceTypeId", operator = "equals", value = "CUST_RTN_INVOICE")})}, conditions = {@Condition(type = NotEmpty.class, params = {"invoice.invoiceId"}), @Condition(type = Compare.class, params = {"invoice.statusId", "equals", "INVOICE_IN_PROCESS"})}), link = @MenuLink(target = "setInvoiceStatus", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId"), @MenuParameter(paramName = "statusId", value = "INVOICE_RECEIVED")})),
            @MenuItem(name = "statusToSent", title = "${uiLabelMap.AccountingInvoiceStatusToSent}", widgetStyle = "+${styles.action_run_sys} ${styles.action_updatestatus}", condition = @MenuItemCondition(conditions = {@Condition(type = Or.class, tree = {@ConditionNode(type = And.class), @ConditionNode(parent = 0, not = true, type = Empty.class, params = {"invoice.invoiceId"}), @ConditionNode(parent = 0, type = Compare.class, params = {"invoice.statusId", "equals", "INVOICE_IN_PROCESS"}), @ConditionNode(parent = 0, type = Compare.class, params = {"invoice.invoiceTypeId", "equals", "SALES_INVOICE"}), @ConditionNode(type = And.class), @ConditionNode(parent = 4, not = true, type = Empty.class, params = {"invoice.invoiceId"}), @ConditionNode(parent = 4, type = Compare.class, params = {"invoice.statusId", "equals", "INVOICE_APPROVED"}), @ConditionNode(parent = 4, type = Compare.class, params = {"invoice.invoiceTypeId", "equals", "SALES_INVOICE"})})}), link = @MenuLink(target = "setInvoiceStatus", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId"), @MenuParameter(paramName = "statusId", value = "INVOICE_SENT")})),
            @MenuItem(name = "statusToReady", title = "${uiLabelMap.AccountingInvoiceStatusToReady}", widgetStyle = "+${styles.action_run_sys} ${styles.action_updatestatus}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_IN_PROCESS"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_SENT"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_RECEIVED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_APPROVED")})}, conditions = {@Condition(type = NotEmpty.class, params = {"invoice.invoiceId"})}), link = @MenuLink(target = "setInvoiceStatus", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId"), @MenuParameter(paramName = "statusId", value = "INVOICE_READY")})),
            @MenuItem(name = "statusToPaid", title = "${uiLabelMap.AccountingInvoiceStatusToPaid}", widgetStyle = "+${styles.action_run_sys} ${styles.action_complete}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_READY")})}, conditions = {@Condition(type = NotEmpty.class, params = {"invoice.invoiceId"})}), link = @MenuLink(target = "setInvoiceStatus", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId"), @MenuParameter(paramName = "statusId", value = "INVOICE_PAID")})),
            @MenuItem(name = "statusToWriteoff", title = "${uiLabelMap.AccountingInvoiceStatusToWriteoff}", widgetStyle = "+${styles.action_run_sys} ${styles.action_terminate}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_READY")})}, conditions = {@Condition(type = NotEmpty.class, params = {"invoice.invoiceId"})}), link = @MenuLink(target = "setInvoiceStatus", requestConfirmation = true, confirmationMessage = "You want to writeoff this invoice number ${invoice.invoiceId}?", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId"), @MenuParameter(paramName = "statusId", value = "INVOICE_WRITEOFF")})),
            @MenuItem(name = "statusToInProcess", title = "${uiLabelMap.AccountingInvoiceStatusToInProcess}", widgetStyle = "+${styles.action_run_sys} ${styles.action_updatestatus}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_SENT"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_RECEIVED")})}, conditions = {@Condition(type = NotEmpty.class, params = {"invoice.invoiceId"})}), link = @MenuLink(target = "setInvoiceStatus", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId"), @MenuParameter(paramName = "statusId", value = "INVOICE_IN_PROCESS")})),
            @MenuItem(name = "statusToCancelled", title = "${uiLabelMap.AccountingInvoiceStatusToCancelled}", widgetStyle = "+${styles.action_run_sys} ${styles.action_terminate}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_IN_PROCESS"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_SENT"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_RECEIVED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "invoice.statusId", operator = "equals", value = "INVOICE_READY")})}, conditions = {@Condition(type = NotEmpty.class, params = {"invoice.invoiceId"})}), link = @MenuLink(target = "setInvoiceStatus", requestConfirmation = true, confirmationMessage = "${uiLabelMap.AccountingConfirmationCancelOrder}", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId"), @MenuParameter(paramName = "statusId", value = "INVOICE_CANCELLED")})),
            @MenuItem(name = "addtax", title = "${uiLabelMap.AccountingAddTax}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"invoice.invoiceId"}), @Condition(type = Compare.class, params = {"invoice.statusId", "equals", "INVOICE_IN_PROCESS"})}), link = @MenuLink(target = "addtax", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId")})),
            @MenuItem(name = "pdf", title = "${uiLabelMap.CommonPdf}", widgetStyle = "+${styles.action_run_sys} ${styles.action_export}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"invoice.invoiceId"})}), link = @MenuLink(target = "invoice.pdf", targetWindow = "_BLANK", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId")})),
            @MenuItem(name = "pdfDfltCur", title = "${uiLabelMap.AccountingInvoicePDFDefaultCur} (${defaultOrganizationPartyCurrencyUomId})", widgetStyle = "+${styles.action_run_sys} ${styles.action_export}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"invoice.invoiceId"}), @Condition(type = CompareField.class, params = {"invoice.currencyUomId", "not-equals", "defaultOrganizationPartyCurrencyUomId"})}), link = @MenuLink(target = "invoice.pdf", targetWindow = "_BLANK", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId"), @MenuParameter(paramName = "currency", fromField = "defaultOrganizationPartyCurrencyUomId")})),
            @MenuItem(name = "sendPerEmail", title = "${uiLabelMap.CommonPdf}: ${uiLabelMap.CommonSendPerEmail}", widgetStyle = "+${styles.action_run_sys} ${styles.action_send}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"invoice.invoiceId"})}), link = @MenuLink(target = "sendPerEmail", targetWindow = "_blank", parameters = {@MenuParameter(paramName = "invoiceId", fromField = "invoice.invoiceId")}))
        }
    )
    public interface InvoiceSubTabBar {}

    @Menu(
        name = "ImportExportSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "exportTransactions", title = "${uiLabelMap.AccountingAcctgTrans}", sortMode = "off", link = @MenuLink(target = "ExportTransactions")),
            @MenuItem(name = "importInvoice", title = "${uiLabelMap.AccountingInvoice}", sortMode = "off", link = @MenuLink(target = "ImportExportInvoice"))
        }
    )
    public interface ImportExportSideBar {}

    @Menu(
        name = "TransactionsSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        actions = @MenuActions(set = {@SetAction(field = "organizationPartyId", fromField = "parameters.organizationPartyId", defaultValue = "${defaultOrganizationPartyId}")})
    )
    public interface TransactionsSideBar {}

    @Menu(
        name = "TransactionReportsSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "TransactionTotals",
        alwaysExpandSelectedOrAncestor = "true",
        actions = @MenuActions(set = {@SetAction(field = "organizationPartyId", fromField = "parameters.organizationPartyId", defaultValue = "${defaultOrganizationPartyId}")}),
        items = {
            @MenuItem(name = "SalesInvoiceByProductCategorySummary", title = "${uiLabelMap.PageTitleSalesInvoiceByProductCategorySummary}", link = @MenuLink(target = "SalesInvoiceByProductCategorySummary", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "TrialBalance", title = "${uiLabelMap.AccountingTrialBalance}", link = @MenuLink(target = "TrialBalance", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "IncomeStatement", title = "${uiLabelMap.AccountingIncomeStatement}", link = @MenuLink(target = "IncomeStatement", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "ComparativeIncomeStatement", title = "${uiLabelMap.AccountingComparativeIncomeStatement}", link = @MenuLink(target = "ComparativeIncomeStatement", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "BalanceSheet", title = "${uiLabelMap.AccountingBalanceSheet}", link = @MenuLink(target = "BalanceSheet", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "ComparativeBalanceSheet", title = "${uiLabelMap.AccountingComparativeBalanceSheet}", link = @MenuLink(target = "ComparativeBalanceSheet", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "TransactionTotals", title = "${uiLabelMap.AccountingTransactionTotals}", link = @MenuLink(target = "TransactionTotals", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "InventoryValuation", title = "${uiLabelMap.AccountingInventoryValuation}", link = @MenuLink(target = "InventoryValuation", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "CashFlowStatement", title = "${uiLabelMap.AccountingCashFlowStatement}", link = @MenuLink(target = "CashFlowStatement", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "ComparativeCashFlowStatement", title = "${uiLabelMap.AccountingComparativeCashFlowStatement}", link = @MenuLink(target = "ComparativeCashFlowStatement", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")}))
        }
    )
    public interface TransactionReportsSideBar {}

    @Menu(
        name = "OrganizationSubTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "addCompany", title = "${uiLabelMap.AccountingNewCompany}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = Compare.class, params = {"hasPrefPermission", "equals", "true", "Boolean"})}), link = @MenuLink(target = "AddCompany"))
        }
    )
    public interface OrganizationSubTabBar {}

    @Menu(
        name = "OrganizationTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "PartyAccountsSummary",
        items = {
            @MenuItem(name = "PartyAccountsSummary", title = "${uiLabelMap.AcctgPartyGlJournalSummary}", sortMode = "off", link = @MenuLink(target = "PartyAccountsSummary", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "AccountReconciliation", title = "${uiLabelMap.AccountingAcctTransEntryRecon}", link = @MenuLink(target = "findGlAccountReconciliation", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "AccountReconciliations", title = "${uiLabelMap.AccountingAcctGlRecon}", link = @MenuLink(target = "findGlAccountReconciliations", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "FindAcctgTrans", title = "${uiLabelMap.AccountingAcctgTrans}", link = @MenuLink(target = "FindAcctgTrans", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "FindAcctgTransEntries", title = "${uiLabelMap.AccountingAcctgTransEntries}", link = @MenuLink(target = "FindAcctgTransEntries", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "OrganizationAccountingReports", title = "${uiLabelMap.AccountingTrialBalance}", link = @MenuLink(target = "TrialBalance", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "ChecksTabButton", title = "${uiLabelMap.AccountingChecks}", link = @MenuLink(target = "listChecksToPrint", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")}))
        }
    )
    public interface OrganizationTabBar {}

    @Menu(
        name = "PartyAccountingChecksTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "GlAccountSalInvoice",
        selectedMenuItemContextFieldName = "activeSubMenuItem2",
        items = {
            @MenuItem(name = "PrintChecksTabButton", title = "${uiLabelMap.AccountingPrintChecks}", link = @MenuLink(target = "listChecksToPrint", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "SendChecksTabButton", title = "${uiLabelMap.AccountingSendChecks}", link = @MenuLink(target = "listChecksToSend", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")}))
        }
    )
    public interface PartyAccountingChecksTabBar {}

    @Menu(
        name = "PaymentTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "findPayments", title = "${uiLabelMap.CommonFind} ${uiLabelMap.AccountingInvoicePayments}", sortMode = "off", link = @MenuLink(target = "findPayments")),
            @MenuItem(name = "paymentOverview", title = "${uiLabelMap.AccountingPayment} ${uiLabelMap.AccountingPaymentTabOverview}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"payment.paymentId"})}), link = @MenuLink(target = "paymentOverview", parameters = {@MenuParameter(paramName = "paymentId", fromField = "payment.paymentId")})),
            @MenuItem(name = "editPayment", title = "${uiLabelMap.AccountingPayment}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"payment.paymentId"})}), link = @MenuLink(target = "editPayment", parameters = {@MenuParameter(paramName = "paymentId", fromField = "payment.paymentId")})),
            @MenuItem(name = "editPaymentApplications", title = "${uiLabelMap.AccountingPayment} ${uiLabelMap.AccountingPaymentTabApplications}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "payment.statusId", operator = "equals", value = "PMNT_NOT_PAID"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "payment.statusId", operator = "equals", value = "PMNT_RECEIVED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "payment.statusId", operator = "equals", value = "PMNT_SENT")})}, conditions = {@Condition(type = NotEmpty.class, params = {"payment.paymentId"})}), link = @MenuLink(target = "editPaymentApplications", parameters = {@MenuParameter(paramName = "paymentId", fromField = "payment.paymentId")})),
            @MenuItem(name = "editPaymentApplications", title = "${uiLabelMap.AccountingPayment} ${uiLabelMap.AccountingPaymentTabApplications}"),
            @MenuItem(name = "authorizeTransaction", title = "${uiLabelMap.AccountingAuthorize}", link = @MenuLink(target = "AuthorizeTransaction")),
            @MenuItem(name = "captureTransaction", title = "${uiLabelMap.AccountingCapture}", link = @MenuLink(target = "CaptureTransaction")),
            @MenuItem(name = "gatewayResponses", title = "${uiLabelMap.AccountingGatewayResponses}", link = @MenuLink(target = "FindGatewayResponses")),
            @MenuItem(name = "manualTransaction", title = "${uiLabelMap.AccountingManualTransaction}", link = @MenuLink(target = "ManualTransaction"))
        }
    )
    public interface PaymentTabBar {}

    @Menu(
        name = "PaymentSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "PaymentTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface PaymentSideBar {}

    @Menu(
        name = "PaymentSubTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        actions = @MenuActions(set = {@SetAction(field = "isDisbursement", value = "${groovy:if(context.payment != null) return org.ofbiz.accounting.util.UtilAccounting.isDisbursement(context.payment)}")}),
        items = {
            @MenuItem(name = "createNew", title = "${uiLabelMap.CommonNew} ${uiLabelMap.CommonPayment}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"payment.paymentId"})}), link = @MenuLink(target = "newPayment")),
            @MenuItem(name = "statusToSend", title = "${uiLabelMap.AccountingPaymentTabStatusToSent}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"payment.paymentId"}), @Condition(type = Compare.class, params = {"isDisbursement", "equals", "true"}), @Condition(type = Compare.class, params = {"payment.statusId", "equals", "PMNT_NOT_PAID"})}), link = @MenuLink(target = "setPaymentStatus", parameters = {@MenuParameter(paramName = "paymentId", fromField = "payment.paymentId"), @MenuParameter(paramName = "statusId", value = "PMNT_SENT")})),
            @MenuItem(name = "statusToReceived", title = "${uiLabelMap.AccountingPaymentTabStatusToReceived}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"payment.paymentId"}), @Condition(type = Compare.class, params = {"isDisbursement", "equals", "false"}), @Condition(type = Compare.class, params = {"payment.statusId", "equals", "PMNT_NOT_PAID"})}), link = @MenuLink(target = "setPaymentStatus", parameters = {@MenuParameter(paramName = "paymentId", fromField = "payment.paymentId"), @MenuParameter(paramName = "statusId", value = "PMNT_RECEIVED")})),
            @MenuItem(name = "statusToCancelled", title = "${uiLabelMap.AccountingPaymentTabStatusToCancelled}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"payment.paymentId"}), @Condition(type = Compare.class, params = {"payment.statusId", "equals", "PMNT_NOT_PAID"})}), link = @MenuLink(target = "setPaymentStatus", requestConfirmation = true, confirmationMessage = "You want to cancel this payment number ${payment.paymentId}?", parameters = {@MenuParameter(paramName = "paymentId", fromField = "payment.paymentId"), @MenuParameter(paramName = "statusId", value = "PMNT_CANCELLED")})),
            @MenuItem(name = "statusToConfirmed", title = "${uiLabelMap.AccountingPaymentTabStatusToConfirmed}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "payment.statusId", operator = "equals", value = "PMNT_RECEIVED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "payment.statusId", operator = "equals", value = "PMNT_SENT")})}, conditions = {@Condition(type = NotEmpty.class, params = {"payment.paymentId"})}), link = @MenuLink(target = "setPaymentStatus", parameters = {@MenuParameter(paramName = "paymentId", fromField = "payment.paymentId"), @MenuParameter(paramName = "statusId", value = "PMNT_CONFIRMED")})),
            @MenuItem(name = "printAsCheck", title = "${uiLabelMap.AccountingPrintAsCheck}", widgetStyle = "+${styles.action_run_sys} ${styles.action_export}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"payment.paymentId"}), @Condition(type = Compare.class, params = {"payment.statusId", "equals", "PMNT_NOT_PAID"})}), link = @MenuLink(target = "printChecks.pdf", targetWindow = "_BLANK", parameters = {@MenuParameter(paramName = "paymentId", fromField = "payment.paymentId")})),
            @MenuItem(name = "statusToVoidPayment", title = "${uiLabelMap.AccountingPaymentTabStatusToVoid}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"payment.paymentId"}), @Condition(type = Compare.class, params = {"payment.statusId", "not-equals", "PMNT_CONFIRMED"}), @Condition(type = Compare.class, params = {"payment.statusId", "not-equals", "PMNT_VOID"})}), link = @MenuLink(target = "voidPayment", parameters = {@MenuParameter(paramName = "paymentId", fromField = "payment.paymentId")}))
        }
    )
    public interface PaymentSubTabBar {}

    @Menu(
        name = "PaymentsSubTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "newPayment", title = "${uiLabelMap.AccountingNewPayment}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "newPayment")),
            @MenuItem(name = "FindSalesInvoicesByDueDate", title = "${uiLabelMap.AccountingFindSalesInvoicesByDueDate}", widgetStyle = "+${styles.action_nav} ${styles.action_find}", link = @MenuLink(target = "FindSalesInvoicesByDueDate")),
            @MenuItem(name = "FindPurchaseInvoicesByDueDate", title = "${uiLabelMap.AccountingFindPurchaseInvoicesByDueDate}", widgetStyle = "+${styles.action_nav} ${styles.action_find}", link = @MenuLink(target = "FindPurchaseInvoicesByDueDate"))
        }
    )
    public interface PaymentsSubTabBar {}

    @Menu(
        name = "PaymentGroupTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "PaymentGroupOverview", title = "${uiLabelMap.AccountingPayment} ${uiLabelMap.CommonGroup} ${uiLabelMap.AccountingPaymentTabOverview}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"paymentGroup.paymentGroupId"})}), link = @MenuLink(target = "PaymentGroupOverview", parameters = {@MenuParameter(paramName = "paymentGroupId", fromField = "paymentGroup.paymentGroupId")})),
            @MenuItem(name = "EditPaymentGroup", title = "${uiLabelMap.AccountingPayment} ${uiLabelMap.CommonGroup}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"paymentGroup.paymentGroupId"})}), link = @MenuLink(target = "EditPaymentGroup", parameters = {@MenuParameter(paramName = "paymentGroupId", fromField = "paymentGroup.paymentGroupId")})),
            @MenuItem(name = "EditPaymentGroupMember", title = "${uiLabelMap.AccountingPayment} ${uiLabelMap.AccountingGroupMembers}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"paymentGroup.paymentGroupId"})}), link = @MenuLink(target = "EditPaymentGroupMember", parameters = {@MenuParameter(paramName = "paymentGroupId", fromField = "paymentGroup.paymentGroupId")}))
        }
    )
    public interface PaymentGroupTabBar {}

    @Menu(
        name = "PaymentGroupSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "PaymentGroupTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface PaymentGroupSideBar {}

    @Menu(
        name = "PaymentGroupSubTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "createNew", title = "${uiLabelMap.AccountingCreateNewPaymentGroup}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditPaymentGroup")),
            @MenuItem(name = "depositSlip", title = "${uiLabelMap.AccountingDepositSlip}", widgetStyle = "+${styles.action_run_sys} ${styles.action_export}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"display", "equals", "true"}), @Condition(type = Compare.class, params = {"paymentGroup.paymentGroupTypeId", "equals", "BATCH_PAYMENT"}), @Condition(type = NotEmpty.class, params = {"paymentGroupMembers"})}), link = @MenuLink(target = "DepositSlip.pdf", targetWindow = "_BLANK", parameters = {@MenuParameter(paramName = "paymentGroupId", fromField = "paymentGroup.paymentGroupId")})),
            @MenuItem(name = "printCheck", title = "${uiLabelMap.AccountingPrintChecks}", widgetStyle = "+${styles.action_run_sys} ${styles.action_export}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"display", "equals", "true"}), @Condition(type = Compare.class, params = {"paymentGroup.paymentGroupTypeId", "equals", "CHECK_RUN"}), @Condition(type = NotEmpty.class, params = {"paymentGroupMembers"})}), link = @MenuLink(target = "printChecks.pdf", targetWindow = "_BLANK", parameters = {@MenuParameter(paramName = "paymentGroupId", fromField = "paymentGroup.paymentGroupId")})),
            @MenuItem(name = "cancelpaymentGroup", title = "${uiLabelMap.AccountingCancelBatchPayments}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"paymentGroupMembers"}), @Condition(type = NotEmpty.class, params = {"paymentGroup"}), @Condition(type = Empty.class, params = {"glReconciliationId"}), @Condition(type = Compare.class, params = {"paymentGroup.paymentGroupTypeId", "equals", "BATCH_PAYMENT"})}), link = @MenuLink(target = "cancelPaymentGroup", parameters = {@MenuParameter(paramName = "paymentGroupId", fromField = "paymentGroup.paymentGroupId")})),
            @MenuItem(name = "cancelCheckRunPayments", title = "${uiLabelMap.AccountingCancelCheckRun}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"paymentGroupMembers"}), @Condition(type = NotEmpty.class, params = {"paymentGroup"}), @Condition(type = Compare.class, params = {"paymentGroup.paymentGroupTypeId", "equals", "CHECK_RUN"})}), link = @MenuLink(target = "cancelCheckRunPayments", parameters = {@MenuParameter(paramName = "paymentGroupId", fromField = "paymentGroup.paymentGroupId")}))
        }
    )
    public interface PaymentGroupSubTabBar {}

    @Menu(
        name = "SettingsSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "settings",
        alwaysExpandSelectedOrAncestor = "true",
        actions = @MenuActions(set = {@SetAction(field = "organizationPartyId", fromField = "parameters.organizationPartyId", defaultValue = "${defaultOrganizationPartyId}")}),
        items = {
            @MenuItem(name = "settings", title = "${uiLabelMap.AccountingCompanies}", sortMode = "off", link = @MenuLink(target = "settings")),
            @MenuItem(name = "InvoiceItemTypes", title = "${uiLabelMap.AccountingInvoiceItemType}", link = @MenuLink(target = "editInvoiceItemType")),
            @MenuItem(name = "ViewRateAmounts", title = "${uiLabelMap.AccountingRates}", link = @MenuLink(target = "viewRateAmounts")),
            @MenuItem(name = "PaymentMethodTypes", title = "${uiLabelMap.CommonPaymentMethodType}", link = @MenuLink(target = "editPaymentMethodType")),
            @MenuItem(name = "ViewFXConversions", title = "${uiLabelMap.AccountingFX}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = ServicePermission.class, params = {"acctgFxPermissionCheck", "UPDATE"})}), link = @MenuLink(target = "viewFXConversions", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "PaymentGatewayConfig", title = "${uiLabelMap.AccountingPaymentGatewayConfig}", condition = @MenuItemCondition(mode = "omit", or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifHasPermission = {@com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "PAYPROC", action = "_ADMIN"), @com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "ACCOUNTING", action = "_ADMIN")})}), link = @MenuLink(target = "FindPaymentGatewayConfig"), subMenus = {@SubMenu(name = "PaymentGatewayConfig", include = "component://accounting/widget/AccountingMenus.xml#PaymentGatewayConfigSideBar")}),
            @MenuItem(name = "Journals", title = "${uiLabelMap.AccountingGlJournals}", link = @MenuLink(target = "journals", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "GlAccountAssignment", title = "${uiLabelMap.AccountingGlAccountDefault}", link = @MenuLink(target = "GlAccountAssignment", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")})),
            @MenuItem(name = "FindGlAccountCategory", title = "${uiLabelMap.FormFieldTitle_glAccountCategory}", link = @MenuLink(target = "FindGlAccountCategory")),
            @MenuItem(name = "TaxAuthorities", title = "${uiLabelMap.AccountingTaxAuthorities}", link = @MenuLink(target = "FindTaxAuthority"), subMenus = {@SubMenu(name = "TaxAuthority", include = "component://accounting/widget/AccountingMenus.xml#TaxAuthoritySideBar")}),
            @MenuItem(name = "findVendors", title = "${uiLabelMap.AccountingVendors}", link = @MenuLink(target = "findVendors")),
            @MenuItem(name = "TimePeriods", title = "${uiLabelMap.AccountingTimePeriods}", link = @MenuLink(target = "TimePeriods", parameters = {@MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")}))
        }
    )
    public interface SettingsSideBar {}

    @Menu(
        name = "PaymentGatewayConfigSideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "PaymentGatewayConfig",
        alwaysExpandSelectedOrAncestor = "true",
        items = {
            @MenuItem(name = "paymentGatewayConfigTab", title = "${uiLabelMap.AccountingPaymentGatewayConfig}", link = @MenuLink(target = "FindPaymentGatewayConfig")),
            @MenuItem(name = "paymentGatewayConfigTypesTab", title = "${uiLabelMap.AccountingPaymentGatewayConfigTypes}", link = @MenuLink(target = "FindPaymentGatewayConfigTypes"))
        }
    )
    public interface PaymentGatewayConfigSideBar {}

    @Menu(
        name = "TaxAuthorityTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditTaxAuthority", title = "${uiLabelMap.AccountingTaxAuthority}", link = @MenuLink(target = "EditTaxAuthority", parameters = {@MenuParameter(paramName = "taxAuthPartyId", fromField = "taxAuthPartyId"), @MenuParameter(paramName = "taxAuthGeoId", fromField = "taxAuthGeoId")})),
            @MenuItem(name = "EditTaxAuthorityAssocs", title = "${uiLabelMap.CommonAssocs}", link = @MenuLink(target = "EditTaxAuthorityAssocs", parameters = {@MenuParameter(paramName = "taxAuthPartyId", fromField = "taxAuthPartyId"), @MenuParameter(paramName = "taxAuthGeoId", fromField = "taxAuthGeoId")})),
            @MenuItem(name = "EditTaxAuthorityGlAccounts", title = "${uiLabelMap.AccountingGlAccs}", link = @MenuLink(target = "EditTaxAuthorityGlAccounts", parameters = {@MenuParameter(paramName = "taxAuthPartyId", fromField = "taxAuthPartyId"), @MenuParameter(paramName = "taxAuthGeoId", fromField = "taxAuthGeoId")})),
            @MenuItem(name = "EditTaxAuthorityRateProducts", title = "${uiLabelMap.AccountingProductRates}", link = @MenuLink(target = "EditTaxAuthorityRateProducts", parameters = {@MenuParameter(paramName = "taxAuthPartyId", fromField = "taxAuthPartyId"), @MenuParameter(paramName = "taxAuthGeoId", fromField = "taxAuthGeoId")}))
        }
    )
    public interface TaxAuthorityTabBar {}

    @Menu(
        name = "TaxAuthoritySideBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "TaxAuthorityTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        alwaysExpandSelectedOrAncestor = "true"
    )
    public interface TaxAuthoritySideBar {}

    @Menu(
        name = "NewTaxAuthoritySubTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditTaxAuthority", title = "${uiLabelMap.AccountingNewTaxAuthority}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditTaxAuthority"))
        }
    )
    public interface NewTaxAuthoritySubTabBar {}

    @Menu(
        name = "EditTaxAuthoritySubTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditTaxAuthority", title = "${uiLabelMap.AccountingNewTaxAuthority}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"taxAuthority"})}), link = @MenuLink(target = "EditTaxAuthority"))
        }
    )
    public interface EditTaxAuthoritySubTabBar {}

    @Menu(
        name = "ListTaxAuthorityPartiesSubTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditTaxAuthorityPartyInfo", title = "${uiLabelMap.AccountingNewTaxAuthorityPartyInfo}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditTaxAuthorityPartyInfo", parameters = {@MenuParameter(paramName = "taxAuthPartyId", fromField = "taxAuthPartyId"), @MenuParameter(paramName = "taxAuthGeoId", fromField = "taxAuthGeoId")}))
        }
    )
    public interface ListTaxAuthorityPartiesSubTabBar {}

    @Menu(
        name = "EditTaxAuthorityPartyInfoSubTabBar",
        location = "component://accounting/widget/AccountingMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditTaxAuthority", title = "${uiLabelMap.CommonBack}", widgetStyle = "+${styles.action_nav} ${styles.action_cancel}", link = @MenuLink(target = "EditTaxAuthority", text = "${uiLabelMap.CommonBack}", style = "${styles.link_nav} ", parameters = {@MenuParameter(paramName = "taxAuthPartyId", fromField = "taxAuthPartyId"), @MenuParameter(paramName = "taxAuthGeoId", fromField = "taxAuthGeoId")})),
            @MenuItem(name = "EditTaxAuthorityPartyInfo", title = "${uiLabelMap.AccountingNewTaxAuthorityPartyInfo}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"partyTaxAuthInfo"})}), link = @MenuLink(target = "EditTaxAuthorityPartyInfo", parameters = {@MenuParameter(paramName = "taxAuthPartyId", fromField = "taxAuthPartyId"), @MenuParameter(paramName = "taxAuthGeoId", fromField = "taxAuthGeoId")}))
        }
    )
    public interface EditTaxAuthorityPartyInfoSubTabBar {}

}
