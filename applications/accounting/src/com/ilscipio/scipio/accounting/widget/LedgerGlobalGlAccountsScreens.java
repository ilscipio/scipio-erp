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
public class LedgerGlobalGlAccountsScreens {

    @Screen(name = "AssignGlAccount", location = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AcctgAssignGlAccount")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "AssignGlAccount")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "AcctgAssignGlAccount")
    @DecoratorScreen(
        name = "CommonGLDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "GlSettingTabBar", location = "component://accounting/widget/AccountingMenus.xml"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "AssignGlAccount", location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml"
                )})})
        }
    )
    public interface AssignGlAccount {}

    @Screen(name = "GlAccountNavigate", location = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AcctgNavigateAccts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountNavigate")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "AcctgNavigateAccts")
    @Action(type = ActionType.SET, field = "glAccountId", fromField = "requestParameters.glAccountId")
    @Action(type = ActionType.SET, field = "trail", fromField = "requestParameters.trail")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/script/com/ilscipio/scipio/accounting/ledger/tree/EditGLAccountTreeCore.groovy")
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[+0]", value = "/base-theme/bower_components/jquery.cookie/jquery.cookie.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_STYLESHEET[+0]", value = "/base-theme/bower_components/jstree/dist/themes/default/style.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[+0]", value = "/base-theme/bower_components/jstree/dist/jstree.min.js", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/generated/GlAccountNavigate_script1.groovy")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/accounting/control/ScpEgltCommon.js?t=${ScpEgltCommon}", global = true)
    @DecoratorScreen(
        name = "CommonGLDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/ledger/tree/EditGLAccountTree.ftl"
            )})
        }
    )
    public interface GlAccountNavigate {}

    @Screen(name = "ListGlAccountEntries", location = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewGlAccountEntries")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListGlAccountOrganization")
    @Action(type = ActionType.SET, field = "glAccountId", fromField = "requestParameters.glAccountId")
    @Action(type = ActionType.ENTITY_AND, entityName = "AcctgTransEntry", list = "entries", fieldMaps = {@FieldMap(fieldName = "glAccountId")})
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(widgets = {
                    @Widget(type = WidgetType.INCLUDE_TREE, name = "ListGlAccountTree", location = "component://accounting/widget/ledger/AccountingTrees.xml"
                )}),
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListAcctgTransEntries", location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml"
                )})})
        }
    )
    public interface ListGlAccountEntries {}

    @Screen(name = "ListAcctgTransEntries", location = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewAccountingTransaction")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "chartofaccounts")
    @Action(type = ActionType.SET, field = "acctgTransId", fromField = "requestParameters.acctgTransId")
    @Action(type = ActionType.ENTITY_AND, entityName = "AcctgTransEntry", list = "entries", fieldMaps = {@FieldMap(fieldName = "acctgTransId")})
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "GlAccountTabBar", location = "component://accounting/widget/AccountingMenus.xml"
            )}, screenlets = {
                @Screenlet(widgets = {
                    @Widget(type = WidgetType.INCLUDE_TREE, name = "ListGlAccountTree", location = "component://accounting/widget/ledger/AccountingTrees.xml"
                )}),
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListAcctgTransEntries", location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml"
                )})})
        }
    )
    public interface ListAcctgTransEntries {}

    @Screen(name = "AddGlAccount", location = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml")
    @Action(type = ActionType.SET, field = "parameters.glAccountId", valueType = "Object")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "EditGlobalGlAccount")}))
    public interface AddGlAccount {}

    @Screen(name = "ListGlAccounts", location = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListAccounts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListAccounts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Chartofaccounts")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonGLDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "GlAccountListCombinedTabBar", location = "component://accounting/widget/AccountingMenus.xml"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListGlAccount", location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml"
                )})})
        }
    )
    public interface ListGlAccounts {}

    @Screen(name = "ListGlAccountsReport", location = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListAccounts")
    @Action(type = ActionType.SET, field = "pageLayoutName", value = "simple-landscape")
    @Action(type = ActionType.SET, field = "paginate", value = "false")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListGlAccountPdf", location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml"
            )})
        }
    )
    public interface ListGlAccountsReport {}

    @Screen(name = "GlAccountDetail", location = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml")
    @Action(type = ActionType.ENTITY_ONE, entityName = "GlAccount", valueField = "currentValue", useCache = true, fieldMaps = {@FieldMap(fieldName = "glAccountId")})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.FormFieldTitle_accountName}: ${currentValue.accountName}")}))
    public interface GlAccountDetail {}

    @Screen(name = "EditGlobalGlAccount", location = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "GetCheckGlAccountActions")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: (context.glAccount != null || context.glAccountId) ? 'PageTitleEditGlAccount' : 'PageTitleAddGlAccount'}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "GlAccountNavigate")
    @DecoratorScreen(
        name = "CommonGLDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditGlAccount", location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml"
                )})})
        }
    )
    public interface EditGlobalGlAccount {}

    @Screen(name = "GetGlAccountActions", location = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml")
    @IfAction(order = 0, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Compare.class, params = {"GetGlAccountActions_run", "equals", "true", "Boolean"})}), then = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/generated/GetGlAccountActions_script1.groovy"), @Action(type = ActionType.ENTITY_ONE, entityName = "GlAccount", valueField = "glAccount")}))
    public interface GetGlAccountActions {}

    @Screen(name = "GetCheckGlAccountActions", location = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml")
    @Action(order = 0, type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "GetGlAccountActions")
    @IfAction(order = 1, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"glAccount"}), @Condition(type = NotEmpty.class, params = {"glAccountId"})}), then = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/generated/GetCheckGlAccountActions_script1.groovy")}))
    public interface GetCheckGlAccountActions {}

    @Screen(name = "ViewRateAmounts", location = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingRateAmounts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.AccountingRateAmounts}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewRateAmounts")
    @DecoratorScreen(
        name = "CommonGLDecorator",
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

    @Screen(name = "ViewFXConversions", location = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFX")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.AccountingFX}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewFXConversions")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "UomConversionDated", list = "conversions", orderBy = {"uomId", "uomIdTo", "fromDate"})
    @DecoratorScreen(
        name = "CommonGLDecorator",
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
