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
public class SettingsTaxAuthorityScreens {

    @Screen(name = "FindTaxAuthority", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindTaxAuthority")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindTaxAuthority")
    @DecoratorScreen(
        name = "CommonTaxAuthorityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "NewTaxAuthoritySubTabBar", location = "component://accounting/widget/AccountingMenus.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindTaxAuthority", location = "component://accounting/widget/settings/TaxAuthorityForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListTaxAuthorities", location = "component://accounting/widget/settings/TaxAuthorityForms.xml"
                    )}))})})
        }
    )
    public interface FindTaxAuthority {}

    @Screen(name = "EditTaxAuthority", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml")
    @Action(type = ActionType.SET, field = "taxAuthPartyId", fromField = "parameters.taxAuthPartyId")
    @Action(type = ActionType.SET, field = "taxAuthGeoId", fromField = "parameters.taxAuthGeoId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "TaxAuthority", valueField = "taxAuthority")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.taxAuthority ? 'PageTitleEditTaxAuthority' : 'AccountingNewTaxAuthority'}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "${groovy: context.taxAuthority ? 'EditTaxAuthority' : 'NewTaxAuthority'}")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${groovy: context.taxAuthority ? 'PageTitleEditTaxAuthority' : 'AccountingNewTaxAuthority'}")
    @DecoratorScreen(
        name = "CommonTaxAuthorityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "EditTaxAuthoritySubTabBar", location = "component://accounting/widget/AccountingMenus.xml"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditTaxAuthority", location = "component://accounting/widget/settings/TaxAuthorityForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"taxAuthority"})}), widgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ListTaxAuthorityPartiesWidgets"
                        )}))})
        }
    )
    public interface EditTaxAuthority {}

    @Screen(name = "EditTaxAuthorityCategories", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditTaxAuthorityCategories")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditTaxAuthorityCategories")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditTaxAuthorityCategories")
    @Action(type = ActionType.SET, field = "taxAuthPartyId", fromField = "parameters.taxAuthPartyId")
    @Action(type = ActionType.SET, field = "taxAuthGeoId", fromField = "parameters.taxAuthGeoId")
    @DecoratorScreen(
        name = "CommonTaxAuthorityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "EditTaxAuthorityCategoriesWidgets", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml"
            )})
        }
    )
    public interface EditTaxAuthorityCategories {}

    @Screen(name = "EditTaxAuthorityCategoriesWidgets", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ListTaxAuthorityCategories", location = "component://accounting/widget/settings/TaxAuthorityForms.xml", position = 1)}, screenlets = {@Screenlet(title = "${uiLabelMap.ProductCategories}", name = "TaxAuthorityCategoriesPanel", collapsible = true, includeForms = {@IncludeForm(name = "AddTaxAuthorityCategory", location = "component://accounting/widget/settings/TaxAuthorityForms.xml")}, position = 0)}))
    public interface EditTaxAuthorityCategoriesWidgets {}

    @Screen(name = "EditTaxAuthorityAssocs", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditTaxAuthorityAssocs")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditTaxAuthorityAssocs")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditTaxAuthorityAssocs")
    @Action(type = ActionType.SET, field = "taxAuthPartyId", fromField = "parameters.taxAuthPartyId")
    @Action(type = ActionType.SET, field = "taxAuthGeoId", fromField = "parameters.taxAuthGeoId")
    @DecoratorScreen(
        name = "CommonTaxAuthorityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListTaxAuthorityAssocs", location = "component://accounting/widget/settings/TaxAuthorityForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddTaxAuthorityAssoc}", name = "TaxAuthorityAssocsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddTaxAuthorityAssoc", location = "component://accounting/widget/settings/TaxAuthorityForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditTaxAuthorityAssocs {}

    @Screen(name = "EditTaxAuthorityGlAccounts", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditTaxAuthorityGlAccounts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditTaxAuthorityGlAccounts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditTaxAuthorityGlAccounts")
    @Action(type = ActionType.SET, field = "taxAuthPartyId", fromField = "parameters.taxAuthPartyId")
    @Action(type = ActionType.SET, field = "taxAuthGeoId", fromField = "parameters.taxAuthGeoId")
    @DecoratorScreen(
        name = "CommonTaxAuthorityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListTaxAuthorityGlAccounts", location = "component://accounting/widget/settings/TaxAuthorityForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddTaxAuthorityGlAccount}", name = "TaxAuthorityGlAccountPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddTaxAuthorityGlAccount", location = "component://accounting/widget/settings/TaxAuthorityForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditTaxAuthorityGlAccounts {}

    @Screen(name = "EditTaxAuthorityRateProducts", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditTaxAuthorityRateProducts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditTaxAuthorityRateProducts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditTaxAuthorityRateProducts")
    @Action(type = ActionType.SET, field = "taxAuthPartyId", fromField = "parameters.taxAuthPartyId")
    @Action(type = ActionType.SET, field = "taxAuthGeoId", fromField = "parameters.taxAuthGeoId")
    @DecoratorScreen(
        name = "CommonTaxAuthorityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "EditTaxAuthorityCategoriesWidgets", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "EditTaxAuthorityRateProductWidgets", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml"
            )})
        }
    )
    public interface EditTaxAuthorityRateProducts {}

    @Screen(name = "EditTaxAuthorityRateProductWidgets", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ListTaxAuthorityRateProducts", location = "component://accounting/widget/settings/TaxAuthorityForms.xml", position = 1)}, screenlets = {@Screenlet(title = "${uiLabelMap.AccountingProductRates}", name = "TaxAuthorityCategoryPanel", collapsible = true, includeForms = {@IncludeForm(name = "AddTaxAuthorityRateProduct", location = "component://accounting/widget/settings/TaxAuthorityForms.xml", position = 1)}, labels = {@Label(text = "${uiLabelMap.AccountingTaxAuthorityCategoryAdvice}", position = 0)}, position = 0)}))
    public interface EditTaxAuthorityRateProductWidgets {}

    @Screen(name = "ListTaxAuthorityParties", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListTaxAuthorityParties")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListTaxAuthorityParties")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "Standard costs")
    @Action(type = ActionType.SET, field = "taxAuthPartyId", fromField = "parameters.taxAuthPartyId")
    @Action(type = ActionType.SET, field = "taxAuthGeoId", fromField = "parameters.taxAuthGeoId")
    @DecoratorScreen(
        name = "CommonTaxAuthorityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ListTaxAuthorityPartiesWidgets", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml"
            )})
        }
    )
    public interface ListTaxAuthorityParties {}

    @Screen(name = "ListTaxAuthorityPartiesWidgets", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ListTaxAuthorityParties", location = "component://accounting/widget/settings/TaxAuthorityForms.xml", position = 1)}, screenlets = {@Screenlet(title = "${uiLabelMap.PartyParties}", includeForms = {@IncludeForm(name = "FindTaxAuthorityParties", location = "component://accounting/widget/settings/TaxAuthorityForms.xml", position = 1)}, includeMenus = {@IncludeMenu(name = "ListTaxAuthorityPartiesSubTabBar", location = "component://accounting/widget/AccountingMenus.xml", position = 0)}, position = 0)}))
    public interface ListTaxAuthorityPartiesWidgets {}

    @Screen(name = "EditTaxAuthorityPartyInfo", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml")
    @Action(type = ActionType.SET, field = "taxAuthPartyId", fromField = "parameters.taxAuthPartyId")
    @Action(type = ActionType.SET, field = "taxAuthGeoId", fromField = "parameters.taxAuthGeoId")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyTaxAuthInfo", valueField = "partyTaxAuthInfo")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.partyTaxAuthInfo ? 'PageTitleEditTaxAuthorityPartyInfo' : 'PageTitleNewTaxAuthorityPartyInfo'}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "${groovy: context.partyTaxAuthInfo ? 'EditTaxAuthorityPartyInfo' : 'NewTaxAuthorityPartyInfo'}")
    @DecoratorScreen(
        name = "CommonTaxAuthorityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "EditTaxAuthorityPartyInfoSubTabBar", location = "component://accounting/widget/AccountingMenus.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "EditTaxAuthorityPartyInfoWidgets", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml"
            )})
        }
    )
    public interface EditTaxAuthorityPartyInfo {}

    @Screen(name = "EditTaxAuthorityPartyInfoWidgets", location = "component://accounting/widget/settings/TaxAuthorityScreens.xml")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyTaxAuthInfo", valueField = "partyTaxAuthInfo")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(includeForms = {@IncludeForm(name = "EditTaxAuthorityPartyInfo", location = "component://accounting/widget/settings/TaxAuthorityForms.xml")})}))
    public interface EditTaxAuthorityPartyInfoWidgets {}

}
