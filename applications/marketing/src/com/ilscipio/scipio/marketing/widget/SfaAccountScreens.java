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
package com.ilscipio.scipio.marketing.widget;

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
public class SfaAccountScreens {

    @Screen(name = "FindAccounts", location = "component://marketing/widget/sfa/AccountScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "currentUrl", value = "FindAccounts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Accounts")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/marketing/WEB-INF/actions/generated/FindAccounts_script1.groovy")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.SfaAcccounts}")
    @Action(type = ActionType.SET, field = "findScreenShowResults", value = "true")
    @DecoratorScreen(
        name = "CommonAccountDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "AccountSubTabBar", location = "component://marketing/widget/sfa/SfaMenus.xml"
            )}, decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindAccounts", location = "component://marketing/widget/sfa/forms/AccountForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListAccounts", location = "component://marketing/widget/sfa/forms/AccountForms.xml"
                    )}))})})
        }
    )
    public interface FindAccounts {}

    @Screen(name = "NewAccount", location = "component://marketing/widget/sfa/AccountScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Accounts")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCreateAccount")
    @Action(type = ActionType.SET, field = "accountType", fromField = "parameters.accountType")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCountryGeoId", resource = "general", property = "country.geo.id.default", defaultValue = "USA")
    @Action(type = ActionType.SET, field = "dependentForm", value = "NewAccount")
    @Action(type = ActionType.SET, field = "paramKey", value = "countryGeoId")
    @Action(type = ActionType.SET, field = "mainId", value = "countryGeoId")
    @Action(type = ActionType.SET, field = "dependentId", value = "stateProvinceGeoId")
    @Action(type = ActionType.SET, field = "requestName", value = "getAssociatedStateList")
    @Action(type = ActionType.SET, field = "responseName", value = "stateList")
    @Action(type = ActionType.SET, field = "dependentKeyName", value = "geoId")
    @Action(type = ActionType.SET, field = "descName", value = "geoName")
    @Action(type = ActionType.SET, field = "selectedDependentOption", fromField = "selectedStateName", defaultValue = "_none_")
    @DecoratorScreen(
        name = "CommonAccountDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "NewAccount", location = "component://marketing/widget/sfa/forms/AccountForms.xml", position = 1
                )}, htmlTemplates = {
                    @HtmlTemplate(location = "component://common/webcommon/includes/setDependentDropdownValuesJs.ftl", position = 0
                )})})
        }
    )
    public interface NewAccount {}

    @Screen(name = "ContactMechTypeOnly", location = "component://marketing/widget/sfa/AccountScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.contactMechTypeId", "equals", "ELECTRONIC_ADDRESS"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "FindInfoStringContactMech", location = "component://marketing/widget/sfa/forms/AccountForms.xml")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.contactMechTypeId", "equals", "POSTAL_ADDRESS"})}), widgets = @Widgets(sections = {@SectionNested(actions = @Actions(value = {@Action(type = ActionType.ENTITY_CONDITION, entityName = "Geo", list = "states", conditions = {@ConditionExpr(fieldName = "geoTypeId", value = "STATE"), @ConditionExpr(fieldName = "geoTypeId", value = "PROVINCE"), @ConditionExpr(fieldName = "geoTypeId", value = "TERRITORY")}, orderBy = {"geoName"}), @Action(type = ActionType.ENTITY_CONDITION, entityName = "Geo", list = "countries", conditions = {@ConditionExpr(fieldName = "geoTypeId", value = "COUNTRY")}, orderBy = {"geoName"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "FindPostalAddress", location = "component://marketing/widget/sfa/forms/AccountForms.xml")}))}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.contactMechTypeId", "equals", "TELECOM_NUMBER"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "FindTelecomNumber", location = "component://marketing/widget/sfa/forms/AccountForms.xml")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.contactMechTypeId", "equals", "EMAIL_ADDRESS"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "FindInfoStringContactMech", location = "component://marketing/widget/sfa/forms/AccountForms.xml")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.contactMechTypeId", "equals", "IP_ADDRESS"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "FindInfoStringContactMech", location = "component://marketing/widget/sfa/forms/AccountForms.xml")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.contactMechTypeId", "equals", "DOMAIN_NAME"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "FindInfoStringContactMech", location = "component://marketing/widget/sfa/forms/AccountForms.xml")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.contactMechTypeId", "equals", "WEB_ADDRESS"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "FindInfoStringContactMech", location = "component://marketing/widget/sfa/forms/AccountForms.xml")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.contactMechTypeId", "equals", "INTERNAL_PARTYID"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "FindInfoStringContactMech", location = "component://marketing/widget/sfa/forms/AccountForms.xml")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.contactMechTypeId", "equals", "LDAP_ADDRESS"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "FindInfoStringContactMech", location = "component://marketing/widget/sfa/forms/AccountForms.xml")}))
    public interface ContactMechTypeOnly {}

}
