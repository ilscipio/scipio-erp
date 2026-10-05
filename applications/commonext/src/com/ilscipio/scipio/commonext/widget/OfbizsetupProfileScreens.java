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
package com.ilscipio.scipio.commonext.widget;

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
public class OfbizsetupProfileScreens {

    @Screen(name = "FirstCustomer", location = "component://commonext/widget/ofbizsetup/ProfileScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PartyCreateNewCustomer")
    @Action(type = ActionType.SET, field = "activeSubMenuItemTop", value = "firstcustomer")
    @Action(type = ActionType.SET, field = "target", value = "createCustomer")
    @Action(type = ActionType.SET, field = "partyId", value = "CUST")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PartyRole", list = "parties", conditions = {@ConditionExpr(fieldName = "roleTypeId", operator = "equals", value = "CUSTOMER")})
    @Action(type = ActionType.SET, field = "partyId", fromField = "parties[0].partyId")
    @Action(type = ActionType.SET, field = "customerPartyId", fromField = "customerRel.partyIdFrom")
    @Action(type = ActionType.SET, field = "previousParams", fromField = "_PREVIOUS_PARAMS_", fromScope = "user")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCountryGeoId", resource = "general", property = "country.geo.id.default", defaultValue = "USA")
    @DecoratorScreen(
        name = "CommonSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"PARTYMGR", "_CREATE"
                })}), widgets = @InlineWidgets(sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Empty.class, params = {"parties"})}), actions = @Actions(value = {
                            @Action(type = ActionType.SET, field = "dependentForm", value = "NewCustomer"
                        ),
                        @Action(type = ActionType.SET, field = "depFormFieldPrefix", value = "NewUser_"
                    ),
                    @Action(type = ActionType.SET, field = "paramKey", value = "countryGeoId"
                ),
                @Action(type = ActionType.SET, field = "mainId", value = "USER_COUNTRY"
            ),
            @Action(type = ActionType.SET, field = "dependentId", value = "USER_STATE"
            ),
            @Action(type = ActionType.SET, field = "requestName", value = "getAssociatedStateList"
            ),
            @Action(type = ActionType.SET, field = "responseName", value = "stateList"
            ),
            @Action(type = ActionType.SET, field = "dependentKeyName", value = "geoId"
            ),
            @Action(type = ActionType.SET, field = "descName", value = "geoName"
            ),
            @Action(type = ActionType.SET, field = "selectedDependentOption", value = "_none_"
            )}), widgets = @WidgetsForContainer(value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/setDependentDropdownValuesJs.ftl"
            )}, screenlets = {
                @ScreenletNested(includeForms = {
                    @IncludeForm(name = "NewCustomer", location = "component://commonext/widget/ofbizsetup/SetupForms.xml"
                
            )})}), failWidgets = @WidgetsForContainer(value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "viewprofile"
            )}))}), failWidgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrCreatePermissionError}", style = "common-msg-error-perm"
            )}))})
        }
    )
    public interface FirstCustomer {}

    @Screen(name = "viewprofile", location = "component://commonext/widget/ofbizsetup/ProfileScreens.xml")
    @Action(type = ActionType.SET, field = "parameters.partyId", fromField = "partyId")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "Party", location = "component://party/widget/partymgr/ProfileScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "Contact", location = "component://party/widget/partymgr/ProfileScreens.xml")}))
    public interface viewprofile {}

}
