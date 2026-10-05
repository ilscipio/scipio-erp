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
package com.ilscipio.scipio.humanres.widget;

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
public class PartyQualScreens {

    @Screen(name = "FindPartyQuals", location = "component://humanres/widget/PartyQualScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResFindPartyQual")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Employees")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "partyQualTypeId", fromField = "parameters.partyQualTypeId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate")
    @Action(type = ActionType.SET, field = "partyQualCtx", fromField = "parameters")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.HumanResNewPartyQual}", style = "${styles.link_nav} ${styles.action_add}", target = "NewPartyQual"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindPartyQuals", location = "component://humanres/widget/forms/PartyQualForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartyQuals", location = "component://humanres/widget/forms/PartyQualForms.xml"
                        )}))})})
        }
    )
    public interface FindPartyQuals {}

    @Screen(name = "EditPartyQuals", location = "component://humanres/widget/PartyQualScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEditPartyQual")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPartyQuals")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "partyQualCtx.partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "parameters.insideEmployee", value = "true")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartyQuals", location = "component://humanres/widget/forms/PartyQualForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddPartyQual}", name = "AddPartyQualPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPartyQual", location = "component://humanres/widget/forms/PartyQualForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditPartyQuals {}

    @Screen(name = "NewPartyQual", location = "component://humanres/widget/PartyQualScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Employees")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyQual", valueField = "partyQual")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResNewPartyQual}", includeForms = {
                    @IncludeForm(name = "AddPartyQual", location = "component://humanres/widget/forms/PartyQualForms.xml"
                )})})
        }
    )
    public interface NewPartyQual {}

}
