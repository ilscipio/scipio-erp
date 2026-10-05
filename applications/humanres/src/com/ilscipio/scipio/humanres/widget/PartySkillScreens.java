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
public class PartySkillScreens {

    @Screen(name = "FindPartySkills", location = "component://humanres/widget/PartySkillScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResFindPartySkill")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Employees")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "partySkillsCtx", fromField = "parameters")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.HumanResNewPartySkill}", style = "${styles.link_nav} ${styles.action_add}", target = "NewPartySkill"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindPartySkills", location = "component://humanres/widget/forms/PartySkillForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartySkills", location = "component://humanres/widget/forms/PartySkillForms.xml"
                        )}))})})
        }
    )
    public interface FindPartySkills {}

    @Screen(name = "EditPartySkills", location = "component://humanres/widget/PartySkillScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewPartySkill")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPartySkills")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "skillTypeId", fromField = "parameters.skillTypeId")
    @Action(type = ActionType.SET, field = "partySkillsCtx.partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "parameters.insideEmployee", value = "true")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartySkills", location = "component://humanres/widget/forms/PartySkillForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddPartySkill}", name = "AddPartySkillPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPartySkills", location = "component://humanres/widget/forms/PartySkillForms.xml"
                )})})
        }
    )
    public interface EditPartySkills {}

    @Screen(name = "NewPartySkill", location = "component://humanres/widget/PartySkillScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEditPartySkill")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Employees")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.employeePartyId")
    @Action(type = ActionType.SET, field = "skillTypeId", fromField = "parameters.skillTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartySkill", valueField = "partySkill")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResNewPartySkill}", includeForms = {
                    @IncludeForm(name = "AddPartySkills", location = "component://humanres/widget/forms/PartySkillForms.xml"
                )})})
        }
    )
    public interface NewPartySkill {}

}
