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
package com.ilscipio.scipio.party.widget;

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
public class PartymgrPartyClassificationScreens {

    @Screen(name = "EditPartyClassifications", location = "component://party/widget/partymgr/PartyClassificationScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewPartyClassifications")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPartyClassifications")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PartyClassifications")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartyClassifications", location = "component://party/widget/partymgr/PartyClassificationForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyClassifications}", name = "AddPartyClassificationPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPartyClassification", location = "component://party/widget/partymgr/PartyClassificationForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditPartyClassifications {}

    @Screen(name = "EditPartyClassificationGroupParties", location = "component://party/widget/partymgr/PartyClassificationScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewPartyClassificationGroupParties")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPartyClassificationGroupParties")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PartyClassificationGroupParties")
    @Action(type = ActionType.SET, field = "partyClassificationGroupId", fromField = "parameters.partyClassificationGroupId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyClassificationGroup", valueField = "partyClassificationGroup")
    @DecoratorScreen(
        name = "CommonPartyClassificationDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartyClassificationGroupParties", location = "component://party/widget/partymgr/PartyClassificationForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyClassificationGroups}", name = "AddPartyClassificationGroupPartiesPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPartyClassificationParty", location = "component://party/widget/partymgr/PartyClassificationForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditPartyClassificationGroupParties {}

    @Screen(name = "FindPartyClassificationGroups", location = "component://party/widget/partymgr/PartyClassificationScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindPartyClassificationGroups")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindPartyClassificationGroups")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PartyClassificationGroups")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @DecoratorScreen(
        name = "CommonPartyClassificationDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.PartyCreateNewPartyClassificationGroup}", style = "${styles.link_nav} ${styles.action_add}", target = "EditPartyClassificationGroup"
                    )}),
                    @Container(style = "screenlet-body", includeForms = {
                        @IncludeForm(name = "ListPartyClassificationGroups", location = "component://party/widget/partymgr/PartyClassificationForms.xml"
                    )})})})
        }
    )
    public interface FindPartyClassificationGroups {}

    @Screen(name = "EditPartyClassificationGroup", location = "component://party/widget/partymgr/PartyClassificationScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditPartyClassificationGroup")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPartyClassificationGroup")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PartyClassificationGroup")
    @Action(type = ActionType.SET, field = "partyClassificationGroupId", fromField = "parameters.partyClassificationGroupId")
    @Action(type = ActionType.SET, field = "partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyClassificationGroup", valueField = "partyClassificationGroup")
    @DecoratorScreen(
        name = "CommonPartyClassificationDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditPartyClassificationGroup", location = "component://party/widget/partymgr/PartyClassificationForms.xml"
                )})})
        }
    )
    public interface EditPartyClassificationGroup {}

}
