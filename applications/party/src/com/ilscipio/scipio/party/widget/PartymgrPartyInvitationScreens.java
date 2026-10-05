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
public class PartymgrPartyInvitationScreens {

    @Screen(name = "FindPartyInvitations", location = "component://party/widget/partymgr/PartyInvitationScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PartyFindPartyInvitations")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "partyinv")
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.PartyInvitationNewPartyInvitation}", style = "${styles.link_nav} ${styles.action_add}", target = "editPartyInvitation"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindPartyInvitations", location = "component://party/widget/partymgr/PartyInvitationForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartyInvitations", location = "component://party/widget/partymgr/PartyInvitationForms.xml"
                        )}))})), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrViewPermissionError}", style = "common-msg-error-perm"
                        )}))})
        }
    )
    public interface FindPartyInvitations {}

    @Screen(name = "PartyInvitations", location = "component://party/widget/partymgr/PartyInvitationScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitlePartyInvitation")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "partyinv")
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(title = "${uiLabelMap[titleProperty]}}", includeForms = {
                        @IncludeForm(name = "ListPartyInvitations", location = "component://party/widget/partymgr/PartyInvitationForms.xml", position = 1
                    )}, containers = {
                        @Container(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.PartyInvitationNewPartyInvitation}", style = "${styles.link_nav} ${styles.action_add}", target = "editPartyInvitation"
                        )}, position = 0)})}), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrViewPermissionError}", style = "common-msg-error-perm"
                        )}))})
        }
    )
    public interface PartyInvitations {}

    @Screen(name = "EditPartyInvitation", location = "component://party/widget/partymgr/PartyInvitationScreens.xml")
    @Action(type = ActionType.SET, field = "partyInvitationId", fromField = "parameters.partyInvitationId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyInvitation", valueField = "partyInvitation")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.partyInvitation ? 'PageTitlePartyInvitation' : 'PartyInvitationNewPartyInvitation'}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "${groovy: context.partyInvitation ? 'EditPartyInvitation' : 'NewPartyInvitation'}")
    @DecoratorScreen(
        name = "CommonPartyInvitationDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditPartyInvitation", location = "component://party/widget/partymgr/PartyInvitationForms.xml"
                )})})
        }
    )
    public interface EditPartyInvitation {}

    @Screen(name = "EditPartyInvitationsGroupAssocs", location = "component://party/widget/partymgr/PartyInvitationScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditPartyInvitationGroupAssoc")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PartyInvitationGroupAssocs")
    @Action(type = ActionType.SET, field = "partyInvitationId", fromField = "parameters.partyInvitationId")
    @DecoratorScreen(
        name = "CommonPartyInvitationDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartyInvitationGroupAssocs", location = "component://party/widget/partymgr/PartyInvitationForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddPartyInvitationGroupAssoc}", name = "AddPartyInvitationsGroupAssocsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPartyInvitationGroupAssoc", location = "component://party/widget/partymgr/PartyInvitationForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditPartyInvitationsGroupAssocs {}

    @Screen(name = "EditPartyInvitationsRoleAssocs", location = "component://party/widget/partymgr/PartyInvitationScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditPartyInvitationRoleAssoc")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PartyInvitationRoleAssocs")
    @Action(type = ActionType.SET, field = "partyInvitationId", fromField = "parameters.partyInvitationId")
    @DecoratorScreen(
        name = "CommonPartyInvitationDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartyInvitationRoleAssocs", location = "component://party/widget/partymgr/PartyInvitationForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddPartyInvitationRoleAssoc}", name = "AddPartyInvitationRoleAssocsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPartyInvitationRoleAssoc", location = "component://party/widget/partymgr/PartyInvitationForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditPartyInvitationsRoleAssocs {}

}
