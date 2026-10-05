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
public class PartymgrPartyContactListScreens {

    @Screen(name = "ListPartyContactLists", location = "component://party/widget/partymgr/PartyContactListScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListContactList")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ContactList")
    @Action(type = ActionType.SET, field = "partyId", value = "${parameters.partyId}")
    @Action(type = ActionType.SET, field = "entityName", value = "ContactListParty")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartyContactLists", location = "component://party/widget/partymgr/PartyContactListForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyContactListPartyCreate}", name = "AddPartyContactListPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPartyContactList", location = "component://party/widget/partymgr/PartyContactListForms.xml"
                )}, position = 0)})
        }
    )
    public interface ListPartyContactLists {}

    @Screen(name = "ListLookupContactList", location = "component://party/widget/partymgr/PartyContactListScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ContactList")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${uiLabelMap.PageTitleListContactList}")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleListContactList}")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/ListContactList")
    @Action(type = ActionType.SET, field = "contactListId", fromField = "parameters.contactListId")
    @Action(type = ActionType.SET, field = "entityName", value = "ContactList")
    @Action(type = ActionType.SET, field = "searchFields", value = "[contactListId, contactListName, description]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListLookupContactList", location = "component://party/widget/partymgr/PartyContactListForms.xml"
            )})
        }
    )
    public interface ListLookupContactList {}

}
