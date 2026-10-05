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
public class ContactListMenus {

    @Menu(
        name = "ContactListTabBar",
        location = "component://marketing/widget/ContactListMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "ContactList",
        selectedMenuItemContextFieldName = "activeContactListSubMenuItem",
        items = {
            @MenuItem(name = "ContactList", title = "${uiLabelMap.MarketingContactList}", link = @MenuLink(target = "EditContactList", parameters = {@MenuParameter(paramName = "contactListId", fromField = "contactListId")})),
            @MenuItem(name = "ContactListParty", title = "${uiLabelMap.PartyParties}", link = @MenuLink(target = "FindContactListParties", parameters = {@MenuParameter(paramName = "contactListId", fromField = "contactListId"), @MenuParameter(paramName = "hideExpired", value = "Y")})),
            @MenuItem(name = "ContactListCommEvent", title = "${uiLabelMap.PartyCommEvents}", link = @MenuLink(target = "FindContactListCommEvents", parameters = {@MenuParameter(paramName = "contactListId", fromField = "contactListId")})),
            @MenuItem(name = "ContactListImportParty", title = "${uiLabelMap.MarketingContactListPartiesImport}", link = @MenuLink(target = "FindImportContactListParties", parameters = {@MenuParameter(paramName = "contactListId", fromField = "contactListId")})),
            @MenuItem(name = "WebSiteContactList", title = "${uiLabelMap.MarketingWebSiteContactList}", link = @MenuLink(target = "webSiteContactList", parameters = {@MenuParameter(paramName = "contactListId", fromField = "contactListId")}))
        }
    )
    public interface ContactListTabBar {}

    @Menu(
        name = "ContactListSideBar",
        location = "component://marketing/widget/ContactListMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ContactListTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "ContactList",
        selectedMenuItemContextFieldName = "activeContactListSubMenuItem"
    )
    public interface ContactListSideBar {}

    @Menu(
        name = "ContactListCommBar",
        location = "component://marketing/widget/ContactListMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "Preview", title = "Preview", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"communicationEvent"})}), link = @MenuLink(target = "PreviewContactListCommEvent", targetWindow = "_blank", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "communicationEventId"), @MenuParameter(paramName = "contactListId", fromField = "contactListId")})),
            @MenuItem(name = "Publish", title = "Publish", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"communicationEvent"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "equals", "COM_PENDING"})}), link = @MenuLink(target = "updateContactListCommEvent", linkType = LinkType.HIDDEN_FORM, parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "communicationEventId"), @MenuParameter(paramName = "contactListId", fromField = "contactListId"), @MenuParameter(paramName = "statusId", value = "COM_IN_PROGRESS")}))
        }
    )
    public interface ContactListCommBar {}

}
