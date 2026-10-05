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
public class PartymgrEmailPartyScreens {

    @Screen(name = "CreatePartyNotification", location = "component://party/widget/partymgr/EmailPartyScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyNotificationMailCreate}")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/templates/email/CreatePartyNotification.ftl", platform = "email"
            )})
        }
    )
    public interface CreatePartyNotification {}

    @Screen(name = "ChangePersonalInfoNotification", location = "component://party/widget/partymgr/EmailPartyScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyNotificationMailChange}")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/templates/email/ChangePersonalInfoNotification.ftl", platform = "email"
            )})
        }
    )
    public interface ChangePersonalInfoNotification {}

    @Screen(name = "VerifyEmailAddressNotification", location = "component://party/widget/partymgr/EmailPartyScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyNotificationMailVerification}")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/templates/email/VerifyEmailAddressNotification.ftl", platform = "email"
            )})
        }
    )
    public interface VerifyEmailAddressNotification {}

    @Screen(name = "AccountActivatedNotification", location = "component://party/widget/partymgr/EmailPartyScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyNotificationMailActivated}")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/templates/email/AccountActivatedNotification.ftl", platform = "email"
            )})
        }
    )
    public interface AccountActivatedNotification {}

}
