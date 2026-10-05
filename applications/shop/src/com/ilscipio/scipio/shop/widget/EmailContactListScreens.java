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
package com.ilscipio.scipio.shop.widget;

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
public class EmailContactListScreens {

    @Screen(name = "ContactListVerifyEmail", location = "component://shop/widget/EmailContactListScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "EcommerceSubscriptionVerifyEmail")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "partyName", autoFieldMap = false, fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "contactListParty.partyId")})
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/ContactListVerifyEmail.ftl", platform = "email"
            )})
        }
    )
    public interface ContactListVerifyEmail {}

    @Screen(name = "signupforcontactlist", location = "component://shop/widget/EmailContactListScreens.xml")
    @Action(type = ActionType.SET, field = "partyId", fromField = "userLogin.partyId")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/ContactList.groovy")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/miniSignUpForContactList.ftl", platform = "email"
            )})
        }
    )
    public interface signupforcontactlist {}

    @Screen(name = "ContactUsEmailNotification", location = "component://shop/widget/EmailContactListScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/ContactUsEmail.ftl", platform = "email"
            )})
        }
    )
    public interface ContactUsEmailNotification {}

    @Screen(name = "ContactListSubscribeEmail", location = "component://shop/widget/EmailContactListScreens.xml")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContactList", valueField = "contactList")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "partyName")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/ContactListSubscribeEmail.ftl", platform = "email"
            )})
        }
    )
    public interface ContactListSubscribeEmail {}

    @Screen(name = "ContactListUnsubscribeVerifyEmail", location = "component://shop/widget/EmailContactListScreens.xml")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContactList", valueField = "contactList")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "partyName")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/ContactListUnsubscribeVerifyEmail.ftl", platform = "email"
            )})
        }
    )
    public interface ContactListUnsubscribeVerifyEmail {}

    @Screen(name = "ContactListUnsubscribeEmail", location = "component://shop/widget/EmailContactListScreens.xml")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContactList", valueField = "contactList")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "partyName")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/ContactListUnsubscribeEmail.ftl", platform = "email"
            )})
        }
    )
    public interface ContactListUnsubscribeEmail {}

    @Screen(name = "ContactListEmailTemplate", location = "component://shop/widget/EmailContactListScreens.xml")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContactList", valueField = "contactList")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "partyName")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/ContactListEmailTemplate.ftl", platform = "email"
            )})
        }
    )
    public interface ContactListEmailTemplate {}

}
