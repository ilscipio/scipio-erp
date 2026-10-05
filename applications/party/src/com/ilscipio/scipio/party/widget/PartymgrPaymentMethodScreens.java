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
public class PartymgrPaymentMethodScreens {

    @Screen(name = "PaymentMethodDecorator", location = "component://party/widget/partymgr/PaymentMethodScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "viewprofile")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/HasPartyPermissions.groovy")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                    @OrCondition(ifTrue = {"hasViewPermission", "hasPayInfoPermission"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(containers = {
                    @Container(labels = {
                        @Label(text = "${uiLabelMap.AccountingCardInfoNotBelongToYou}"
                    )}),
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonBack}", style = "${styles.link_nav_cancel}", target = "authview/${donePage}"
                    )})}))})
        }
    )
    public interface PaymentMethodDecorator {}

    @Screen(name = "editcreditcard", location = "component://party/widget/partymgr/PaymentMethodScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditCreditCard")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editcreditcard")
    @Action(type = ActionType.SET, field = "cardNumberMinDisplay", value = "min")
    @Action(type = ActionType.SET, field = "showToolTip", value = "true")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/HasPartyPermissions.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/EditPaymentMethod.groovy")
    @DecoratorScreen(
        name = "PaymentMethodDecorator",
        location = "component://party/widget/partymgr/PaymentMethodScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/editcreditcard.ftl"
            )})
        }
    )
    public interface editcreditcard {}

    @Screen(name = "editgiftcard", location = "component://party/widget/partymgr/PaymentMethodScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editgiftcard")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/HasPartyPermissions.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/EditPaymentMethod.groovy")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy:context.giftCard ? 'PageTitleEditGiftCard' : 'AccountingCreateNewGiftCard'}")
    @DecoratorScreen(
        name = "PaymentMethodDecorator",
        location = "component://party/widget/partymgr/PaymentMethodScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/editgiftcard.ftl"
            )})
        }
    )
    public interface editgiftcard {}

    @Screen(name = "editeftaccount", location = "component://party/widget/partymgr/PaymentMethodScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditEftAccount")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editeftaccount")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/HasPartyPermissions.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/EditPaymentMethod.groovy")
    @DecoratorScreen(
        name = "PaymentMethodDecorator",
        location = "component://party/widget/partymgr/PaymentMethodScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/editeftaccount.ftl"
            )})
        }
    )
    public interface editeftaccount {}

    @Screen(name = "EditBillingAccount", location = "component://party/widget/partymgr/PaymentMethodScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditBillingAccount")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditBillingAccount")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.SET, field = "billingAccountId", fromField = "parameters.billingAccountId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "BillingAccount", valueField = "billingAccount")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "roleTypeId", fromField = "parameters.roleTypeId")
    @DecoratorScreen(
        name = "PaymentMethodDecorator",
        location = "component://party/widget/partymgr/PaymentMethodScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditBillingAccount", location = "component://party/widget/partymgr/PaymentMethodForms.xml"
                )})})
        }
    )
    public interface EditBillingAccount {}

}
