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
public class SfaServicesScreens {

    @Screen(name = "PartyCommunicationEvents", location = "component://marketing/widget/sfa/ServicesScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCommEvents")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "MyCommunicationEvents")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/sfa/WEB-INF/action/services/FindMyCommunication.groovy")
    @DecoratorScreen(
        name = "CommonServiceDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://marketing/webapp/sfa/services/FindMyCommunication.ftl"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_SCREEN, name = "PartyCommunicationEventsResults"
                    )}))})})
        }
    )
    public interface PartyCommunicationEvents {}

    @Screen(name = "PartyCommunicationEventsResults", location = "component://marketing/widget/sfa/ServicesScreens.xml")
    @Action(type = ActionType.SET, field = "internalNotesOnly", fromField = "internalNotesOnly", defaultValue = "false")
    @Action(type = ActionType.SET, field = "partyId", fromField = "communicationPartyId", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/sfa/WEB-INF/action/services/MyCommunicationList.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://marketing/webapp/sfa/services/MyCommunicationList.ftl")}))
    public interface PartyCommunicationEventsResults {}

    @Screen(name = "FindRequest", location = "component://marketing/widget/sfa/ServicesScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderFindRequests")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindRequest")
    @Action(type = ActionType.SET, field = "entityName", value = "CustRequest")
    @DecoratorScreen(
        name = "CommonServiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindRequests", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListRequests", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                    )}))})})
        }
    )
    public interface FindRequest {}

    @Screen(name = "ViewRequest", location = "component://marketing/widget/sfa/ServicesScreens.xml")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindRequest")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewRequest")
    @Action(type = ActionType.SET, field = "showRequestManagementLinks", value = "Y")
    @DecoratorScreen(
        name = "CommonServiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ViewCustRequest", location = "component://order/widget/ordermgr/CustRequestScreens.xml"
            )})
        }
    )
    public interface ViewRequest {}

    @Screen(name = "EditRequest", location = "component://marketing/widget/sfa/ServicesScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderRequest")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindRequest")
    @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.SET, field = "statusId", fromField = "custRequest.statusId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "StatusItem", valueField = "currentStatus")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @DecoratorScreen(
        name = "CommonServiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Compare.class, params = {"parameters.small", "equals", "Y"
                    })}), widgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "EditSmallCustRequest", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                    )}), failWidgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "EditCustRequest", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                    )}))})})
        }
    )
    public interface EditRequest {}

}
