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
public class SfaOpportunityScreens {

    @Screen(name = "FindSalesOpportunity", location = "component://marketing/widget/sfa/OpportunityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "SfaFindOpportunities")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Opportunities")
    @Action(type = ActionType.SET, field = "findScreenShowResults", value = "true")
    @DecoratorScreen(
        name = "CommonOpportunityDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonCreateNew}", style = "${styles.link_nav} ${styles.action_add}", target = "EditSalesOpportunity"
                )})}, decorators = {
                    @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindSalesOpportunity", location = "component://marketing/widget/sfa/forms/OpportunityForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListSalesOpportunity", location = "component://marketing/widget/sfa/forms/OpportunityForms.xml"
                        )}))})})
        }
    )
    public interface FindSalesOpportunity {}

    @Screen(name = "EditSalesOpportunity", location = "component://marketing/widget/sfa/OpportunityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "SfaEditOpportunity")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditSalesOpportunity")
    @Action(type = ActionType.SET, field = "salesOpportunityId", fromField = "parameters.salesOpportunityId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SalesOpportunity", valueField = "salesOpportunity")
    @Action(type = ActionType.SERVICE, serviceName = "findPartyInSalesOpportunityRole", resultMapName = "accountPartyResult", fieldMaps = {@FieldMap(fieldName = "salesOpportunityId", fromField = "parameters.salesOpportunityId"), @FieldMap(fieldName = "roleTypeId", value = "ACCOUNT")})
    @Action(type = ActionType.SERVICE, serviceName = "findPartyInSalesOpportunityRole", resultMapName = "leadPartyResult", fieldMaps = {@FieldMap(fieldName = "salesOpportunityId", fromField = "parameters.salesOpportunityId"), @FieldMap(fieldName = "roleTypeId", value = "LEAD")})
    @Action(type = ActionType.SET, field = "leadPartyId", fromField = "leadPartyResult.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @Action(type = ActionType.ENTITY_AND, entityName = "PartyRelationshipAndDetail", list = "partyAccount", fieldMaps = {@FieldMap(fieldName = "partyIdTo", fromField = "communicationEvent.partyIdFrom"), @FieldMap(fieldName = "roleTypeIdFrom", value = "ACCOUNT"), @FieldMap(fieldName = "roleTypeIdTo", value = "CONTACT")})
    @Action(type = ActionType.SET, field = "accountPartyId", fromField = "accountPartyResult.partyId", defaultValue = "${partyAccount[0].partyIdFrom}")
    @DecoratorScreen(
        name = "CommonOpportunityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditSalesOpportunity", location = "component://marketing/widget/sfa/forms/OpportunityForms.xml"
                )})})
        }
    )
    public interface EditSalesOpportunity {}

    @Screen(name = "ViewSalesOpportunity", location = "component://marketing/widget/sfa/OpportunityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "SfaOpportunityInfo")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewSalesOpportunity")
    @Action(type = ActionType.SET, field = "salesOpportunityId", fromField = "parameters.salesOpportunityId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SalesOpportunity", valueField = "salesOpportunity")
    @Action(type = ActionType.SERVICE, serviceName = "findPartyInSalesOpportunityRole", resultMapName = "accountPartyResult", fieldMaps = {@FieldMap(fieldName = "salesOpportunityId", fromField = "parameters.salesOpportunityId"), @FieldMap(fieldName = "roleTypeId", value = "ACCOUNT")})
    @Action(type = ActionType.SET, field = "accountParty.accountPartyId", fromField = "accountPartyResult.partyId")
    @Action(type = ActionType.SERVICE, serviceName = "findPartyInSalesOpportunityRole", resultMapName = "leadPartyResult", fieldMaps = {@FieldMap(fieldName = "salesOpportunityId", fromField = "parameters.salesOpportunityId"), @FieldMap(fieldName = "roleTypeId", value = "LEAD")})
    @Action(type = ActionType.SET, field = "leadParty.leadPartyId", fromField = "leadPartyResult.partyId")
    @DecoratorScreen(
        name = "CommonOpportunityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ViewSalesOpportunity", location = "component://marketing/widget/sfa/forms/OpportunityForms.xml"
                )})})
        }
    )
    public interface ViewSalesOpportunity {}

    @Screen(name = "OpportunityCommEvent", location = "component://marketing/widget/sfa/OpportunityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListCommunications")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PartyCommEvents")
    @Action(type = ActionType.SET, field = "activeSubMenu2Item", value = "CommunicationEvent")
    @Action(type = ActionType.SET, field = "salesOpportunityId", fromField = "parameters.salesOpportunityId")
    @Action(type = ActionType.SERVICE, serviceName = "findPartyInSalesOpportunityRole", resultMapName = "leadPartyResult", fieldMaps = {@FieldMap(fieldName = "salesOpportunityId", fromField = "parameters.salesOpportunityId"), @FieldMap(fieldName = "roleTypeId", value = "LEAD")})
    @Action(type = ActionType.SET, field = "partyId", fromField = "leadPartyResult.partyId", defaultValue = "${parameters.partyId}")
    @Action(type = ActionType.ENTITY_AND, entityName = "Party", list = "partyperson", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId"), @FieldMap(fieldName = "partyTypeId", value = "PERSON")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CommunicationEventAndRole", list = "commEvents", conditions = {@ConditionExpr(fieldName = "partyId", operator = "equals", value = "${partyId}")}, orderBy = {"-entryDate"})
    @Action(type = ActionType.ENTITY_AND, entityName = "PartyRelationship", list = "contacts", filterByDate = true, fieldMaps = {@FieldMap(fieldName = "partyIdFrom", fromField = "partyId"), @FieldMap(fieldName = "roleTypeIdFrom", value = "ACCOUNT"), @FieldMap(fieldName = "roleTypeIdTo", value = "CONTACT")}, orderBy = {"partyIdTo"})
    @DecoratorScreen(
        name = "CommonOpportunityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "CommEventTabBar", location = "component://party/widget/partymgr/PartyMenus.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_MENU, name = "CommSubTabBar", location = "component://party/widget/partymgr/PartyMenus.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleListCommunications} ${partyId}", includeForms = {
                    @IncludeForm(name = "ListCommEvents", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )})})
        }
    )
    public interface OpportunityCommEvent {}

}
