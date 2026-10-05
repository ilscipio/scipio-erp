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
public class SfaLookupScreens {

    @Screen(name = "LookupLeads", location = "component://marketing/widget/sfa/LookupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "partyRelationshipTypeId", value = "LEAD_OWNER")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.SfaFindLeads}")
    @Action(type = ActionType.SET, field = "partyTypeId", value = "PERSON")
    @Action(type = ActionType.SET, field = "currentUrl", value = "LookupLeads")
    @Action(type = ActionType.SET, field = "entityName", value = "PartyRoleAndContactMechDetail")
    @Action(type = ActionType.SET, field = "searchFields", value = "[partyId, firstName, lastName, middleName, groupName]")
    @Action(type = ActionType.SET, field = "searchDistinct", value = "true")
    @Action(type = ActionType.SERVICE, serviceName = "findParty")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "FindLeads", location = "component://marketing/widget/sfa/forms/LeadForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupLead", location = "component://marketing/widget/sfa/forms/LookupForms.xml"
            )})
        }
    )
    public interface LookupLeads {}

    @Screen(name = "LookupAccounts", location = "component://marketing/widget/sfa/LookupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "partyRelationshipTypeId", value = "ACCOUNT")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.SfaFindAccounts}")
    @Action(type = ActionType.SET, field = "partyTypeId", value = "PARTY_GROUP")
    @Action(type = ActionType.SET, field = "currentUrl", value = "LookupAccounts")
    @Action(type = ActionType.SET, field = "entityName", value = "PartyRoleAndContactMechDetail")
    @Action(type = ActionType.SET, field = "searchFields", value = "[partyId, firstName, lastName, middleName, groupName]")
    @Action(type = ActionType.SET, field = "searchDistinct", value = "true")
    @Action(type = ActionType.SERVICE, serviceName = "findParty")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "FindAccounts", location = "component://marketing/widget/sfa/forms/AccountForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupAccount", location = "component://marketing/widget/sfa/forms/LookupForms.xml"
            )})
        }
    )
    public interface LookupAccounts {}

    @Screen(name = "LookupAccountLeads", location = "component://marketing/widget/sfa/LookupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "partyRelationshipTypeId", value = "ACCOUNT")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.SfaFindAccountLeads}")
    @Action(type = ActionType.SET, field = "partyTypeId", value = "PARTY_GROUP")
    @Action(type = ActionType.SET, field = "currentUrl", value = "LookupAccountLeads")
    @Action(type = ActionType.SET, field = "entityName", value = "PartyRoleAndContactMechDetail")
    @Action(type = ActionType.SET, field = "searchFields", value = "[partyId, firstName, lastName, middleName, groupName]")
    @Action(type = ActionType.SET, field = "searchDistinct", value = "true")
    @Action(type = ActionType.SERVICE, serviceName = "findParty")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "FindAccounts", location = "component://marketing/widget/sfa/forms/AccountForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupAccountLead", location = "component://marketing/widget/sfa/forms/LookupForms.xml"
            )})
        }
    )
    public interface LookupAccountLeads {}

}
