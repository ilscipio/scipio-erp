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
package com.ilscipio.scipio.commonext.widget;

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
public class CommonScreens {

    @Screen(name = "ApplicationDecorator", location = "component://commonext/widget/CommonScreens.xml")
    @Action(order = 0, type = ActionType.PROPERTY_MAP, resource = "CommonExtUiLabels", mapName = "uiLabelMap", global = true)
    @Action(order = 1, type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "partyNameView", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "userLogin.partyId")})
    @Action(order = 2, type = ActionType.SET, field = "line.text", value = "${uiLabelMap.CommonWelcome} ${person.firstName} ${person.middleName} ${person.lastName}")
    @Action(order = 3, type = ActionType.SET, field = "line.urlText", value = "[${userLogin.userLoginId}]")
    @Action(order = 4, type = ActionType.SET, field = "line.url", value = "/partymgr/control/viewprofile?partyId=${userLogin.partyId}")
    @Action(order = 5, type = ActionType.SET, field = "layoutSettings.topLines[]", fromField = "line", global = true)
    @Action(order = 6, type = ActionType.SCRIPT, location = "component://commonext/webapp/ofbizsetup/organization/changeOrgPartyId.groovy")
    @Action(order = 7, type = ActionType.SET, field = "helpTopic", value = "${groovy: context.webappName?.toUpperCase() + '_' + requestAttributes._CURRENT_VIEW_}")
    @Action(order = 8, type = ActionType.ENTITY_AND, entityName = "ContentAssoc", list = "pageAvail", fieldMaps = {@FieldMap(fieldName = "mapKey", fromField = "helpTopic")})
    @Action(order = 9, type = ActionType.ENTITY_AND, entityName = "WebAnalyticsConfig", list = "layoutSettings.WEB_ANALYTICS", fieldMaps = {@FieldMap(fieldName = "webAnalyticsTypeId", value = "BACKEND_ANALYTICS")})
    @IfAction(order = 10, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"PartyAcctgPrefAndGroupList"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "defaultOrganizationPartyId", value = "${userPreferences.ORGANIZATION_PARTY}", global = true), @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default"), @Action(type = ActionType.ENTITY_ONE, entityName = "PartyAcctgPrefAndGroup", valueField = "orgParty", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "defaultOrganizationPartyId"), @FieldMap(fieldName = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}), @Action(type = ActionType.ENTITY_ONE, entityName = "PartyGroup", valueField = "orgPartyLogoMap", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "defaultOrganizationPartyId")}), @Action(type = ActionType.ENTITY_AND, entityName = "PartyContent", list = "orgPartyContentMap", filterByDate = true, fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "defaultOrganizationPartyId"), @FieldMap(fieldName = "partyContentTypeId", value = "LGOIMGURL")}, orderBy = {"-fromDate"}), @Action(type = ActionType.SET, field = "orgContentId", fromField = "orgPartyContentMap[0].contentId"), @Action(type = ActionType.SET, field = "orgPartyContent", value = "${groovy: orgContentId!=null?'/content/control/stream?contentId=' + orgContentId + externalKeyParam : ''}"), @Action(type = ActionType.SET, field = "layoutSettings.organizationLogoLinkUrl", fromField = "orgPartyContent", defaultValue = "${orgPartyLogoMap.logoImageUrl}", global = true), @Action(type = ActionType.SET, field = "defaultOrganizationPartyCurrencyUomId", fromField = "orgParty.baseCurrencyUomId", defaultValue = "${defaultCurrencyUomId}", global = true), @Action(type = ActionType.SET, field = "defaultOrganizationPartyGroupName", fromField = "orgParty.groupName", global = true), @Action(type = ActionType.SET, field = "dropdown.hiddenFieldList", fromField = "hiddenFields", global = true), @Action(type = ActionType.SET, field = "dropdown.action", value = "setUserPreference"), @Action(type = ActionType.SET, field = "dropdown.textBegin", value = "${uiLabelMap.CommonDefaultOrganizationPartyId} :"), @Action(type = ActionType.SET, field = "dropdown.dropDownList", fromField = "PartyAcctgPrefAndGroupList"), @Action(type = ActionType.SET, field = "dropdown.selectionName", value = "userPrefValue"), @Action(type = ActionType.SET, field = "dropdown.selectedKey", value = "${defaultOrganizationPartyId}"), @Action(type = ActionType.SET, field = "dropdown.textEnd", value = "[${defaultOrganizationPartyId}]"), @Action(type = ActionType.SET, field = "layoutSettings.topLines[]", fromField = "dropdown", global = true)}))
    @IfAction(order = 11, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"PartyAcctgPrefAndGroupList"})}), then = @Actions())
    @DecoratorScreen(
        name = "GlobalDecorator",
        location = "component://common/widget/CommonScreens.xml"
    )
    public interface ApplicationDecorator {}

}
