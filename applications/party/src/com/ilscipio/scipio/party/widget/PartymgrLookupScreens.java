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
public class PartymgrLookupScreens {

    @Screen(name = "LookupPartyName", location = "component://party/widget/partymgr/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyLookupPartyByName}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "PartyNameView")
    @Action(type = ActionType.SET, field = "searchFields", value = "[partyId, firstName, middleName, lastName, groupName]")
    @Action(type = ActionType.SET, field = "displayFields", value = "[firstName, lastName, groupName]")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/StatusCondition.groovy")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupPartyName", location = "component://party/widget/partymgr/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupPartyName", location = "component://party/widget/partymgr/LookupForms.xml"
            )})
        }
    )
    public interface LookupPartyName {}

    @Screen(name = "LookupPartyEmail", location = "component://party/widget/partymgr/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyLookupPartyByName}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "PartyNameContactMechView")
    @Action(type = ActionType.SET, field = "searchFields", value = "[contactMechId, partyId, firstName, middleName, lastName, groupName]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupPartyEmail", location = "component://party/widget/partymgr/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupPartyEmail", location = "component://party/widget/partymgr/LookupForms.xml"
            )})
        }
    )
    public interface LookupPartyEmail {}

    @Screen(name = "LookupCustomerName", location = "component://party/widget/partymgr/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyLookupPartyByName}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "PartyRoleNameDetail")
    @Action(type = ActionType.SET, field = "searchFields", value = "[partyId, firstName, middleName, lastName, groupName]")
    @Action(type = ActionType.SET, field = "andCondition", value = "${groovy: return org.ofbiz.entity.condition.EntityCondition.makeCondition('roleTypeId', 'CUSTOMER')}")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupCustomerName", location = "component://party/widget/partymgr/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupCustomerName", location = "component://party/widget/partymgr/LookupForms.xml"
            )})
        }
    )
    public interface LookupCustomerName {}

    @Screen(name = "LookupCustomerNameForSalesRep", location = "component://party/widget/partymgr/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyLookupPartyByName}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupCustomerNameForSalesRep", location = "component://party/widget/partymgr/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupCustomerNameForSalesRep", location = "component://party/widget/partymgr/LookupForms.xml"
            )})
        }
    )
    public interface LookupCustomerNameForSalesRep {}

    @Screen(name = "LookupPerson", location = "component://party/widget/partymgr/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyLookupPartyPerson}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "entityName", value = "PartyAndPerson")
    @Action(type = ActionType.SET, field = "searchFields", value = "[partyId, firstName, middleName, lastName]")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/StatusCondition.groovy")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupPerson", location = "component://party/widget/partymgr/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupPerson", location = "component://party/widget/partymgr/LookupForms.xml"
            )})
        }
    )
    public interface LookupPerson {}

    @Screen(name = "LookupContact", location = "component://party/widget/partymgr/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyLookupContact}")
    @Action(type = ActionType.SET, field = "entityName", value = "PartyRoleAndPartyDetail")
    @Action(type = ActionType.SET, field = "parameters.roleTypeId", value = "CONTACT")
    @Action(type = ActionType.SET, field = "searchFields", value = "[partyId, firstName, middleName, lastName]")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/StatusCondition.groovy")
    @Action(type = ActionType.SET, field = "andCondition", value = "${groovy: return org.ofbiz.entity.condition.EntityCondition.makeCondition([                      org.ofbiz.entity.condition.EntityCondition.makeCondition('roleTypeId', 'CONTACT')])}")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupPerson", location = "component://party/widget/partymgr/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupPerson", location = "component://party/widget/partymgr/LookupForms.xml"
            )})
        }
    )
    public interface LookupContact {}

    @Screen(name = "LookupLead", location = "component://party/widget/partymgr/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyLookupLead}")
    @Action(type = ActionType.SET, field = "entityName", value = "PartyRoleAndPartyDetail")
    @Action(type = ActionType.SET, field = "parameters.roleTypeId", value = "LEAD")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupPerson", location = "component://party/widget/partymgr/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupPerson", location = "component://party/widget/partymgr/LookupForms.xml"
            )})
        }
    )
    public interface LookupLead {}

    @Screen(name = "LookupPartyAndUserLoginAndPerson", location = "component://party/widget/partymgr/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyLookupPartyAndUserLoginAndPerson}")
    @Action(type = ActionType.SET, field = "entityName", value = "PartyAndUserLoginAndPerson")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "searchFields", value = "[userLoginId, partyId, firstName, lastName]")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/StatusCondition.groovy")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupPartyAndUserLoginAndPerson", location = "component://party/widget/partymgr/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupPartyAndUserLoginAndPerson", location = "component://party/widget/partymgr/LookupForms.xml"
            )})
        }
    )
    public interface LookupPartyAndUserLoginAndPerson {}

    @Screen(name = "LookupUserLoginAndPartyDetails", location = "component://party/widget/partymgr/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyLookupUserLoginAndPartyDetails}")
    @Action(type = ActionType.SET, field = "entityName", value = "UserLoginAndPartyDetails")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "searchFields", value = "[userLoginId, partyId, firstName, lastName, groupName]")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/StatusCondition.groovy")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupUserLoginAndPartyDetails", location = "component://party/widget/partymgr/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupUserLoginAndPartyDetails", location = "component://party/widget/partymgr/LookupForms.xml"
            )})
        }
    )
    public interface LookupUserLoginAndPartyDetails {}

    @Screen(name = "LookupPartyGroup", location = "component://party/widget/partymgr/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyLookupPartyGroup}")
    @Action(type = ActionType.SET, field = "entityName", value = "PartyAndGroup")
    @Action(type = ActionType.SET, field = "searchFields", value = "[partyId, groupName, comments]")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/StatusCondition.groovy")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupPartyGroup", location = "component://party/widget/partymgr/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupPartyGroup", location = "component://party/widget/partymgr/LookupForms.xml"
            )})
        }
    )
    public interface LookupPartyGroup {}

    @Screen(name = "LookupAccount", location = "component://party/widget/partymgr/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyLookupAccount}")
    @Action(type = ActionType.SET, field = "entityName", value = "PartyRoleAndPartyDetail")
    @Action(type = ActionType.SET, field = "searchFields", value = "[partyId, groupName, firstName, lastName]")
    @Action(type = ActionType.SET, field = "conditionFields.roleTypeId", value = "ACCOUNT")
    @Action(type = ActionType.SET, field = "parameters.roleTypeId", value = "ACCOUNT")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupPartyGroup", location = "component://party/widget/partymgr/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupPartyGroup", location = "component://party/widget/partymgr/LookupForms.xml"
            )})
        }
    )
    public interface LookupAccount {}

    @Screen(name = "LookupPartyClassificationGroup", location = "component://party/widget/partymgr/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyLookupPartyClassificationGroup}")
    @Action(type = ActionType.SET, field = "entityName", value = "PartyClassificationGroup")
    @Action(type = ActionType.SET, field = "searchFields", value = "[partyClassificationGroupId, parentGroupId, description]")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupPartyClassificationGroup", location = "component://party/widget/partymgr/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupPartyClassificationGroup", location = "component://party/widget/partymgr/LookupForms.xml"
            )})
        }
    )
    public interface LookupPartyClassificationGroup {}

    @Screen(name = "LookupCommEvent", location = "component://party/widget/partymgr/LookupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "entityName", value = "CommunicationEvent")
    @Action(type = ActionType.SET, field = "searchFields", value = "[communicationEventId, subject]")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyLookupPartyCommEvent}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ContactList")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupCommEvent", location = "component://party/widget/partymgr/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListLookupCommEvent", location = "component://party/widget/partymgr/LookupForms.xml"
            )})
        }
    )
    public interface LookupCommEvent {}

    @Screen(name = "LookupContactMech", location = "component://party/widget/partymgr/LookupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyLookupContactMech}")
    @Action(type = ActionType.SET, field = "entityName", value = "PartyAndContactMech")
    @Action(type = ActionType.SET, field = "searchFields", value = "[contactMechId, partyId, contactMechTypeId, infoString, paToName]")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/StatusCondition.groovy")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupContactMech", location = "component://party/widget/partymgr/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupContactMech", location = "component://party/widget/partymgr/LookupForms.xml"
            )})
        }
    )
    public interface LookupContactMech {}

    @Screen(name = "LookupInternalOrganization", location = "component://party/widget/partymgr/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyLookupInternalOrganization}")
    @Action(type = ActionType.SET, field = "entityName", value = "PartyRoleAndPartyDetail")
    @Action(type = ActionType.SET, field = "roleTypeId", value = "INTERNAL_ORGANIZATIO")
    @Action(type = ActionType.SET, field = "searchFields", value = "[partyId, groupName, partyGroupComments]")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/StatusCondition.groovy")
    @Action(type = ActionType.SET, field = "andCondition", value = "${groovy: return org.ofbiz.entity.condition.EntityCondition.makeCondition([context.andCondition,                      org.ofbiz.entity.condition.EntityCondition.makeCondition('roleTypeId', 'INTERNAL_ORGANIZATIO')])}")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupInternalOrganization", location = "component://party/widget/partymgr/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupPartyGroup", location = "component://party/widget/partymgr/LookupForms.xml"
            )})
        }
    )
    public interface LookupInternalOrganization {}

}
