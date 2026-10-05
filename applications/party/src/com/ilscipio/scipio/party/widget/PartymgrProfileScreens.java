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
public class PartymgrProfileScreens {

    @Screen(name = "Party", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "parameters.partyId", fromField = "parameters.partyId", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "parameters.partyId")})
    @Action(type = ActionType.ENTITY_AND, entityName = "PartyNameHistory", list = "partyNameHistoryList", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "parameters.partyId")}, orderBy = {"-changeDate"})
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyAndGroup", valueField = "lookupGroup", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "parameters.partyId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyAndPerson", valueField = "lookupPerson", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "parameters.partyId")})
    @Action(type = ActionType.ENTITY_AND, entityName = "PartyContent", list = "partyContentList", filterByDate = true, fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "parameters.partyId")}, orderBy = {"-fromDate"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"lookupPerson"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "lookupParty", fromField = "lookupPerson")}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ScipioPartyInfo")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"lookupGroup"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "lookupParty", fromField = "lookupGroup")}), widgets = @Widgets(sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"PARTYMGR", "_GRP_UPDATE"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ScipioPartyInfo")}))}))
    public interface Party {}

    @Screen(name = "Contact", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/GetContactMechs.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/GetUserLoginPrimaryEmail.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"contactMeches"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/profileblocks/Contact.ftl")}))
    public interface Contact {}

    @Screen(name = "PartyIdentifications", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_AND, entityName = "PartyIdentificationAndParty", list = "listIt", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId")})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"listIt"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PartyPartyIdentifications}", includeForms = {@IncludeForm(name = "listPartyIdentification", location = "component://party/widget/partymgr/PartyForms.xml")})}))
    public interface PartyIdentifications {}

    @Screen(name = "PaymentMethods", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/GetPaymentMethods.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"paymentMethodValueMaps"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/profileblocks/PaymentMethods.ftl")}))
    public interface PaymentMethods {}

    @Screen(name = "Attributes", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @Action(type = ActionType.ENTITY_AND, entityName = "PartyAttribute", list = "attributes", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId")})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/profileblocks/Attributes.ftl")}))
    public interface Attributes {}

    @Screen(name = "Cart", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/GetCurrentCart.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/profileblocks/Cart.ftl")}))
    public interface Cart {}

    @Screen(name = "LoyaltyPoints", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/GetLoyaltyPoints.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Compare.class, params = {"totalSubRemainingAmount", "equals", "0"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/profileblocks/LoyaltyPoints.ftl")}))
    public interface LoyaltyPoints {}

    @Screen(name = "UserLogin", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @Action(type = ActionType.ENTITY_AND, entityName = "UserLogin", list = "userLogins", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId")})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"userLogins"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/profileblocks/UserLogin.ftl")}))
    public interface UserLogin {}

    @Screen(name = "Visits", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.ENTITY_AND, entityName = "Visit", list = "visits", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId")}, orderBy = {"-fromDate"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"visits"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/profileblocks/Visits.ftl")}))
    public interface Visits {}

    @Screen(name = "FinAccounts", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "FinAccount", list = "ownedFinAccountList", filterByDate = true, conditions = {@ConditionExpr(fieldName = "ownerPartyId", operator = "equals", fromField = "parameters.partyId")}, orderBy = {"-fromDate"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = NotEmpty.class, params = {"ownedFinAccountList"}), @Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PageTitleFinancialAccountSummary}", name = "fin-account-summary", containers = {@Container(id = "apply-service-credit", includeForms = {@IncludeForm(name = "ApplyServiceCredit", location = "component://party/widget/partymgr/PartyForms.xml", position = 1)}, labels = {@Label(text = "${uiLabelMap.AccountingApplyServiceCredit}", style = "heading", position = 0)})}, widgets = {@Widget(type = WidgetType.ITERATE_SECTION, list = "ownedFinAccountList", entry = "ownedFinAccount", viewSize = 3, paginateTarget = "viewprofile", name = "FinAccounts-iterate1", location = "component://party/widget/partymgr/ProfileScreens.xml")})}))
    public interface FinAccounts {}

    @Screen(name = "FinAccounts-iterate1", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "FinAccountTrans", list = "ownedFinAccountTransList", conditions = {@ConditionExpr(fieldName = "finAccountId", fromField = "ownedFinAccount.finAccountId")}, orderBy = {"-transactionDate"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "FinAccountAuth", list = "ownedFinAccountAuthList", filterByDate = true, conditions = {@ConditionExpr(fieldName = "finAccountId", fromField = "ownedFinAccount.finAccountId")}, orderBy = {"-authorizationDate"})
    @Action(type = ActionType.ENTITY_ONE, entityName = "Uom", valueField = "accountCurrencyUom", autoFieldMap = false, useCache = true, fieldMaps = {@FieldMap(fieldName = "uomId", fromField = "ownedFinAccount.currencyUomId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "StatusItem", valueField = "finAccountStatusItem", fieldMaps = {@FieldMap(fieldName = "statusId", fromField = "ownedFinAccount.statusId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "FinAccountType", valueField = "ownedFinAccountType", autoFieldMap = false, useCache = true, fieldMaps = {@FieldMap(fieldName = "finAccountTypeId", fromField = "ownedFinAccount.finAccountTypeId")})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/profileblocks/FinAccounts.ftl")}))
    public interface FinAccounts_iterate1 {}

    @Screen(name = "SerializedInventory", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "InventoryItem", list = "inventoryItemList", conditions = {@ConditionExpr(fieldName = "inventoryItemTypeId", operator = "equals", value = "SERIALIZED_INV_ITEM"), @ConditionExpr(fieldName = "ownerPartyId", operator = "equals", fromField = "parameters.partyId")}, orderBy = {"-createdStamp"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"inventoryItemList"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/profileblocks/SerializedInventory.ftl")}))
    public interface SerializedInventory {}

    @Screen(name = "Subscriptions", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Subscription", list = "subscriptionList", filterByDate = true, conditions = {@ConditionExpr(fieldName = "partyId", operator = "equals", fromField = "parameters.partyId")}, orderBy = {"-fromDate"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"subscriptionList"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ProductSubscriptions}", name = "subscription-summary", collapsible = true, includeForms = {@IncludeForm(name = "ListSubscriptions", location = "component://party/widget/partymgr/PartyForms.xml")})}))
    public interface Subscriptions {}

    @Screen(name = "Content", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PartyContentType", list = "partyContentTypes", orderBy = {"description"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "MimeType", list = "mimeTypes", orderBy = {"description", "mimeTypeId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "RoleType", list = "roles", orderBy = {"description", "roleTypeId"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/profileblocks/Content.ftl")}))
    public interface Content {}

    @Screen(name = "ContentList", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_AND, entityName = "PartyContent", list = "partyContent", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId")})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyIdPermissionCheck", "VIEW"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/profileblocks/ContentList.ftl")}), failWidgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonPermissionError}", style = "common-msg-error-perm")}))
    public interface ContentList {}

    @Screen(name = "Notes", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.ENTITY_AND, entityName = "PartyNoteView", list = "notes", fieldMaps = {@FieldMap(fieldName = "targetPartyId", fromField = "partyId")}, orderBy = {"-noteDateTime"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/profileblocks/Notes.ftl")}))
    public interface Notes {}

    @Screen(name = "ShipperAccount", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PartyShipperAccount}", includeForms = {@IncludeForm(name = "ListCarrierAccounts", location = "component://party/widget/partymgr/PartyForms.xml", position = 1)}, includeMenus = {@IncludeMenu(name = "addShipper", location = "component://party/widget/partymgr/PartyMenus.xml", position = 0)})}))
    public interface ShipperAccount {}

    @Screen(name = "contactsAndAccounts", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "partyRelContacts"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "partyRelAccounts")}))
    public interface contactsAndAccounts {}

    @Screen(name = "partyRelContacts", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @Action(type = ActionType.ENTITY_AND, entityName = "PartyRelationship", list = "contacts", filterByDate = true, fieldMaps = {@FieldMap(fieldName = "partyIdFrom", fromField = "parameters.partyId"), @FieldMap(fieldName = "roleTypeIdFrom", value = "ACCOUNT"), @FieldMap(fieldName = "roleTypeIdTo", value = "CONTACT")}, orderBy = {"partyIdTo"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"party.partyTypeId", "equals", "PARTY_GROUP"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PartyListRelatedContacts}", includeForms = {@IncludeForm(name = "ListRelatedContacts", location = "component://party/widget/partymgr/PartyForms.xml", position = 2)}, includeMenus = {@IncludeMenu(name = "RelContactAccountsSubTabBar", location = "component://party/widget/partymgr/PartyMenus.xml", position = 0)}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"parameters.editPartyRel"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "AddContact", location = "component://party/widget/partymgr/PartyForms.xml")}), position = 1)})}))
    public interface partyRelContacts {}

    @Screen(name = "partyRelAccounts", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_AND, entityName = "PartyRelationship", list = "accounts", filterByDate = true, fieldMaps = {@FieldMap(fieldName = "partyIdTo", fromField = "parameters.partyId"), @FieldMap(fieldName = "roleTypeIdFrom", value = "ACCOUNT"), @FieldMap(fieldName = "roleTypeIdTo", value = "CONTACT")}, orderBy = {"partyIdFrom"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"party.partyTypeId", "equals", "PERSON"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PartyListRelatedAccounts}", includeForms = {@IncludeForm(name = "ListRelatedAccounts", location = "component://party/widget/partymgr/PartyForms.xml", position = 2)}, includeMenus = {@IncludeMenu(name = "RelContactAccountsSubTabBar", location = "component://party/widget/partymgr/PartyMenus.xml", position = 0)}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"parameters.editPartyRel"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "AddAccount", location = "component://party/widget/partymgr/PartyForms.xml")}), position = 1)})}))
    public interface partyRelAccounts {}

    @Screen(name = "mytasks", location = "component://party/widget/partymgr/ProfileScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = CompareField.class, params = {"parameters.partyId", "equals", "userLogin.partyId"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SERVICE, serviceName = "getWorkEffortAssignedActivities")
    @Action(type = ActionType.SERVICE, serviceName = "getWorkEffortAssignedActivitiesByRole")
    @Action(type = ActionType.SERVICE, serviceName = "getWorkEffortAssignedActivitiesByGroup")
    @Action(type = ActionType.SERVICE, serviceName = "getWorkEffortAssignedTasks")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/profileblocks/mytasks.ftl")}))
    public interface mytasks {}

    @Screen(name = "PartySalesOpportunities", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_AND, entityName = "SalesOpportunityAndRole", list = "salesOpportunities", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId")}, orderBy = {"salesOpportunityId"})
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderOpportunities}", includeForms = {@IncludeForm(name = "PartySalesOpportunities", location = "component://party/widget/partymgr/PartyForms.xml", position = 1)}, includeMenus = {@IncludeMenu(name = "SalesOpportunitiesSubTabBar", location = "component://party/widget/partymgr/PartyMenus.xml", position = 0)})}))
    public interface PartySalesOpportunities {}

    @Screen(name = "ProductStores", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductStoreRole", list = "productStoreRoles", filterByDate = true, fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId")}, orderBy = {"-fromDate"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/profileblocks/ProductStores.ftl")}))
    public interface ProductStores {}

    @Screen(name = "ScipioPartyInfo", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyIcsAvsOverride", valueField = "avsOverride", fieldMaps = {@FieldMap(fieldName = "partyId")})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"partyId"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/profileblocks/ScipioPartyInfo.ftl")}))
    public interface ScipioPartyInfo {}

    @Screen(name = "ScipioListUserCommunications", location = "component://party/widget/partymgr/ProfileScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PartyCommunications}", includeForms = {@IncludeForm(name = "ListPartyCommEvents", location = "component://party/widget/partymgr/CommunicationEventForms.xml", position = 1)}, includeMenus = {@IncludeMenu(name = "CommunicationSubTabBar", location = "component://party/widget/partymgr/PartyMenus.xml", position = 0)})}))
    public interface ScipioListUserCommunications {}

}
