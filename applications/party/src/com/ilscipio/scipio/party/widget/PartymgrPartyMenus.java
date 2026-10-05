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
public class PartymgrPartyMenus {

    @Menu(
        name = "PartyAppBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        title = "${uiLabelMap.Party}",
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "find", title = "${uiLabelMap.PartyParties}", link = @MenuLink(target = "findparty")),
            @MenuItem(name = "mycomm", title = "${uiLabelMap.PartyMyCommunications}", link = @MenuLink(target = "MyCommunicationEvents")),
            @MenuItem(name = "comm", title = "${uiLabelMap.PartyCommunications}", link = @MenuLink(target = "FindCommunicationEvents")),
            @MenuItem(name = "visits", title = "${uiLabelMap.PartyVisits}", link = @MenuLink(target = "findVisits")),
            @MenuItem(name = "loggedinusers", title = "${uiLabelMap.PartyLoggedInUsers}", link = @MenuLink(target = "listLoggedInUsers")),
            @MenuItem(name = "security", title = "${uiLabelMap.CommonSecurity}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}), link = @MenuLink(target = "FindSecurityGroup")),
            @MenuItem(name = "partyinv", title = "${uiLabelMap.PartyInvitation}", link = @MenuLink(target = "partyInvitation")),
            @MenuItem(name = "importexport", title = "${uiLabelMap.CommonImportExport}", link = @MenuLink(target = "ImportExport"))
        }
    )
    public interface PartyAppBar {}

    @Menu(
        name = "PartyAppSideBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        title = "${uiLabelMap.PartyManager}",
        extendsMenu = "CommonAppSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "PartyAppBar")
        },
        alwaysExpandSelectedOrAncestor = "true",
        items = {
            @MenuItem(name = "find", subMenus = {@SubMenu(name = "Profile", include = "component://party/widget/partymgr/PartyMenus.xml#ProfileSideBar")}),
            @MenuItem(name = "comm", subMenus = {@SubMenu(name = "CommEvent", include = "component://party/widget/partymgr/PartyMenus.xml#CommEventSideBar")}),
            @MenuItem(name = "security", subMenus = {@SubMenu(name = "SecurityGroup", include = "component://common/widget/SecurityMenus.xml#SecurityGroupSideBar")}),
            @MenuItem(name = "partyinv", subMenus = {@SubMenu(name = "PartyInvitation", include = "component://party/widget/partymgr/PartyMenus.xml#PartyInvitationSideBar")})
        }
    )
    public interface PartyAppSideBar {}

    @Menu(
        name = "ProfileTabBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "viewprofile",
        items = {
            @MenuItem(name = "viewprofile", title = "${uiLabelMap.PartyProfile}", sortMode = "off", condition = @MenuItemCondition(conditions = {@Condition(type = True.class, params = {"specificPartyItems"})}), link = @MenuLink(target = "viewprofile", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "viewroles", title = "${uiLabelMap.PartyRoles}", condition = @MenuItemCondition(conditions = {@Condition(type = True.class, params = {"specificPartyItems"})}), link = @MenuLink(target = "viewroles", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "viewidentifications", title = "${uiLabelMap.PartyPartyIdentifications}", condition = @MenuItemCondition(conditions = {@Condition(type = True.class, params = {"specificPartyItems"})}), link = @MenuLink(target = "viewidentifications", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "EditPartyRelationships", title = "${uiLabelMap.PartyRelationships}", condition = @MenuItemCondition(conditions = {@Condition(type = True.class, params = {"specificPartyItems"})}), link = @MenuLink(target = "EditPartyRelationships", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "viewvendor", title = "${uiLabelMap.PartyVendor}", condition = @MenuItemCondition(conditions = {@Condition(type = True.class, params = {"specificPartyItems"})}), link = @MenuLink(target = "viewvendor", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "EditPartyTaxAuthInfos", title = "${uiLabelMap.PartyTaxAuthInfos}", condition = @MenuItemCondition(conditions = {@Condition(type = True.class, params = {"specificPartyItems"})}), link = @MenuLink(target = "EditPartyTaxAuthInfos", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "EditPartyRates", title = "${uiLabelMap.CommonRates}", condition = @MenuItemCondition(conditions = {@Condition(type = True.class, params = {"specificPartyItems"})}), link = @MenuLink(target = "EditPartyRates", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "editShoppingList", title = "${uiLabelMap.PartyShoppingLists}", condition = @MenuItemCondition(conditions = {@Condition(type = True.class, params = {"specificPartyItems"})}), link = @MenuLink(target = "editShoppingList", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "PartyContents", title = "${uiLabelMap.PartyContent}", condition = @MenuItemCondition(conditions = {@Condition(type = True.class, params = {"specificPartyItems"})}), link = @MenuLink(target = "EditPartyContents", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "FinancialHistory", title = "${uiLabelMap.PartyFinancialHistory}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = True.class, params = {"specificPartyItems"}), @Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}), link = @MenuLink(target = "PartyFinancialHistory", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "PartyGeoLocation", title = "${uiLabelMap.CommonGeoLocation}", condition = @MenuItemCondition(conditions = {@Condition(type = True.class, params = {"specificPartyItems"})}), link = @MenuLink(target = "PartyGeoLocation", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "createNew", title = "${uiLabelMap.AccountingBillingAccount}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}), link = @MenuLink(target = "/accounting/control/FindBillingAccount", urlMode = UrlMode.INTER_APP, parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "PartyCommEvents", title = "${uiLabelMap.PartyCommunications}", condition = @MenuItemCondition(conditions = {@Condition(type = True.class, params = {"specificPartyItems"})}), link = @MenuLink(target = "ListPartyCommEvents", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "findRequest", title = "${uiLabelMap.PartyPartyRequests}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = True.class, params = {"specificPartyItems"}), @Condition(type = HasPermission.class, params = {"ORDERMGR", "_VIEW"})}), link = @MenuLink(target = "/ordermgr/control/FindRequest", urlMode = UrlMode.INTER_APP, parameters = {@MenuParameter(paramName = "lookupFlag", value = "Y"), @MenuParameter(paramName = "fromPartyId", fromField = "partyId"), @MenuParameter(paramName = "externaLoginKey", fromField = "externalLoginKey")})),
            @MenuItem(name = "findQuote", title = "${uiLabelMap.OrderOrderQuotes}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = True.class, params = {"specificPartyItems"}), @Condition(type = HasPermission.class, params = {"ORDERMGR", "_VIEW"})}), link = @MenuLink(target = "/ordermgr/control/FindQuote", urlMode = UrlMode.INTER_APP, parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId"), @MenuParameter(paramName = "externalLoginKey", fromField = "externalLoginKey")})),
            @MenuItem(name = "searchOrder", title = "${uiLabelMap.OrderOrders}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = True.class, params = {"specificPartyItems"}), @Condition(type = HasPermission.class, params = {"ORDERMGR", "_VIEW"})}), link = @MenuLink(target = "/ordermgr/control/searchorders", linkType = LinkType.HIDDEN_FORM, urlMode = UrlMode.INTER_APP, parameters = {@MenuParameter(paramName = "lookupFlag", value = "Y"), @MenuParameter(paramName = "hideFields", value = "Y"), @MenuParameter(paramName = "partyId", fromField = "partyId"), @MenuParameter(paramName = "externalLoginKey", fromField = "externalLoginKey"), @MenuParameter(paramName = "viewIndex", value = "1"), @MenuParameter(paramName = "viewSize", value = "20")})),
            @MenuItem(name = "ContactList", title = "${uiLabelMap.PartyContactLists}", condition = @MenuItemCondition(conditions = {@Condition(type = True.class, params = {"specificPartyItems"})}), link = @MenuLink(target = "ListPartyContactLists", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")}))
        }
    )
    public interface ProfileTabBar {}

    @Menu(
        name = "ProfileSideBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ProfileTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "viewprofile"
    )
    public interface ProfileSideBar {}

    @Menu(
        name = "ProfileSubTabBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "updatePerson", title = "${uiLabelMap.CommonEdit}", widgetStyle = "+${styles.action_nav} ${styles.action_update}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"lookupPerson"})}), link = @MenuLink(target = "editperson", parameters = {@MenuParameter(paramName = "partyId", fromField = "parameters.partyId")})),
            @MenuItem(name = "updateGroup", title = "${uiLabelMap.CommonEdit}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"lookupGroup"})}), link = @MenuLink(target = "editpartygroup", parameters = {@MenuParameter(paramName = "partyId", fromField = "party.partyId")})),
            @MenuItem(name = "createUserLogin", title = "${uiLabelMap.CreateUserLogin}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"PARTYMGR", "_CREATE"})}), link = @MenuLink(target = "ProfileCreateNewLogin", parameters = {@MenuParameter(paramName = "partyId", fromField = "parameters.partyId")})),
            @MenuItem(name = "EditUserLoginSecurityGroups", title = "${uiLabelMap.SecurityGroups}", widgetStyle = "+${styles.action_nav} ${styles.action_update}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"PARTYMGR", "_CREATE"})}), link = @MenuLink(target = "EditUserLoginSecurityGroups", parameters = {@MenuParameter(paramName = "userLoginId", fromField = "parameters.partyId")})),
            @MenuItem(name = "createIdentification", title = "${uiLabelMap.CommonNew} ${uiLabelMap.PartyPartyIdentification}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "viewidentifications", parameters = {@MenuParameter(paramName = "partyId", fromField = "parameters.partyId")})),
            @MenuItem(name = "editcontactmech", title = "${uiLabelMap.PartyCreateNewContact}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"PARTYMGR", "_CREATE"})}), link = @MenuLink(target = "editcontactmech", parameters = {@MenuParameter(paramName = "partyId", fromField = "parameters.partyId")})),
            @MenuItem(name = "createNewEftAccount", title = "${uiLabelMap.AccountingCreateNewEftAccount}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(mode = "omit", or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifHasPermission = {@com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "PAY_INFO", action = "_CREATE"), @com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "ACCOUNTING", action = "_CREATE")})}), link = @MenuLink(target = "editeftaccount", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "createNewGiftCard", title = "${uiLabelMap.AccountingCreateNewGiftCard}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(mode = "omit", or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifHasPermission = {@com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "PAY_INFO", action = "_CREATE"), @com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "ACCOUNTING", action = "_CREATE")})}), link = @MenuLink(target = "editgiftcard", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "createNewCreditCard", title = "${uiLabelMap.AccountingCreateNewCreditCard}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(mode = "omit", or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifHasPermission = {@com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "PAY_INFO", action = "_CREATE"), @com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "ACCOUNTING", action = "_CREATE")})}), link = @MenuLink(target = "editcreditcard", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "newQuote", title = "${uiLabelMap.OrderNewQuote}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"ORDERMGR", "_CREATE"})}), link = @MenuLink(target = "/ordermgr/control/EditQuote", urlMode = UrlMode.INTER_APP, parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId"), @MenuParameter(paramName = "externaLoginKey", fromField = "externalLoginKey")})),
            @MenuItem(name = "newOrder", title = "${uiLabelMap.OrderNewOrder}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"ORDERMGR", "_CREATE"})}), link = @MenuLink(target = "/ordermgr/control/checkinits", urlMode = UrlMode.INTER_APP, parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId"), @MenuParameter(paramName = "externaLoginKey", fromField = "externalLoginKey")}))
        }
    )
    public interface ProfileSubTabBar {}

    @Menu(
        name = "create-new-party",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        menuContainerStyle = "+basic-nav",
        items = {
            @MenuItem(name = "create-party-group", title = "${uiLabelMap.PartyCreateNewPartyGroup}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "editpartygroup", parameters = {@MenuParameter(paramName = "create_new", value = "Y"), @MenuParameter(paramName = "groupName", fromField = "parameters.groupName")})),
            @MenuItem(name = "create-person", title = "${uiLabelMap.PartyCreateNewPerson}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "editperson", parameters = {@MenuParameter(paramName = "create_new", value = "Y"), @MenuParameter(paramName = "lastName", fromField = "parameters.lastName"), @MenuParameter(paramName = "firstName", fromField = "parameters.firstName")})),
            @MenuItem(name = "create-customer", title = "${uiLabelMap.PartyCreateNewCustomer}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "NewCustomer")),
            @MenuItem(name = "create-prospect", title = "${uiLabelMap.PartyCreateNewProspect}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "NewProspect")),
            @MenuItem(name = "create-employee", title = "${uiLabelMap.PartyCreateNewEmployee}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "NewEmployee"))
        }
    )
    public interface create_new_party {}

    @Menu(
        name = "NewPartySubTabBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "create-new-party")
        }
    )
    public interface NewPartySubTabBar {}

    @Menu(
        name = "NewPartyButtonDropdown",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        title = "${uiLabelMap.CommonNew}",
        titleStyle = "+${styles.action_nav} ${styles.action_add}",
        extendsMenu = "CommonButtonDropdownMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "create-new-party")
        }
    )
    public interface NewPartyButtonDropdown {}

    @Menu(
        name = "PartyInvitationTabBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditPartyInvitation", title = "${uiLabelMap.PartyInvitation}", link = @MenuLink(target = "editPartyInvitation", parameters = {@MenuParameter(paramName = "partyInvitationId", fromField = "partyInvitationId")})),
            @MenuItem(name = "PartyInvitationGroupAssocs", title = "${uiLabelMap.PartyInvitationGroupAssoc}", link = @MenuLink(target = "PartyInvitationGroupAssocs", parameters = {@MenuParameter(paramName = "partyInvitationId", fromField = "partyInvitationId")})),
            @MenuItem(name = "PartyInvitationRoleAssocs", title = "${uiLabelMap.PartyInvitationRoleAssoc}", link = @MenuLink(target = "PartyInvitationRoleAssocs", parameters = {@MenuParameter(paramName = "partyInvitationId", fromField = "partyInvitationId")}))
        }
    )
    public interface PartyInvitationTabBar {}

    @Menu(
        name = "PartyInvitationSideBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "PartyInvitationTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface PartyInvitationSideBar {}

    @Menu(
        name = "PartyInvitationSubTabBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "editPartyInvitation", title = "${uiLabelMap.PartyInvitationNewPartyInvitation}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "editPartyInvitation")),
            @MenuItem(name = "invitationNewOrder", title = "${uiLabelMap.PartyInvitationNewOrder}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "/ordermgr/control/orderentry", urlMode = UrlMode.INTER_APP, parameters = {@MenuParameter(paramName = "partyId", fromField = "partyInvitation.partyIdFrom")}))
        }
    )
    public interface PartyInvitationSubTabBar {}

    @Menu(
        name = "addShipper",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        items = {
            @MenuItem(name = "new", title = "${uiLabelMap.CommonCreateNew}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"PARTYMGR", "_CREATE"})}), link = @MenuLink(target = "editCarrierAccount", parameters = {@MenuParameter(paramName = "partyId", fromField = "party.partyId")}))
        }
    )
    public interface addShipper {}

    @Menu(
        name = "communicationsMenu",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        items = {
            @MenuItem(name = "newEmail", title = "${uiLabelMap.PartyNewEmail}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"PARTYMGR_CME-EMAIL_CREATE"})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "communicationEventTypeId", value = "EMAIL_COMMUNICATION"), @MenuParameter(paramName = "partyIdFrom", fromField = "partyId"), @MenuParameter(paramName = "my", value = "My"), @MenuParameter(paramName = "statusId", value = "COM_PENDING"), @MenuParameter(paramName = "form", value = "new"), @MenuParameter(paramName = "portalPageId", fromField = "parameters.portalPageId")})),
            @MenuItem(name = "newNote", title = "${uiLabelMap.PartyNewInternalNote}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"PARTYMGR_CME-NOTE_CREATE"})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "communicationEventTypeId", value = "COMMENT_NOTE"), @MenuParameter(paramName = "partyIdFrom", fromField = "partyId"), @MenuParameter(paramName = "my", value = "My"), @MenuParameter(paramName = "statusId", value = "COM_PENDING"), @MenuParameter(paramName = "form", value = "new"), @MenuParameter(paramName = "portalPageId", fromField = "parameters.portalPageId")})),
            @MenuItem(name = "openEvents", title = "${uiLabelMap.PartyOpenEvents}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"parameters.all", "equals", "true"})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "portalPageId", fromField = "parameters.portalPageId"), @MenuParameter(paramName = "all", value = "false")})),
            @MenuItem(name = "allOtherEvents", title = "${uiLabelMap.PartyAllEvents}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifEmpty = {"parameters.all"}, ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "parameters.all", operator = "equals", value = "false")})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "portalPageId", fromField = "parameters.portalPageId"), @MenuParameter(paramName = "all", value = "true")}))
        }
    )
    public interface communicationsMenu {}

    @Menu(
        name = "MyCommSubTabBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        actions = @MenuActions(script = {@ScriptAction(location = "component://party/webapp/partymgr/WEB-INF/actions/communication/GetMyCommunicationEventRole.groovy")}),
        items = {
            @MenuItem(name = "newEmail", title = "${uiLabelMap.PartyNewEmail}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"PARTYMGR_CME-EMAIL_CREATE"}), @Condition(type = Empty.class, params = {"communicationEvent"})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "communicationEventTypeId", value = "EMAIL_COMMUNICATION"), @MenuParameter(paramName = "partyIdFrom", fromField = "parameters.userLogin.partyId"), @MenuParameter(paramName = "my", value = "My"), @MenuParameter(paramName = "statusId", value = "COM_PENDING"), @MenuParameter(paramName = "form", value = "new"), @MenuParameter(paramName = "portalPageId", fromField = "parameters.portalPageId")})),
            @MenuItem(name = "newInternalNote", title = "${uiLabelMap.PartyNewInternalNote}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"PARTYMGR_CME-NOTE_CREATE"}), @Condition(type = Empty.class, params = {"communicationEvent"})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "communicationEventTypeId", value = "COMMENT_NOTE"), @MenuParameter(paramName = "partyIdFrom", fromField = "parameters.userLogin.partyId"), @MenuParameter(paramName = "my", value = "My"), @MenuParameter(paramName = "statusId", value = "COM_PENDING"), @MenuParameter(paramName = "form", value = "new"), @MenuParameter(paramName = "portalPageId", fromField = "parameters.portalPageId")})),
            @MenuItem(name = "reply", title = "${uiLabelMap.PartyReply}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = NotEmpty.class, params = {"communicationEvent"}), @Condition(type = NotEmpty.class, params = {"communicationEvent.partyIdFrom"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "not-equals", "COM_PENDING"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "not-equals", "COM_UNKNOWN_PARTY"}), @Condition(type = Compare.class, params = {"communicationEvent.partyIdFrom", "not-equals", "${partyId}"}), @Condition(type = HasPermission.class, params = {"PARTYMGR_CME-EMAIL_UPDATE"}), @Condition(type = HasPermission.class, params = {"PARTYMGR_CME-NOTE_CREATE"})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "parentCommEventId", fromField = "communicationEvent.communicationEventId"), @MenuParameter(paramName = "my", value = "My"), @MenuParameter(paramName = "statusId", value = "COM_PENDING"), @MenuParameter(paramName = "partyIdFrom", fromField = "parameters.partyId"), @MenuParameter(paramName = "action", value = "REPLY"), @MenuParameter(paramName = "form", value = "new"), @MenuParameter(paramName = "portalPageId", fromField = "parameters.portalPageId")})),
            @MenuItem(name = "replyAll", title = "${uiLabelMap.PartyReplyAll}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = NotEmpty.class, params = {"communicationEvent"}), @Condition(type = NotEmpty.class, params = {"communicationEvent.partyIdFrom"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "not-equals", "COM_PENDING"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "not-equals", "COM_UNKNOWN_PARTY"}), @Condition(type = Compare.class, params = {"communicationEvent.partyIdFrom", "not-equals", "${partyId}"}), @Condition(type = HasPermission.class, params = {"PARTYMGR_CME-EMAIL_UPDATE"}), @Condition(type = HasPermission.class, params = {"PARTYMGR_CME-NOTE_CREATE"})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "parentCommEventId", fromField = "communicationEvent.communicationEventId"), @MenuParameter(paramName = "my", value = "My"), @MenuParameter(paramName = "statusId", value = "COM_PENDING"), @MenuParameter(paramName = "partyIdFrom", fromField = "parameters.partyId"), @MenuParameter(paramName = "action", value = "REPLYALL"), @MenuParameter(paramName = "form", value = "new"), @MenuParameter(paramName = "portalPageId", fromField = "parameters.portalPageId")})),
            @MenuItem(name = "forward", title = "${uiLabelMap.PartyForward}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = NotEmpty.class, params = {"communicationEvent"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "not-equals", "COM_PENDING"}), @Condition(type = HasPermission.class, params = {"PARTYMGR_CME-EMAIL_UPDATE"}), @Condition(type = HasPermission.class, params = {"PARTYMGR_CME-NOTE_CREATE"})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "communicationEventTypeId", fromField = "communicationEvent.communicationEventTypeId"), @MenuParameter(paramName = "origCommEventId", fromField = "parameters.communicationEventId"), @MenuParameter(paramName = "partyIdFrom", fromField = "parameters.partyId"), @MenuParameter(paramName = "my", value = "My"), @MenuParameter(paramName = "statusId", value = "COM_PENDING"), @MenuParameter(paramName = "action", value = "FORWARD"), @MenuParameter(paramName = "form", value = "new"), @MenuParameter(paramName = "portalPageId", fromField = "parameters.portalPageId")})),
            @MenuItem(name = "createRequestFromCommEvent", title = "${uiLabelMap.PartyCreateRequestFromCommEvent}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = NotEmpty.class, params = {"communicationEvent"}), @Condition(type = NotEmpty.class, params = {"communicationEvent.partyIdFrom"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "not-equals", "COM_PENDING"}), @Condition(type = HasPermission.class, params = {"ORDERMGR_CRQ_CREATE"}), @Condition(type = Compare.class, params = {"projectMgrExists", "equals", "false"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "equals", "COM_ENTERED"})}), link = @MenuLink(target = "editRequestFromCommEvent", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "communicationEvent.communicationEventId"), @MenuParameter(paramName = "my", fromField = "parameters.my")})),
            @MenuItem(name = "createRequestFromCommEvent1", title = "${uiLabelMap.PartyCreateRequestFromCommEvent}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = NotEmpty.class, params = {"communicationEvent"}), @Condition(type = NotEmpty.class, params = {"communicationEvent.partyIdFrom"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "not-equals", "COM_PENDING"}), @Condition(type = HasPermission.class, params = {"ORDERMGR_CRQ_CREATE"}), @Condition(type = Compare.class, params = {"projectMgrExists", "equals", "true"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "equals", "COM_ENTERED"})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "communicationEvent.communicationEventId"), @MenuParameter(paramName = "my", fromField = "parameters.my"), @MenuParameter(paramName = "form", value = "request"), @MenuParameter(paramName = "portalPageId", fromField = "parameters.portalPageId")})),
            @MenuItem(name = "createSalesOpportunity", title = "${uiLabelMap.PartyCommEventCreateOpportunity}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"communicationEvent"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "not-equals", "COM_PENDING"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "equals", "COM_ENTERED"})}), link = @MenuLink(target = "/crm/control/EditSalesOpportunity", urlMode = UrlMode.INTER_APP, parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "parameters.communicationEventId")})),
            @MenuItem(name = "close", title = "${uiLabelMap.CommonClose}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = NotEmpty.class, params = {"communicationEvent"}), @Condition(type = Compare.class, params = {"communicationEventRole.statusId", "equals", "COM_ROLE_READ"}), @Condition(type = HasPermission.class, params = {"PARTYMGR_CME-EMAIL_UPDATE"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "not-equals", "COM_UNKNOWN_PARTY"})}), link = @MenuLink(target = "setCommunicationEventRoleStatus", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "parameters.communicationEventId"), @MenuParameter(paramName = "partyId", fromField = "parameters.partyId"), @MenuParameter(paramName = "roleTypeId", fromField = "parameters.roleTypeId"), @MenuParameter(paramName = "statusId", value = "COM_ROLE_COMPLETED"), @MenuParameter(paramName = "portalPageId", fromField = "parameters.portalPageId")})),
            @MenuItem(name = "delete", title = "${uiLabelMap.CommonDelete}", condition = @MenuItemCondition(mode = "omit", or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifHasPermission = {@com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "PARTYMGR_CME-EMAIL_DELETE"), @com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "PARTYMGR_ADMIN")})}, conditions = {@Condition(type = NotEmpty.class, params = {"communicationEvent"})}), link = @MenuLink(target = "deleteCommunicationEvent", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "communicationEvent.communicationEventId"), @MenuParameter(paramName = "form", value = "list"), @MenuParameter(paramName = "portalPageId", fromField = "parameters.portalPageId")}))
        }
    )
    public interface MyCommSubTabBar {}

    @Menu(
        name = "CommEventTabBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        actions = @MenuActions(script = {@ScriptAction(location = "component://party/webapp/party/WEB-INF/actions/generated/CommEventTabBar_script1.groovy")}),
        items = {
            @MenuItem(name = "OverView", title = "${uiLabelMap.CommonOverview}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"CommEventTabBar_communicationEvent"})}), link = @MenuLink(target = "ViewCommunicationEvent", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "parameters.communicationEventId")})),
            @MenuItem(name = "CommunicationEvent", title = "${uiLabelMap.PartyCommEvent}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"CommEventTabBar_communicationEvent"})}), link = @MenuLink(target = "EditCommunicationEvent", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "parameters.communicationEventId")})),
            @MenuItem(name = "UpdateCommPurposes", title = "${uiLabelMap.PartyEventPurpose}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"CommEventTabBar_communicationEvent"})}), link = @MenuLink(target = "UpdateCommPurposes", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "parameters.communicationEventId")})),
            @MenuItem(name = "UpdateCommRoles", title = "${uiLabelMap.PartyRoles}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"CommEventTabBar_communicationEvent"})}), link = @MenuLink(target = "UpdateCommRoles", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "parameters.communicationEventId")})),
            @MenuItem(name = "UpdateCommWorkEfforts", title = "${uiLabelMap.PartyCommWorkEfforts}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"CommEventTabBar_communicationEvent"})}), link = @MenuLink(target = "ListCommWorkEfforts", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "parameters.communicationEventId")})),
            @MenuItem(name = "CommContent", title = "${uiLabelMap.CommonContent}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"CommEventTabBar_communicationEvent"})}), link = @MenuLink(target = "ListCommContent", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "parameters.communicationEventId")})),
            @MenuItem(name = "ListUnknownPartyComms", title = "${uiLabelMap.PartyEmailFromUnknownParties}", link = @MenuLink(target = "listUnknownPartyComms", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "parameters.communicationEventId")})),
            @MenuItem(name = "FindCommunicationByOrder", title = "${uiLabelMap.PartyFindCommunicationsByOrder}", link = @MenuLink(target = "FindCommunicationByOrder", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "parameters.communicationEventId")})),
            @MenuItem(name = "UpdateCommOrders", title = "${uiLabelMap.OrderOrders}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"CommEventTabBar_communicationEvent"})}), link = @MenuLink(target = "UpdateCommOrders", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "parameters.communicationEventId")})),
            @MenuItem(name = "UpdateCommProducts", title = "${uiLabelMap.ProductProducts}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"CommEventTabBar_communicationEvent"})}), link = @MenuLink(target = "UpdateCommProducts", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "parameters.communicationEventId")})),
            @MenuItem(name = "ContactList", title = "${uiLabelMap.PartyContactLists}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"partyId"})}), link = @MenuLink(target = "ListPartyContactLists", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")}))
        }
    )
    public interface CommEventTabBar {}

    @Menu(
        name = "CommEventSideBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "CommEventTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface CommEventSideBar {}

    @Menu(
        name = "CommSubTabBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "new", title = "${uiLabelMap.PartyNewCommunication}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditCommunicationEvent")),
            @MenuItem(name = "edit", title = "${uiLabelMap.CommonEdit}", widgetStyle = "+${styles.action_nav} ${styles.action_update}", link = @MenuLink(target = "EditCommunicationEvent", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "parameters.communicationEventId")})),
            @MenuItem(name = "reply", title = "${uiLabelMap.PartyReply}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"parameters.communicationEventId"}), @Condition(type = NotEmpty.class, params = {"communicationEvent.partyIdFrom"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "not-equals", "COM_UNKNOWN_PARTY"})}), link = @MenuLink(target = "EditCommunicationEvent", parameters = {@MenuParameter(paramName = "parentCommEventId", fromField = "parameters.communicationEventId"), @MenuParameter(paramName = "partyIdFrom", fromField = "parameters.partyId"), @MenuParameter(paramName = "action", value = "REPLY")})),
            @MenuItem(name = "forward", title = "${uiLabelMap.PartyForward}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"parameters.communicationEventId"}), @Condition(type = NotEmpty.class, params = {"communicationEvent.partyIdFrom"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "not-equals", "COM_UNKNOWN_PARTY"})}), link = @MenuLink(target = "EditCommunicationEvent", parameters = {@MenuParameter(paramName = "origCommEventId", fromField = "parameters.communicationEventId"), @MenuParameter(paramName = "action", value = "FORWARD")})),
            @MenuItem(name = "createRequestFromCommEvent", title = "${uiLabelMap.PartyCreateRequestFromCommEvent}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"parameters.communicationEventId"}), @Condition(type = NotEmpty.class, params = {"communicationEvent.partyIdFrom"}), @Condition(type = HasPermission.class, params = {"ORDERMGR_CRQ_CREATE"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "equals", "COM_ENTERED"})}), link = @MenuLink(target = "editRequestFromCommEvent", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "parameters.communicationEventId")})),
            @MenuItem(name = "createSalesOpportunity", title = "${uiLabelMap.PartyCommEventCreateOpportunity}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"communicationEvent"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "not-equals", "COM_PENDING"}), @Condition(type = Compare.class, params = {"communicationEvent.statusId", "equals", "COM_ENTERED"})}), link = @MenuLink(target = "/crm/control/EditSalesOpportunity", urlMode = UrlMode.INTER_APP, parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "parameters.communicationEventId")})),
            @MenuItem(name = "delete", title = "${uiLabelMap.CommonDelete}", condition = @MenuItemCondition(mode = "omit", or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifHasPermission = {@com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "PARTYMGR_CME-EMAIL_DELETE"), @com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "PARTYMGR_ADMIN")})}, conditions = {@Condition(type = NotEmpty.class, params = {"communicationEventRole"})}), link = @MenuLink(target = "RemoveCommunicationEventRole", parameters = {@MenuParameter(paramName = "communicationEventId", fromField = "parameters.communicationEventId"), @MenuParameter(paramName = "roleTypeId", fromField = "communicationEventRole.roleTypeId"), @MenuParameter(paramName = "deleteCommEventIfLast", value = "Y"), @MenuParameter(paramName = "delContentDataResource", value = "Y")}))
        }
    )
    public interface CommSubTabBar {}

    @Menu(
        name = "RelContactAccountsSubTabBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "add", title = "${uiLabelMap.CommonNew}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId"), @MenuParameter(paramName = "portalPageId", fromField = "portalPageId"), @MenuParameter(paramName = "editPartyRel", value = "Y")}))
        }
    )
    public interface RelContactAccountsSubTabBar {}

    @Menu(
        name = "PartyClassificationTabBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditPartyClassificationGroup", title = "${uiLabelMap.PartyClassificationGroups}", link = @MenuLink(target = "EditPartyClassificationGroup", parameters = {@MenuParameter(paramName = "partyClassificationGroupId", fromField = "partyClassificationGroupId")})),
            @MenuItem(name = "EditPartyClassificationGroupParties", title = "${uiLabelMap.PartyParties}", link = @MenuLink(target = "EditPartyClassificationGroupParties", parameters = {@MenuParameter(paramName = "partyClassificationGroupId", fromField = "partyClassificationGroupId")}))
        }
    )
    public interface PartyClassificationTabBar {}

    @Menu(
        name = "PartyClassificationSideBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "PartyClassificationTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface PartyClassificationSideBar {}

    @Menu(
        name = "PartyContentAddEditSubTabBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "NewPartyContent", title = "${uiLabelMap.CommonNew}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"content"})}), link = @MenuLink(target = "EditPartyContents", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")}))
        }
    )
    public interface PartyContentAddEditSubTabBar {}

    @Menu(
        name = "SalesOpportunitiesSubTabBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "add", title = "${uiLabelMap.CommonNew}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditSalesOpportunity", parameters = {@MenuParameter(paramName = "leadPartyId", fromField = "partyId")}))
        }
    )
    public interface SalesOpportunitiesSubTabBar {}

    @Menu(
        name = "CommunicationSubTabBar",
        location = "component://party/widget/partymgr/PartyMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "new", title = "${uiLabelMap.PartyNewCommunication}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditCommunicationEvent", parameters = {@MenuParameter(paramName = "partyIdTo", fromField = "partyId")}))
        }
    )
    public interface CommunicationSubTabBar {}

}
