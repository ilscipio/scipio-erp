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
public class SfaSfaMenus {

    @Menu(
        name = "SfaAppBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        title = "${uiLabelMap.SfaManager}",
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "Services", title = "${uiLabelMap.SfaServices}", link = @MenuLink(target = "MyCommunicationEvents", parameters = {@MenuParameter(paramName = "noConditionFind", value = "Y")})),
            @MenuItem(name = "Marketing", title = "${uiLabelMap.MarketingMarketing}", link = @MenuLink(target = "FindMarketingCampaign")),
            @MenuItem(name = "Sales", title = "${uiLabelMap.SfaSales}", link = @MenuLink(target = "FindAccounts")),
            @MenuItem(name = "Calendar", title = "${uiLabelMap.SfaCalendar}", link = @MenuLink(target = "Calendar")),
            @MenuItem(name = "Analytics", title = "${uiLabelMap.SfaAnalytics}", link = @MenuLink(target = "AnalyticsSales")),
            @MenuItem(name = "Settings", title = "${uiLabelMap.SfaSettings}", link = @MenuLink(target = "DataSources")),
            @MenuItem(name = "Contacts", title = "${uiLabelMap.SfaContacts}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"_NON_EXISTENT_FIELD_"})}), link = @MenuLink(target = "FindContacts")),
            @MenuItem(name = "Leads", title = "${uiLabelMap.SfaLeads}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"_NON_EXISTENT_FIELD_"})}), link = @MenuLink(target = "FindLeads")),
            @MenuItem(name = "Opportunities", title = "${uiLabelMap.SfaOpportunities}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"_NON_EXISTENT_FIELD_"})}), link = @MenuLink(target = "FindSalesOpportunity"))
        }
    )
    public interface SfaAppBar {}

    @Menu(
        name = "SfaAppSideBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        title = "${uiLabelMap.SfaManager}",
        extendsMenu = "CommonAppSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "SfaAppBar")
        },
        alwaysExpandSelectedOrAncestor = "true",
        items = {
            @MenuItem(name = "Services", subMenus = {@SubMenu(name = "Services", include = "component://marketing/widget/ServicesMenus.xml#ServicesSideBar")}),
            @MenuItem(name = "Marketing", subMenus = {@SubMenu(name = "Marketing", include = "component://marketing/widget/MarketingMenus.xml#MarketingSideBar")}),
            @MenuItem(name = "Sales", subMenus = {@SubMenu(name = "Sales", include = "component://marketing/widget/SalesMenus.xml#SalesSideBar")}),
            @MenuItem(name = "Calendar"),
            @MenuItem(name = "Analytics", subMenus = {@SubMenu(name = "Analytics", include = "component://marketing/widget/AnalyticsMenus.xml#AnalyticsSideBar")}),
            @MenuItem(name = "Settings", subMenus = {@SubMenu(name = "DataSource", include = "component://marketing/widget/DataSourceMenus.xml#DataSourceSideBar")}),
            @MenuItem(name = "Contacts", subMenus = {@SubMenu(name = "Contact", include = "component://marketing/widget/sfa/SfaMenus.xml#ContactSideBar")}),
            @MenuItem(name = "Leads", subMenus = {@SubMenu(name = "Lead", include = "component://marketing/widget/sfa/SfaMenus.xml#LeadSideBar")}),
            @MenuItem(name = "Opportunities", subMenus = {@SubMenu(name = "Opportunity", include = "component://marketing/widget/sfa/SfaMenus.xml#OpportunitySideBar")})
        }
    )
    public interface SfaAppSideBar {}

    @Menu(
        name = "AccountTabBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "find", title = "${uiLabelMap.CommonFind}", link = @MenuLink(target = "FindAccounts")),
            @MenuItem(name = "profile", title = "${uiLabelMap.PartyProfile}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.partyId"})}), link = @MenuLink(target = "viewprofile", parameters = {@MenuParameter(paramName = "partyId", fromField = "parameters.partyId")}))
        }
    )
    public interface AccountTabBar {}

    @Menu(
        name = "AccountSideBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "SfaAppSideBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface AccountSideBar {}

    @Menu(
        name = "AccountSubTabBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2} ${styles.menu_noclear}",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "NewAccounts", title = "${uiLabelMap.PageTitleCreateAccount}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "NewAccount")),
            @MenuItem(name = "ViewSfaCommEvent", title = "${uiLabelMap.PartyCommunications}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"partyId"})}), link = @MenuLink(target = "ListPartyCommEvents", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId"), @MenuParameter(paramName = "activeSubMenuItem", value = "Accounts")}))
        }
    )
    public interface AccountSubTabBar {}

    @Menu(
        name = "AccountFindTabBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "all", title = "${uiLabelMap.SfaAllAccounts}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"parameters.all", "equals", "false"})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "all", value = "true")})),
            @MenuItem(name = "my", title = "${uiLabelMap.SfaMyAccounts}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifEmpty = {"parameters.all"}, ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "parameters.all", operator = "equals", value = "true")})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "all", value = "false")}))
        }
    )
    public interface AccountFindTabBar {}

    @Menu(
        name = "ContactTabBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "find", title = "${uiLabelMap.CommonFind}", link = @MenuLink(target = "FindContacts")),
            @MenuItem(name = "profile", title = "${uiLabelMap.PartyProfile}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.partyId"})}), link = @MenuLink(target = "viewprofile", parameters = {@MenuParameter(paramName = "partyId", fromField = "parameters.partyId")})),
            @MenuItem(name = "MergeContacts", title = "${uiLabelMap.SfaMergeContacts}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.partyId"})}), link = @MenuLink(target = "MergeContacts", parameters = {@MenuParameter(paramName = "partyId", fromField = "parameters.partyId")}))
        }
    )
    public interface ContactTabBar {}

    @Menu(
        name = "ContactSideBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ContactTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface ContactSideBar {}

    @Menu(
        name = "ContactSubTabBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2} ${styles.menu_noclear}",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "NewContact", title = "${uiLabelMap.CommonCreateNew}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "NewContact")),
            @MenuItem(name = "ViewSfaCommEvent", title = "${uiLabelMap.PartyCommunications}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"partyId"})}), link = @MenuLink(target = "ListPartyCommEvents", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId"), @MenuParameter(paramName = "activeSubMenuItem", value = "Contacts")}))
        }
    )
    public interface ContactSubTabBar {}

    @Menu(
        name = "ContactFindTabBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "all", title = "${uiLabelMap.SfaAllContacts}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"parameters.all", "equals", "false"})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "all", value = "true")})),
            @MenuItem(name = "my", title = "${uiLabelMap.SfaMyContacts}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifEmpty = {"parameters.all"}, ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "parameters.all", operator = "equals", value = "true")})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "all", value = "false")}))
        }
    )
    public interface ContactFindTabBar {}

    @Menu(
        name = "EventSideBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "EventTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface EventSideBar {}

    @Menu(
        name = "EventTabBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "find", title = "${uiLabelMap.CommonFind}", link = @MenuLink(target = "Events"))
        }
    )
    public interface EventTabBar {}

    @Menu(
        name = "EventSubTabBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2} ${styles.menu_noclear}",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        actions = @MenuActions(script = {@ScriptAction(location = "component://marketing/webapp/marketing/WEB-INF/actions/generated/EventSubTabBar_script1.groovy")}),
        items = {
            @MenuItem(name = "NewEvent", title = "${uiLabelMap.CommonCreateNew}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = True.class, params = {"isNewEvent"})}), link = @MenuLink(target = "EditEvent")),
            @MenuItem(name = "ViewCalendar", title = "${uiLabelMap.CommonCalendar}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"workEffort"})}), link = @MenuLink(target = "Calendar", parameters = {@MenuParameter(paramName = "period", value = "month"), @MenuParameter(paramName = "startDate", fromField = "targetPeriodStart")})),
            @MenuItem(name = "ViewWorkEffort", title = "${uiLabelMap.WorkEffortWorkEffort}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"workEffort"})}), link = @MenuLink(target = "/workeffort/control/WorkEffortSummary", urlMode = UrlMode.INTER_APP, parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffort.workEffortId")}))
        }
    )
    public interface EventSubTabBar {}

    @Menu(
        name = "LeadTabBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "find", title = "${uiLabelMap.CommonFind}", link = @MenuLink(target = "FindLeads")),
            @MenuItem(name = "profile", title = "${uiLabelMap.PartyProfile}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.partyId"})}), link = @MenuLink(target = "viewprofile", parameters = {@MenuParameter(paramName = "partyId", fromField = "parameters.partyId"), @MenuParameter(paramName = "partyGroupId", value = "${parameters.partyGroupId}")})),
            @MenuItem(name = "ConvertLead", title = "${uiLabelMap.SfaConvertLead}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.partyId"})}), link = @MenuLink(target = "ConvertLead", parameters = {@MenuParameter(paramName = "partyId", fromField = "parameters.partyId"), @MenuParameter(paramName = "partyGroupId", value = "${parameters.partyGroupId}")})),
            @MenuItem(name = "CloneLead", title = "${uiLabelMap.SfaCloneLead}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.partyId"})}), link = @MenuLink(target = "CloneLead", parameters = {@MenuParameter(paramName = "partyId", fromField = "parameters.partyId"), @MenuParameter(paramName = "partyGroupId", value = "${parameters.partyGroupId}")})),
            @MenuItem(name = "MergeLeads", title = "${uiLabelMap.SfaMergeLeads}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.partyId"})}), link = @MenuLink(target = "MergeLeads", parameters = {@MenuParameter(paramName = "partyId", fromField = "parameters.partyId"), @MenuParameter(paramName = "partyGroupId", value = "${parameters.partyGroupId}")}))
        }
    )
    public interface LeadTabBar {}

    @Menu(
        name = "LeadSideBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "LeadTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface LeadSideBar {}

    @Menu(
        name = "LeadSubTabBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2} ${styles.menu_noclear}",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "NewLead", title = "${uiLabelMap.CommonCreateNew}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "NewLead")),
            @MenuItem(name = "NewLeadFromVCard", title = "${uiLabelMap.PageTitleCreateLeadFromVCard}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "NewLeadFromVCard")),
            @MenuItem(name = "ViewSfaCommEvent", title = "${uiLabelMap.PartyCommunications}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"partyId"})}), link = @MenuLink(target = "ListPartyCommEvents", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId"), @MenuParameter(paramName = "activeSubMenuItem", value = "Leads")})),
            @MenuItem(name = "AddRelatedCompany", title = "${uiLabelMap.PageTitleAddRelatedCompany}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"partyId"}), @Condition(type = Empty.class, params = {"relatedCompanies"})}), link = @MenuLink(target = "AddRelatedCompany", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId"), @MenuParameter(paramName = "activeSubMenuItem", value = "Leads")}))
        }
    )
    public interface LeadSubTabBar {}

    @Menu(
        name = "LeadFindTabBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "all", title = "${uiLabelMap.SfaAllLeads}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"parameters.all", "equals", "false"})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "all", value = "true")})),
            @MenuItem(name = "my", title = "${uiLabelMap.SfaMyLeads}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifEmpty = {"parameters.all"}, ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "parameters.all", operator = "equals", value = "true")})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", parameters = {@MenuParameter(paramName = "all", value = "false")}))
        }
    )
    public interface LeadFindTabBar {}

    @Menu(
        name = "OpportunityTabBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "ViewSalesOpportunity", title = "${uiLabelMap.SfaOpportunitySummary}", link = @MenuLink(target = "ViewSalesOpportunity", parameters = {@MenuParameter(paramName = "salesOpportunityId", fromField = "parameters.salesOpportunityId")})),
            @MenuItem(name = "EditSalesOpportunity", title = "${uiLabelMap.SfaEditOpportunity}", link = @MenuLink(target = "EditSalesOpportunity", parameters = {@MenuParameter(paramName = "salesOpportunityId", fromField = "parameters.salesOpportunityId")})),
            @MenuItem(name = "PartyCommEvents", title = "${uiLabelMap.PartyCommunications}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifNotEmpty = {"leadPartyId", "leadParty.leadPartyId", "partyId"})}, conditions = {@Condition(type = NotEmpty.class, params = {"parameters.salesOpportunityId"})}), link = @MenuLink(target = "ListPartyCommEvents", parameters = {@MenuParameter(paramName = "salesOpportunityId", fromField = "parameters.salesOpportunityId")}))
        }
    )
    public interface OpportunityTabBar {}

    @Menu(
        name = "OpportunitySideBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "OpportunityTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface OpportunitySideBar {}

    @Menu(
        name = "OpportunitySubTabBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2} ${styles.menu_noclear}",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "NewCommEvent", title = "${uiLabelMap.PartyNewEmail}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "/partymgr/control/NewDraftCommunicationEvent", linkType = LinkType.HIDDEN_FORM, urlMode = UrlMode.INTER_APP, parameters = {@MenuParameter(paramName = "communicationEventTypeId", value = "EMAIL_COMMUNICATION"), @MenuParameter(paramName = "partyIdFrom", fromField = "userLogin.partyId"), @MenuParameter(paramName = "my", value = "My"), @MenuParameter(paramName = "statusId", value = "COM_PENDING")})),
            @MenuItem(name = "reply", title = "${uiLabelMap.PartyReply}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"communicationEvent"}), @Condition(type = NotEmpty.class, params = {"communicationEvent.partyIdFrom"})}), link = @MenuLink(target = "/partymgr/control/NewDraftCommunicationEvent", linkType = LinkType.HIDDEN_FORM, urlMode = UrlMode.INTER_APP, parameters = {@MenuParameter(paramName = "parentCommEventId", fromField = "parameters.communicationEventId"), @MenuParameter(paramName = "my", value = "My"), @MenuParameter(paramName = "statusId", value = "COM_PENDING"), @MenuParameter(paramName = "partyIdFrom", fromField = "userLogin.partyId"), @MenuParameter(paramName = "action", value = "REPLY")})),
            @MenuItem(name = "replyAll", title = "${uiLabelMap.PartyReplyAll}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"communicationEvent"}), @Condition(type = NotEmpty.class, params = {"communicationEvent.partyIdFrom"})}), link = @MenuLink(target = "/partymgr/control/NewDraftCommunicationEvent", linkType = LinkType.HIDDEN_FORM, urlMode = UrlMode.INTER_APP, parameters = {@MenuParameter(paramName = "parentCommEventId", fromField = "parameters.communicationEventId"), @MenuParameter(paramName = "my", value = "My"), @MenuParameter(paramName = "statusId", value = "COM_PENDING"), @MenuParameter(paramName = "partyIdFrom", fromField = "userLogin.partyId"), @MenuParameter(paramName = "action", value = "REPLYALL")})),
            @MenuItem(name = "forward", title = "${uiLabelMap.PartyForward}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"communicationEvent"})}), link = @MenuLink(target = "/partymgr/control/NewDraftCommunicationEvent", linkType = LinkType.HIDDEN_FORM, urlMode = UrlMode.INTER_APP, parameters = {@MenuParameter(paramName = "communicationEventTypeId", fromField = "communicationEvent.communicationEventTypeId"), @MenuParameter(paramName = "origCommEventId", fromField = "parameters.communicationEventId"), @MenuParameter(paramName = "partyIdFrom", fromField = "parameters.partyId"), @MenuParameter(paramName = "my", value = "My"), @MenuParameter(paramName = "statusId", value = "COM_PENDING"), @MenuParameter(paramName = "action", value = "FORWARD")})),
            @MenuItem(name = "newInternalNote", title = "${uiLabelMap.PartyNewInternalNote}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"PARTYMGR_CME-NOTE_CREATE"}), @Condition(type = Empty.class, params = {"communicationEvent"})}), link = @MenuLink(target = "/partymgr/control/NewDraftCommunicationEvent", linkType = LinkType.HIDDEN_FORM, urlMode = UrlMode.INTER_APP, parameters = {@MenuParameter(paramName = "communicationEventTypeId", value = "COMMENT_NOTE"), @MenuParameter(paramName = "partyIdFrom", fromField = "parameters.userLogin.partyId"), @MenuParameter(paramName = "my", value = "My"), @MenuParameter(paramName = "statusId", value = "COM_PENDING")}))
        }
    )
    public interface OpportunitySubTabBar {}

    @Menu(
        name = "SalesForecastTabBar",
        location = "component://marketing/widget/sfa/SfaMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "NewSalesForecast", title = "${uiLabelMap.SfaNewSalesForecast}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = True.class, params = {"isNewSalesForecast"})}), link = @MenuLink(target = "EditSalesForecast")),
            @MenuItem(name = "EditSalesForecast", title = "${uiLabelMap.SfaSalesForecast}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"salesForecast"})}), link = @MenuLink(target = "EditSalesForecast", parameters = {@MenuParameter(paramName = "salesForecastId", fromField = "parameters.salesForecastId")})),
            @MenuItem(name = "EditSalesForecastDetail", title = "${uiLabelMap.ProductPickingDetail}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"salesForecast"})}), link = @MenuLink(target = "EditSalesForecastDetail", parameters = {@MenuParameter(paramName = "salesForecastId", fromField = "parameters.salesForecastId")}))
        }
    )
    public interface SalesForecastTabBar {}

}
