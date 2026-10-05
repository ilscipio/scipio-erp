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
package com.ilscipio.scipio.workeffort.controller;

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Responses;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.webapp.event.TestEvent;

/**
 * Auto-generated annotation-based controller definitions.
 *
 * <p>Generated from controller.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ControllerDef {
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "main",
        type = "screen",
        page = "component://workeffort/widget/CommonScreens.xml#main",
        controller = "workeffort"
    )
    public static final String VIEW_MAIN = "main";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "RequestList",
        type = "screen",
        page = "component://workeffort/widget/CustRequestScreens.xml#RequestList",
        controller = "workeffort"
    )
    public static final String VIEW_REQUESTLIST = "RequestList";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "mytasks",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortScreens.xml#mytasks",
        controller = "workeffort"
    )
    public static final String VIEW_MYTASKS = "mytasks";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "UserJobs",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortScreens.xml#UserJobs",
        controller = "workeffort"
    )
    public static final String VIEW_USERJOBS = "UserJobs";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "calendar",
        type = "screen",
        page = "component://workeffort/widget/CalendarScreens.xml#CalendarWithDecorator",
        controller = "workeffort"
    )
    public static final String VIEW_CALENDAR = "calendar";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "WorkEffortSummary",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortRelatedSummaryScreens.xml#WorkEffortSummary",
        controller = "workeffort"
    )
    public static final String VIEW_WORKEFFORTSUMMARY = "WorkEffortSummary";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindWorkEffort",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortScreens.xml#FindWorkEffort",
        controller = "workeffort"
    )
    public static final String VIEW_FINDWORKEFFORT = "FindWorkEffort";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditWorkEffort",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortScreens.xml#EditWorkEffort",
        controller = "workeffort"
    )
    public static final String VIEW_EDITWORKEFFORT = "EditWorkEffort";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ListWorkEfforts",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortScreens.xml#ListWorkEfforts",
        controller = "workeffort"
    )
    public static final String VIEW_LISTWORKEFFORTS = "ListWorkEfforts";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ChildWorkEfforts",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortScreens.xml#ChildWorkEfforts",
        controller = "workeffort"
    )
    public static final String VIEW_CHILDWORKEFFORTS = "ChildWorkEfforts";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "AddWorkEffortAndAssoc",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortScreens.xml#AddWorkEffortAndAssoc",
        controller = "workeffort"
    )
    public static final String VIEW_ADDWORKEFFORTANDASSOC = "AddWorkEffortAndAssoc";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditWorkEffortAndAssoc",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortScreens.xml#EditWorkEffortAndAssoc",
        controller = "workeffort"
    )
    public static final String VIEW_EDITWORKEFFORTANDASSOC = "EditWorkEffortAndAssoc";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditWorkEffortAssoc",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortScreens.xml#EditWorkEffortAssoc",
        controller = "workeffort"
    )
    public static final String VIEW_EDITWORKEFFORTASSOC = "EditWorkEffortAssoc";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "AddWorkEffortAssoc",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortScreens.xml#AddWorkEffortAssoc",
        controller = "workeffort"
    )
    public static final String VIEW_ADDWORKEFFORTASSOC = "AddWorkEffortAssoc";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ListWorkEffortEventReminders",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortScreens.xml#ListWorkEffortEventReminders",
        controller = "workeffort"
    )
    public static final String VIEW_LISTWORKEFFORTEVENTREMINDERS = "ListWorkEffortEventReminders";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ListWorkEffortFixedAssetAssigns",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortScreens.xml#ListWorkEffortFixedAssetAssigns",
        controller = "workeffort"
    )
    public static final String VIEW_LISTWORKEFFORTFIXEDASSETASSIGNS = "ListWorkEffortFixedAssetAssigns";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ListWorkEffortPartyAssigns",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortScreens.xml#ListWorkEffortPartyAssigns",
        controller = "workeffort"
    )
    public static final String VIEW_LISTWORKEFFORTPARTYASSIGNS = "ListWorkEffortPartyAssigns";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditWorkEffortRates",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortScreens.xml#EditWorkEffortRates",
        controller = "workeffort"
    )
    public static final String VIEW_EDITWORKEFFORTRATES = "EditWorkEffortRates";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ListWorkEffortCommEvents",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortScreens.xml#ListWorkEffortCommEvents",
        controller = "workeffort"
    )
    public static final String VIEW_LISTWORKEFFORTCOMMEVENTS = "ListWorkEffortCommEvents";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ListWorkEffortShopLists",
        type = "screen",
        page = "component://workeffort/widget/WorkEffortScreens.xml#ListWorkEffortShopLists",
        controller = "workeffort"
    )
    public static final String VIEW_LISTWORKEFFORTSHOPLISTS = "ListWorkEffortShopLists";


    // Auto-generated split (Part 2)
    public static class Part2 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListWorkEffortRequests",
            type = "screen",
            page = "component://workeffort/widget/WorkEffortScreens.xml#ListWorkEffortRequests",
            controller = "workeffort"
        )
        public static final String VIEW_LISTWORKEFFORTREQUESTS = "ListWorkEffortRequests";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListWorkEffortRequirements",
            type = "screen",
            page = "component://workeffort/widget/WorkEffortScreens.xml#ListWorkEffortRequirements",
            controller = "workeffort"
        )
        public static final String VIEW_LISTWORKEFFORTREQUIREMENTS = "ListWorkEffortRequirements";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListWorkEffortQuotes",
            type = "screen",
            page = "component://workeffort/widget/WorkEffortScreens.xml#ListWorkEffortQuotes",
            controller = "workeffort"
        )
        public static final String VIEW_LISTWORKEFFORTQUOTES = "ListWorkEffortQuotes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListWorkEffortOrderHeaders",
            type = "screen",
            page = "component://workeffort/widget/WorkEffortScreens.xml#ListWorkEffortOrderHeaders",
            controller = "workeffort"
        )
        public static final String VIEW_LISTWORKEFFORTORDERHEADERS = "ListWorkEffortOrderHeaders";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditWorkEffortTimeEntries",
            type = "screen",
            page = "component://workeffort/widget/WorkEffortScreens.xml#EditWorkEffortTimeEntries",
            controller = "workeffort"
        )
        public static final String VIEW_EDITWORKEFFORTTIMEENTRIES = "EditWorkEffortTimeEntries";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "MyTimesheets",
            type = "screen",
            page = "component://workeffort/widget/TimesheetScreens.xml#MyTimesheets",
            controller = "workeffort"
        )
        public static final String VIEW_MYTIMESHEETS = "MyTimesheets";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindTimesheet",
            type = "screen",
            page = "component://workeffort/widget/TimesheetScreens.xml#FindTimesheet",
            controller = "workeffort"
        )
        public static final String VIEW_FINDTIMESHEET = "FindTimesheet";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditTimesheet",
            type = "screen",
            page = "component://workeffort/widget/TimesheetScreens.xml#EditTimesheet",
            controller = "workeffort"
        )
        public static final String VIEW_EDITTIMESHEET = "EditTimesheet";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditTimesheetRoles",
            type = "screen",
            page = "component://workeffort/widget/TimesheetScreens.xml#EditTimesheetRoles",
            controller = "workeffort"
        )
        public static final String VIEW_EDITTIMESHEETROLES = "EditTimesheetRoles";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditTimesheetEntries",
            type = "screen",
            page = "component://workeffort/widget/TimesheetScreens.xml#EditTimesheetEntries",
            controller = "workeffort"
        )
        public static final String VIEW_EDITTIMESHEETENTRIES = "EditTimesheetEntries";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditWorkEffortNotes",
            type = "screen",
            page = "component://workeffort/widget/WorkEffortScreens.xml#EditWorkEffortNotes",
            controller = "workeffort"
        )
        public static final String VIEW_EDITWORKEFFORTNOTES = "EditWorkEffortNotes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditWorkEffortContents",
            type = "screen",
            page = "component://workeffort/widget/WorkEffortScreens.xml#EditWorkEffortContents",
            controller = "workeffort"
        )
        public static final String VIEW_EDITWORKEFFORTCONTENTS = "EditWorkEffortContents";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditWorkEffortGoodStandards",
            type = "screen",
            page = "component://workeffort/widget/WorkEffortScreens.xml#EditWorkEffortGoodStandards",
            controller = "workeffort"
        )
        public static final String VIEW_EDITWORKEFFORTGOODSTANDARDS = "EditWorkEffortGoodStandards";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditWorkEffortReviews",
            type = "screen",
            page = "component://workeffort/widget/WorkEffortScreens.xml#EditWorkEffortReviews",
            controller = "workeffort"
        )
        public static final String VIEW_EDITWORKEFFORTREVIEWS = "EditWorkEffortReviews";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditWorkEffortKeywords",
            type = "screen",
            page = "component://workeffort/widget/WorkEffortScreens.xml#EditWorkEffortKeywords",
            controller = "workeffort"
        )
        public static final String VIEW_EDITWORKEFFORTKEYWORDS = "EditWorkEffortKeywords";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditWorkEffortContactMechs",
            type = "screen",
            page = "component://workeffort/widget/WorkEffortScreens.xml#EditWorkEffortContactMechs",
            controller = "workeffort"
        )
        public static final String VIEW_EDITWORKEFFORTCONTACTMECHS = "EditWorkEffortContactMechs";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WorkEffortSearchOptions",
            type = "screen",
            page = "component://workeffort/widget/WorkEffortScreens.xml#WorkEffortSearchOptions",
            controller = "workeffort"
        )
        public static final String VIEW_WORKEFFORTSEARCHOPTIONS = "WorkEffortSearchOptions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WorkEffortSearchResults",
            type = "screen",
            page = "component://workeffort/widget/WorkEffortScreens.xml#WorkEffortSearchResults",
            controller = "workeffort"
        )
        public static final String VIEW_WORKEFFORTSEARCHRESULTS = "WorkEffortSearchResults";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupWorkEffort",
            type = "screen",
            page = "component://workeffort/widget/LookupScreens.xml#LookupWorkEffort",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPWORKEFFORT = "LookupWorkEffort";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupTimesheet",
            type = "screen",
            page = "component://workeffort/widget/LookupScreens.xml#LookupTimesheet",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPTIMESHEET = "LookupTimesheet";

    }

    // Auto-generated split (Part 3)
    public static class Part3 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyName",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyName",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPPARTYNAME = "LookupPartyName";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPerson",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPerson",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPPERSON = "LookupPerson";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyGroup",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyGroup",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPPARTYGROUP = "LookupPartyGroup";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyAndUserLoginAndPerson",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyAndUserLoginAndPerson",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPPARTYANDUSERLOGINANDPERSON = "LookupPartyAndUserLoginAndPerson";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupCommEvent",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupCommEvent",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPCOMMEVENT = "LookupCommEvent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProduct",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPPRODUCT = "LookupProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupVariantProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupVariantProduct",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPVARIANTPRODUCT = "LookupVariantProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProductFeature",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProductFeature",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPPRODUCTFEATURE = "LookupProductFeature";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupFacility",
            type = "screen",
            page = "component://product/widget/facility/LookupScreens.xml#LookupFacility",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPFACILITY = "LookupFacility";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupFixedAsset",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupFixedAsset",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPFIXEDASSET = "LookupFixedAsset";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupShoppingList",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupShoppingList",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPSHOPPINGLIST = "LookupShoppingList";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupCustRequest",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupCustRequest",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPCUSTREQUEST = "LookupCustRequest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupCustRequestItem",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupCustRequestItem",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPCUSTREQUESTITEM = "LookupCustRequestItem";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupRequirement",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupRequirement",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPREQUIREMENT = "LookupRequirement";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupQuote",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupQuote",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPQUOTE = "LookupQuote";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupQuoteItem",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupQuoteItem",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPQUOTEITEM = "LookupQuoteItem";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupOrderHeader",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupOrderHeader",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPORDERHEADER = "LookupOrderHeader";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupInvoice",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupInvoice",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPINVOICE = "LookupInvoice";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupContent",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#LookupContent",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPCONTENT = "LookupContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupContactMech",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupContactMech",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPCONTACTMECH = "LookupContactMech";

    }

    // Auto-generated split (Part 4)
    public static class Part4 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPreferredContactMech",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#LookupPreferredContactMech",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPPREFERREDCONTACTMECH = "LookupPreferredContactMech";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupContactList",
            type = "screen",
            page = "component://party/widget/partymgr/PartyContactListScreens.xml#ListLookupContactList",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPCONTACTLIST = "LookupContactList";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAgreementWorkEffortApplics",
            type = "screen",
            page = "component://workeffort/widget/WorkEffortScreens.xml#EditAgreementWorkEffortApplics",
            controller = "workeffort"
        )
        public static final String VIEW_EDITAGREEMENTWORKEFFORTAPPLICS = "EditAgreementWorkEffortApplics";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupAgreement",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupAgreement",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPAGREEMENT = "LookupAgreement";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupAgreementItem",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupAgreementItem",
            controller = "workeffort"
        )
        public static final String VIEW_LOOKUPAGREEMENTITEM = "LookupAgreementItem";

        @Request(
            uri = "view",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        public interface View {}

        @Request(
            uri = "chain",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "/view")
        @Response(name = "error", type = "view", value = "error")
        public static String chain(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.webapp.event.TestEvent.test
            return TestEvent.test(request, response);
        }

        @Request(
            uri = "main",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        public interface Main {}

        @Request(
            uri = "requestlist",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestList")
        public interface Requestlist {}

        @Request(
            uri = "mytasks",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "mytasks")
        public interface Mytasks {}

        @Request(
            uri = "UserJobs",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "UserJobs")
        public interface UserJobs {}

        @Request(
            uri = "calendar",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "calendar", saveHomeView = "true")
        public interface Calendar {}

        @Request(
            uri = "WorkEffortSummary",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WorkEffortSummary")
        public interface WorkEffortSummary {}

        @Request(
            uri = "FindWorkEffort",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindWorkEffort")
        public interface FindWorkEffort {}

        @Request(
            uri = "ListWorkEfforts",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEfforts")
        public interface ListWorkEfforts {}

        @Request(
            uri = "ChildWorkEfforts",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ChildWorkEfforts")
        public interface ChildWorkEfforts {}

        @Request(
            uri = "EditWorkEffortAssoc",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortAssoc")
        public interface EditWorkEffortAssoc {}

        @Request(
            uri = "AddWorkEffortAssoc",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddWorkEffortAssoc")
        public interface AddWorkEffortAssoc {}

        @Request(
            uri = "EditWorkEffortAndAssoc",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortAndAssoc")
        public interface EditWorkEffortAndAssoc {}

        @Request(
            uri = "AddWorkEffortAndAssoc",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddWorkEffortAndAssoc")
        public interface AddWorkEffortAndAssoc {}

    }

    // Auto-generated split (Part 5)
    public static class Part5 {
        @Request(
            uri = "EditWorkEffort",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffort")
        public interface EditWorkEffort {}

        @Request(
            uri = "createWorkEffort",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffort")
        @Response(name = "error", type = "view", value = "EditWorkEffort")
        @Event(type = "service", invoke = "createWorkEffort")
        public static String createWorkEffort(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createWorkEffortAndPartyAssign",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-home")
        @Response(name = "error", type = "view-last")
        @Event(type = "service", invoke = "createWorkEffortAndPartyAssign")
        public static String createWorkEffortAndPartyAssign(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createWorkEffortAssoc",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortAssoc")
        @Response(name = "error", type = "view", value = "AddWorkEffortAssoc")
        @Event(type = "service", invoke = "createWorkEffortAssoc")
        public static String createWorkEffortAssoc(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffortAssoc",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortAssoc")
        @Response(name = "error", type = "view", value = "EditWorkEffortAssoc")
        @Event(type = "service", invoke = "updateWorkEffortAssoc")
        public static String updateWorkEffortAssoc(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeWorkEffortAssoc",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "ChildWorkEfforts")
        @Response(name = "error", type = "view", value = "EditWorkEffortAssoc")
        @Event(type = "service", invoke = "removeWorkEffortAssoc")
        public static String removeWorkEffortAssoc(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createWorkEffortAndAssoc",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortAndAssoc")
        @Response(name = "error", type = "view", value = "AddWorkEffortAndAssoc")
        @Event(type = "service", invoke = "createWorkEffortAndAssoc")
        public static String createWorkEffortAndAssoc(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffortAndAssoc",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortAndAssoc")
        @Response(name = "error", type = "view", value = "EditWorkEffortAndAssoc")
        @Event(type = "service", invoke = "updateWorkEffortAndAssoc")
        public static String updateWorkEffortAndAssoc(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffort",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-home", value = "EditWorkEffort")
        @Response(name = "error", type = "view", value = "EditWorkEffort")
        @Event(type = "service", invoke = "updateWorkEffort")
        public static String updateWorkEffort(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffort",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-home", value = "ListWorkEfforts")
        @Response(name = "error", type = "view", value = "ListWorkEfforts")
        @Event(type = "service", invoke = "deleteWorkEffort")
        public static String deleteWorkEffort(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListWorkEffortFixedAssetAssigns",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortFixedAssetAssigns")
        public interface ListWorkEffortFixedAssetAssigns {}

        @Request(
            uri = "createWorkEffortFixedAssetAssign",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortFixedAssetAssigns")
        @Response(name = "error", type = "view", value = "ListWorkEffortFixedAssetAssigns")
        @Event(type = "service", invoke = "createWorkEffortFixedAssetAssign")
        public static String createWorkEffortFixedAssetAssign(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffortFixedAssetAssign",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortFixedAssetAssigns")
        @Response(name = "error", type = "view", value = "ListWorkEffortFixedAssetAssigns")
        @Event(type = "service", invoke = "updateWorkEffortFixedAssetAssign")
        public static String updateWorkEffortFixedAssetAssign(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortFixedAssetAssign",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortFixedAssetAssigns")
        @Response(name = "error", type = "view", value = "ListWorkEffortFixedAssetAssigns")
        @Event(type = "service", invoke = "removeWorkEffortFixedAssetAssign")
        public static String deleteWorkEffortFixedAssetAssign(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListWorkEffortPartyAssigns",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortPartyAssigns")
        public interface ListWorkEffortPartyAssigns {}

        @Request(
            uri = "createWorkEffortPartyAssign",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-home", value = "ListWorkEffortPartyAssigns")
        @Response(name = "error", type = "view-home", value = "ListWorkEffortPartyAssigns")
        @Event(type = "service", invoke = "assignPartyToWorkEffort")
        public static String createWorkEffortPartyAssign(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffortPartyAssign",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortPartyAssigns")
        @Response(name = "error", type = "view", value = "ListWorkEffortPartyAssigns")
        @Event(type = "service", invoke = "updatePartyToWorkEffortAssignment")
        public static String updateWorkEffortPartyAssign(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortPartyAssign",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-home", value = "ListWorkEffortPartyAssigns")
        @Response(name = "error", type = "view-home", value = "ListWorkEffortPartyAssigns")
        @Event(type = "service", invoke = "deletePartyToWorkEffortAssignment")
        public static String deleteWorkEffortPartyAssign(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditWorkEffortRates",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortRates")
        public interface EditWorkEffortRates {}

        @Request(
            uri = "updateWorkEffortRate",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortRates")
        @Response(name = "error", type = "view", value = "EditWorkEffortRates")
        @Event(type = "service", invoke = "updateRateAmount")
        public static String updateWorkEffortRate(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 6)
    public static class Part6 {
        @Request(
            uri = "deleteWorkEffortRate",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortRates")
        @Response(name = "error", type = "view", value = "EditWorkEffortRates")
        @Event(type = "service", invoke = "expireRateAmount")
        public static String deleteWorkEffortRate(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListWorkEffortCommEvents",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortCommEvents")
        public interface ListWorkEffortCommEvents {}

        @Request(
            uri = "createCommunicationEvent",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortCommEvents")
        @Response(name = "error", type = "view", value = "ListWorkEffortCommEvents")
        @Event(type = "service", invoke = "createCommunicationEventWorkEff")
        public static String createCommunicationEvent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createWorkEffortCommEvent",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortCommEvents")
        @Response(name = "error", type = "view", value = "ListWorkEffortCommEvents")
        @Event(type = "service", invoke = "createCommunicationEventWorkEff")
        public static String createWorkEffortCommEvent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCommunicationEventWorkEff",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortCommEvents")
        @Response(name = "error", type = "view", value = "ListWorkEffortCommEvents")
        @Event(type = "service", invoke = "updateCommunicationEventWorkEff")
        public static String updateCommunicationEventWorkEff(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteCommunicationEventWorkEff",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortCommEvents")
        @Response(name = "error", type = "view", value = "ListWorkEffortCommEvents")
        @Event(type = "service", invoke = "deleteCommunicationEventWorkEff")
        public static String deleteCommunicationEventWorkEff(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListWorkEffortRequests",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortRequests")
        public interface ListWorkEffortRequests {}

        @Request(
            uri = "createWorkEffortRequest",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortRequests")
        @Response(name = "error", type = "view", value = "ListWorkEffortRequests")
        @Event(type = "service", invoke = "createWorkEffortRequest")
        public static String createWorkEffortRequest(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortRequest",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortRequests")
        @Response(name = "error", type = "view", value = "ListWorkEffortRequests")
        @Event(type = "service", invoke = "deleteWorkEffortRequest")
        public static String deleteWorkEffortRequest(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createWorkEffortRequestItem",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortRequests")
        @Response(name = "error", type = "view", value = "ListWorkEffortRequests")
        @Event(type = "service", invoke = "createWorkEffortRequestItemAndRequestItem")
        public static String createWorkEffortRequestItem(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortRequestItem",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortRequests")
        @Response(name = "error", type = "view", value = "ListWorkEffortRequests")
        @Event(type = "service", invoke = "deleteWorkEffortRequestItem")
        public static String deleteWorkEffortRequestItem(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListWorkEffortQuotes",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortQuotes")
        public interface ListWorkEffortQuotes {}

        @Request(
            uri = "createWorkEffortQuote",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortQuotes")
        @Response(name = "error", type = "view", value = "ListWorkEffortQuotes")
        @Event(type = "service", invoke = "createWorkEffortQuote")
        public static String createWorkEffortQuote(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortQuote",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortQuotes")
        @Response(name = "error", type = "view", value = "ListWorkEffortQuotes")
        @Event(type = "service", invoke = "deleteWorkEffortQuote")
        public static String deleteWorkEffortQuote(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createWorkEffortQuoteItem",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortQuotes")
        @Response(name = "error", type = "view", value = "ListWorkEffortQuotes")
        @Event(type = "service", invoke = "createQuoteItem")
        public static String createWorkEffortQuoteItem(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortQuoteItem",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortQuotes")
        @Response(name = "error", type = "view", value = "ListWorkEffortQuotes")
        @Event(type = "service", invoke = "removeQuoteItem")
        public static String deleteWorkEffortQuoteItem(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListWorkEffortRequirements",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortRequirements")
        public interface ListWorkEffortRequirements {}

        @Request(
            uri = "createWorkEffortRequirement",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortRequirements")
        @Response(name = "error", type = "view", value = "ListWorkEffortRequirements")
        @Event(type = "service", invoke = "createWorkRequirementFulfillment")
        public static String createWorkEffortRequirement(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortRequirement",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortRequirements")
        @Response(name = "error", type = "view", value = "ListWorkEffortRequirements")
        @Event(type = "service", invoke = "deleteWorkRequirementFulfillment")
        public static String deleteWorkEffortRequirement(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListWorkEffortShopLists",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortShopLists")
        public interface ListWorkEffortShopLists {}

    }

    // Auto-generated split (Part 7)
    public static class Part7 {
        @Request(
            uri = "createShoppingListWorkEffort",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortShopLists")
        @Response(name = "error", type = "view", value = "ListWorkEffortShopLists")
        @Event(type = "service", invoke = "createShoppingListWorkEffort")
        public static String createShoppingListWorkEffort(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteShoppingListWorkEffort",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortShopLists")
        @Response(name = "error", type = "view", value = "ListWorkEffortShopLists")
        @Event(type = "service", invoke = "deleteShoppingListWorkEffort")
        public static String deleteShoppingListWorkEffort(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListWorkEffortOrderHeaders",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortOrderHeaders")
        public interface ListWorkEffortOrderHeaders {}

        @Request(
            uri = "createWorkEffortOrderHeader",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortOrderHeaders")
        @Response(name = "error", type = "view", value = "ListWorkEffortOrderHeaders")
        @Event(type = "service", invoke = "createOrderHeaderWorkEffort")
        public static String createWorkEffortOrderHeader(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortOrderHeader",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortOrderHeaders")
        @Response(name = "error", type = "view", value = "ListWorkEffortOrderHeaders")
        @Event(type = "service", invoke = "deleteOrderHeaderWorkEffort")
        public static String deleteWorkEffortOrderHeader(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditWorkEffortTimeEntries",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortTimeEntries")
        public interface EditWorkEffortTimeEntries {}

        @Request(
            uri = "createWorkEffortTimeEntry",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortTimeEntries")
        @Response(name = "error", type = "view", value = "EditWorkEffortTimeEntries")
        @Event(type = "service", invoke = "createTimeEntry")
        public static String createWorkEffortTimeEntry(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffortTimeEntry",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortTimeEntries")
        @Response(name = "error", type = "view", value = "EditWorkEffortTimeEntries")
        @Event(type = "service", invoke = "updateTimeEntry")
        public static String updateWorkEffortTimeEntry(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortTimeEntry",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortTimeEntries")
        @Response(name = "error", type = "view", value = "EditWorkEffortTimeEntries")
        @Event(type = "service", invoke = "deleteTimeEntry")
        public static String deleteWorkEffortTimeEntry(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addWorkEffortTimeToInvoice",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortTimeEntries")
        @Response(name = "error", type = "view", value = "EditWorkEffortTimeEntries")
        @Event(type = "service", invoke = "addWorkEffortTimeToInvoice")
        public static String addWorkEffortTimeToInvoice(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addWorkEffortTimeToNewInvoice",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortTimeEntries")
        @Response(name = "error", type = "view", value = "EditWorkEffortTimeEntries")
        @Event(type = "service", invoke = "addWorkEffortTimeToNewInvoice")
        public static String addWorkEffortTimeToNewInvoice(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "MyTimesheets",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "MyTimesheets")
        public interface MyTimesheets {}

        @Request(
            uri = "createTimesheetForThisWeek",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "MyTimesheets")
        @Response(name = "error", type = "view", value = "MyTimesheets")
        @Event(type = "service", invoke = "createTimesheetForThisWeek")
        public static String createTimesheetForThisWeek(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createQuickTimeEntry",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "MyTimesheets")
        @Response(name = "error", type = "view", value = "MyTimesheets")
        @Event(type = "service", invoke = "createTimeEntry")
        public static String createQuickTimeEntry(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindTimesheet",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindTimesheet")
        public interface FindTimesheet {}

        @Request(
            uri = "EditTimesheet",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTimesheet")
        public interface EditTimesheet {}

        @Request(
            uri = "createTimesheet",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTimesheet")
        @Response(name = "error", type = "view", value = "EditTimesheet")
        @Event(type = "service", invoke = "createTimesheet")
        public static String createTimesheet(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTimesheet",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTimesheet")
        @Response(name = "error", type = "view", value = "EditTimesheet")
        @Event(type = "service", invoke = "updateTimesheet")
        public static String updateTimesheet(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addTimesheetToInvoice",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTimesheet")
        @Response(name = "error", type = "view", value = "EditTimesheet")
        @Event(type = "service", invoke = "addTimesheetToInvoice")
        public static String addTimesheetToInvoice(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addTimesheetToNewInvoice",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTimesheet")
        @Response(name = "error", type = "view", value = "EditTimesheet")
        @Event(type = "service", invoke = "addTimesheetToNewInvoice")
        public static String addTimesheetToNewInvoice(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 8)
    public static class Part8 {
        @Request(
            uri = "EditTimesheetRoles",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTimesheetRoles")
        public interface EditTimesheetRoles {}

        @Request(
            uri = "createTimesheetRole",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTimesheetRoles")
        @Response(name = "error", type = "view", value = "EditTimesheetRoles")
        @Event(type = "service", invoke = "createTimesheetRole")
        public static String createTimesheetRole(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTimesheetRole",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTimesheetRoles")
        @Response(name = "error", type = "view", value = "EditTimesheetRoles")
        public interface UpdateTimesheetRole {}

        @Request(
            uri = "deleteTimesheetRole",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTimesheetRoles")
        @Response(name = "error", type = "view", value = "EditTimesheetRoles")
        @Event(type = "service", invoke = "deleteTimesheetRole")
        public static String deleteTimesheetRole(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditTimesheetEntries",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTimesheetEntries")
        public interface EditTimesheetEntries {}

        @Request(
            uri = "createTimesheetEntry",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTimesheetEntries")
        @Response(name = "error", type = "view", value = "EditTimesheetEntries")
        @Event(type = "service", invoke = "createTimeEntry")
        public static String createTimesheetEntry(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTimesheetEntry",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTimesheetEntries")
        @Response(name = "error", type = "view", value = "EditTimesheetEntries")
        @Event(type = "service", invoke = "updateTimeEntry")
        public static String updateTimesheetEntry(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteTimesheetEntry",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTimesheetEntries")
        @Response(name = "error", type = "view", value = "EditTimesheetEntries")
        @Event(type = "service", invoke = "deleteTimeEntry")
        public static String deleteTimesheetEntry(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditWorkEffortNotes",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortNotes")
        public interface EditWorkEffortNotes {}

        @Request(
            uri = "createWorkEffortNote",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortNotes")
        @Response(name = "error", type = "view", value = "EditWorkEffortNotes")
        @Event(type = "service", invoke = "createWorkEffortNote")
        public static String createWorkEffortNote(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffortNote",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortNotes")
        @Response(name = "error", type = "view", value = "EditWorkEffortNotes")
        @Event(type = "service", invoke = "updateWorkEffortNote")
        public static String updateWorkEffortNote(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditWorkEffortContents",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortContents")
        public interface EditWorkEffortContents {}

        @Request(
            uri = "createWorkEffortContent",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortContents")
        @Response(name = "error", type = "view", value = "EditWorkEffortContents")
        @Event(type = "service", invoke = "createWorkEffortContent")
        public static String createWorkEffortContent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffortContent",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortContents")
        @Response(name = "error", type = "view", value = "EditWorkEffortContents")
        @Event(type = "service", invoke = "updateWorkEffortContent")
        public static String updateWorkEffortContent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortContent",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortContents")
        @Response(name = "error", type = "view", value = "EditWorkEffortContents")
        @Event(type = "service", invoke = "deleteWorkEffortContent")
        public static String deleteWorkEffortContent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditWorkEffortGoodStandards",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortGoodStandards")
        public interface EditWorkEffortGoodStandards {}

        @Request(
            uri = "createWorkEffortGoodStandard",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortGoodStandards")
        @Response(name = "error", type = "view", value = "EditWorkEffortGoodStandards")
        @Event(type = "service", invoke = "createWorkEffortGoodStandard")
        public static String createWorkEffortGoodStandard(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffortGoodStandard",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortGoodStandards")
        @Response(name = "error", type = "view", value = "EditWorkEffortGoodStandards")
        @Event(type = "service", invoke = "updateWorkEffortGoodStandard")
        public static String updateWorkEffortGoodStandard(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeWorkEffortGoodStandard",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortGoodStandards")
        @Response(name = "error", type = "view", value = "EditWorkEffortGoodStandards")
        @Event(type = "service", invoke = "removeWorkEffortGoodStandard")
        public static String removeWorkEffortGoodStandard(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditWorkEffortReviews",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortReviews")
        public interface EditWorkEffortReviews {}

    }

    // Auto-generated split (Part 9)
    public static class Part9 {
        @Request(
            uri = "createWorkEffortReview",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortReviews")
        @Response(name = "error", type = "view", value = "EditWorkEffortReviews")
        @Event(type = "service", invoke = "createWorkEffortReview")
        public static String createWorkEffortReview(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffortReview",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortReviews")
        @Response(name = "error", type = "view", value = "EditWorkEffortReviews")
        @Event(type = "service", invoke = "updateWorkEffortReview")
        public static String updateWorkEffortReview(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortReview",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortReviews")
        @Response(name = "error", type = "view", value = "EditWorkEffortReviews")
        @Event(type = "service", invoke = "deleteWorkEffortReview")
        public static String deleteWorkEffortReview(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditWorkEffortKeywords",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortKeywords")
        public interface EditWorkEffortKeywords {}

        @Request(
            uri = "createWorkEffortKeyword",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortKeywords")
        @Response(name = "error", type = "view", value = "EditWorkEffortKeywords")
        @Event(type = "service", invoke = "createWorkEffortKeyword")
        public static String createWorkEffortKeyword(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortKeyword",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortKeywords")
        @Response(name = "error", type = "view", value = "EditWorkEffortKeywords")
        @Event(type = "service", invoke = "deleteWorkEffortKeyword")
        public static String deleteWorkEffortKeyword(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createWorkEffortKeywords",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortKeywords")
        @Response(name = "error", type = "view", value = "EditWorkEffortKeywords")
        @Event(type = "service", invoke = "createWorkEffortKeywords")
        public static String createWorkEffortKeywords(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortKeywords",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortKeywords")
        @Response(name = "error", type = "view", value = "EditWorkEffortKeywords")
        @Event(type = "service", invoke = "deleteWorkEffortKeywords")
        public static String deleteWorkEffortKeywords(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditWorkEffortContactMechs",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortContactMechs")
        public interface EditWorkEffortContactMechs {}

        @Request(
            uri = "createWorkEffortContactMech",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortContactMechs")
        @Response(name = "error", type = "view", value = "EditWorkEffortContactMechs")
        @Event(type = "service", invoke = "createWorkEffortContactMech")
        public static String createWorkEffortContactMech(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortContactMech",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffortContactMechs")
        @Response(name = "error", type = "view", value = "EditWorkEffortContactMechs")
        @Event(type = "service", invoke = "deleteWorkEffortContactMech")
        public static String deleteWorkEffortContactMech(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "WorkEffortSearchOptions",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WorkEffortSearchOptions")
        public interface WorkEffortSearchOptions {}

        @Request(
            uri = "WorkEffortSearchResults",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WorkEffortSearchResults")
        public interface WorkEffortSearchResults {}

        @Request(
            uri = "DuplicateWorkEffort",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffort")
        @Response(name = "error", type = "view", value = "EditWorkEffort")
        @Event(type = "service", invoke = "duplicateWorkEffort")
        public static String duplicateWorkEffort(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "LookupWorkEffort",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupWorkEffort")
        public interface LookupWorkEffort {}

        @Request(
            uri = "LookupTimesheet",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupTimesheet")
        public interface LookupTimesheet {}

        @Request(
            uri = "LookupPartyName",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyName")
        public interface LookupPartyName {}

        @Request(
            uri = "LookupPerson",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPerson")
        public interface LookupPerson {}

        @Request(
            uri = "LookupPartyGroup",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyGroup")
        public interface LookupPartyGroup {}

        @Request(
            uri = "LookupPartyAndUserLoginAndPerson",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyAndUserLoginAndPerson")
        public interface LookupPartyAndUserLoginAndPerson {}

    }

    // Auto-generated split (Part 10)
    public static class Part10 {
        @Request(
            uri = "LookupCommEvent",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupCommEvent")
        public interface LookupCommEvent {}

        @Request(
            uri = "LookupContactMech",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupContactMech")
        public interface LookupContactMech {}

        @Request(
            uri = "LookupPreferredContactMech",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPreferredContactMech")
        public interface LookupPreferredContactMech {}

        @Request(
            uri = "LookupContactList",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupContactList")
        public interface LookupContactList {}

        @Request(
            uri = "LookupProduct",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProduct")
        public interface LookupProduct {}

        @Request(
            uri = "LookupVariantProduct",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupVariantProduct")
        public interface LookupVariantProduct {}

        @Request(
            uri = "LookupProductFeature",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductFeature")
        public interface LookupProductFeature {}

        @Request(
            uri = "LookupFacility",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupFacility")
        public interface LookupFacility {}

        @Request(
            uri = "LookupFixedAsset",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupFixedAsset")
        public interface LookupFixedAsset {}

        @Request(
            uri = "LookupShoppingList",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupShoppingList")
        public interface LookupShoppingList {}

        @Request(
            uri = "LookupCustRequest",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupCustRequest")
        public interface LookupCustRequest {}

        @Request(
            uri = "LookupCustRequestItem",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupCustRequestItem")
        public interface LookupCustRequestItem {}

        @Request(
            uri = "LookupRequirement",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupRequirement")
        public interface LookupRequirement {}

        @Request(
            uri = "LookupQuote",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupQuote")
        public interface LookupQuote {}

        @Request(
            uri = "LookupQuoteItem",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupQuoteItem")
        public interface LookupQuoteItem {}

        @Request(
            uri = "LookupOrderHeader",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupOrderHeader")
        public interface LookupOrderHeader {}

        @Request(
            uri = "LookupInvoice",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupInvoice")
        public interface LookupInvoice {}

        @Request(
            uri = "LookupContent",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupContent")
        public interface LookupContent {}

        @Request(
            uri = "LookupAgreement",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupAgreement")
        public interface LookupAgreement {}

        @Request(
            uri = "LookupAgreementItem",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupAgreementItem")
        public interface LookupAgreementItem {}

    }

    // Auto-generated split (Part 11)
    public static class Part11 {
        @Request(
            uri = "EditAgreementWorkEffortApplics",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementWorkEffortApplics")
        public interface EditAgreementWorkEffortApplics {}

        @Request(
            uri = "createAgreementWorkEffortApplic",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementWorkEffortApplics")
        @Response(name = "error", type = "view", value = "EditAgreementWorkEffortApplics")
        @Event(type = "service", invoke = "createAgreementWorkEffortApplic")
        public static String createAgreementWorkEffortApplic(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteAgreementWorkEffortApplic",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementWorkEffortApplics")
        @Response(name = "error", type = "view", value = "EditAgreementWorkEffortApplics")
        @Event(type = "service", invoke = "deleteAgreementWorkEffortApplic")
        public static String deleteAgreementWorkEffortApplic(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createWorkEffortEventReminder",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortEventReminders")
        @Response(name = "error", type = "view", value = "ListWorkEffortEventReminders")
        @Event(type = "service", invoke = "createWorkEffortEventReminder")
        public static String createWorkEffortEventReminder(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffortEventReminder",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortEventReminders")
        @Response(name = "error", type = "view", value = "ListWorkEffortEventReminders")
        @Event(type = "service", invoke = "updateWorkEffortEventReminder")
        public static String updateWorkEffortEventReminder(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortEventReminder",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortEventReminders")
        @Response(name = "error", type = "view", value = "ListWorkEffortEventReminders")
        @Event(type = "service", invoke = "deleteWorkEffortEventReminder")
        public static String deleteWorkEffortEventReminder(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "listWorkEffortEventReminders",
            controller = "workeffort",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEffortEventReminders")
        public interface ListWorkEffortEventReminders {}


    }
}
