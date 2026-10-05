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
package com.ilscipio.scipio.marketing.controller;

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Responses;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Auto-generated annotation-based controller definitions.
 *
 * <p>Generated from controller.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class SfaControllerDef {
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "main",
        type = "screen",
        page = "component://marketing/widget/sfa/CommonScreens.xml#main",
        controller = "sfa"
    )
    public static final String VIEW_MAIN = "main";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "viewprofile",
        type = "screen",
        page = "component://marketing/widget/sfa/CommonScreens.xml#ViewProfile",
        controller = "sfa"
    )
    public static final String VIEW_VIEWPROFILE = "viewprofile";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindSalesOpportunity",
        type = "screen",
        page = "component://marketing/widget/sfa/OpportunityScreens.xml#FindSalesOpportunity",
        controller = "sfa"
    )
    public static final String VIEW_FINDSALESOPPORTUNITY = "FindSalesOpportunity";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ViewSalesOpportunity",
        type = "screen",
        page = "component://marketing/widget/sfa/OpportunityScreens.xml#ViewSalesOpportunity",
        controller = "sfa"
    )
    public static final String VIEW_VIEWSALESOPPORTUNITY = "ViewSalesOpportunity";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditSalesOpportunity",
        type = "screen",
        page = "component://marketing/widget/sfa/OpportunityScreens.xml#EditSalesOpportunity",
        controller = "sfa"
    )
    public static final String VIEW_EDITSALESOPPORTUNITY = "EditSalesOpportunity";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindAccounts",
        type = "screen",
        page = "component://marketing/widget/sfa/AccountScreens.xml#FindAccounts",
        controller = "sfa"
    )
    public static final String VIEW_FINDACCOUNTS = "FindAccounts";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "NewAccount",
        type = "screen",
        page = "component://marketing/widget/sfa/AccountScreens.xml#NewAccount",
        controller = "sfa"
    )
    public static final String VIEW_NEWACCOUNT = "NewAccount";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ContactMechTypeOnly",
        type = "screen",
        page = "component://marketing/widget/sfa/AccountScreens.xml#ContactMechTypeOnly",
        controller = "sfa"
    )
    public static final String VIEW_CONTACTMECHTYPEONLY = "ContactMechTypeOnly";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindLeads",
        type = "screen",
        page = "component://marketing/widget/sfa/LeadScreens.xml#FindLeads",
        controller = "sfa"
    )
    public static final String VIEW_FINDLEADS = "FindLeads";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "NewLead",
        type = "screen",
        page = "component://marketing/widget/sfa/LeadScreens.xml#NewLead",
        controller = "sfa"
    )
    public static final String VIEW_NEWLEAD = "NewLead";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "CloneLead",
        type = "screen",
        page = "component://marketing/widget/sfa/LeadScreens.xml#CloneLead",
        controller = "sfa"
    )
    public static final String VIEW_CLONELEAD = "CloneLead";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ConvertLead",
        type = "screen",
        page = "component://marketing/widget/sfa/LeadScreens.xml#ConvertLead",
        controller = "sfa"
    )
    public static final String VIEW_CONVERTLEAD = "ConvertLead";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "MergeLeads",
        type = "screen",
        page = "component://marketing/widget/sfa/LeadScreens.xml#MergeLeads",
        controller = "sfa"
    )
    public static final String VIEW_MERGELEADS = "MergeLeads";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "NewLeadFromVCard",
        type = "screen",
        page = "component://marketing/widget/sfa/LeadScreens.xml#NewLeadFromVCard",
        controller = "sfa"
    )
    public static final String VIEW_NEWLEADFROMVCARD = "NewLeadFromVCard";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "AddRelatedCompany",
        type = "screen",
        page = "component://marketing/widget/sfa/LeadScreens.xml#AddRelatedCompany",
        controller = "sfa"
    )
    public static final String VIEW_ADDRELATEDCOMPANY = "AddRelatedCompany";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindContacts",
        type = "screen",
        page = "component://marketing/widget/sfa/ContactScreens.xml#FindContacts",
        controller = "sfa"
    )
    public static final String VIEW_FINDCONTACTS = "FindContacts";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "NewContact",
        type = "screen",
        page = "component://marketing/widget/sfa/ContactScreens.xml#NewContact",
        controller = "sfa"
    )
    public static final String VIEW_NEWCONTACT = "NewContact";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "MergeContacts",
        type = "screen",
        page = "component://marketing/widget/sfa/ContactScreens.xml#MergeContacts",
        controller = "sfa"
    )
    public static final String VIEW_MERGECONTACTS = "MergeContacts";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "NewContactFromVCard",
        type = "screen",
        page = "component://marketing/widget/sfa/ContactScreens.xml#NewContactFromVCard",
        controller = "sfa"
    )
    public static final String VIEW_NEWCONTACTFROMVCARD = "NewContactFromVCard";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ViewPartiesCreatedByVCard",
        type = "screen",
        page = "component://marketing/widget/sfa/ContactScreens.xml#ViewPartiesCreatedByVCard",
        controller = "sfa"
    )
    public static final String VIEW_VIEWPARTIESCREATEDBYVCARD = "ViewPartiesCreatedByVCard";


    // Auto-generated split (Part 2)
    public static class Part2 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindSalesForecast",
            type = "screen",
            page = "component://marketing/widget/sfa/ForecastScreens.xml#FindSalesForecast",
            controller = "sfa"
        )
        public static final String VIEW_FINDSALESFORECAST = "FindSalesForecast";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditSalesForecast",
            type = "screen",
            page = "component://marketing/widget/sfa/ForecastScreens.xml#EditSalesForecast",
            controller = "sfa"
        )
        public static final String VIEW_EDITSALESFORECAST = "EditSalesForecast";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditSalesForecastDetail",
            type = "screen",
            page = "component://marketing/widget/sfa/ForecastScreens.xml#EditSalesForecastDetail",
            controller = "sfa"
        )
        public static final String VIEW_EDITSALESFORECASTDETAIL = "EditSalesForecastDetail";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "Events",
            type = "screen",
            page = "component://marketing/widget/sfa/EventScreens.xml#main",
            controller = "sfa"
        )
        public static final String VIEW_EVENTS = "Events";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEvent",
            type = "screen",
            page = "component://marketing/widget/sfa/EventScreens.xml#EditEvent",
            controller = "sfa"
        )
        public static final String VIEW_EDITEVENT = "EditEvent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupSalesForecast",
            type = "screen",
            page = "component://marketing/widget/LookupScreens.xml#LookupSalesForecast",
            controller = "sfa"
        )
        public static final String VIEW_LOOKUPSALESFORECAST = "LookupSalesForecast";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProduct",
            controller = "sfa"
        )
        public static final String VIEW_LOOKUPPRODUCT = "LookupProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProductCategory",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProductCategory",
            controller = "sfa"
        )
        public static final String VIEW_LOOKUPPRODUCTCATEGORY = "LookupProductCategory";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupLeads",
            type = "screen",
            page = "component://marketing/widget/sfa/LookupScreens.xml#LookupLeads",
            controller = "sfa"
        )
        public static final String VIEW_LOOKUPLEADS = "LookupLeads";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupAccounts",
            type = "screen",
            page = "component://marketing/widget/sfa/LookupScreens.xml#LookupAccounts",
            controller = "sfa"
        )
        public static final String VIEW_LOOKUPACCOUNTS = "LookupAccounts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupAccountLeads",
            type = "screen",
            page = "component://marketing/widget/sfa/LookupScreens.xml#LookupAccountLeads",
            controller = "sfa"
        )
        public static final String VIEW_LOOKUPACCOUNTLEADS = "LookupAccountLeads";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListPartyCommEvents",
            type = "screen",
            page = "component://marketing/widget/sfa/OpportunityScreens.xml#OpportunityCommEvent",
            controller = "sfa"
        )
        public static final String VIEW_LISTPARTYCOMMEVENTS = "ListPartyCommEvents";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "MyCommunicationEvents",
            type = "screen",
            page = "component://marketing/widget/sfa/ServicesScreens.xml#PartyCommunicationEvents",
            controller = "sfa"
        )
        public static final String VIEW_MYCOMMUNICATIONEVENTS = "MyCommunicationEvents";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindMarketingCampaign",
            type = "screen",
            page = "component://marketing/widget/MarketingCampaignScreens.xml#FindMarketingCampaign",
            controller = "sfa"
        )
        public static final String VIEW_FINDMARKETINGCAMPAIGN = "FindMarketingCampaign";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindProductPromo",
            type = "screen",
            page = "component://marketing/widget/PromoScreens.xml#FindProductPromo",
            controller = "sfa"
        )
        public static final String VIEW_FINDPRODUCTPROMO = "FindProductPromo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPromo",
            type = "screen",
            page = "component://marketing/widget/PromoScreens.xml#EditProductPromo",
            controller = "sfa"
        )
        public static final String VIEW_EDITPRODUCTPROMO = "EditProductPromo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPromoRules",
            type = "screen",
            page = "component://marketing/widget/PromoScreens.xml#EditProductPromoRules",
            controller = "sfa"
        )
        public static final String VIEW_EDITPRODUCTPROMORULES = "EditProductPromoRules";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPromoStores",
            type = "screen",
            page = "component://marketing/widget/PromoScreens.xml#EditProductPromoStores",
            controller = "sfa"
        )
        public static final String VIEW_EDITPRODUCTPROMOSTORES = "EditProductPromoStores";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindProductPromoCode",
            type = "screen",
            page = "component://marketing/widget/PromoScreens.xml#FindProductPromoCode",
            controller = "sfa"
        )
        public static final String VIEW_FINDPRODUCTPROMOCODE = "FindProductPromoCode";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPromoCode",
            type = "screen",
            page = "component://marketing/widget/PromoScreens.xml#EditProductPromoCode",
            controller = "sfa"
        )
        public static final String VIEW_EDITPRODUCTPROMOCODE = "EditProductPromoCode";

    }

    // Auto-generated split (Part 3)
    public static class Part3 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPromoContent",
            type = "screen",
            page = "component://marketing/widget/PromoScreens.xml#EditProductPromoContent",
            controller = "sfa"
        )
        public static final String VIEW_EDITPRODUCTPROMOCONTENT = "EditProductPromoContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "Calendar",
            type = "screen",
            page = "component://marketing/widget/sfa/CalendarScreens.xml#CalendarWithDecorator",
            controller = "sfa"
        )
        public static final String VIEW_CALENDAR = "Calendar";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindRequest",
            type = "screen",
            page = "component://marketing/widget/sfa/ServicesScreens.xml#FindRequest",
            controller = "sfa"
        )
        public static final String VIEW_FINDREQUEST = "FindRequest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewRequest",
            type = "screen",
            page = "component://marketing/widget/sfa/ServicesScreens.xml#ViewRequest",
            controller = "sfa"
        )
        public static final String VIEW_VIEWREQUEST = "ViewRequest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditRequest",
            type = "screen",
            page = "component://marketing/widget/sfa/ServicesScreens.xml#EditRequest",
            controller = "sfa"
        )
        public static final String VIEW_EDITREQUEST = "EditRequest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AnalyticsSales",
            type = "screen",
            page = "component://marketing/widget/sfa/AnalyticsScreens.xml#AnalyticsSales",
            controller = "sfa"
        )
        public static final String VIEW_ANALYTICSSALES = "AnalyticsSales";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AnalyticsTracking",
            type = "screen",
            page = "component://marketing/widget/sfa/AnalyticsScreens.xml#AnalyticsTracking",
            controller = "sfa"
        )
        public static final String VIEW_ANALYTICSTRACKING = "AnalyticsTracking";

        @Request(
            uri = "main",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "main", saveHomeView = "true")
        public interface Main {}

        @Request(
            uri = "FindSalesOpportunity",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindSalesOpportunity")
        public interface FindSalesOpportunity {}

        @Request(
            uri = "ViewSalesOpportunity",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewSalesOpportunity")
        public interface ViewSalesOpportunity {}

        @Request(
            uri = "EditSalesOpportunity",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSalesOpportunity")
        public interface EditSalesOpportunity {}

        @Request(
            uri = "createSalesOpportunity",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewSalesOpportunity")
        @Response(name = "error", type = "view", value = "EditSalesOpportunity")
        @Event(type = "service", invoke = "createSalesOpportunity")
        public static String createSalesOpportunity(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSalesOpportunity",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "ViewSalesOpportunity")
        @Response(name = "error", type = "view", value = "EditSalesOpportunity")
        @Event(type = "service", invoke = "updateSalesOpportunity")
        public static String updateSalesOpportunity(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "closeSalesOpportunity",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "FindSalesOpportunity")
        @Response(name = "error", type = "view", value = "FindSalesOpportunity")
        @Event(type = "service", invoke = "updateSalesOpportunity")
        public static String closeSalesOpportunity(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindAccounts",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindAccounts", saveHomeView = "true")
        public interface FindAccounts {}

        @Request(
            uri = "NewAccount",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewAccount")
        public interface NewAccount {}

        @Request(
            uri = "createAccount",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindAccounts")
        @Response(name = "error", type = "view", value = "NewAccount")
        @Event(type = "service", invoke = "createAccount")
        public static String createAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ContactMechTypeOnly",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ContactMechTypeOnly")
        public interface ContactMechTypeOnly {}

        @Request(
            uri = "FindLeads",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindLeads", saveHomeView = "true")
        public interface FindLeads {}

        @Request(
            uri = "NewLead",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewLead")
        public interface NewLead {}

    }

    // Auto-generated split (Part 4)
    public static class Part4 {
        @Request(
            uri = "createLead",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "viewprofile")
        @Response(name = "error", type = "view", value = "NewLead")
        @Event(type = "service", invoke = "createLead")
        public static String createLead(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ConvertLead",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ConvertLead")
        public interface ConvertLead {}

        @Request(
            uri = "convertLead",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "viewprofile")
        @Response(name = "error", type = "view", value = "ConvertLead")
        @Event(type = "service", invoke = "convertLeadToContact")
        public static String convertLead_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "CloneLead",
            controller = "sfa",
            secure = "true",
            auth = "true",
            externalView = "false"
        )
        @Response(name = "success", type = "view", value = "CloneLead")
        public interface CloneLead {}

        @Request(
            uri = "MergeLeads",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "MergeLeads")
        public interface MergeLeads {}

        @Request(
            uri = "mergeLeads",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "viewprofile")
        @Response(name = "error", type = "view", value = "MergeLeads")
        public interface MergeLeads1 {}

        @Request(
            uri = "NewLeadFromVCard",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewLeadFromVCard")
        public interface NewLeadFromVCard {}

        @Request(
            uri = "createLeadFromVCard",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewPartiesCreatedByVCard")
        @Response(name = "error", type = "view", value = "NewLeadFromVCard")
        @Event(type = "service", invoke = "importVCard")
        public static String createLeadFromVCard(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickAddLead",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "viewprofile")
        @Event(type = "service", invoke = "createLead")
        public static String quickAddLead(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createLeadPartyDataSource",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "viewprofile")
        @Event(type = "service", invoke = "createPartyDataSource")
        public static String createLeadPartyDataSource(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "AddRelatedCompany",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddRelatedCompany")
        public interface AddRelatedCompany {}

        @Request(
            uri = "FindContacts",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindContacts", saveHomeView = "true")
        public interface FindContacts {}

        @Request(
            uri = "NewContact",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewContact")
        public interface NewContact {}

        @Request(
            uri = "createContact",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "viewprofile")
        @Response(name = "error", type = "view", value = "NewContact")
        @Event(type = "service", invoke = "createContact")
        public static String createContact(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "MergeContacts",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "MergeContacts")
        public interface MergeContacts {}

        @Request(
            uri = "mergeContacts",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "viewprofile")
        @Response(name = "error", type = "view", value = "MergeContacts")
        @Event(type = "service", invoke = "mergeContacts")
        public static String mergeContacts_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "NewContactFromVCard",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewContactFromVCard")
        public interface NewContactFromVCard {}

        @Request(
            uri = "createContactFromVCard",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewPartiesCreatedByVCard")
        @Response(name = "error", type = "view", value = "NewContactFromVCard")
        @Event(type = "service", invoke = "importVCard")
        public static String createContactFromVCard(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createVCardFromContact",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindContacts")
        @Response(name = "error", type = "view", value = "FindContacts")
        @Event(type = "service", invoke = "exportVCard")
        public static String createVCardFromContact(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickAddContact",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "viewprofile")
        @Event(type = "service", invoke = "createContact")
        public static String quickAddContact(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 5)
    public static class Part5 {
        @Request(
            uri = "FindSalesForecast",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindSalesForecast")
        public interface FindSalesForecast {}

        @Request(
            uri = "EditSalesForecast",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSalesForecast")
        public interface EditSalesForecast {}

        @Request(
            uri = "createSalesForecast",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSalesForecast")
        @Response(name = "error", type = "view", value = "EditSalesForecast")
        @Event(type = "service", invoke = "createSalesForecast")
        public static String createSalesForecast(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSalesForecast",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSalesForecast")
        @Response(name = "error", type = "view", value = "EditSalesForecast")
        @Event(type = "service", invoke = "updateSalesForecast")
        public static String updateSalesForecast(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditSalesForecastDetail",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSalesForecastDetail")
        public interface EditSalesForecastDetail {}

        @Request(
            uri = "createSalesForecastDetail",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSalesForecastDetail")
        @Response(name = "error", type = "view", value = "EditSalesForecastDetail")
        @Event(type = "service", invoke = "createSalesForecastDetail")
        public static String createSalesForecastDetail(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSalesForecastDetail",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSalesForecastDetail")
        @Response(name = "error", type = "view", value = "EditSalesForecastDetail")
        @Event(type = "service", invoke = "updateSalesForecastDetail")
        public static String updateSalesForecastDetail(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteSalesForecastDetail",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSalesForecastDetail")
        @Response(name = "error", type = "view", value = "EditSalesForecastDetail")
        @Event(type = "service", invoke = "deleteSalesForecastDetail")
        public static String deleteSalesForecastDetail(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "Events",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Events", saveHomeView = "true")
        public interface Events {}

        @Request(
            uri = "EditEvent",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEvent", saveHomeView = "true")
        public interface EditEvent {}

        @Request(
            uri = "createCommunicationEvent",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last")
        @Response(name = "error", type = "view", value = "EditCommunicationEvent")
        @Event(type = "service", invoke = "createCommunicationEvent")
        public static String createCommunicationEvent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createEventWorkEffortAndPartyAssign",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEvent")
        @Response(name = "error", type = "view", value = "EditEvent")
        @Event(type = "service", invoke = "createWorkEffortAndPartyAssign")
        public static String createEventWorkEffortAndPartyAssign(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEventWorkEffort",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEvent")
        @Response(name = "error", type = "view", value = "EditEvent")
        @Event(type = "service", invoke = "updateWorkEffort")
        public static String updateEventWorkEffort(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEventWorkEffortReturn",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Events")
        @Response(name = "error", type = "view", value = "EditEvent")
        @Event(type = "service", invoke = "updateWorkEffort")
        public static String updateEventWorkEffortReturn(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "MyCommunicationEvents",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "MyCommunicationEvents")
        public interface MyCommunicationEvents {}

        @Request(
            uri = "FindMarketingCampaign",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindMarketingCampaign")
        public interface FindMarketingCampaign {}

        @Request(
            uri = "Calendar",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Calendar")
        public interface Calendar {}

        @Request(
            uri = "DataSources",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindDataSource")
        public interface DataSources {}

        @Request(
            uri = "AnalyticsSales",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AnalyticsSales")
        public interface AnalyticsSales {}

        @Request(
            uri = "AnalyticsTracking",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AnalyticsTracking")
        public interface AnalyticsTracking {}

    }

    // Auto-generated split (Part 6)
    public static class Part6 {
        @Request(
            uri = "AnalyticsFindTrackingCodes",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Event(type = "groovy", path = "component://marketing/webapp/marketing/WEB-INF/actions/analytics/AnalyticsTracking.groovy", invoke = "findTrackingCodes")
        public static String analyticsFindTrackingCodes(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindRequest",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindRequest")
        public interface FindRequest {}

        @Request(
            uri = "ViewRequest",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewRequest")
        public interface ViewRequest {}

        @Request(
            uri = "EditRequest",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequest")
        public interface EditRequest {}

        @Request(
            uri = "setCustRequestStatus",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindRequest")
        @Response(name = "error", type = "view", value = "EditRequest")
        @Event(type = "service", invoke = "setCustRequestStatus")
        public static String setCustRequestStatus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createWorkEffort",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Calendar")
        @Response(name = "error", type = "view", value = "Calendar")
        @Event(type = "service", invoke = "createWorkEffort")
        public static String createWorkEffort(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createWorkEffortAndPartyAssign",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Calendar")
        @Response(name = "error", type = "view", value = "Calendar")
        @Event(type = "service", invoke = "createWorkEffortAndPartyAssign")
        public static String createWorkEffortAndPartyAssign(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createWorkEffortAssoc",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Calendar")
        @Response(name = "error", type = "view", value = "Calendar")
        @Event(type = "service", invoke = "createWorkEffortAssoc")
        public static String createWorkEffortAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffortAssoc",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Calendar")
        @Response(name = "error", type = "view", value = "Calendar")
        @Event(type = "service", invoke = "updateWorkEffortAssoc")
        public static String updateWorkEffortAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createWorkEffortAndAssoc",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Calendar")
        @Response(name = "error", type = "view", value = "Calendar")
        @Event(type = "service", invoke = "createWorkEffortAndAssoc")
        public static String createWorkEffortAndAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffortAndAssoc",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Calendar")
        @Response(name = "error", type = "view", value = "Calendar")
        @Event(type = "service", invoke = "updateWorkEffortAndAssoc")
        public static String updateWorkEffortAndAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffort",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Calendar")
        @Response(name = "error", type = "view", value = "Calendar")
        @Event(type = "service", invoke = "updateWorkEffort")
        public static String updateWorkEffort(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffort",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Calendar")
        @Response(name = "error", type = "view", value = "Calendar")
        @Event(type = "service", invoke = "deleteWorkEffort")
        public static String deleteWorkEffort(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createWorkEffortPartyAssign",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Calendar")
        @Response(name = "error", type = "view", value = "Calendar")
        @Event(type = "service", invoke = "assignPartyToWorkEffort")
        public static String createWorkEffortPartyAssign(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffortPartyAssign",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Calendar")
        @Response(name = "error", type = "view", value = "Calendar")
        @Event(type = "service", invoke = "updatePartyToWorkEffortAssignment")
        public static String updateWorkEffortPartyAssign(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortPartyAssign",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Calendar")
        @Response(name = "error", type = "view", value = "Calendar")
        @Event(type = "service", invoke = "deletePartyToWorkEffortAssignment")
        public static String deleteWorkEffortPartyAssign(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "LookupSalesForecast",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupSalesForecast")
        public interface LookupSalesForecast {}

        @Request(
            uri = "LookupProduct",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProduct")
        public interface LookupProduct {}

        @Request(
            uri = "LookupProductCategory",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductCategory")
        public interface LookupProductCategory {}

        @Request(
            uri = "LookupLeads",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupLeads")
        public interface LookupLeads {}

    }

    // Auto-generated split (Part 7)
    public static class Part7 {
        @Request(
            uri = "LookupAccounts",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupAccounts")
        public interface LookupAccounts {}

        @Request(
            uri = "LookupAccountLeads",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupAccountLeads")
        public interface LookupAccountLeads {}

        @Request(
            uri = "LookupProductStore",
            controller = "sfa",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductStore")
        public interface LookupProductStore {}


    }
}
