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
import com.ilscipio.scipio.product.category.CategoryEvents;

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
        page = "component://marketing/widget/CommonScreens.xml#main",
        controller = "marketing"
    )
    public static final String VIEW_MAIN = "main";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "MarketingReport",
        type = "screen",
        page = "component://marketing/widget/MarketingReportScreens.xml#MarketingReportList",
        controller = "marketing"
    )
    public static final String VIEW_MARKETINGREPORT = "MarketingReport";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ListDataSource",
        type = "screen",
        page = "component://marketing/widget/DataSourceScreens.xml#ListDataSource",
        controller = "marketing"
    )
    public static final String VIEW_LISTDATASOURCE = "ListDataSource";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindDataSource",
        type = "screen",
        page = "component://marketing/widget/DataSourceScreens.xml#ListDataSource",
        controller = "marketing"
    )
    public static final String VIEW_FINDDATASOURCE = "FindDataSource";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditDataSource",
        type = "screen",
        page = "component://marketing/widget/DataSourceScreens.xml#EditDataSource",
        controller = "marketing"
    )
    public static final String VIEW_EDITDATASOURCE = "EditDataSource";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindDataSourceType",
        type = "screen",
        page = "component://marketing/widget/DataSourceScreens.xml#ListDataSourceType",
        controller = "marketing"
    )
    public static final String VIEW_FINDDATASOURCETYPE = "FindDataSourceType";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditDataSourceType",
        type = "screen",
        page = "component://marketing/widget/DataSourceScreens.xml#EditDataSourceType",
        controller = "marketing"
    )
    public static final String VIEW_EDITDATASOURCETYPE = "EditDataSourceType";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindTrackingCode",
        type = "screen",
        page = "component://marketing/widget/TrackingCodeScreens.xml#ListTrackingCode",
        controller = "marketing"
    )
    public static final String VIEW_FINDTRACKINGCODE = "FindTrackingCode";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditTrackingCode",
        type = "screen",
        page = "component://marketing/widget/TrackingCodeScreens.xml#EditTrackingCode",
        controller = "marketing"
    )
    public static final String VIEW_EDITTRACKINGCODE = "EditTrackingCode";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindTrackingCodeOrders",
        type = "screen",
        page = "component://marketing/widget/TrackingCodeScreens.xml#ListTrackingCodeOrders",
        controller = "marketing"
    )
    public static final String VIEW_FINDTRACKINGCODEORDERS = "FindTrackingCodeOrders";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindTrackingCodeVisits",
        type = "screen",
        page = "component://marketing/widget/TrackingCodeScreens.xml#ListTrackingCodeVisits",
        controller = "marketing"
    )
    public static final String VIEW_FINDTRACKINGCODEVISITS = "FindTrackingCodeVisits";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditTrackingCodeType",
        type = "screen",
        page = "component://marketing/widget/TrackingCodeScreens.xml#EditTrackingCodeType",
        controller = "marketing"
    )
    public static final String VIEW_EDITTRACKINGCODETYPE = "EditTrackingCodeType";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindTrackingCodeType",
        type = "screen",
        page = "component://marketing/widget/TrackingCodeScreens.xml#ListTrackingCodeType",
        controller = "marketing"
    )
    public static final String VIEW_FINDTRACKINGCODETYPE = "FindTrackingCodeType";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindMarketingCampaign",
        type = "screen",
        page = "component://marketing/widget/MarketingCampaignScreens.xml#FindMarketingCampaign",
        controller = "marketing"
    )
    public static final String VIEW_FINDMARKETINGCAMPAIGN = "FindMarketingCampaign";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditMarketingCampaign",
        type = "screen",
        page = "component://marketing/widget/MarketingCampaignScreens.xml#EditMarketingCampaign",
        controller = "marketing"
    )
    public static final String VIEW_EDITMARKETINGCAMPAIGN = "EditMarketingCampaign";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindSegmentGroup",
        type = "screen",
        page = "component://marketing/widget/SegmentScreens.xml#FindSegmentGroup",
        controller = "marketing"
    )
    public static final String VIEW_FINDSEGMENTGROUP = "FindSegmentGroup";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "viewSegmentGroup",
        type = "screen",
        page = "component://marketing/widget/SegmentScreens.xml#EditSegmentGroup",
        controller = "marketing"
    )
    public static final String VIEW_VIEWSEGMENTGROUP = "viewSegmentGroup";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "listSegmentGroupClass",
        type = "screen",
        page = "component://marketing/widget/SegmentScreens.xml#listSegmentGroupClass",
        controller = "marketing"
    )
    public static final String VIEW_LISTSEGMENTGROUPCLASS = "listSegmentGroupClass";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "listSegmentGroupGeo",
        type = "screen",
        page = "component://marketing/widget/SegmentScreens.xml#listSegmentGroupGeo",
        controller = "marketing"
    )
    public static final String VIEW_LISTSEGMENTGROUPGEO = "listSegmentGroupGeo";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "listSegmentGroupRole",
        type = "screen",
        page = "component://marketing/widget/SegmentScreens.xml#listSegmentGroupRole",
        controller = "marketing"
    )
    public static final String VIEW_LISTSEGMENTGROUPROLE = "listSegmentGroupRole";


    // Auto-generated split (Part 2)
    public static class Part2 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindContactLists",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#FindContactLists",
            controller = "marketing"
        )
        public static final String VIEW_FINDCONTACTLISTS = "FindContactLists";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListContactLists",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#ListContactLists",
            controller = "marketing"
        )
        public static final String VIEW_LISTCONTACTLISTS = "ListContactLists";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContactList",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#EditContactList",
            controller = "marketing"
        )
        public static final String VIEW_EDITCONTACTLIST = "EditContactList";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListContactListParties",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#ListContactListParties",
            controller = "marketing"
        )
        public static final String VIEW_LISTCONTACTLISTPARTIES = "ListContactListParties";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListContactListParty",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#ListContactListParties",
            controller = "marketing"
        )
        public static final String VIEW_LISTCONTACTLISTPARTY = "ListContactListParty";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContactListParty",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#EditContactListParty",
            controller = "marketing"
        )
        public static final String VIEW_EDITCONTACTLISTPARTY = "EditContactListParty";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindContactListParties",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#FindContactListParties",
            controller = "marketing"
        )
        public static final String VIEW_FINDCONTACTLISTPARTIES = "FindContactListParties";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ContactListOptOut",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#OptOutResponse",
            controller = "marketing"
        )
        public static final String VIEW_CONTACTLISTOPTOUT = "ContactListOptOut";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WebSiteContactList",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#WebSiteContactList",
            controller = "marketing"
        )
        public static final String VIEW_WEBSITECONTACTLIST = "WebSiteContactList";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListContactListCommEvents",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#ListContactListCommEvents",
            controller = "marketing"
        )
        public static final String VIEW_LISTCONTACTLISTCOMMEVENTS = "ListContactListCommEvents";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContactListCommEvent",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#EditContactListCommEvent",
            controller = "marketing"
        )
        public static final String VIEW_EDITCONTACTLISTCOMMEVENT = "EditContactListCommEvent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindContactListCommEvents",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#FindContactListCommEvents",
            controller = "marketing"
        )
        public static final String VIEW_FINDCONTACTLISTCOMMEVENTS = "FindContactListCommEvents";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PreviewContactListCommEvent",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#PreviewContactListCommEvent",
            controller = "marketing"
        )
        public static final String VIEW_PREVIEWCONTACTLISTCOMMEVENT = "PreviewContactListCommEvent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindImportContactListParties",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#FindImportContactListParties",
            controller = "marketing"
        )
        public static final String VIEW_FINDIMPORTCONTACTLISTPARTIES = "FindImportContactListParties";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ContactMechTypeOnly",
            type = "screen",
            page = "component://marketing/widget/sfa/AccountScreens.xml#ContactMechTypeOnly",
            controller = "marketing"
        )
        public static final String VIEW_CONTACTMECHTYPEONLY = "ContactMechTypeOnly";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupSegmentGroup",
            type = "screen",
            page = "component://marketing/widget/LookupScreens.xml#LookupSegmentGroup",
            controller = "marketing"
        )
        public static final String VIEW_LOOKUPSEGMENTGROUP = "LookupSegmentGroup";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupContactList",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#LookupContactList",
            controller = "marketing"
        )
        public static final String VIEW_LOOKUPCONTACTLIST = "LookupContactList";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPreferredContactMech",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#LookupPreferredContactMech",
            controller = "marketing"
        )
        public static final String VIEW_LOOKUPPREFERREDCONTACTMECH = "LookupPreferredContactMech";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProductStore",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProductStore",
            controller = "marketing"
        )
        public static final String VIEW_LOOKUPPRODUCTSTORE = "LookupProductStore";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyName",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyName",
            controller = "marketing"
        )
        public static final String VIEW_LOOKUPPARTYNAME = "LookupPartyName";

    }

    // Auto-generated split (Part 3)
    public static class Part3 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyClassificationGroup",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyClassificationGroup",
            controller = "marketing"
        )
        public static final String VIEW_LOOKUPPARTYCLASSIFICATIONGROUP = "LookupPartyClassificationGroup";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupCommEvent",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupCommEvent",
            controller = "marketing"
        )
        public static final String VIEW_LOOKUPCOMMEVENT = "LookupCommEvent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupContactMech",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupContactMech",
            controller = "marketing"
        )
        public static final String VIEW_LOOKUPCONTACTMECH = "LookupContactMech";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupUserLoginAndPartyDetails",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupUserLoginAndPartyDetails",
            controller = "marketing"
        )
        public static final String VIEW_LOOKUPUSERLOGINANDPARTYDETAILS = "LookupUserLoginAndPartyDetails";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "TrackingCodeReport",
            type = "screen",
            page = "component://marketing/widget/MarketingReportScreens.xml#TrackingCodeReport",
            controller = "marketing"
        )
        public static final String VIEW_TRACKINGCODEREPORT = "TrackingCodeReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "MarketingCampaignReport",
            type = "screen",
            page = "component://marketing/widget/MarketingReportScreens.xml#MarketingCampaignReport",
            controller = "marketing"
        )
        public static final String VIEW_MARKETINGCAMPAIGNREPORT = "MarketingCampaignReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EmailStatusReport",
            type = "screen",
            page = "component://marketing/widget/MarketingReportScreens.xml#EmailStatusReport",
            controller = "marketing"
        )
        public static final String VIEW_EMAILSTATUSREPORT = "EmailStatusReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PartyStatusReport",
            type = "screen",
            page = "component://marketing/widget/MarketingReportScreens.xml#PartyStatusReport",
            controller = "marketing"
        )
        public static final String VIEW_PARTYSTATUSREPORT = "PartyStatusReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListWorkEfforts",
            type = "screen",
            page = "component://workeffort/widget/WorkEffortScreens.xml#ListWorkEfforts",
            controller = "marketing"
        )
        public static final String VIEW_LISTWORKEFFORTS = "ListWorkEfforts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditWorkEffort",
            type = "screen",
            page = "component://workeffort/widget/WorkEffortScreens.xml#EditWorkEffort",
            controller = "marketing"
        )
        public static final String VIEW_EDITWORKEFFORT = "EditWorkEffort";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindProductPromo",
            type = "screen",
            page = "component://product/widget/catalog/PromoScreens.xml#FindProductPromo",
            controller = "marketing"
        )
        public static final String VIEW_FINDPRODUCTPROMO = "FindProductPromo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPromo",
            type = "screen",
            page = "component://product/widget/catalog/PromoScreens.xml#EditProductPromo",
            controller = "marketing"
        )
        public static final String VIEW_EDITPRODUCTPROMO = "EditProductPromo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPromoRules",
            type = "screen",
            page = "component://product/widget/catalog/PromoScreens.xml#EditProductPromoRules",
            controller = "marketing"
        )
        public static final String VIEW_EDITPRODUCTPROMORULES = "EditProductPromoRules";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPromoStores",
            type = "screen",
            page = "component://product/widget/catalog/PromoScreens.xml#EditProductPromoStores",
            controller = "marketing"
        )
        public static final String VIEW_EDITPRODUCTPROMOSTORES = "EditProductPromoStores";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindProductPromoCode",
            type = "screen",
            page = "component://product/widget/catalog/PromoScreens.xml#FindProductPromoCode",
            controller = "marketing"
        )
        public static final String VIEW_FINDPRODUCTPROMOCODE = "FindProductPromoCode";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPromoCode",
            type = "screen",
            page = "component://product/widget/catalog/PromoScreens.xml#EditProductPromoCode",
            controller = "marketing"
        )
        public static final String VIEW_EDITPRODUCTPROMOCODE = "EditProductPromoCode";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPromoContent",
            type = "screen",
            page = "component://product/widget/catalog/PromoScreens.xml#EditProductPromoContent",
            controller = "marketing"
        )
        public static final String VIEW_EDITPRODUCTPROMOCONTENT = "EditProductPromoContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStorePromos",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStorePromos",
            controller = "marketing"
        )
        public static final String VIEW_EDITPRODUCTSTOREPROMOS = "EditProductStorePromos";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindPriceRules",
            type = "screen",
            page = "component://product/widget/catalog/PriceScreens.xml#FindProductPriceRule",
            controller = "marketing"
        )
        public static final String VIEW_FINDPRICERULES = "FindPriceRules";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPriceRules",
            type = "screen",
            page = "component://product/widget/catalog/PriceScreens.xml#EditProductPriceRules",
            controller = "marketing"
        )
        public static final String VIEW_EDITPRODUCTPRICERULES = "EditProductPriceRules";

    }

    // Auto-generated split (Part 4)
    public static class Part4 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "listMiniproduct",
            type = "screen",
            page = "component://product/widget/catalog/CommonScreens.xml#listMiniproduct",
            controller = "marketing"
        )
        public static final String VIEW_LISTMINIPRODUCT = "listMiniproduct";

        @Request(
            uri = "MarketingReport",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "MarketingReport")
        public interface MarketingReport {}

        @Request(
            uri = "view",
            controller = "marketing",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "main")
        public interface View {}

        @Request(
            uri = "main",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        public interface Main {}

        @Request(
            uri = "ListDataSource",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListDataSource")
        public interface ListDataSource {}

        @Request(
            uri = "FindDataSource",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindDataSource")
        public interface FindDataSource {}

        @Request(
            uri = "EditDataSource",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataSource")
        public interface EditDataSource {}

        @Request(
            uri = "createDataSource",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataSource")
        @Response(name = "error", type = "view", value = "EditDataSource")
        @Event(type = "service", invoke = "createDataSource")
        public static String createDataSource(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateDataSource",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataSource")
        @Response(name = "error", type = "view", value = "EditDataSource")
        @Event(type = "service", invoke = "updateDataSource")
        public static String updateDataSource(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteDataSource",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindDataSource")
        @Response(name = "error", type = "view", value = "FindDataSource")
        @Event(type = "service", invoke = "deleteDataSource")
        public static String deleteDataSource(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindDataSourceType",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindDataSourceType")
        public interface FindDataSourceType {}

        @Request(
            uri = "EditDataSourceType",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataSourceType")
        public interface EditDataSourceType {}

        @Request(
            uri = "createDataSourceType",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataSourceType")
        @Response(name = "error", type = "view", value = "EditDataSourceType")
        @Event(type = "service", invoke = "createDataSourceType")
        public static String createDataSourceType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateDataSourceType",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataSourceType")
        @Response(name = "error", type = "view", value = "EditDataSourceType")
        @Event(type = "service", invoke = "updateDataSourceType")
        public static String updateDataSourceType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteDataSourceType",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindDataSourceType")
        @Response(name = "error", type = "view", value = "FindDataSourceType")
        @Event(type = "service", invoke = "deleteDataSourceType")
        public static String deleteDataSourceType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindTrackingCode",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindTrackingCode")
        public interface FindTrackingCode {}

        @Request(
            uri = "EditTrackingCode",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTrackingCode")
        public interface EditTrackingCode {}

        @Request(
            uri = "createTrackingCode",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindTrackingCode")
        @Response(name = "error", type = "view", value = "EditTrackingCode")
        @Event(type = "service", invoke = "createTrackingCode")
        public static String createTrackingCode(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTrackingCode",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTrackingCode")
        @Response(name = "error", type = "view", value = "EditTrackingCode")
        @Event(type = "service", invoke = "updateTrackingCode")
        public static String updateTrackingCode(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteTrackingCode",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindTrackingCode")
        @Response(name = "error", type = "view", value = "FindTrackingCode")
        @Event(type = "service", invoke = "deleteTrackingCode")
        public static String deleteTrackingCode(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 5)
    public static class Part5 {
        @Request(
            uri = "FindTrackingCodeOrders",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindTrackingCodeOrders")
        public interface FindTrackingCodeOrders {}

        @Request(
            uri = "FindTrackingCodeVisits",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindTrackingCodeVisits")
        public interface FindTrackingCodeVisits {}

        @Request(
            uri = "FindTrackingCodeType",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindTrackingCodeType")
        public interface FindTrackingCodeType {}

        @Request(
            uri = "EditTrackingCodeType",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTrackingCodeType")
        public interface EditTrackingCodeType {}

        @Request(
            uri = "createTrackingCodeType",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTrackingCodeType")
        @Response(name = "error", type = "view", value = "EditTrackingCodeType")
        @Event(type = "service", invoke = "createTrackingCodeType")
        public static String createTrackingCodeType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTrackingCodeType",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTrackingCodeType")
        @Response(name = "error", type = "view", value = "EditTrackingCodeType")
        @Event(type = "service", invoke = "updateTrackingCodeType")
        public static String updateTrackingCodeType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteTrackingCodeType",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindTrackingCodeType")
        @Response(name = "error", type = "view", value = "FindTrackingCodeType")
        @Event(type = "service", invoke = "deleteTrackingCodeType")
        public static String deleteTrackingCodeType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindMarketingCampaign",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindMarketingCampaign")
        public interface FindMarketingCampaign {}

        @Request(
            uri = "EditMarketingCampaign",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditMarketingCampaign")
        public interface EditMarketingCampaign {}

        @Request(
            uri = "createMarketingCampaign",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditMarketingCampaign")
        @Response(name = "error", type = "view", value = "EditMarketingCampaign")
        @Event(type = "service", invoke = "createMarketingCampaign")
        public static String createMarketingCampaign(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateMarketingCampaign",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditMarketingCampaign")
        @Response(name = "error", type = "view", value = "EditMarketingCampaign")
        @Event(type = "service", invoke = "updateMarketingCampaign")
        public static String updateMarketingCampaign(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeMarketingCampaign",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindMarketingCampaign")
        @Response(name = "error", type = "view", value = "FindMarketingCampaign")
        @Event(type = "service", invoke = "deleteMarketingCampaign")
        public static String removeMarketingCampaign(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "viewSegmentGroup",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewSegmentGroup")
        public interface ViewSegmentGroup {}

        @Request(
            uri = "FindSegmentGroup",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindSegmentGroup")
        public interface FindSegmentGroup {}

        @Request(
            uri = "createSegmentGroup",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewSegmentGroup")
        @Response(name = "error", type = "view", value = "viewSegmentGroup")
        @Event(type = "service", invoke = "createSegmentGroup")
        public static String createSegmentGroup(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSegmentGroup",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewSegmentGroup")
        @Response(name = "error", type = "view", value = "viewSegmentGroup")
        @Event(type = "service", invoke = "updateSegmentGroup")
        public static String updateSegmentGroup(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteSegmentGroup",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindSegmentGroup")
        @Response(name = "error", type = "view", value = "FindSegmentGroup")
        @Event(type = "service", invoke = "deleteSegmentGroup")
        public static String deleteSegmentGroup(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "listSegmentGroupClass",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listSegmentGroupClass")
        public interface ListSegmentGroupClass {}

        @Request(
            uri = "createSegmentGroupClassification",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listSegmentGroupClass")
        @Response(name = "error", type = "view", value = "listSegmentGroupClass")
        @Event(type = "service", invoke = "createSegmentGroupClassification")
        public static String createSegmentGroupClassification(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSegmentGroupClassification",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listSegmentGroupClass")
        @Response(name = "error", type = "view", value = "listSegmentGroupClass")
        @Event(type = "service", invoke = "updateSegmentGroupClassification")
        public static String updateSegmentGroupClassification(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 6)
    public static class Part6 {
        @Request(
            uri = "deleteSegmentGroupClassification",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listSegmentGroupClass")
        @Response(name = "error", type = "view", value = "listSegmentGroupClass")
        @Event(type = "service", invoke = "deleteSegmentGroupClassification")
        public static String deleteSegmentGroupClassification(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "listSegmentGroupGeo",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listSegmentGroupGeo")
        public interface ListSegmentGroupGeo {}

        @Request(
            uri = "createSegmentGroupGeo",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listSegmentGroupGeo")
        @Response(name = "error", type = "view", value = "listSegmentGroupGeo")
        @Event(type = "service", invoke = "createSegmentGroupGeo")
        public static String createSegmentGroupGeo(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSegmentGroupGeo",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listSegmentGroupGeo")
        @Response(name = "error", type = "view", value = "listSegmentGroupGeo")
        @Event(type = "service", invoke = "updateSegmentGroupGeo")
        public static String updateSegmentGroupGeo(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteSegmentGroupGeo",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listSegmentGroupGeo")
        @Response(name = "error", type = "view", value = "listSegmentGroupGeo")
        @Event(type = "service", invoke = "deleteSegmentGroupGeo")
        public static String deleteSegmentGroupGeo(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "listSegmentGroupRole",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listSegmentGroupRole")
        public interface ListSegmentGroupRole {}

        @Request(
            uri = "createSegmentGroupRole",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listSegmentGroupRole")
        @Response(name = "error", type = "view", value = "listSegmentGroupRole")
        @Event(type = "service", invoke = "createSegmentGroupRole")
        public static String createSegmentGroupRole(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSegmentGroupRole",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listSegmentGroupRole")
        @Response(name = "error", type = "view", value = "listSegmentGroupRole")
        @Event(type = "service", invoke = "updateSegmentGroupRole")
        public static String updateSegmentGroupRole(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteSegmentGroupRole",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listSegmentGroupRole")
        @Response(name = "error", type = "view", value = "listSegmentGroupRole")
        @Event(type = "service", invoke = "deleteSegmentGroupRole")
        public static String deleteSegmentGroupRole(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindContactLists",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindContactLists")
        public interface FindContactLists {}

        @Request(
            uri = "ListContactLists",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListContactLists")
        public interface ListContactLists {}

        @Request(
            uri = "EditContactList",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactList")
        public interface EditContactList {}

        @Request(
            uri = "LookupContactList",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupContactList")
        public interface LookupContactList {}

        @Request(
            uri = "createContactList",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactList")
        @Response(name = "error", type = "view", value = "EditContactList")
        @Event(type = "service", invoke = "createContactList")
        public static String createContactList(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContactList",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactList")
        @Response(name = "error", type = "view", value = "EditContactList")
        @Event(type = "service", invoke = "updateContactList")
        public static String updateContactList(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContactList",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListContactLists")
        @Response(name = "error", type = "view", value = "ListContactLists")
        @Event(type = "service", invoke = "removeContactList")
        public static String removeContactList(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditContactListParty",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactListParty")
        public interface EditContactListParty {}

        @Request(
            uri = "FindContactListParties",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindContactListParties")
        public interface FindContactListParties {}

        @Request(
            uri = "ListContactListParties",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListContactListParties")
        public interface ListContactListParties {}

        @Request(
            uri = "createContactListParty",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactListParty")
        @Response(name = "error", type = "view", value = "EditContactListParty")
        @Event(type = "service", invoke = "createContactListParty")
        public static String createContactListParty(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 7)
    public static class Part7 {
        @Request(
            uri = "updateContactListParty",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactListParty")
        @Response(name = "error", type = "view", value = "EditContactListParty")
        @Event(type = "service", invoke = "updateContactListParty")
        public static String updateContactListParty(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContactListParty",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListContactListParties")
        @Response(name = "error", type = "view", value = "ListContactListParties")
        @Event(type = "service", invoke = "deleteContactListParty")
        public static String removeContactListParty(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "importContactListParties",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "FindImportContactListParties")
        @Response(name = "error", type = "request-redirect", value = "FindImportContactListParties")
        @Event(type = "simple", path = "component://marketing/script/org/ofbiz/marketing/contact/ContactListEvents.xml", invoke = "importContactListParties")
        public static String importContactListParties(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "contactListOptOut",
            controller = "marketing",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "ContactListOptOut")
        @Event(type = "service", invoke = "updateContactListPartyNoUserLogin")
        public static String contactListOptOut(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "webSiteContactList",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteContactList")
        public interface WebSiteContactList {}

        @Request(
            uri = "createWebSiteContactList",
            controller = "marketing",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteContactList")
        @Response(name = "error", type = "view", value = "WebSiteContactList")
        @Event(type = "service", invoke = "createWebSiteContactList")
        public static String createWebSiteContactList(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWebSiteContactList",
            controller = "marketing",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteContactList")
        @Response(name = "error", type = "view", value = "WebSiteContactList")
        @Event(type = "service", invoke = "updateWebSiteContactList")
        public static String updateWebSiteContactList(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWebSiteContactList",
            controller = "marketing",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteContactList")
        @Response(name = "error", type = "view", value = "WebSiteContactList")
        @Event(type = "service", invoke = "deleteWebSiteContactList")
        public static String deleteWebSiteContactList(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListContactListCommEvents",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListContactListCommEvents")
        public interface ListContactListCommEvents {}

        @Request(
            uri = "EditContactListCommEvent",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactListCommEvent")
        public interface EditContactListCommEvent {}

        @Request(
            uri = "FindContactListCommEvents",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindContactListCommEvents")
        public interface FindContactListCommEvents {}

        @Request(
            uri = "FindImportContactListParties",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindImportContactListParties")
        public interface FindImportContactListParties {}

        @Request(
            uri = "PreviewContactListCommEvent",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PreviewContactListCommEvent")
        public interface PreviewContactListCommEvent {}

        @Request(
            uri = "createContactListCommEvent",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactListCommEvent")
        @Response(name = "error", type = "view", value = "EditContactListCommEvent")
        @Event(type = "service", invoke = "createCommunicationEvent")
        public static String createContactListCommEvent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContactListCommEvent",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactListCommEvent")
        @Response(name = "error", type = "view", value = "EditContactListCommEvent")
        @Event(type = "service", invoke = "updateCommunicationEvent")
        public static String updateContactListCommEvent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContactListCommEvent",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListContactListCommEvents")
        @Response(name = "error", type = "view", value = "ListContactListCommEvents")
        @Event(type = "service", invoke = "deleteCommunicationEvent")
        public static String removeContactListCommEvent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "expireContactListParty",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "ListContactListParties")
        @Response(name = "error", type = "view", value = "ListContactListParties")
        @Event(type = "service", invoke = "updateContactListParty")
        public static String expireContactListParty(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ContactMechTypeOnly",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ContactMechTypeOnly")
        public interface ContactMechTypeOnly {}

        @Request(
            uri = "LookupSegmentGroup",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupSegmentGroup")
        public interface LookupSegmentGroup {}

        @Request(
            uri = "LookupProductStore",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductStore")
        public interface LookupProductStore {}

    }

    // Auto-generated split (Part 8)
    public static class Part8 {
        @Request(
            uri = "LookupPartyName",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyName")
        public interface LookupPartyName {}

        @Request(
            uri = "LookupPartyClassificationGroup",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyClassificationGroup")
        public interface LookupPartyClassificationGroup {}

        @Request(
            uri = "LookupContactMech",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupContactMech")
        public interface LookupContactMech {}

        @Request(
            uri = "LookupCommEvent",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupCommEvent")
        public interface LookupCommEvent {}

        @Request(
            uri = "LookupPreferredContactMech",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPreferredContactMech")
        public interface LookupPreferredContactMech {}

        @Request(
            uri = "TrackingCodeReport",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "TrackingCodeReport")
        public interface TrackingCodeReport {}

        @Request(
            uri = "MarketingCampaignReport",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "MarketingCampaignReport")
        public interface MarketingCampaignReport {}

        @Request(
            uri = "EmailStatusReport",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EmailStatusReport")
        public interface EmailStatusReport {}

        @Request(
            uri = "PartyStatusReport",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PartyStatusReport")
        public interface PartyStatusReport {}

        @Request(
            uri = "ListWorkEfforts",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWorkEfforts")
        public interface ListWorkEfforts {}

        @Request(
            uri = "EditWorkEffort",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWorkEffort")
        public interface EditWorkEffort {}

        @Request(
            uri = "FindProductPromo",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductPromo")
        public interface FindProductPromo {}

        @Request(
            uri = "EditProductPromo",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromo")
        public interface EditProductPromo {}

        @Request(
            uri = "createProductPromo",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromo")
        @Response(name = "error", type = "view", value = "EditProductPromo")
        @Event(type = "service", invoke = "createProductPromo")
        public static String createProductPromo(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPromo",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromo")
        @Response(name = "error", type = "view", value = "EditProductPromo")
        @Event(type = "service", invoke = "updateProductPromo")
        public static String updateProductPromo(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindProductPromoCode",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductPromoCode")
        public interface FindProductPromoCode {}

        @Request(
            uri = "deleteProductPromoCode",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductPromoCode")
        @Response(name = "error", type = "view", value = "FindProductPromoCode")
        @Event(type = "service", invoke = "deleteProductPromoCode")
        public static String deleteProductPromoCode(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductPromoCode",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoCode")
        public interface EditProductPromoCode {}

        @Request(
            uri = "createProductPromoCode",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoCode")
        @Response(name = "error", type = "view", value = "EditProductPromoCode")
        @Event(type = "service", invoke = "createProductPromoCode")
        public static String createProductPromoCode(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPromoCode",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoCode")
        @Response(name = "error", type = "view", value = "EditProductPromoCode")
        @Event(type = "service", invoke = "updateProductPromoCode")
        public static String updateProductPromoCode(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 9)
    public static class Part9 {
        @Request(
            uri = "createProductPromoCodeEmail",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoCode")
        @Response(name = "error", type = "view", value = "EditProductPromoCode")
        @Event(type = "service", invoke = "createProductPromoCodeEmail")
        public static String createProductPromoCodeEmail(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductPromoCodeEmail",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoCode")
        @Response(name = "error", type = "view", value = "EditProductPromoCode")
        @Event(type = "service", invoke = "deleteProductPromoCodeEmail")
        public static String deleteProductPromoCodeEmail(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createBulkProductPromoCodeEmail",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoCode")
        @Response(name = "error", type = "view", value = "EditProductPromoCode")
        @Event(type = "service", invoke = "createBulkProductPromoCodeEmail")
        public static String createBulkProductPromoCodeEmail(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductPromoCodeParty",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoCode")
        @Response(name = "error", type = "view", value = "EditProductPromoCode")
        @Event(type = "service", invoke = "createProductPromoCodeParty")
        public static String createProductPromoCodeParty(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductPromoCodeParty",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoCode")
        @Response(name = "error", type = "view", value = "EditProductPromoCode")
        @Event(type = "service", invoke = "deleteProductPromoCodeParty")
        public static String deleteProductPromoCodeParty(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductPromoCodeSet",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductPromoCode")
        @Response(name = "error", type = "view", value = "FindProductPromoCode")
        @Event(type = "service", invoke = "createProductPromoCodeSet")
        public static String createProductPromoCodeSet(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createBulkProductPromoCode",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductPromoCode")
        @Response(name = "error", type = "view", value = "FindProductPromoCode")
        @Event(type = "service", invoke = "createBulkProductPromoCode")
        public static String createBulkProductPromoCode(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductPromoContent",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoContent")
        @Response(name = "error", type = "view", value = "EditProductPromoContent")
        public interface EditProductPromoContent {}

        @Request(
            uri = "removeContentFromProductPromo",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoContent")
        @Response(name = "error", type = "view", value = "EditProductPromoContent")
        @Event(type = "service", invoke = "removeProductPromoContent")
        public static String removeContentFromProductPromo(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addImageContentForProductPromo",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoContent")
        @Response(name = "error", type = "view", value = "EditProductPromoContent")
        @Event(type = "service", invoke = "addImageForProductPromo")
        public static String addImageContentForProductPromo(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getChild",
            controller = "marketing",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        public static String getChild(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: com.ilscipio.scipio.product.category.CategoryEvents.getChildCategoryTree
            return CategoryEvents.getChildCategoryTree(request, response);
        }

        @Request(
            uri = "listMiniproduct",
            controller = "marketing",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "listMiniproduct")
        public interface ListMiniproduct {}

        @Request(
            uri = "EditProductPromoRules",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        public interface EditProductPromoRules {}

        @Request(
            uri = "createProductPromoRule",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "createProductPromoRule")
        public static String createProductPromoRule(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPromoRule",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "updateProductPromoRule")
        public static String updateProductPromoRule(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductPromoRule",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "deleteProductPromoRule")
        public static String deleteProductPromoRule(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductPromoCond",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "createProductPromoCond")
        public static String createProductPromoCond(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPromoCond",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "updateProductPromoCond")
        public static String updateProductPromoCond(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "LookupUserLoginAndPartyDetails",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupUserLoginAndPartyDetails")
        public interface LookupUserLoginAndPartyDetails {}

        @Request(
            uri = "deleteProductPromoCond",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "deleteProductPromoCond")
        public static String deleteProductPromoCond(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 10)
    public static class Part10 {
        @Request(
            uri = "createProductPromoAction",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "createProductPromoAction")
        public static String createProductPromoAction(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPromoAction",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "updateProductPromoAction")
        public static String updateProductPromoAction(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductPromoAction",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "deleteProductPromoAction")
        public static String deleteProductPromoAction(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductPromoCategory",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "createProductPromoCategory")
        public static String createProductPromoCategory(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPromoCategory",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "updateProductPromoCategory")
        public static String updateProductPromoCategory(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductPromoCategory",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "deleteProductPromoCategory")
        public static String deleteProductPromoCategory(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductPromoProduct",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "createProductPromoProduct")
        public static String createProductPromoProduct(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPromoProduct",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "updateProductPromoProduct")
        public static String updateProductPromoProduct(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductPromoProduct",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "deleteProductPromoProduct")
        public static String deleteProductPromoProduct(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductPromoStores",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoStores")
        public interface EditProductPromoStores {}

        @Request(
            uri = "promo_createProductStorePromoAppl",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoStores")
        @Response(name = "error", type = "view", value = "EditProductPromoStores")
        @Event(type = "service", invoke = "createProductStorePromoAppl")
        public static String promoCreateProductStorePromoAppl(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "promo_updateProductStorePromoAppl",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoStores")
        @Response(name = "error", type = "view", value = "EditProductPromoStores")
        @Event(type = "service", invoke = "updateProductStorePromoAppl")
        public static String promoUpdateProductStorePromoAppl(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "promo_deleteProductStorePromoAppl",
            controller = "marketing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoStores")
        @Response(name = "error", type = "view", value = "EditProductPromoStores")
        @Event(type = "service", invoke = "deleteProductStorePromoAppl")
        public static String promoDeleteProductStorePromoAppl(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }


    }
}
