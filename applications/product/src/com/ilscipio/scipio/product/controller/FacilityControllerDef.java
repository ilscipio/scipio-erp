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
package com.ilscipio.scipio.product.controller;

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Responses;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.shipment.shipment.ShipmentEvents;

/**
 * Auto-generated annotation-based controller definitions.
 *
 * <p>Generated from controller.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class FacilityControllerDef {
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "main",
        type = "screen",
        page = "component://product/widget/facility/CommonScreens.xml#main",
        controller = "facility"
    )
    public static final String VIEW_MAIN = "main";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindFacility",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#FindFacility",
        controller = "facility"
    )
    public static final String VIEW_FINDFACILITY = "FindFacility";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FacilitySearchResults",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#FacilitySearchResults",
        controller = "facility"
    )
    public static final String VIEW_FACILITYSEARCHRESULTS = "FacilitySearchResults";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditFacility",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#EditFacility",
        controller = "facility"
    )
    public static final String VIEW_EDITFACILITY = "EditFacility";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindFacilityTransfers",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#FindFacilityTransfers",
        controller = "facility"
    )
    public static final String VIEW_FINDFACILITYTRANSFERS = "FindFacilityTransfers";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindFacilityLocation",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#FindFacilityLocation",
        controller = "facility"
    )
    public static final String VIEW_FINDFACILITYLOCATION = "FindFacilityLocation";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditFacilityLocation",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#EditFacilityLocation",
        controller = "facility"
    )
    public static final String VIEW_EDITFACILITYLOCATION = "EditFacilityLocation";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditFacilityInventoryItems",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#EditFacilityInventoryItems",
        controller = "facility"
    )
    public static final String VIEW_EDITFACILITYINVENTORYITEMS = "EditFacilityInventoryItems";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "SearchInventoryItemsByLabels",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#SearchInventoryItemsByLabels",
        controller = "facility"
    )
    public static final String VIEW_SEARCHINVENTORYITEMSBYLABELS = "SearchInventoryItemsByLabels";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ViewFacilityInventoryByProduct",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#ViewFacilityInventoryByProduct",
        controller = "facility"
    )
    public static final String VIEW_VIEWFACILITYINVENTORYBYPRODUCT = "ViewFacilityInventoryByProduct";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ViewFacilityInventoryByProductSimple",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#ViewFacilityInventoryByProductSimple",
        controller = "facility"
    )
    public static final String VIEW_VIEWFACILITYINVENTORYBYPRODUCTSIMPLE = "ViewFacilityInventoryByProductSimple";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ViewFacilityInventoryByProductReport",
        type = "screenfop",
        page = "component://product/widget/facility/FacilityScreens.xml#ViewFacilityInventoryByProductReport",
        contentType = "application/pdf",
        encoding = "none",
        controller = "facility"
    )
    public static final String VIEW_VIEWFACILITYINVENTORYBYPRODUCTREPORT = "ViewFacilityInventoryByProductReport";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ViewFacilityInventoryByProductExport",
        type = "screenxml",
        page = "component://product/widget/facility/FacilityScreens.xml#ViewFacilityInventoryByProductReport",
        contentType = "text/xml",
        controller = "facility"
    )
    public static final String VIEW_VIEWFACILITYINVENTORYBYPRODUCTEXPORT = "ViewFacilityInventoryByProductExport";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ViewContactMechs",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#ViewContactMechs",
        controller = "facility"
    )
    public static final String VIEW_VIEWCONTACTMECHS = "ViewContactMechs";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditContactMech",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#EditContactMech",
        controller = "facility"
    )
    public static final String VIEW_EDITCONTACTMECH = "EditContactMech";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditInventoryItem",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#EditInventoryItem",
        controller = "facility"
    )
    public static final String VIEW_EDITINVENTORYITEM = "EditInventoryItem";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ViewInventoryItemDetail",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#ViewInventoryItemDetail",
        controller = "facility"
    )
    public static final String VIEW_VIEWINVENTORYITEMDETAIL = "ViewInventoryItemDetail";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditInventoryItemLabels",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#EditInventoryItemLabels",
        controller = "facility"
    )
    public static final String VIEW_EDITINVENTORYITEMLABELS = "EditInventoryItemLabels";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "TransferInventoryItem",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#TransferInventoryItem",
        controller = "facility"
    )
    public static final String VIEW_TRANSFERINVENTORYITEM = "TransferInventoryItem";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "TransferInventoryItemDetail",
        type = "screen",
        page = "component://product/widget/facility/FacilityScreens.xml#TransferInventoryItemDetail",
        controller = "facility"
    )
    public static final String VIEW_TRANSFERINVENTORYITEMDETAIL = "TransferInventoryItemDetail";


    // Auto-generated split (Part 2)
    public static class Part2 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ReceiveInventory",
            type = "screen",
            page = "component://product/widget/facility/FacilityScreens.xml#ReceiveInventory",
            controller = "facility"
        )
        public static final String VIEW_RECEIVEINVENTORY = "ReceiveInventory";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "UpdatedInventoryItemStatus",
            type = "screen",
            page = "component://product/widget/facility/FacilityScreens.xml#UpdatedInventoryItemStatus",
            controller = "facility"
        )
        public static final String VIEW_UPDATEDINVENTORYITEMSTATUS = "UpdatedInventoryItemStatus";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindFacilityPhysicalInventory",
            type = "screen",
            page = "component://product/widget/facility/FacilityScreens.xml#FindFacilityPhysicalInventory",
            controller = "facility"
        )
        public static final String VIEW_FINDFACILITYPHYSICALINVENTORY = "FindFacilityPhysicalInventory";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PicklistOptions",
            type = "screen",
            page = "component://product/widget/facility/FacilityScreens.xml#PicklistOptions",
            controller = "facility"
        )
        public static final String VIEW_PICKLISTOPTIONS = "PicklistOptions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PicklistManage",
            type = "screen",
            page = "component://product/widget/facility/FacilityScreens.xml#PicklistManage",
            controller = "facility"
        )
        public static final String VIEW_PICKLISTMANAGE = "PicklistManage";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PickMoveStock",
            type = "screen",
            page = "component://product/widget/facility/FacilityScreens.xml#PickMoveStock",
            controller = "facility"
        )
        public static final String VIEW_PICKMOVESTOCK = "PickMoveStock";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PickMoveStockSimple",
            type = "screenfop",
            page = "component://product/widget/facility/FacilityScreens.xml#PickMoveStockSimple.fo",
            contentType = "application/pdf",
            encoding = "none",
            controller = "facility"
        )
        public static final String VIEW_PICKMOVESTOCKSIMPLE = "PickMoveStockSimple";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PicklistReport.pdf",
            type = "screenfop",
            page = "component://product/widget/facility/FacilityScreens.xml#PicklistReport.fo",
            contentType = "application/pdf",
            encoding = "none",
            controller = "facility"
        )
        public static final String VIEW_PICKLISTREPORT_PDF = "PicklistReport.pdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PrintPickSheets.pdf",
            type = "screenfop",
            page = "component://product/widget/facility/FacilityScreens.xml#PrintPickSheets.fo",
            contentType = "application/pdf",
            encoding = "none",
            controller = "facility"
        )
        public static final String VIEW_PRINTPICKSHEETS_PDF = "PrintPickSheets.pdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ReviewOrdersNotPickedOrPacked",
            type = "screen",
            page = "component://product/widget/facility/FacilityScreens.xml#ReviewOrdersNotPickedOrPacked",
            controller = "facility"
        )
        public static final String VIEW_REVIEWORDERSNOTPICKEDORPACKED = "ReviewOrdersNotPickedOrPacked";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PackOrder",
            type = "screen",
            page = "component://product/widget/facility/ShipmentScreens.xml#PackOrder",
            controller = "facility"
        )
        public static final String VIEW_PACKORDER = "PackOrder";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PackingSlip.pdf",
            type = "screenfop",
            page = "component://product/widget/facility/ShipmentScreens.xml#PackingSlip.fo",
            contentType = "application/pdf",
            encoding = "none",
            controller = "facility"
        )
        public static final String VIEW_PACKINGSLIP_PDF = "PackingSlip.pdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ShipmentBarCode.pdf",
            type = "screenfop",
            page = "component://product/widget/facility/ShipmentScreens.xml#ShipmentBarCode.fo",
            contentType = "application/pdf",
            encoding = "none",
            controller = "facility"
        )
        public static final String VIEW_SHIPMENTBARCODE_PDF = "ShipmentBarCode.pdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ShipmentManifest.pdf",
            type = "screenfop",
            page = "component://product/widget/facility/ShipmentScreens.xml#ShipmentManifest.fo",
            contentType = "application/pdf",
            encoding = "none",
            controller = "facility"
        )
        public static final String VIEW_SHIPMENTMANIFEST_PDF = "ShipmentManifest.pdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "VerifyPick",
            type = "screen",
            page = "component://product/widget/facility/ShipmentScreens.xml#VerifyPick",
            controller = "facility"
        )
        public static final String VIEW_VERIFYPICK = "VerifyPick";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WeightPackageOnly",
            type = "screen",
            page = "component://product/widget/facility/ShipmentScreens.xml#WeightPackageOnly",
            controller = "facility"
        )
        public static final String VIEW_WEIGHTPACKAGEONLY = "WeightPackageOnly";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ScheduleShipmentRouteSegment",
            type = "screen",
            page = "component://product/widget/facility/FacilityScreens.xml#ScheduleShipmentRouteSegment",
            controller = "facility"
        )
        public static final String VIEW_SCHEDULESHIPMENTROUTESEGMENT = "ScheduleShipmentRouteSegment";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "Labels",
            type = "screen",
            page = "component://product/widget/facility/FacilityScreens.xml#Labels",
            controller = "facility"
        )
        public static final String VIEW_LABELS = "Labels";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "BatchPrintShippingLabels",
            type = "screenfop",
            page = "component://product/widget/facility/FacilityScreens.xml#BatchPrintShippingLabels",
            contentType = "application/pdf",
            encoding = "none",
            controller = "facility"
        )
        public static final String VIEW_BATCHPRINTSHIPPINGLABELS = "BatchPrintShippingLabels";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFacilityGeoPoint",
            type = "screen",
            page = "component://product/widget/facility/FacilityScreens.xml#EditFacilityGeoPoint",
            controller = "facility"
        )
        public static final String VIEW_EDITFACILITYGEOPOINT = "EditFacilityGeoPoint";

    }

    // Auto-generated split (Part 3)
    public static class Part3 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindShipment",
            type = "screen",
            page = "component://product/widget/facility/ShipmentScreens.xml#FindShipment",
            controller = "facility"
        )
        public static final String VIEW_FINDSHIPMENT = "FindShipment";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditShipment",
            type = "screen",
            page = "component://product/widget/facility/ShipmentScreens.xml#EditShipment",
            controller = "facility"
        )
        public static final String VIEW_EDITSHIPMENT = "EditShipment";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditShipmentItems",
            type = "screen",
            page = "component://product/widget/facility/ShipmentScreens.xml#EditShipmentItems",
            controller = "facility"
        )
        public static final String VIEW_EDITSHIPMENTITEMS = "EditShipmentItems";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditShipmentPlan",
            type = "screen",
            page = "component://product/widget/facility/ShipmentScreens.xml#EditShipmentPlan",
            controller = "facility"
        )
        public static final String VIEW_EDITSHIPMENTPLAN = "EditShipmentPlan";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewShipmentReceipts",
            type = "screen",
            page = "component://product/widget/facility/ShipmentScreens.xml#ViewShipmentReceipts",
            controller = "facility"
        )
        public static final String VIEW_VIEWSHIPMENTRECEIPTS = "ViewShipmentReceipts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditShipmentPackages",
            type = "screen",
            page = "component://product/widget/facility/ShipmentScreens.xml#EditShipmentPackages",
            controller = "facility"
        )
        public static final String VIEW_EDITSHIPMENTPACKAGES = "EditShipmentPackages";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditShipmentRouteSegments",
            type = "screen",
            page = "component://product/widget/facility/ShipmentScreens.xml#EditShipmentRouteSegments",
            controller = "facility"
        )
        public static final String VIEW_EDITSHIPMENTROUTESEGMENTS = "EditShipmentRouteSegments";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddItemsFromOrder",
            type = "screen",
            page = "component://product/widget/facility/ShipmentScreens.xml#AddItemsFromOrder",
            controller = "facility"
        )
        public static final String VIEW_ADDITEMSFROMORDER = "AddItemsFromOrder";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddItemsFromInventory",
            type = "screen",
            page = "component://product/widget/facility/ShipmentScreens.xml#AddItemsFromInventory",
            controller = "facility"
        )
        public static final String VIEW_ADDITEMSFROMINVENTORY = "AddItemsFromInventory";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ReceiveInventoryAgainstPurchaseOrder",
            type = "screen",
            page = "component://product/widget/facility/ShipmentScreens.xml#ReceiveInventoryAgainstPurchaseOrder",
            controller = "facility"
        )
        public static final String VIEW_RECEIVEINVENTORYAGAINSTPURCHASEORDER = "ReceiveInventoryAgainstPurchaseOrder";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "QuickShipOrder",
            type = "screen",
            page = "component://product/widget/facility/ShipmentScreens.xml#QuickShipOrder",
            controller = "facility"
        )
        public static final String VIEW_QUICKSHIPORDER = "QuickShipOrder";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "InventoryReports",
            type = "screen",
            page = "component://product/widget/facility/ReportScreens.xml#InventoryReports",
            controller = "facility"
        )
        public static final String VIEW_INVENTORYREPORTS = "InventoryReports";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "InventoryItemTotals",
            type = "screen",
            page = "component://product/widget/facility/FacilityScreens.xml#InventoryItemTotals",
            controller = "facility"
        )
        public static final String VIEW_INVENTORYITEMTOTALS = "InventoryItemTotals";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "InventoryItemGrandTotals",
            type = "screen",
            page = "component://product/widget/facility/FacilityScreens.xml#InventoryItemGrandTotals",
            controller = "facility"
        )
        public static final String VIEW_INVENTORYITEMGRANDTOTALS = "InventoryItemGrandTotals";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "InventoryAverageCosts",
            type = "screen",
            page = "component://product/widget/facility/FacilityScreens.xml#InventoryAverageCosts",
            controller = "facility"
        )
        public static final String VIEW_INVENTORYAVERAGECOSTS = "InventoryAverageCosts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "InventoryItemTotalsExport",
            type = "screencsv",
            page = "component://product/widget/facility/FacilityScreens.xml#InventoryItemTotalsExport",
            contentType = "text/csv",
            encoding = "none",
            controller = "facility"
        )
        public static final String VIEW_INVENTORYITEMTOTALSEXPORT = "InventoryItemTotalsExport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FacilityLocationGeoLocation",
            type = "screen",
            page = "component://product/widget/facility/FacilityScreens.xml#FacilityLocationGeoLocation",
            controller = "facility"
        )
        public static final String VIEW_FACILITYLOCATIONGEOLOCATION = "FacilityLocationGeoLocation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "GetPartyGeoLocation",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#GetPartyGeoLocation",
            controller = "facility"
        )
        public static final String VIEW_GETPARTYGEOLOCATION = "GetPartyGeoLocation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupOrderHeaderAndShipInfo",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupOrderHeaderAndShipInfo",
            controller = "facility"
        )
        public static final String VIEW_LOOKUPORDERHEADERANDSHIPINFO = "LookupOrderHeaderAndShipInfo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPurchaseOrderHeaderAndShipInfo",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupPurchaseOrderHeaderAndShipInfo",
            controller = "facility"
        )
        public static final String VIEW_LOOKUPPURCHASEORDERHEADERANDSHIPINFO = "LookupPurchaseOrderHeaderAndShipInfo";

    }

    // Auto-generated split (Part 4)
    public static class Part4 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupOrderHeader",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupOrderHeader",
            controller = "facility"
        )
        public static final String VIEW_LOOKUPORDERHEADER = "LookupOrderHeader";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProduct",
            controller = "facility"
        )
        public static final String VIEW_LOOKUPPRODUCT = "LookupProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupVariantProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupVariantProduct",
            controller = "facility"
        )
        public static final String VIEW_LOOKUPVARIANTPRODUCT = "LookupVariantProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProductCategory",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProductCategory",
            controller = "facility"
        )
        public static final String VIEW_LOOKUPPRODUCTCATEGORY = "LookupProductCategory";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupFacility",
            type = "screen",
            page = "component://product/widget/facility/LookupScreens.xml#LookupFacility",
            controller = "facility"
        )
        public static final String VIEW_LOOKUPFACILITY = "LookupFacility";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupFacilityLocation",
            type = "screen",
            page = "component://product/widget/facility/LookupScreens.xml#LookupFacilityLocation",
            controller = "facility"
        )
        public static final String VIEW_LOOKUPFACILITYLOCATION = "LookupFacilityLocation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProductInventoryLocation",
            type = "screen",
            page = "component://product/widget/facility/LookupScreens.xml#LookupProductInventoryLocation",
            controller = "facility"
        )
        public static final String VIEW_LOOKUPPRODUCTINVENTORYLOCATION = "LookupProductInventoryLocation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyName",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyName",
            controller = "facility"
        )
        public static final String VIEW_LOOKUPPARTYNAME = "LookupPartyName";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupInventoryItem",
            type = "screen",
            page = "component://product/widget/facility/LookupScreens.xml#LookupInventoryItem",
            controller = "facility"
        )
        public static final String VIEW_LOOKUPINVENTORYITEM = "LookupInventoryItem";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupContent",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#LookupContent",
            controller = "facility"
        )
        public static final String VIEW_LOOKUPCONTENT = "LookupContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindShipmentGatewayConfig",
            type = "screen",
            page = "component://product/widget/facility/ShipmentGatewayConfigScreens.xml#FindShipmentGatewayConfig",
            controller = "facility"
        )
        public static final String VIEW_FINDSHIPMENTGATEWAYCONFIG = "FindShipmentGatewayConfig";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditShipmentGatewayConfig",
            type = "screen",
            page = "component://product/widget/facility/ShipmentGatewayConfigScreens.xml#EditShipmentGatewayConfig",
            controller = "facility"
        )
        public static final String VIEW_EDITSHIPMENTGATEWAYCONFIG = "EditShipmentGatewayConfig";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindShipmentGatewayConfigTypes",
            type = "screen",
            page = "component://product/widget/facility/ShipmentGatewayConfigScreens.xml#FindShipmentGatewayConfigTypes",
            controller = "facility"
        )
        public static final String VIEW_FINDSHIPMENTGATEWAYCONFIGTYPES = "FindShipmentGatewayConfigTypes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditShipmentGatewayConfigType",
            type = "screen",
            page = "component://product/widget/facility/ShipmentGatewayConfigScreens.xml#EditShipmentGatewayConfigType",
            controller = "facility"
        )
        public static final String VIEW_EDITSHIPMENTGATEWAYCONFIGTYPE = "EditShipmentGatewayConfigType";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FacilitySettings",
            type = "screen",
            page = "component://product/widget/facility/FacilityScreens.xml#ViewContactMechs",
            controller = "facility"
        )
        public static final String VIEW_FACILITYSETTINGS = "FacilitySettings";

        @Request(
            uri = "view",
            controller = "facility",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "main")
        public interface ViewDef {}

        @Request(
            uri = "main",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        public interface Main {}

        @Request(
            uri = "FindFacility",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindFacility")
        public interface FindFacility {}

        @Request(
            uri = "FacilitySearchResults",
            controller = "facility",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "FacilitySearchResults")
        public interface FacilitySearchResults {}

        @Request(
            uri = "EditFacility",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFacility")
        public interface EditFacility {}

    }

    // Auto-generated split (Part 5)
    public static class Part5 {
        @Request(
            uri = "CreateFacility",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFacility")
        @Response(name = "error", type = "view", value = "EditFacility")
        @Event(type = "service", invoke = "createFacility")
        public static String createFacility(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateFacility",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFacility")
        @Response(name = "error", type = "view", value = "EditFacility")
        @Event(type = "service", invoke = "updateFacility")
        public static String updateFacility(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindFacilityLocation",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindFacilityLocation")
        public interface FindFacilityLocation {}

        @Request(
            uri = "EditFacilityLocation",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFacilityLocation")
        public interface EditFacilityLocation {}

        @Request(
            uri = "CreateFacilityLocation",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFacilityLocation")
        @Response(name = "error", type = "view", value = "EditFacilityLocation")
        @Event(type = "service", invoke = "createFacilityLocation")
        public static String createFacilityLocation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateFacilityLocation",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFacilityLocation")
        @Response(name = "error", type = "view", value = "EditFacilityLocation")
        @Event(type = "service", invoke = "updateFacilityLocation")
        public static String updateFacilityLocation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductFacilityLocation",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFacilityLocation")
        @Response(name = "error", type = "view", value = "EditFacilityLocation")
        @Event(type = "service", invoke = "createProductFacilityLocation")
        public static String createProductFacilityLocation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductFacilityLocation",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFacilityLocation")
        @Response(name = "error", type = "view", value = "EditFacilityLocation")
        @Event(type = "service", invoke = "updateProductFacilityLocation")
        public static String updateProductFacilityLocation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductFacilityLocation",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFacilityLocation")
        @Response(name = "error", type = "view", value = "EditFacilityLocation")
        @Event(type = "service", invoke = "deleteProductFacilityLocation")
        public static String deleteProductFacilityLocation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditFacilityInventoryItems",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFacilityInventoryItems")
        public interface EditFacilityInventoryItems {}

        @Request(
            uri = "SearchInventoryItemsByLabels",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SearchInventoryItemsByLabels")
        public interface SearchInventoryItemsByLabels {}

        @Request(
            uri = "ViewFacilityInventoryByProduct",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewFacilityInventoryByProduct")
        public interface ViewFacilityInventoryByProduct {}

        @Request(
            uri = "ViewFacilityInventoryByProductSimple",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewFacilityInventoryByProductSimple")
        public interface ViewFacilityInventoryByProductSimple {}

        @Request(
            uri = "ViewFacilityInventoryByProductReport",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewFacilityInventoryByProductReport")
        public interface ViewFacilityInventoryByProductReport {}

        @Request(
            uri = "ViewFacilityInventoryByProductExport",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewFacilityInventoryByProductExport")
        public interface ViewFacilityInventoryByProductExport {}

        @Request(
            uri = "FindFacilityTransfers",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindFacilityTransfers")
        public interface FindFacilityTransfers {}

        @Request(
            uri = "ViewContactMechs",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewContactMechs")
        public interface ViewContactMechs {}

        @Request(
            uri = "EditContactMech",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactMech")
        public interface EditContactMech {}

        @Request(
            uri = "createContactMech",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactMech")
        @Response(name = "error", type = "view", value = "EditContactMech")
        @Event(type = "service", invoke = "createFacilityContactMech")
        public static String createContactMech(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContactMech",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactMech")
        @Response(name = "error", type = "view", value = "EditContactMech")
        @Event(type = "service", invoke = "updateFacilityContactMech")
        public static String updateContactMech(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 6)
    public static class Part6 {
        @Request(
            uri = "deleteContactMech",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactMech")
        @Response(name = "error", type = "view", value = "EditContactMech")
        @Event(type = "service", invoke = "deleteFacilityContactMech")
        public static String deleteContactMech(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPostalAddressAndPurpose",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactMech")
        @Response(name = "error", type = "view", value = "EditContactMech")
        @Event(type = "service", invoke = "createFacilityPostalAddress")
        public static String createPostalAddressAndPurpose(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPostalAddress",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactMech")
        @Response(name = "error", type = "view", value = "EditContactMech")
        @Event(type = "service", invoke = "createFacilityPostalAddress")
        public static String createPostalAddress(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePostalAddress",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactMech")
        @Response(name = "error", type = "view", value = "EditContactMech")
        @Event(type = "service", invoke = "updateFacilityPostalAddress")
        public static String updatePostalAddress(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createTelecomNumber",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactMech")
        @Response(name = "error", type = "view", value = "EditContactMech")
        @Event(type = "service", invoke = "createFacilityTelecomNumber")
        public static String createTelecomNumber(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTelecomNumber",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactMech")
        @Response(name = "error", type = "view", value = "EditContactMech")
        @Event(type = "service", invoke = "updateFacilityTelecomNumber")
        public static String updateTelecomNumber(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createEmailAddress",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactMech")
        @Response(name = "error", type = "view", value = "EditContactMech")
        @Event(type = "service", invoke = "createFacilityEmailAddress")
        public static String createEmailAddress(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmailAddress",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactMech")
        @Response(name = "error", type = "view", value = "EditContactMech")
        @Event(type = "service", invoke = "updateFacilityEmailAddress")
        public static String updateEmailAddress(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createFacilityContactMechPurpose",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactMech")
        @Response(name = "error", type = "view", value = "EditContactMech")
        @Event(type = "service", invoke = "createFacilityContactMechPurpose")
        public static String createFacilityContactMechPurpose(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteFacilityContactMechPurpose",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContactMech")
        @Response(name = "error", type = "view", value = "EditContactMech")
        @Event(type = "service", invoke = "deleteFacilityContactMechPurpose")
        public static String deleteFacilityContactMechPurpose(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditInventoryItem",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditInventoryItem")
        public interface EditInventoryItem {}

        @Request(
            uri = "ViewInventoryItemDetail",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewInventoryItemDetail")
        public interface ViewInventoryItemDetail {}

        @Request(
            uri = "EditInventoryItemLabels",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditInventoryItemLabels")
        public interface EditInventoryItemLabels {}

        @Request(
            uri = "CreateInventoryItem",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditInventoryItem")
        @Response(name = "error", type = "view", value = "EditInventoryItem")
        @Event(type = "service", invoke = "createInventoryItem")
        public static String createInventoryItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateInventoryItem",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditInventoryItem")
        @Response(name = "error", type = "view", value = "EditInventoryItem")
        @Event(type = "service", invoke = "updateInventoryItem")
        public static String updateInventoryItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPhysicalInventoryAndVariance",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditInventoryItem")
        @Response(name = "error", type = "view", value = "EditInventoryItem")
        @Event(type = "service", invoke = "createPhysicalInventoryAndVariance")
        public static String createPhysicalInventoryAndVariance(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPhysicalVariances",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindFacilityPhysicalInventory")
        @Response(name = "error", type = "view", value = "FindFacilityPhysicalInventory")
        @Event(type = "service-multi", invoke = "createPhysicalInventoryAndVariance")
        public static String createPhysicalVariances(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createInventoryItemLabelApplFromItem",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditInventoryItemLabels")
        @Response(name = "error", type = "view", value = "EditInventoryItemLabels")
        @Event(type = "service", invoke = "createInventoryItemLabelAppl")
        public static String createInventoryItemLabelApplFromItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateInventoryItemLabelApplFromItem",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditInventoryItemLabels")
        @Response(name = "error", type = "view", value = "EditInventoryItemLabels")
        @Event(type = "service", invoke = "updateInventoryItemLabelAppl")
        public static String updateInventoryItemLabelApplFromItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteInventoryItemLabelApplFromItem",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditInventoryItemLabels")
        @Response(name = "error", type = "view", value = "EditInventoryItemLabels")
        @Event(type = "service", invoke = "deleteInventoryItemLabelAppl")
        public static String deleteInventoryItemLabelApplFromItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 7)
    public static class Part7 {
        @Request(
            uri = "FindFacilityPhysicalInventory",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindFacilityPhysicalInventory")
        public interface FindFacilityPhysicalInventory {}

        @Request(
            uri = "cancelReceivedItems",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ReceiveInventory")
        @Response(name = "error", type = "view", value = "ReceiveInventory")
        @Event(type = "service", invoke = "cancelReceivedItems")
        public static String cancelReceivedItems(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "TransferInventoryItem",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "TransferInventoryItem")
        public interface TransferInventoryItem {}

        @Request(
            uri = "TransferInventoryItemDetail",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "TransferInventoryItemDetail")
        @Response(name = "error", type = "none")
        public interface TransferInventoryItemDetail {}

        @Request(
            uri = "CreateInventoryTransfer",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindFacilityTransfers")
        @Response(name = "error", type = "view", value = "FindFacilityTransfers")
        @Event(type = "service", invoke = "createInventoryTransfer")
        public static String createInventoryTransfer(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateInventoryTransfer",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindFacilityTransfers")
        @Response(name = "error", type = "view", value = "FindFacilityTransfers")
        @Event(type = "service", invoke = "updateInventoryTransfer")
        public static String updateInventoryTransfer(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "CompleteRequestedTransfers",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindFacilityTransfers")
        @Response(name = "error", type = "view", value = "FindFacilityTransfers")
        @Event(type = "service-multi", invoke = "updateInventoryTransfer")
        public static String completeRequestedTransfers(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ReceiveInventory",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ReceiveInventory")
        public interface ReceiveInventory {}

        @Request(
            uri = "receiveInventoryProduct",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "checkForceShipmentReceived")
        @Response(name = "error", type = "view", value = "ReceiveInventory")
        @Event(type = "service-multi", invoke = "receiveInventoryProduct")
        public static String receiveInventoryProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "checkForceShipmentReceived",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ReceiveInventory")
        @Response(name = "error", type = "view", value = "ReceiveInventory")
        public static String checkForceShipmentReceived(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.shipment.shipment.ShipmentEvents.checkForceShipmentReceived
            return ShipmentEvents.checkForceShipmentReceived(request, response);
        }

        @Request(
            uri = "receiveSingleInventoryProduct",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "EditInventoryItem")
        @Response(name = "error", type = "view", value = "ReceiveInventory")
        @Event(type = "service", invoke = "receiveInventoryProduct")
        public static String receiveSingleInventoryProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdatedInventoryItemStatus",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "UpdatedInventoryItemStatus")
        public interface UpdatedInventoryItemStatus {}

        @Request(
            uri = "PicklistOptions",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PicklistOptions")
        public interface PicklistOptions {}

        @Request(
            uri = "createPicklistFromOrders",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PicklistOptions")
        @Response(name = "error", type = "view", value = "PicklistOptions")
        @Event(type = "service", invoke = "createPicklistFromOrders")
        public static String createPicklistFromOrders(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "PicklistManage",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PicklistManage")
        public interface PicklistManage {}

        @Request(
            uri = "updatePicklist",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PicklistManage")
        @Response(name = "error", type = "view", value = "PicklistManage")
        @Event(type = "service", invoke = "updatePicklist")
        public static String updatePicklist(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPicklistRole",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PicklistManage")
        @Response(name = "error", type = "view", value = "PicklistManage")
        @Event(type = "service", invoke = "createPicklistRole")
        public static String createPicklistRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePicklistBin",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PicklistManage")
        @Response(name = "error", type = "view", value = "PicklistManage")
        @Event(type = "service", invoke = "updatePicklistBin")
        public static String updatePicklistBin(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePicklistBin",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PicklistManage")
        @Response(name = "error", type = "view", value = "PicklistManage")
        @Event(type = "service", invoke = "deletePicklistBin")
        public static String deletePicklistBin(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePicklistItem",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PicklistManage")
        @Response(name = "error", type = "view", value = "PicklistManage")
        @Event(type = "service", invoke = "deletePicklistItem")
        public static String deletePicklistItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 8)
    public static class Part8 {
        @Request(
            uri = "editPicklistItem",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PicklistManage")
        @Response(name = "error", type = "view", value = "PicklistManage")
        @Event(type = "service", invoke = "editPicklistItem")
        public static String editPicklistItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "PicklistReport.pdf",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PicklistReport.pdf")
        public interface PicklistReportPdf {}

        @Request(
            uri = "PickMoveStock",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PickMoveStock")
        public interface PickMoveStock {}

        @Request(
            uri = "PickMoveStockSimple",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PickMoveStockSimple")
        public interface PickMoveStockSimple {}

        @Request(
            uri = "processPhysicalStockMove",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PickMoveStock")
        @Response(name = "error", type = "view", value = "PickMoveStock")
        @Event(type = "service-multi", invoke = "processPhysicalStockMove")
        public static String processPhysicalStockMove(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "processQuickStockMove",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PickMoveStock")
        @Response(name = "error", type = "view", value = "PickMoveStock")
        @Event(type = "service", invoke = "processPhysicalStockMove")
        public static String processQuickStockMove(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "printPickSheets",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PrintPickSheets.pdf")
        @Response(name = "error", type = "view", value = "PicklistOptions")
        @Event(type = "service", invoke = "printPickSheets")
        public static String printPickSheets(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ReviewOrdersNotPickedOrPacked",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ReviewOrdersNotPickedOrPacked")
        public interface ReviewOrdersNotPickedOrPacked {}

        @Request(
            uri = "VerifyPick",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "VerifyPick")
        public interface VerifyPick {}

        @Request(
            uri = "processVerifyPick",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "VerifyPick")
        @Response(name = "error", type = "view", value = "VerifyPick")
        @Event(type = "service", invoke = "verifySingleItem")
        public static String processVerifyPick(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "processBulkVerifyPick",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "VerifyPick")
        @Response(name = "error", type = "view", value = "VerifyPick")
        @Event(type = "service-multi", invoke = "verifyBulkItem")
        public static String processBulkVerifyPick(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "cancelAllRows",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "VerifyPick")
        @Response(name = "error", type = "view", value = "VerifyPick")
        @Event(type = "service", invoke = "cancelAllRows")
        public static String cancelAllRows(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "completeVerifiedPick",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "VerifyPick")
        @Response(name = "error", type = "view", value = "VerifyPick")
        @Event(type = "service", invoke = "completeVerifiedPick")
        public static String completeVerifiedPick(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "WeightPackageOnly",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WeightPackageOnly")
        @Response(name = "error", type = "view", value = "WeightPackageOnly")
        public interface WeightPackageOnly {}

        @Request(
            uri = "setPackageInfo",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WeightPackageOnly")
        @Response(name = "error", type = "view", value = "WeightPackageOnly")
        @Event(type = "service", invoke = "setPackageInfo")
        public static String setPackageInfo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePackedLine",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WeightPackageOnly")
        @Response(name = "error", type = "view", value = "WeightPackageOnly")
        @Event(type = "service", invoke = "updatePackedLine")
        public static String updatePackedLine(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePackedLine",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WeightPackageOnly")
        @Response(name = "error", type = "view", value = "WeightPackageOnly")
        @Event(type = "service", invoke = "deletePackedLine")
        public static String deletePackedLine(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "shipNow",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WeightPackageOnly")
        @Response(name = "error", type = "view", value = "WeightPackageOnly")
        @Event(type = "service", invoke = "completeShipment")
        public static String shipNow(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "HoldShipment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WeightPackageOnly")
        @Response(name = "error", type = "view", value = "WeightPackageOnly")
        public interface HoldShipment {}

        @Request(
            uri = "completePackage",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WeightPackageOnly")
        @Response(name = "error", type = "request", value = "savePackagesInfo")
        @Event(type = "service", invoke = "completePackage")
        public static String completePackage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 9)
    public static class Part9 {
        @Request(
            uri = "savePackagesInfo",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WeightPackageOnly")
        @Response(name = "error", type = "view", value = "WeightPackageOnly")
        @Event(type = "service", invoke = "savePackagesInfo")
        public static String savePackagesInfo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "PackOrder",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PackOrder")
        public interface PackOrder {}

        @Request(
            uri = "ProcessPackOrder",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PackOrder")
        @Response(name = "error", type = "view", value = "PackOrder")
        @Event(type = "service", invoke = "packSingleItem")
        public static String processPackOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ProcessBulkPackOrder",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PackOrder")
        @Response(name = "error", type = "view", value = "PackOrder")
        @Event(type = "service-multi", invoke = "packBulkItems")
        public static String processBulkPackOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "SetNextPackageSeq",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PackOrder")
        @Response(name = "error", type = "view", value = "PackOrder")
        @Event(type = "service", invoke = "setNextPackageSeq")
        public static String setNextPackageSeq(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ClearPackLine",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PackOrder")
        @Response(name = "error", type = "view", value = "PackOrder")
        @Event(type = "service", invoke = "clearPackLine")
        public static String clearPackLine(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ClearPackAll",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PackOrder")
        @Response(name = "error", type = "view", value = "PackOrder")
        @Event(type = "service", invoke = "clearPackAll")
        public static String clearPackAll(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "calcPackSessionAdditionalShippingCharge",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PackOrder")
        @Response(name = "error", type = "view", value = "PackOrder")
        @Event(type = "service", invoke = "calcPackSessionAdditionalShippingCharge")
        public static String calcPackSessionAdditionalShippingCharge(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "CompletePack",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PackOrder")
        @Response(name = "error", type = "view", value = "PackOrder")
        @Event(type = "service", invoke = "completePack")
        public static String completePack(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "PackingSlip.pdf",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PackingSlip.pdf")
        public interface PackingSlipPdf {}

        @Request(
            uri = "ShipmentBarCode.pdf",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ShipmentBarCode.pdf")
        public interface ShipmentBarCodePdf {}

        @Request(
            uri = "ShipmentManifest.pdf",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ShipmentManifest.pdf")
        public interface ShipmentManifestPdf {}

        @Request(
            uri = "EditFacilityGeoPoint",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFacilityGeoPoint")
        public interface EditFacilityGeoPoint {}

        @Request(
            uri = "createUpdateFacilityGeoPoint",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFacilityGeoPoint")
        @Response(name = "error", type = "view", value = "EditFacilityGeoPoint")
        @Event(type = "service", invoke = "createUpdateFacilityGeoPoint")
        public static String createUpdateFacilityGeoPoint(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "Scheduling",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ScheduleShipmentRouteSegment")
        public interface Scheduling {}

        @Request(
            uri = "ScheduleShipmentRouteSegment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ScheduleShipmentRouteSegment")
        public interface ScheduleShipmentRouteSegment {}

        @Request(
            uri = "BatchScheduleShipmentRouteSegments",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "error", type = "view", value = "ScheduleShipmentRouteSegment")
        @Response(name = "success", type = "request", value = "ScheduleShipmentsWithCarriers")
        @Event(type = "service-multi", invoke = "updateShipmentRouteSegment")
        public static String batchScheduleShipmentRouteSegments(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "BatchUpdateShipmentRouteSegments",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Labels")
        @Response(name = "error", type = "view", value = "Labels")
        @Event(type = "service-multi", invoke = "updateShipmentRouteSegment")
        public static String batchUpdateShipmentRouteSegments(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ScheduleShipmentsWithCarriers",
            controller = "facility",
            secure = "true",
            auth = "true",
            directRequest = "false"
        )
        @Response(name = "error", type = "view", value = "ScheduleShipmentRouteSegment")
        @Response(name = "success", type = "view", value = "Labels")
        @Event(type = "service-multi", invoke = "quickScheduleShipmentRouteSegment")
        public static String scheduleShipmentsWithCarriers(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "Labels",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Labels")
        public interface Labels {}

    }

    // Auto-generated split (Part 10)
    public static class Part10 {
        @Request(
            uri = "BatchPrintShippingLabels",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BatchPrintShippingLabels")
        public interface BatchPrintShippingLabels {}

        @Request(
            uri = "FindShipment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindShipment")
        public interface FindShipment {}

        @Request(
            uri = "EditShipment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipment")
        public interface EditShipment {}

        @Request(
            uri = "createShipment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditShipment")
        @Response(name = "error", type = "view", value = "EditShipment")
        @Event(type = "service", invoke = "createShipment")
        public static String createShipment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateShipment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipment")
        @Response(name = "error", type = "view", value = "EditShipment")
        @Event(type = "service", invoke = "updateShipment")
        public static String updateShipment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createShipmentAndItemsForVendorReturn",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipment")
        @Response(name = "error", type = "view", value = "EditShipment")
        @Event(type = "service", invoke = "createShipmentAndItemsForVendorReturn")
        public static String createShipmentAndItemsForVendorReturn(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setShipmentSettingsFromPrimaryOrder",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipment")
        @Response(name = "error", type = "view", value = "EditShipment")
        @Event(type = "service", invoke = "setShipmentSettingsFromPrimaryOrder")
        public static String setShipmentSettingsFromPrimaryOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickShipOrder",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "QuickShipOrder")
        public interface QuickShipOrder {}

        @Request(
            uri = "quickShipPurchaseOrder",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ReceiveInventory")
        @Response(name = "error", type = "view", value = "ReceiveInventory")
        @Event(type = "service", invoke = "quickShipPurchaseOrder")
        public static String quickShipPurchaseOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createQuickShipment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "QuickShipOrder")
        @Response(name = "error", type = "view", value = "QuickShipOrder")
        @Event(type = "service", invoke = "quickShipEntireOrder")
        public static String createQuickShipment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setQuickPackageWeight",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "QuickShipOrder")
        @Response(name = "error", type = "view", value = "QuickShipOrder")
        @Event(type = "service", invoke = "updateShipmentPackage")
        public static String setQuickPackageWeight(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setQuickRouteInfo",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "quickUpsConfirm")
        @Response(name = "error", type = "view", value = "QuickShipOrder")
        @Event(type = "service", invoke = "updateShipmentRouteSegment")
        public static String setQuickRouteInfo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickUpsConfirm",
            controller = "facility",
            secure = "true",
            auth = "true",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "quickUpsAccept")
        @Response(name = "error", type = "view", value = "QuickShipOrder")
        @Event(type = "service", invoke = "upsShipmentConfirm")
        public static String quickUpsConfirm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickUpsAccept",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "QuickShipOrder")
        @Response(name = "error", type = "view", value = "QuickShipOrder")
        @Event(type = "service", invoke = "upsShipmentAccept")
        public static String quickUpsAccept(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickDhlConfirm",
            controller = "facility",
            secure = "true",
            auth = "true",
            directRequest = "false"
        )
        @Response(name = "success", type = "view", value = "QuickShipOrder")
        @Response(name = "error", type = "view", value = "QuickShipOrder")
        @Event(type = "service", invoke = "dhlShipmentConfirm")
        public static String quickDhlConfirm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditShipmentItems",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentItems")
        public interface EditShipmentItems {}

        @Request(
            uri = "createShipmentItem",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentItems")
        @Response(name = "error", type = "view", value = "EditShipmentItems")
        @Event(type = "service", invoke = "createShipmentItem")
        public static String createShipmentItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteShipmentItem",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentItems")
        @Response(name = "error", type = "view", value = "EditShipmentItems")
        @Event(type = "service", invoke = "deleteShipmentItem")
        public static String deleteShipmentItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteShipmentItemIssuance",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentItems")
        @Response(name = "error", type = "view", value = "EditShipmentItems")
        @Event(type = "service", invoke = "deleteItemIssuance")
        public static String deleteShipmentItemIssuance(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createShipmentItemPackageContent",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentItems")
        @Response(name = "error", type = "view", value = "EditShipmentItems")
        @Event(type = "service", invoke = "createShipmentPackageContent")
        public static String createShipmentItemPackageContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 11)
    public static class Part11 {
        @Request(
            uri = "deleteShipmentItemPackageContent",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentItems")
        @Response(name = "error", type = "view", value = "EditShipmentItems")
        @Event(type = "service", invoke = "deleteShipmentPackageContent")
        public static String deleteShipmentItemPackageContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditShipmentPackages",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentPackages")
        public interface EditShipmentPackages {}

        @Request(
            uri = "createShipmentPackage",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentPackages")
        @Response(name = "error", type = "view", value = "EditShipmentPackages")
        @Event(type = "service", invoke = "createShipmentPackage")
        public static String createShipmentPackage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateShipmentPackage",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentPackages")
        @Response(name = "error", type = "view", value = "EditShipmentPackages")
        @Event(type = "service", invoke = "updateShipmentPackage")
        public static String updateShipmentPackage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteShipmentPackage",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentPackages")
        @Response(name = "error", type = "view", value = "EditShipmentPackages")
        @Event(type = "service", invoke = "deleteShipmentPackage")
        public static String deleteShipmentPackage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createShipmentPackageContent",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentPackages")
        @Response(name = "error", type = "view", value = "EditShipmentPackages")
        @Event(type = "service", invoke = "createShipmentPackageContent")
        public static String createShipmentPackageContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteShipmentPackageContent",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentPackages")
        @Response(name = "error", type = "view", value = "EditShipmentPackages")
        @Event(type = "service", invoke = "deleteShipmentPackageContent")
        public static String deleteShipmentPackageContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createShipmentPackageRouteSeg",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentPackages")
        @Response(name = "error", type = "view", value = "EditShipmentPackages")
        @Event(type = "service", invoke = "createShipmentPackageRouteSeg")
        public static String createShipmentPackageRouteSeg(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateShipmentPackageRouteSeg",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentPackages")
        @Response(name = "error", type = "view", value = "EditShipmentPackages")
        @Event(type = "service", invoke = "updateShipmentPackageRouteSeg")
        public static String updateShipmentPackageRouteSeg(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteShipmentPackageRouteSeg",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentPackages")
        @Response(name = "error", type = "view", value = "EditShipmentPackages")
        @Event(type = "service", invoke = "deleteShipmentPackageRouteSeg")
        public static String deleteShipmentPackageRouteSeg(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditShipmentRouteSegments",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentRouteSegments")
        public interface EditShipmentRouteSegments {}

        @Request(
            uri = "createShipmentRouteSegment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentRouteSegments")
        @Response(name = "error", type = "view", value = "EditShipmentRouteSegments")
        @Event(type = "service", invoke = "createShipmentRouteSegment")
        public static String createShipmentRouteSegment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateShipmentRouteSegment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentRouteSegments")
        @Response(name = "error", type = "view", value = "EditShipmentRouteSegments")
        @Event(type = "service", invoke = "updateShipmentRouteSegment")
        public static String updateShipmentRouteSegment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteShipmentRouteSegment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentRouteSegments")
        @Response(name = "error", type = "view", value = "EditShipmentRouteSegments")
        @Event(type = "service", invoke = "deleteShipmentRouteSegment")
        public static String deleteShipmentRouteSegment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "duplicateShipmentRouteSegment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentRouteSegments")
        @Response(name = "error", type = "view", value = "EditShipmentRouteSegments")
        @Event(type = "service", invoke = "duplicateShipmentRouteSegment")
        public static String duplicateShipmentRouteSegment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createRouteSegmentShipmentPackage",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentRouteSegments")
        @Response(name = "error", type = "view", value = "EditShipmentRouteSegments")
        @Event(type = "service", invoke = "createShipmentPackageRouteSeg")
        public static String createRouteSegmentShipmentPackage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateRouteSegmentShipmentPackage",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentRouteSegments")
        @Response(name = "error", type = "view", value = "EditShipmentRouteSegments")
        @Event(type = "service", invoke = "updateShipmentPackageRouteSeg")
        public static String updateRouteSegmentShipmentPackage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteRouteSegmentShipmentPackage",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentRouteSegments")
        @Response(name = "error", type = "view", value = "EditShipmentRouteSegments")
        @Event(type = "service", invoke = "deleteShipmentPackageRouteSeg")
        public static String deleteRouteSegmentShipmentPackage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "upsShipmentConfirm",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentRouteSegments")
        @Response(name = "error", type = "view", value = "EditShipmentRouteSegments")
        @Event(type = "service", invoke = "upsShipmentConfirm")
        public static String upsShipmentConfirm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "upsShipmentAccept",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentRouteSegments")
        @Response(name = "error", type = "view", value = "EditShipmentRouteSegments")
        @Event(type = "service", invoke = "upsShipmentAccept")
        public static String upsShipmentAccept(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 12)
    public static class Part12 {
        @Request(
            uri = "upsVoidShipment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentRouteSegments")
        @Response(name = "error", type = "view", value = "EditShipmentRouteSegments")
        @Event(type = "service", invoke = "upsVoidShipment")
        public static String upsVoidShipment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "upsTrackShipment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentRouteSegments")
        @Response(name = "error", type = "view", value = "EditShipmentRouteSegments")
        @Event(type = "service", invoke = "upsTrackShipment")
        public static String upsTrackShipment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "dhlShipmentConfirm",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditShipmentRouteSegments")
        @Response(name = "error", type = "view", value = "EditShipmentRouteSegments")
        @Event(type = "service", invoke = "dhlShipmentConfirm")
        public static String dhlShipmentConfirm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "viewShipmentPackageRouteSegLabelImage",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "view", value = "EditShipmentRouteSegments")
        public static String viewShipmentPackageRouteSegLabelImage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.shipment.shipment.ShipmentEvents.viewShipmentPackageRouteSegLabelImage
            return ShipmentEvents.viewShipmentPackageRouteSegLabelImage(request, response);
        }

        @Request(
            uri = "viewShipmentLabel",
            controller = "facility"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "view", value = "EditShipmentRouteSegments")
        public static String viewShipmentLabel(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.shipment.shipment.ShipmentEvents.viewShipmentPackageRouteSegLabelImageUnsafe
            return ShipmentEvents.viewShipmentPackageRouteSegLabelImageUnsafe(request, response);
        }

        @Request(
            uri = "fedexShipmentConfirm",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditShipmentRouteSegments")
        @Response(name = "error", type = "view", value = "EditShipmentRouteSegments")
        @Event(type = "service", invoke = "fedexShipRequest")
        public static String fedexShipmentConfirm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "AddItemsFromOrder",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddItemsFromOrder")
        public interface AddItemsFromOrder {}

        @Request(
            uri = "issueOrderItemToShipment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddItemsFromOrder")
        @Response(name = "error", type = "view", value = "AddItemsFromOrder")
        @Event(type = "service-multi", invoke = "issueOrderItemToShipment")
        public static String issueOrderItemToShipment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "issueOrderItemShipGrpInvResToShipment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddItemsFromOrder")
        @Response(name = "error", type = "view", value = "AddItemsFromOrder")
        @Event(type = "service-multi", invoke = "issueOrderItemShipGrpInvResToShipment")
        public static String issueOrderItemShipGrpInvResToShipment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ReceiveInventoryAgainstPurchaseOrder",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ReceiveInventoryAgainstPurchaseOrder")
        public interface ReceiveInventoryAgainstPurchaseOrder {}

        @Request(
            uri = "issueOrderItemToShipmentAndReceiveAgainstPO",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ReceiveInventoryAgainstPurchaseOrder")
        @Response(name = "error", type = "view", value = "ReceiveInventoryAgainstPurchaseOrder")
        @Event(type = "service-multi", invoke = "issueOrderItemToShipmentAndReceiveAgainstPO")
        public static String issueOrderItemToShipmentAndReceiveAgainstPO(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "completePurchaseOrder",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ReceiveInventoryAgainstPurchaseOrder")
        @Response(name = "error", type = "view", value = "ReceiveInventoryAgainstPurchaseOrder")
        @Event(type = "service", invoke = "completePurchaseOrder")
        public static String completePurchaseOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "AddItemsFromInventory",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddItemsFromInventory")
        public interface AddItemsFromInventory {}

        @Request(
            uri = "issueInventoryItemToShipment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddItemsFromInventory")
        @Response(name = "error", type = "view", value = "AddItemsFromInventory")
        @Event(type = "service", invoke = "issueInventoryItemToShipment")
        public static String issueInventoryItemToShipment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditShipmentPlan",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentPlan")
        public interface EditShipmentPlan {}

        @Request(
            uri = "removeOrderShipmentFromShipment",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentPlan")
        @Event(type = "service", invoke = "removeOrderShipmentFromShipment")
        public static String removeOrderShipmentFromShipment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addToShipmentPlan",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentPlan")
        @Response(name = "error", type = "view", value = "EditShipmentPlan")
        @Event(type = "service-multi", invoke = "addOrderShipmentToShipment")
        public static String addToShipmentPlan(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ViewShipmentReceipts",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewShipmentReceipts")
        public interface ViewShipmentReceipts {}

        @Request(
            uri = "InventoryReports",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "InventoryReports")
        public interface InventoryReports {}

        @Request(
            uri = "InventoryItemTotals",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "InventoryItemTotals")
        public interface InventoryItemTotals {}

    }

    // Auto-generated split (Part 13)
    public static class Part13 {
        @Request(
            uri = "InventoryItemGrandTotals",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "InventoryItemGrandTotals")
        public interface InventoryItemGrandTotals {}

        @Request(
            uri = "InventoryAverageCosts",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "InventoryAverageCosts")
        public interface InventoryAverageCosts {}

        @Request(
            uri = "InventoryItemTotalsExport.csv",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "InventoryItemTotalsExport")
        public interface InventoryItemTotalsExportCsv {}

        @Request(
            uri = "FacilityLocationGeoLocation",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FacilityLocationGeoLocation")
        @Response(name = "error", type = "view", value = "EditFacility")
        public interface FacilityLocationGeoLocation {}

        @Request(
            uri = "FindShipmentGatewayConfig",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindShipmentGatewayConfig")
        public interface FindShipmentGatewayConfig {}

        @Request(
            uri = "EditShipmentGatewayConfig",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentGatewayConfig")
        public interface EditShipmentGatewayConfig {}

        @Request(
            uri = "UpdateShipmentGatewayConfig",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentGatewayConfig")
        @Response(name = "error", type = "view", value = "EditShipmentGatewayConfig")
        @Event(type = "service", invoke = "updateShipmentGatewayConfig")
        public static String updateShipmentGatewayConfig(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateShipmentGatewayConfigDhl",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentGatewayConfig")
        @Response(name = "error", type = "view", value = "EditShipmentGatewayConfig")
        @Event(type = "service", invoke = "updateShipmentGatewayConfigDhl")
        public static String updateShipmentGatewayConfigDhl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateShipmentGatewayConfigFedex",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentGatewayConfig")
        @Response(name = "error", type = "view", value = "EditShipmentGatewayConfig")
        @Event(type = "service", invoke = "updateShipmentGatewayConfigFedex")
        public static String updateShipmentGatewayConfigFedex(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateShipmentGatewayConfigUps",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentGatewayConfig")
        @Response(name = "error", type = "view", value = "EditShipmentGatewayConfig")
        @Event(type = "service", invoke = "updateShipmentGatewayConfigUps")
        public static String updateShipmentGatewayConfigUps(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateShipmentGatewayConfigUsps",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentGatewayConfig")
        @Response(name = "error", type = "view", value = "EditShipmentGatewayConfig")
        @Event(type = "service", invoke = "updateShipmentGatewayConfigUsps")
        public static String updateShipmentGatewayConfigUsps(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindShipmentGatewayConfigTypes",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindShipmentGatewayConfigTypes")
        public interface FindShipmentGatewayConfigTypes {}

        @Request(
            uri = "EditShipmentGatewayConfigType",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentGatewayConfigType")
        public interface EditShipmentGatewayConfigType {}

        @Request(
            uri = "UpdateShipmentGatewayConfigType",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipmentGatewayConfigType")
        @Response(name = "error", type = "view", value = "EditShipmentGatewayConfigType")
        @Event(type = "service", invoke = "updateShipmentGatewayConfigType")
        public static String updateShipmentGatewayConfigType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getConvertedPrice",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "convertUom")
        public static String getConvertedPrice(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "GetPartyGeoLocation",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GetPartyGeoLocation")
        public interface GetPartyGeoLocation {}

        @Request(
            uri = "settings",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FacilitySettings")
        public interface Settings {}

        @Request(
            uri = "LookupOrderHeaderAndShipInfo",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupOrderHeaderAndShipInfo")
        public interface LookupOrderHeaderAndShipInfo {}

        @Request(
            uri = "LookupPurchaseOrderHeaderAndShipInfo",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPurchaseOrderHeaderAndShipInfo")
        public interface LookupPurchaseOrderHeaderAndShipInfo {}

        @Request(
            uri = "LookupOrderHeader",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupOrderHeader")
        public interface LookupOrderHeader {}

    }

    // Auto-generated split (Part 14)
    public static class Part14 {
        @Request(
            uri = "LookupProduct",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProduct")
        public interface LookupProduct {}

        @Request(
            uri = "LookupVariantProduct",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupVariantProduct")
        public interface LookupVariantProduct {}

        @Request(
            uri = "LookupProductCategory",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductCategory")
        public interface LookupProductCategory {}

        @Request(
            uri = "LookupFacility",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupFacility")
        public interface LookupFacility {}

        @Request(
            uri = "LookupFacilityLocation",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupFacilityLocation")
        public interface LookupFacilityLocation {}

        @Request(
            uri = "LookupProductInventoryLocation",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductInventoryLocation")
        public interface LookupProductInventoryLocation {}

        @Request(
            uri = "LookupPartyName",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyName")
        public interface LookupPartyName {}

        @Request(
            uri = "LookupInventoryItem",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupInventoryItem")
        public interface LookupInventoryItem {}

        @Request(
            uri = "LookupContent",
            controller = "facility",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupContent")
        public interface LookupContent {}


    }
}
