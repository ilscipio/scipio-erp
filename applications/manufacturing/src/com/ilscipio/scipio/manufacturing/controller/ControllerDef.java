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
package com.ilscipio.scipio.manufacturing.controller;

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Responses;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.manufacturing.jobshopmgt.ProductionRunEvents;
import org.ofbiz.manufacturing.bom.BOMHelper;

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
        page = "component://manufacturing/widget/manufacturing/DashboardScreens.xml#Dashboard",
        controller = "manufacturing"
    )
    public static final String VIEW_MAIN = "main";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindCalendar",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/CalendarScreens.xml#FindCalendar",
        controller = "manufacturing"
    )
    public static final String VIEW_FINDCALENDAR = "FindCalendar";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditCalendar",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/CalendarScreens.xml#EditCalendar",
        controller = "manufacturing"
    )
    public static final String VIEW_EDITCALENDAR = "EditCalendar";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditCalendarWeek",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/CalendarScreens.xml#EditCalendarWeek",
        controller = "manufacturing"
    )
    public static final String VIEW_EDITCALENDARWEEK = "EditCalendarWeek";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ListCalendarWeek",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/CalendarScreens.xml#ListCalendarWeek",
        controller = "manufacturing"
    )
    public static final String VIEW_LISTCALENDARWEEK = "ListCalendarWeek";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditCalendarExceptionDay",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/CalendarScreens.xml#EditCalendarExceptionDay",
        controller = "manufacturing"
    )
    public static final String VIEW_EDITCALENDAREXCEPTIONDAY = "EditCalendarExceptionDay";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditCalendarExceptionWeek",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/CalendarScreens.xml#EditCalendarExceptionWeek",
        controller = "manufacturing"
    )
    public static final String VIEW_EDITCALENDAREXCEPTIONWEEK = "EditCalendarExceptionWeek";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindRoutingTask",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/RoutingScreens.xml#FindRoutingTask",
        controller = "manufacturing"
    )
    public static final String VIEW_FINDROUTINGTASK = "FindRoutingTask";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditRoutingTask",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/RoutingScreens.xml#EditRoutingTask",
        controller = "manufacturing"
    )
    public static final String VIEW_EDITROUTINGTASK = "EditRoutingTask";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditRoutingTaskCosts",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/RoutingScreens.xml#EditRoutingTaskCosts",
        controller = "manufacturing"
    )
    public static final String VIEW_EDITROUTINGTASKCOSTS = "EditRoutingTaskCosts";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ListRoutingTaskRoutings",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/RoutingScreens.xml#ListRoutingTaskRoutings",
        controller = "manufacturing"
    )
    public static final String VIEW_LISTROUTINGTASKROUTINGS = "ListRoutingTaskRoutings";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ListRoutingTaskProducts",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/RoutingScreens.xml#ListRoutingTaskProducts",
        controller = "manufacturing"
    )
    public static final String VIEW_LISTROUTINGTASKPRODUCTS = "ListRoutingTaskProducts";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditRoutingTaskProduct",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/RoutingScreens.xml#EditRoutingTaskProduct",
        controller = "manufacturing"
    )
    public static final String VIEW_EDITROUTINGTASKPRODUCT = "EditRoutingTaskProduct";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindRouting",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/RoutingScreens.xml#FindRouting",
        controller = "manufacturing"
    )
    public static final String VIEW_FINDROUTING = "FindRouting";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditRouting",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/RoutingScreens.xml#EditRouting",
        controller = "manufacturing"
    )
    public static final String VIEW_EDITROUTING = "EditRouting";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditRoutingTaskAssoc",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/RoutingScreens.xml#EditRoutingTaskAssoc",
        controller = "manufacturing"
    )
    public static final String VIEW_EDITROUTINGTASKASSOC = "EditRoutingTaskAssoc";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditRoutingProductLink",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/RoutingScreens.xml#EditRoutingProductLink",
        controller = "manufacturing"
    )
    public static final String VIEW_EDITROUTINGPRODUCTLINK = "EditRoutingProductLink";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditRoutingTaskFixedAssets",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/RoutingScreens.xml#EditRoutingTaskFixedAssets",
        controller = "manufacturing"
    )
    public static final String VIEW_EDITROUTINGTASKFIXEDASSETS = "EditRoutingTaskFixedAssets";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupPerson",
        type = "screen",
        page = "component://party/widget/partymgr/LookupScreens.xml#LookupPerson",
        controller = "manufacturing"
    )
    public static final String VIEW_LOOKUPPERSON = "LookupPerson";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupCustomerName",
        type = "screen",
        page = "component://party/widget/partymgr/LookupScreens.xml#LookupCustomerName",
        controller = "manufacturing"
    )
    public static final String VIEW_LOOKUPCUSTOMERNAME = "LookupCustomerName";


    // Auto-generated split (Part 2)
    public static class Part2 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProduct",
            controller = "manufacturing"
        )
        public static final String VIEW_LOOKUPPRODUCT = "LookupProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupVariantProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupVariantProduct",
            controller = "manufacturing"
        )
        public static final String VIEW_LOOKUPVARIANTPRODUCT = "LookupVariantProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupVirtualProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupVirtualProduct",
            controller = "manufacturing"
        )
        public static final String VIEW_LOOKUPVIRTUALPRODUCT = "LookupVirtualProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupRouting",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/LookupScreens.xml#LookupRouting",
            controller = "manufacturing"
        )
        public static final String VIEW_LOOKUPROUTING = "LookupRouting";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupRoutingTask",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/LookupScreens.xml#LookupRoutingTask",
            controller = "manufacturing"
        )
        public static final String VIEW_LOOKUPROUTINGTASK = "LookupRoutingTask";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProductFeature",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProductFeature",
            controller = "manufacturing"
        )
        public static final String VIEW_LOOKUPPRODUCTFEATURE = "LookupProductFeature";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductBom",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/BomScreens.xml#EditProductBom",
            controller = "manufacturing"
        )
        public static final String VIEW_EDITPRODUCTBOM = "EditProductBom";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductManufacturingRules",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/BomScreens.xml#EditProductManufacturingRules",
            controller = "manufacturing"
        )
        public static final String VIEW_EDITPRODUCTMANUFACTURINGRULES = "EditProductManufacturingRules";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "BomSimulation",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/BomScreens.xml#BomSimulation",
            controller = "manufacturing"
        )
        public static final String VIEW_BOMSIMULATION = "BomSimulation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindBom",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/BomScreens.xml#FindBom",
            controller = "manufacturing"
        )
        public static final String VIEW_FINDBOM = "FindBom";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditCostCalcs",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/CostScreens.xml#EditCostCalcs",
            controller = "manufacturing"
        )
        public static final String VIEW_EDITCOSTCALCS = "EditCostCalcs";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindMrpPlannedEvents",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/MrpScreens.xml#FindMrpPlannedEvents",
            controller = "manufacturing"
        )
        public static final String VIEW_FINDMRPPLANNEDEVENTS = "FindMrpPlannedEvents";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "MrpExecution",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/MrpScreens.xml#MrpExecution",
            controller = "manufacturing"
        )
        public static final String VIEW_MRPEXECUTION = "MrpExecution";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ManufacturingReports",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/DashboardScreens.xml#ReportsHub",
            controller = "manufacturing"
        )
        public static final String VIEW_MANUFACTURINGREPORTS = "ManufacturingReports";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CreateProductionRun",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/JobshopScreens.xml#CreateProductionRun",
            controller = "manufacturing"
        )
        public static final String VIEW_CREATEPRODUCTIONRUN = "CreateProductionRun";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindProductionRun",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/JobshopScreens.xml#FindProductionRun",
            controller = "manufacturing"
        )
        public static final String VIEW_FINDPRODUCTIONRUN = "FindProductionRun";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductionRun",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/JobshopScreens.xml#EditProductionRun",
            controller = "manufacturing"
        )
        public static final String VIEW_EDITPRODUCTIONRUN = "EditProductionRun";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LinkProductionRun",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/JobshopScreens.xml#LinkProductionRun",
            controller = "manufacturing"
        )
        public static final String VIEW_LINKPRODUCTIONRUN = "LinkProductionRun";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PrintProductionRun",
            type = "screenfop",
            page = "component://manufacturing/widget/manufacturing/JobshopScreens.xml#ProductionRunPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "manufacturing"
        )
        public static final String VIEW_PRINTPRODUCTIONRUN = "PrintProductionRun";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ProductionRunDeclaration",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/JobshopScreens.xml#ProductionRunDeclaration",
            controller = "manufacturing"
        )
        public static final String VIEW_PRODUCTIONRUNDECLARATION = "ProductionRunDeclaration";

    }

    // Auto-generated split (Part 3)
    public static class Part3 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ProductionRunCosts",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/JobshopScreens.xml#ProductionRunCosts",
            controller = "manufacturing"
        )
        public static final String VIEW_PRODUCTIONRUNCOSTS = "ProductionRunCosts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ProductionRunTasks",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/JobshopScreens.xml#ProductionRunTasks",
            controller = "manufacturing"
        )
        public static final String VIEW_PRODUCTIONRUNTASKS = "ProductionRunTasks";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ProductionRunComponents",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/JobshopScreens.xml#ProductionRunComponents",
            controller = "manufacturing"
        )
        public static final String VIEW_PRODUCTIONRUNCOMPONENTS = "ProductionRunComponents";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ProductionRunActualComponents",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/JobshopScreens.xml#ProductionRunActualComponents",
            controller = "manufacturing"
        )
        public static final String VIEW_PRODUCTIONRUNACTUALCOMPONENTS = "ProductionRunActualComponents";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ProductionRunFixedAssets",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/JobshopScreens.xml#ProductionRunFixedAssets",
            controller = "manufacturing"
        )
        public static final String VIEW_PRODUCTIONRUNFIXEDASSETS = "ProductionRunFixedAssets";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyName",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyName",
            controller = "manufacturing"
        )
        public static final String VIEW_LOOKUPPARTYNAME = "LookupPartyName";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ProductionRunContent",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/JobshopScreens.xml#ProductionRunContent",
            controller = "manufacturing"
        )
        public static final String VIEW_PRODUCTIONRUNCONTENT = "ProductionRunContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ProductionRunAssocs",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/JobshopScreens.xml#ProductionRunAssocs",
            controller = "manufacturing"
        )
        public static final String VIEW_PRODUCTIONRUNASSOCS = "ProductionRunAssocs";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WorkWithShipmentPlans",
            type = "screen",
            page = "component://manufacturing/widget/manufacturing/JobshopScreens.xml#WorkWithShipmentPlans",
            controller = "manufacturing"
        )
        public static final String VIEW_WORKWITHSHIPMENTPLANS = "WorkWithShipmentPlans";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ShipmentPlanStockReport",
            type = "screenfop",
            page = "component://manufacturing/widget/manufacturing/ReportScreens.xml#ShipmentPlanStockReport",
            contentType = "application/pdf",
            encoding = "none",
            controller = "manufacturing"
        )
        public static final String VIEW_SHIPMENTPLANSTOCKREPORT = "ShipmentPlanStockReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ShipmentLabel",
            type = "screenfop",
            page = "component://manufacturing/widget/manufacturing/ReportScreens.xml#ShipmentLabel",
            contentType = "application/pdf",
            encoding = "none",
            controller = "manufacturing"
        )
        public static final String VIEW_SHIPMENTLABEL = "ShipmentLabel";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ShipmentWorkEffortTasks",
            type = "screenfop",
            page = "component://manufacturing/widget/manufacturing/ReportScreens.xml#ShipmentWorkEffortTasks",
            contentType = "application/pdf",
            encoding = "none",
            controller = "manufacturing"
        )
        public static final String VIEW_SHIPMENTWORKEFFORTTASKS = "ShipmentWorkEffortTasks";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CuttingListReport",
            type = "screenfop",
            page = "component://manufacturing/widget/manufacturing/ReportScreens.xml#CuttingListReport",
            contentType = "application/pdf",
            encoding = "none",
            controller = "manufacturing"
        )
        public static final String VIEW_CUTTINGLISTREPORT = "CuttingListReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PackageContentsAndOrder",
            type = "screenfop",
            page = "component://manufacturing/widget/manufacturing/ReportScreens.xml#PackageContentsAndOrder",
            contentType = "application/pdf",
            encoding = "none",
            controller = "manufacturing"
        )
        public static final String VIEW_PACKAGECONTENTSANDORDER = "PackageContentsAndOrder";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PRunsProductsStacks",
            type = "screenfop",
            page = "component://manufacturing/widget/manufacturing/ReportScreens.xml#PRunsProductsStacks",
            contentType = "application/pdf",
            encoding = "none",
            controller = "manufacturing"
        )
        public static final String VIEW_PRUNSPRODUCTSSTACKS = "PRunsProductsStacks";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PRunsProductsAndOrder",
            type = "screenfop",
            page = "component://manufacturing/widget/manufacturing/ReportScreens.xml#PRunsProductsAndOrder",
            contentType = "application/pdf",
            encoding = "none",
            controller = "manufacturing"
        )
        public static final String VIEW_PRUNSPRODUCTSANDORDER = "PRunsProductsAndOrder";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "MRPPRunsProductsByFeature",
            type = "screenfop",
            page = "component://manufacturing/widget/manufacturing/ReportScreens.xml#MRPPRunsProductsByFeature",
            contentType = "application/pdf",
            encoding = "none",
            controller = "manufacturing"
        )
        public static final String VIEW_MRPPRUNSPRODUCTSBYFEATURE = "MRPPRunsProductsByFeature";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "SPPRunsProductsByFeature",
            type = "screenfop",
            page = "component://manufacturing/widget/manufacturing/ReportScreens.xml#SPPRunsProductsByFeature",
            contentType = "application/pdf",
            encoding = "none",
            controller = "manufacturing"
        )
        public static final String VIEW_SPPRUNSPRODUCTSBYFEATURE = "SPPRunsProductsByFeature";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "MRPPRunsComponentsByFeature",
            type = "screenfop",
            page = "component://manufacturing/widget/manufacturing/ReportScreens.xml#MRPPRunsComponentsByFeature",
            contentType = "application/pdf",
            encoding = "none",
            controller = "manufacturing"
        )
        public static final String VIEW_MRPPRUNSCOMPONENTSBYFEATURE = "MRPPRunsComponentsByFeature";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "SPPRunsComponentsByFeature",
            type = "screenfop",
            page = "component://manufacturing/widget/manufacturing/ReportScreens.xml#SPPRunsComponentsByFeature",
            contentType = "application/pdf",
            encoding = "none",
            controller = "manufacturing"
        )
        public static final String VIEW_SPPRUNSCOMPONENTSBYFEATURE = "SPPRunsComponentsByFeature";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PRunsInfoAndOrder",
            type = "screenfop",
            page = "component://manufacturing/widget/manufacturing/ReportScreens.xml#PRunsInfoAndOrder",
            contentType = "application/pdf",
            encoding = "none",
            controller = "manufacturing"
        )
        public static final String VIEW_PRUNSINFOANDORDER = "PRunsInfoAndOrder";

    }

    // Auto-generated split (Part 4)
    public static class Part4 {
        @Request(
            uri = "view",
            controller = "manufacturing",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "main")
        public interface ViewDef {}

        @Request(
            uri = "main",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        public interface Main {}

        @Request(
            uri = "LookupPerson",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPerson")
        public interface LookupPerson {}

        @Request(
            uri = "LookupCustomerName",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupCustomerName")
        public interface LookupCustomerName {}

        @Request(
            uri = "LookupRouting",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupRouting")
        public interface LookupRouting {}

        @Request(
            uri = "LookupRoutingTask",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupRoutingTask")
        public interface LookupRoutingTask {}

        @Request(
            uri = "LookupProduct",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProduct")
        public interface LookupProduct {}

        @Request(
            uri = "LookupVirtualProduct",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupVirtualProduct")
        public interface LookupVirtualProduct {}

        @Request(
            uri = "LookupVariantProduct",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupVariantProduct")
        public interface LookupVariantProduct {}

        @Request(
            uri = "LookupProductFeature",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductFeature")
        public interface LookupProductFeature {}

        @Request(
            uri = "FindCalendar",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindCalendar")
        public interface FindCalendar {}

        @Request(
            uri = "EditCalendar",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCalendar")
        public interface EditCalendar {}

        @Request(
            uri = "CreateCalendar",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCalendar")
        @Response(name = "error", type = "view", value = "EditCalendar")
        @Event(type = "service", invoke = "createCalendar")
        public static String createCalendar(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateCalendar",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCalendar")
        @Response(name = "error", type = "view", value = "EditCalendar")
        @Event(type = "service", invoke = "updateCalendar")
        public static String updateCalendar(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "RemoveCalendar",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindCalendar")
        @Response(name = "error", type = "view", value = "FindCalendar")
        @Event(type = "service", invoke = "removeCalendar")
        public static String removeCalendar(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditCalendarWeek",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCalendarWeek")
        public interface EditCalendarWeek {}

        @Request(
            uri = "ListCalendarWeek",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListCalendarWeek")
        public interface ListCalendarWeek {}

        @Request(
            uri = "createCalendarWeek",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCalendarWeek")
        @Response(name = "error", type = "view", value = "EditCalendarWeek")
        @Event(type = "service", invoke = "createCalendarWeek")
        public static String createCalendarWeek(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCalendarWeek",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCalendarWeek")
        @Response(name = "error", type = "view", value = "EditCalendarWeek")
        @Event(type = "service", invoke = "updateCalendarWeek")
        public static String updateCalendarWeek(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "RemoveCalendarWeek",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListCalendarWeek")
        @Response(name = "error", type = "view", value = "ListCalendarWeek")
        @Event(type = "service", invoke = "removeCalendarWeek")
        public static String removeCalendarWeek(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 5)
    public static class Part5 {
        @Request(
            uri = "EditCalendarExceptionDay",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCalendarExceptionDay")
        public interface EditCalendarExceptionDay {}

        @Request(
            uri = "CreateCalendarExceptionDay",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCalendarExceptionDay")
        @Response(name = "error", type = "view", value = "EditCalendarExceptionDay")
        @Event(type = "service", invoke = "createCalendarExceptionDay")
        public static String createCalendarExceptionDay(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateCalendarExceptionDay",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCalendarExceptionDay")
        @Response(name = "error", type = "view", value = "EditCalendarExceptionDay")
        @Event(type = "service", invoke = "updateCalendarExceptionDay")
        public static String updateCalendarExceptionDay(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "RemoveCalendarExceptionDay",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCalendarExceptionDay")
        @Response(name = "error", type = "view", value = "EditCalendarExceptionDay")
        @Event(type = "service", invoke = "removeCalendarExceptionDay")
        public static String removeCalendarExceptionDay(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditCalendarExceptionWeek",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCalendarExceptionWeek")
        public interface EditCalendarExceptionWeek {}

        @Request(
            uri = "CreateCalendarExceptionWeek",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCalendarExceptionWeek")
        @Response(name = "error", type = "view", value = "EditCalendarExceptionWeek")
        @Event(type = "service", invoke = "createCalendarExceptionWeek")
        public static String createCalendarExceptionWeek(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateCalendarExceptionWeek",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCalendarExceptionWeek")
        @Response(name = "error", type = "view", value = "EditCalendarExceptionWeek")
        @Event(type = "service", invoke = "updateCalendarExceptionWeek")
        public static String updateCalendarExceptionWeek(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "RemoveCalendarExceptionWeek",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCalendarExceptionWeek")
        @Response(name = "error", type = "view", value = "EditCalendarExceptionWeek")
        @Event(type = "service", invoke = "removeCalendarExceptionWeek")
        public static String removeCalendarExceptionWeek(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindRoutingTask",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindRoutingTask")
        public interface FindRoutingTask {}

        @Request(
            uri = "EditRoutingTask",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTask")
        public interface EditRoutingTask {}

        @Request(
            uri = "EditRoutingTaskCosts",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTaskCosts")
        public interface EditRoutingTaskCosts {}

        @Request(
            uri = "ListRoutingTaskRoutings",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListRoutingTaskRoutings")
        public interface ListRoutingTaskRoutings {}

        @Request(
            uri = "ListRoutingTaskProducts",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListRoutingTaskProducts")
        public interface ListRoutingTaskProducts {}

        @Request(
            uri = "EditRoutingTaskProduct",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTaskProduct")
        public interface EditRoutingTaskProduct {}

        @Request(
            uri = "CreateRoutingTask",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTask")
        @Response(name = "error", type = "view", value = "EditRoutingTask")
        @Event(type = "service", invoke = "createWorkEffort")
        public static String createRoutingTask(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateRoutingTask",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTask")
        @Response(name = "error", type = "view", value = "EditRoutingTask")
        @Event(type = "service", invoke = "updateWorkEffort")
        public static String updateRoutingTask(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "RemoveRoutingTask",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindRoutingTask")
        @Response(name = "error", type = "view", value = "FindRoutingTask")
        @Event(type = "service", invoke = "deleteWorkEffort")
        public static String removeRoutingTask(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindRouting",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindRouting")
        public interface FindRouting {}

        @Request(
            uri = "EditRouting",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRouting")
        public interface EditRouting {}

        @Request(
            uri = "CreateRouting",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRouting")
        @Response(name = "error", type = "view", value = "EditRouting")
        @Event(type = "service", invoke = "createWorkEffort")
        public static String createRouting(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 6)
    public static class Part6 {
        @Request(
            uri = "UpdateRouting",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRouting")
        @Response(name = "error", type = "view", value = "EditRouting")
        @Event(type = "service", invoke = "updateWorkEffort")
        public static String updateRouting(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditRoutingTaskAssoc",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTaskAssoc")
        public interface EditRoutingTaskAssoc {}

        @Request(
            uri = "AddRoutingTaskAssoc",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTaskAssoc")
        @Response(name = "error", type = "view", value = "EditRoutingTaskAssoc")
        @Event(type = "java", path = "com.ilscipio.scipio.manufacturing.event.RoutingSimpleEvents", invoke = "addRoutingTaskAssoc")
        public static String addRoutingTaskAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateRoutingTaskAssoc",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTaskAssoc")
        @Response(name = "error", type = "view", value = "EditRoutingTaskAssoc")
        @Event(type = "java", path = "com.ilscipio.scipio.manufacturing.event.RoutingSimpleEvents", invoke = "updateRoutingTaskAssoc")
        public static String updateRoutingTaskAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "RemoveRoutingTaskAssoc",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTaskAssoc")
        @Response(name = "error", type = "view", value = "EditRoutingTaskAssoc")
        @Event(type = "service", invoke = "removeWorkEffortAssoc")
        public static String removeRoutingTaskAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateRoutingTaskForRouting",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTaskAssoc")
        @Response(name = "error", type = "view", value = "EditRoutingTaskAssoc")
        @Event(type = "service", invoke = "updateWorkEffort")
        public static String updateRoutingTaskForRouting(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditRoutingProductLink",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingProductLink")
        public interface EditRoutingProductLink {}

        @Request(
            uri = "AddRoutingProductLink",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingProductLink")
        @Response(name = "error", type = "view", value = "EditRoutingProductLink")
        @Event(type = "service", invoke = "createWorkEffortGoodStandard")
        public static String addRoutingProductLink(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateRoutingProductLink",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingProductLink")
        @Response(name = "error", type = "view", value = "EditRoutingProductLink")
        @Event(type = "service", invoke = "updateWorkEffortGoodStandard")
        public static String updateRoutingProductLink(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addRoutingTaskProduct",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTaskProduct")
        @Response(name = "error", type = "view", value = "EditRoutingTaskProduct")
        @Event(type = "service", invoke = "createWorkEffortGoodStandard")
        public static String addRoutingTaskProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateRoutingTaskProduct",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTaskProduct")
        @Response(name = "error", type = "view", value = "EditRoutingTaskProduct")
        @Event(type = "service", invoke = "updateWorkEffortGoodStandard")
        public static String updateRoutingTaskProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addRoutingTaskCost",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTaskCosts")
        @Response(name = "error", type = "view", value = "EditRoutingTaskCosts")
        @Event(type = "service", invoke = "createWorkEffortCostCalc")
        public static String addRoutingTaskCost(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeRoutingTaskCost",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTaskCosts")
        @Response(name = "error", type = "view", value = "EditRoutingTaskCosts")
        @Event(type = "service", invoke = "removeWorkEffortCostCalc")
        public static String removeRoutingTaskCost(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditRoutingTaskFixedAssets",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTaskFixedAssets")
        public interface EditRoutingTaskFixedAssets {}

        @Request(
            uri = "createRoutingTaskFixedAsset",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTaskFixedAssets")
        @Response(name = "error", type = "view", value = "EditRoutingTaskFixedAssets")
        @Event(type = "service", invoke = "createWorkEffortFixedAssetStd")
        public static String createRoutingTaskFixedAsset(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateRoutingTaskFixedAsset",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTaskFixedAssets")
        @Response(name = "error", type = "view", value = "EditRoutingTaskFixedAssets")
        @Event(type = "service", invoke = "updateWorkEffortFixedAssetStd")
        public static String updateRoutingTaskFixedAsset(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeRoutingTaskFixedAsset",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingTaskFixedAssets")
        @Response(name = "error", type = "view", value = "EditRoutingTaskFixedAssets")
        @Event(type = "service", invoke = "removeWorkEffortFixedAssetStd")
        public static String removeRoutingTaskFixedAsset(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditCostCalcs",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCostCalcs")
        public interface EditCostCalcs {}

        @Request(
            uri = "createCostComponentCalc",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCostCalcs")
        @Response(name = "error", type = "view", value = "EditCostCalcs")
        @Event(type = "service", invoke = "createCostComponentCalc")
        public static String createCostComponentCalc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCostComponentCalc",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCostCalcs")
        @Response(name = "error", type = "view", value = "EditCostCalcs")
        @Event(type = "service", invoke = "updateCostComponentCalc")
        public static String updateCostComponentCalc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 7)
    public static class Part7 {
        @Request(
            uri = "removeCostComponentCalc",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCostCalcs")
        @Response(name = "error", type = "view", value = "EditCostCalcs")
        @Event(type = "service", invoke = "removeCostComponentCalc")
        public static String removeCostComponentCalc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "BomSimulation",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BomSimulation")
        public interface BomSimulation {}

        @Request(
            uri = "runBomSimulation",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BomSimulation")
        @Event(type = "service", invoke = "getBOMTree")
        public static String runBomSimulation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductBom",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductBom")
        public interface EditProductBom {}

        @Request(
            uri = "UpdateProductBom",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductBom")
        @Response(name = "error", type = "view", value = "EditProductBom")
        @Event(type = "java", path = "com.ilscipio.scipio.manufacturing.event.BomSimpleMethods", invoke = "eventEditBOM")
        public static String updateProductBom(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindBom",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindBom")
        public interface FindBom {}

        @Request(
            uri = "EditProductManufacturingRules",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductManufacturingRules")
        public interface EditProductManufacturingRules {}

        @Request(
            uri = "AddProductManufacturingRule",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductManufacturingRules")
        @Response(name = "error", type = "view", value = "EditProductManufacturingRules")
        @Event(type = "service", invoke = "addProductManufacturingRule")
        public static String addProductManufacturingRule(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateProductManufacturingRule",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductManufacturingRules")
        @Response(name = "error", type = "view", value = "EditProductManufacturingRules")
        @Event(type = "service", invoke = "updateProductManufacturingRule")
        public static String updateProductManufacturingRule(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "DeleteProductManufacturingRule",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditProductManufacturingRules")
        @Response(name = "error", type = "request-redirect", value = "EditProductManufacturingRules")
        @Event(type = "service", invoke = "deleteProductManufacturingRule")
        public static String deleteProductManufacturingRule(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindInventoryEventPlan",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindMrpPlannedEvents")
        public interface FindInventoryEventPlan {}

        @Request(
            uri = "RunMrp",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "MrpExecution")
        public interface RunMrp {}

        @Request(
            uri = "runMrpGo",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "MrpExecution")
        @Response(name = "error", type = "view", value = "MrpExecution")
        @Event(type = "service", path = "async", invoke = "executeMrp")
        public static String runMrpGo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "CreateProductionRun",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CreateProductionRun")
        public interface CreateProductionRun {}

        @Request(
            uri = "createProductionRun",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "createProductionRunsForProductBom", type = "request", value = "createProductionRunsForProductBom")
        @Response(name = "createProductionRunSingle", type = "request", value = "createProductionRunSingle")
        @Response(name = "error", type = "view", value = "CreateProductionRun")
        @Event(type = "java", path = "com.ilscipio.scipio.manufacturing.event.ProductionRunSimpleEvents", invoke = "createProductionRun")
        public static String createProductionRun_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductionRunsForProductBom",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductionRun")
        @Response(name = "error", type = "view", value = "CreateProductionRun")
        @Event(type = "service", invoke = "createProductionRunsForProductBom")
        public static String createProductionRunsForProductBom(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductionRunSingle",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductionRun")
        @Response(name = "error", type = "view", value = "CreateProductionRun")
        @Event(type = "service", invoke = "createProductionRun")
        public static String createProductionRunSingle(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ShowProductionRun",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "docs_not_printed", type = "view", value = "EditProductionRun")
        @Response(name = "docs_printed", type = "view", value = "ProductionRunDeclaration")
        @Response(name = "error", type = "view", value = "FindProductionRun")
        @Event(type = "groovy", path = "component://manufacturing/webapp/manufacturing/jobshopmgt/ShowProductionRun.groovy")
        public static String showProductionRun(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductionRun",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductionRun")
        public interface EditProductionRun {}

        @Request(
            uri = "PrintProductionRun",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PrintProductionRun")
        public interface PrintProductionRun {}

    }

    // Auto-generated split (Part 8)
    public static class Part8 {
        @Request(
            uri = "LinkProductionRun",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LinkProductionRun")
        public interface LinkProductionRun {}

        @Request(
            uri = "createProductionRunAssoc",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunAssocs")
        @Event(type = "service", invoke = "createProductionRunAssoc")
        public static String createProductionRunAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ManufacturingReports",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ManufacturingReports")
        public interface ManufacturingReports {}

        @Request(
            uri = "FindProductionRun",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductionRun")
        public interface FindProductionRun {}

        @Request(
            uri = "updateProductionRun",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductionRun")
        @Response(name = "error", type = "view", value = "EditProductionRun")
        @Event(type = "service", invoke = "updateProductionRun")
        public static String updateProductionRun(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ProductionRunDeclaration",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
        public interface ProductionRunDeclaration {}

        @Request(
            uri = "ProductionRunCosts",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunCosts")
        public interface ProductionRunCosts {}

        @Request(
            uri = "ProductionRunFixedAssets",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunFixedAssets")
        public interface ProductionRunFixedAssets {}

        @Request(
            uri = "createWorkEffortFixedAssetAssign",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunFixedAssets")
        @Response(name = "error", type = "view", value = "ProductionRunFixedAssets")
        @Event(type = "service", invoke = "createWorkEffortFixedAssetAssign")
        public static String createWorkEffortFixedAssetAssign(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffortFixedAssetAssign",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunFixedAssets")
        @Response(name = "error", type = "view", value = "ProductionRunFixedAssets")
        @Event(type = "service", invoke = "updateWorkEffortFixedAssetAssign")
        public static String updateWorkEffortFixedAssetAssign(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeWorkEffortFixedAssetAssign",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunFixedAssets")
        @Response(name = "error", type = "view", value = "ProductionRunFixedAssets")
        @Event(type = "service", invoke = "removeWorkEffortFixedAssetAssign")
        public static String removeWorkEffortFixedAssetAssign(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductionRunPartyAssign",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunFixedAssets")
        @Response(name = "error", type = "view", value = "ProductionRunFixedAssets")
        @Event(type = "service", invoke = "createProductionRunPartyAssign")
        public static String createProductionRunPartyAssign(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "LookupPartyName",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyName")
        public interface LookupPartyName {}

        @Request(
            uri = "ProductionRunTasks",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunTasks")
        public interface ProductionRunTasks {}

        @Request(
            uri = "addProductionRunRoutingTask",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunTasks")
        @Event(type = "java", path = "com.ilscipio.scipio.manufacturing.event.ProductionRunSimpleEvents", invoke = "addProductionRunRoutingTask")
        public static String addProductionRunRoutingTask(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductionRunRoutingTask",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunTasks")
        @Event(type = "java", path = "com.ilscipio.scipio.manufacturing.event.ProductionRunSimpleEvents", invoke = "editProductionRunRoutingTask")
        public static String updateProductionRunRoutingTask(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductionRunRoutingTask",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunTasks")
        @Response(name = "error", type = "view", value = "ProductionRunTasks")
        @Event(type = "service", invoke = "deleteWorkEffort")
        public static String deleteProductionRunRoutingTask(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ProductionRunComponents",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunComponents")
        public interface ProductionRunComponents {}

        @Request(
            uri = "addProductionRunComponent",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunComponents")
        @Response(name = "error", type = "view", value = "ProductionRunComponents")
        @Event(type = "service", invoke = "addProductionRunComponent")
        public static String addProductionRunComponent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductionRunComponent",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunComponents")
        @Response(name = "error", type = "view", value = "ProductionRunComponents")
        @Event(type = "service", invoke = "updateProductionRunComponent")
        public static String updateProductionRunComponent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductionRunComponent",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunComponents")
        @Response(name = "error", type = "view", value = "ProductionRunComponents")
        @Event(type = "service", invoke = "removeWorkEffortGoodStandard")
        public static String deleteProductionRunComponent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "replaceProductionRunComponent",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunComponents")
        @Response(name = "error", type = "view", value = "ProductionRunComponents")
        @Event(type = "service", invoke = "replaceProductionRunComponent")
        public static String replaceProductionRunComponent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ProductionRunActualComponents",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunActualComponents")
        public interface ProductionRunActualComponents {}

    }

    // Auto-generated split (Part 9)
    public static class Part9 {
        @Request(
            uri = "issueProductionRunTaskComponent",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunActualComponents")
        @Response(name = "error", type = "view", value = "ProductionRunActualComponents")
        @Event(type = "service", invoke = "issueProductionRunTaskComponent")
        public static String issueProductionRunTaskComponent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ProductionRunContent",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunContent")
        public interface ProductionRunContent {}

        @Request(
            uri = "ProductionRunAssocs",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunAssocs")
        public interface ProductionRunAssocs {}

        @Request(
            uri = "removeRoutingProductLink",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRoutingProductLink")
        @Response(name = "error", type = "view", value = "EditRoutingProductLink")
        @Event(type = "service", invoke = "removeWorkEffortGoodStandard")
        public static String removeRoutingProductLink(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeRoutingTaskProduct",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListRoutingTaskProducts")
        @Response(name = "error", type = "view", value = "ListRoutingTaskProducts")
        @Event(type = "service", invoke = "removeWorkEffortGoodStandard")
        public static String removeRoutingTaskProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "changeProductionRunStatusToPrinted",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
        @Response(name = "error", type = "view", value = "EditProductionRun")
        @Event(type = "service", invoke = "changeProductionRunStatus")
        public static String changeProductionRunStatusToPrinted(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "changeProductionRunStatusToClosed",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
        @Response(name = "error", type = "view", value = "EditProductionRun")
        @Event(type = "service", invoke = "changeProductionRunStatus")
        public static String changeProductionRunStatusToClosed(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "changeProductionRunTaskStatus",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
        @Response(name = "error", type = "view", value = "ProductionRunDeclaration")
        @Event(type = "service", invoke = "changeProductionRunTaskStatus")
        public static String changeProductionRunTaskStatus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "issueProductionRunRoutingTask",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
        @Response(name = "error", type = "view", value = "ProductionRunDeclaration")
        @Event(type = "service", invoke = "issueProductionRunTask")
        public static String issueProductionRunRoutingTask(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "issueProductionRunTaskComponents",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
        @Response(name = "error", type = "view", value = "ProductionRunDeclaration")
        @Event(type = "service-multi", invoke = "issueProductionRunTaskComponent")
        public static String issueProductionRunTaskComponents(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductionRunTaskProduct",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
        @Response(name = "error", type = "view", value = "ProductionRunDeclaration")
        @Event(type = "service", invoke = "productionRunTaskProduce")
        public static String createProductionRunTaskProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "productionRunTaskReturnMaterials",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
        @Response(name = "error", type = "view", value = "ProductionRunDeclaration")
        @Event(type = "service-multi", invoke = "productionRunTaskReturnMaterial")
        public static String productionRunTaskReturnMaterials(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductionRunContents",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunContent")
        @Response(name = "error", type = "view", value = "ProductionRunContent")
        @Event(type = "service-multi", invoke = "createWorkEffortContent")
        public static String createProductionRunContents(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductionRunContent",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunContent")
        @Response(name = "error", type = "view", value = "ProductionRunContent")
        @Event(type = "service", invoke = "deleteWorkEffortContent")
        public static String deleteProductionRunContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickRunProductionRunTask",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
        @Response(name = "error", type = "view", value = "ProductionRunDeclaration")
        @Event(type = "service", invoke = "quickRunProductionRunTask")
        public static String quickRunProductionRunTask(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickRunAllProductionRunTasks",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
        @Response(name = "error", type = "view", value = "ProductionRunDeclaration")
        @Event(type = "service", invoke = "quickRunAllProductionRunTasks")
        public static String quickRunAllProductionRunTasks(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickStartAllProductionRunTasks",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
        @Response(name = "error", type = "view", value = "ProductionRunDeclaration")
        @Event(type = "service", invoke = "quickStartAllProductionRunTasks")
        public static String quickStartAllProductionRunTasks(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "scheduleProductionRun",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductionRun")
        @Response(name = "error", type = "view", value = "EditProductionRun")
        @Event(type = "service", invoke = "quickChangeProductionRunStatus")
        public static String scheduleProductionRun(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickChangeProductionRunStatus",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
        @Response(name = "error", type = "view", value = "ProductionRunDeclaration")
        @Event(type = "service", invoke = "quickChangeProductionRunStatus")
        public static String quickChangeProductionRunStatus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "cancelProductionRun",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
        @Response(name = "error", type = "view", value = "ProductionRunDeclaration")
        @Event(type = "service", invoke = "cancelProductionRun")
        public static String cancelProductionRun(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 10)
    public static class Part10 {
        @Request(
            uri = "productionRunProduce",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
        @Response(name = "error", type = "view", value = "ProductionRunDeclaration")
        @Event(type = "service", invoke = "productionRunProduce")
        public static String productionRunProduce(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "productionRunDeclareAndProduce",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
        @Response(name = "error", type = "view", value = "ProductionRunDeclaration")
        public static String productionRunDeclareAndProduce(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.manufacturing.jobshopmgt.ProductionRunEvents.productionRunDeclareAndProduce
            return ProductionRunEvents.productionRunDeclareAndProduce(request, response);
        }

        @Request(
            uri = "updateProductionRunTask",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
        @Response(name = "error", type = "view", value = "ProductionRunDeclaration")
        @Event(type = "service", invoke = "updateProductionRunTask")
        public static String updateProductionRunTask(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductionRunsForShipment",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WorkWithShipmentPlans")
        public static String createProductionRunsForShipment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.manufacturing.bom.BOMHelper.createProductionRunsForShipment
            return BOMHelper.createProductionRunsForShipment(request, response);
        }

        @Request(
            uri = "WorkWithShipmentPlans",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WorkWithShipmentPlans")
        public interface WorkWithShipmentPlans {}

        @Request(
            uri = "CuttingListReport.pdf",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CuttingListReport")
        public interface CuttingListReportPdf {}

        @Request(
            uri = "ShipmentPlanStockReport.pdf",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ShipmentPlanStockReport")
        public interface ShipmentPlanStockReportPdf {}

        @Request(
            uri = "ShipmentLabel.pdf",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ShipmentLabel")
        public interface ShipmentLabelPdf {}

        @Request(
            uri = "ShipmentWorkEffortTasks.pdf",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ShipmentWorkEffortTasks")
        public interface ShipmentWorkEffortTasksPdf {}

        @Request(
            uri = "MRPPRunsProductsByFeature.pdf",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "MRPPRunsProductsByFeature")
        public interface MRPPRunsProductsByFeaturePdf {}

        @Request(
            uri = "SPPRunsProductsByFeature.pdf",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SPPRunsProductsByFeature")
        public interface SPPRunsProductsByFeaturePdf {}

        @Request(
            uri = "MRPPRunsComponentsByFeature.pdf",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "MRPPRunsComponentsByFeature")
        public interface MRPPRunsComponentsByFeaturePdf {}

        @Request(
            uri = "SPPRunsComponentsByFeature.pdf",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SPPRunsComponentsByFeature")
        public interface SPPRunsComponentsByFeaturePdf {}

        @Request(
            uri = "PackageContentsAndOrder.pdf",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PackageContentsAndOrder")
        public interface PackageContentsAndOrderPdf {}

        @Request(
            uri = "PRunsProductsStacks.pdf",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PRunsProductsStacks")
        public interface PRunsProductsStacksPdf {}

        @Request(
            uri = "PRunsProductsAndOrder.pdf",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PRunsProductsAndOrder")
        public interface PRunsProductsAndOrderPdf {}

        @Request(
            uri = "PRunsInfoAndOrder.pdf",
            controller = "manufacturing",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PRunsInfoAndOrder")
        public interface PRunsInfoAndOrderPdf {}


    }
}
