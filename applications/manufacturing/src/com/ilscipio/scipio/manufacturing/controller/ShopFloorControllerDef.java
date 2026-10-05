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
import com.ilscipio.scipio.ce.webapp.control.def.RedirectParameter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Controller definitions for the manufacturing shop floor screen.
 *
 * <p>SCIPIO: 4.0.0: Added for the shop floor task declaration feature.</p>
 */
public class ShopFloorControllerDef {

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ShopFloor",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/ShopFloorScreens.xml#ShopFloor",
        controller = "manufacturing"
    )
    public static final String VIEW_SHOPFLOOR = "ShopFloor";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "CreateProductionRunFromOrder",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/ShopFloorScreens.xml#CreateProductionRunFromOrder",
        controller = "manufacturing"
    )
    public static final String VIEW_CREATEPRODUCTIONRUNFROMORDER = "CreateProductionRunFromOrder";

    @Request(
        uri = "ShopFloor",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "ShopFloor")
    public interface ShopFloor {}

    @Request(
        uri = "shopFloorStartTask",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request-redirect", value = "ShopFloor", redirectParameters = {
        @RedirectParameter(name = "fixedAssetId"),
        @RedirectParameter(name = "facilityId")
    })
    @Response(name = "error", type = "view", value = "ShopFloor")
    @Event(type = "service", invoke = "changeProductionRunTaskStatus")
    public static String shopFloorStartTask(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "shopFloorCompleteTask",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request-redirect", value = "ShopFloor", redirectParameters = {
        @RedirectParameter(name = "fixedAssetId"),
        @RedirectParameter(name = "facilityId")
    })
    @Response(name = "error", type = "view", value = "ShopFloor")
    @Event(type = "service", invoke = "changeProductionRunTaskStatus")
    public static String shopFloorCompleteTask(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "shopFloorSwitchMachine",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request-redirect", value = "ShopFloor", redirectParameters = {
        @RedirectParameter(name = "fixedAssetId", from = "viewFixedAssetId"),
        @RedirectParameter(name = "facilityId")
    })
    @Response(name = "error", type = "view", value = "ShopFloor")
    @Event(type = "service", invoke = "switchProductionRunTaskMachine")
    public static String shopFloorSwitchMachine(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "shopFloorDeclareTask",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "ShopFloor")
    @Response(name = "error", type = "view", value = "ShopFloor")
    @Event(type = "java", path = "com.ilscipio.scipio.manufacturing.shopfloor.ShopFloorEvents", invoke = "declareTask")
    public static String shopFloorDeclareTask(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "updateProductionRunTaskDeclaration",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "ProductionRunDeclaration")
    @Response(name = "error", type = "view", value = "ProductionRunDeclaration")
    @Event(type = "java", path = "com.ilscipio.scipio.manufacturing.shopfloor.ShopFloorEvents", invoke = "declareTask")
    public static String updateProductionRunTaskDeclaration(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "CreateProductionRunFromOrder",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "CreateProductionRunFromOrder")
    public interface CreateProductionRunFromOrder {}

    @Request(
        uri = "createProductionRunsFromOrder",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "FindProductionRun")
    @Response(name = "error", type = "view", value = "CreateProductionRunFromOrder")
    @Event(type = "service", invoke = "createProductionRunsForOrder")
    public static String createProductionRunsFromOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }
}
