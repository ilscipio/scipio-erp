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
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import com.ilscipio.scipio.ce.webapp.control.def.RedirectParameter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Fabrication order controller definitions: find/edit screens plus the create/update/add-run/remove-run/
 * status-change requests, and the java event that creates a production run directly inside an order.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class FabricationControllerDef {

    @View(
        name = "FindFabricationOrders",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/FabricationScreens.xml#FindFabricationOrders",
        controller = "manufacturing"
    )
    public static final String VIEW_FINDFABRICATIONORDERS = "FindFabricationOrders";

    @View(
        name = "EditFabricationOrder",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/FabricationScreens.xml#EditFabricationOrder",
        controller = "manufacturing"
    )
    public static final String VIEW_EDITFABRICATIONORDER = "EditFabricationOrder";

    @Request(
        uri = "FindFabricationOrders",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "FindFabricationOrders")
    public interface FindFabricationOrders {}

    @Request(
        uri = "EditFabricationOrder",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "EditFabricationOrder")
    public interface EditFabricationOrder {}

    @Request(
        uri = "createFabricationOrder",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request-redirect", value = "EditFabricationOrder", redirectParameters = {
        @RedirectParameter(name = "fabricationOrderId")
    })
    @Response(name = "error", type = "view", value = "EditFabricationOrder")
    @Event(type = "service", invoke = "createFabricationOrder")
    public static String createFabricationOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "updateFabricationOrder",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request-redirect", value = "EditFabricationOrder", redirectParameters = {
        @RedirectParameter(name = "fabricationOrderId")
    })
    @Response(name = "error", type = "view", value = "EditFabricationOrder")
    @Event(type = "service", invoke = "updateFabricationOrder")
    public static String updateFabricationOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "addProductionRunToFabricationOrder",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request-redirect", value = "EditFabricationOrder", redirectParameters = {
        @RedirectParameter(name = "fabricationOrderId")
    })
    @Response(name = "error", type = "view", value = "EditFabricationOrder")
    @Event(type = "service", invoke = "addProductionRunToFabricationOrder")
    public static String addProductionRunToFabricationOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "removeProductionRunFromFabricationOrder",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request-redirect", value = "EditFabricationOrder", redirectParameters = {
        @RedirectParameter(name = "fabricationOrderId")
    })
    @Response(name = "error", type = "view", value = "EditFabricationOrder")
    @Event(type = "service", invoke = "removeProductionRunFromFabricationOrder")
    public static String removeProductionRunFromFabricationOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "changeFabricationOrderStatus",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request-redirect", value = "EditFabricationOrder", redirectParameters = {
        @RedirectParameter(name = "fabricationOrderId")
    })
    @Response(name = "error", type = "view", value = "EditFabricationOrder")
    @Event(type = "service", invoke = "changeFabricationOrderStatus")
    public static String changeFabricationOrderStatus(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "createProductionRunInFabricationOrder",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request-redirect", value = "EditFabricationOrder", redirectParameters = {
        @RedirectParameter(name = "fabricationOrderId")
    })
    @Response(name = "error", type = "view", value = "EditFabricationOrder")
    @Event(type = "java", path = "com.ilscipio.scipio.manufacturing.fabrication.FabricationEvents", invoke = "createProductionRunInFabricationOrder")
    public static String createProductionRunInFabricationOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

}
