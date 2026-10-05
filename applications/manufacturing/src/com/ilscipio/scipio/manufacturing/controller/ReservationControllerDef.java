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

import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import com.ilscipio.scipio.ce.webapp.control.def.RedirectParameter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Controller definitions for lot reservation by weight: reserving and releasing a production run task's
 * held inventory lots.
 *
 * <p>SCIPIO: 4.0.0: Added for the lot reservation feature.</p>
 */
public class ReservationControllerDef {

    @Request(
        uri = "reserveProductionRunLot",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request-redirect", value = "ProductionRunComponents", redirectParameters = {
        @RedirectParameter(name = "productionRunId")
    })
    @Response(name = "error", type = "request-redirect", value = "ProductionRunComponents", redirectParameters = {
        @RedirectParameter(name = "productionRunId")
    })
    @Event(type = "service", invoke = "reserveProductionRunLot")
    public static String reserveProductionRunLot(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "releaseProductionRunLot",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request-redirect", value = "ProductionRunComponents", redirectParameters = {
        @RedirectParameter(name = "productionRunId")
    })
    @Response(name = "error", type = "request-redirect", value = "ProductionRunComponents", redirectParameters = {
        @RedirectParameter(name = "productionRunId")
    })
    @Event(type = "service", invoke = "releaseProductionRunLot")
    public static String releaseProductionRunLot(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

}
