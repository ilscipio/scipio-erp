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
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * MRP planning and product-cost controller definitions (MrpRuns, MrpRunDetail, MrpProposals,
 * ProductCost, ProductWhereUsed) plus the requirement-approval events they use.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class PlanningControllerDef {

    @View(
        name = "MrpRuns",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/PlanningScreens.xml#MrpRuns",
        controller = "manufacturing"
    )
    public static final String VIEW_MRPRUNS = "MrpRuns";

    @View(
        name = "MrpRunDetail",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/PlanningScreens.xml#MrpRunDetail",
        controller = "manufacturing"
    )
    public static final String VIEW_MRPRUNDETAIL = "MrpRunDetail";

    @View(
        name = "MrpProposals",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/PlanningScreens.xml#MrpProposals",
        controller = "manufacturing"
    )
    public static final String VIEW_MRPPROPOSALS = "MrpProposals";

    @View(
        name = "ProductCost",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/PlanningScreens.xml#ProductCost",
        controller = "manufacturing"
    )
    public static final String VIEW_PRODUCTCOST = "ProductCost";

    @View(
        name = "ProductWhereUsed",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/PlanningScreens.xml#ProductWhereUsed",
        controller = "manufacturing"
    )
    public static final String VIEW_PRODUCTWHEREUSED = "ProductWhereUsed";

    @Request(
        uri = "MrpRuns",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "MrpRuns")
    public interface MrpRuns {}

    @Request(
        uri = "MrpRunDetail",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "MrpRunDetail")
    public interface MrpRunDetail {}

    @Request(
        uri = "MrpProposals",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "MrpProposals")
    public interface MrpProposals {}

    @Request(
        uri = "ProductCost",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "ProductCost")
    public interface ProductCost {}

    @Request(
        uri = "ProductWhereUsed",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "ProductWhereUsed")
    public interface ProductWhereUsed {}

    @Request(
        uri = "approveMrpProposal",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request-redirect", value = "MrpProposals")
    @Response(name = "error", type = "view", value = "MrpProposals")
    @Event(type = "service", invoke = "updateRequirement")
    public static String approveMrpProposal(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "rejectMrpProposal",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request-redirect", value = "MrpProposals")
    @Response(name = "error", type = "view", value = "MrpProposals")
    @Event(type = "service", invoke = "updateRequirement")
    public static String rejectMrpProposal(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

}
