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
 * Manufacturing capacity planning dashboard controller definitions.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class DashboardControllerDef {

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "Dashboard",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/DashboardScreens.xml#Dashboard",
        controller = "manufacturing"
    )
    public static final String VIEW_DASHBOARD = "Dashboard";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "WorkCenterLoad",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/DashboardScreens.xml#WorkCenterLoad",
        controller = "manufacturing"
    )
    public static final String VIEW_WORKCENTERLOAD = "WorkCenterLoad";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ReportsHub",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/DashboardScreens.xml#ReportsHub",
        controller = "manufacturing"
    )
    public static final String VIEW_REPORTSHUB = "ReportsHub";

    @Request(
        uri = "Dashboard",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "Dashboard")
    public interface Dashboard {}

    @Request(
        uri = "WorkCenterLoad",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "WorkCenterLoad")
    public interface WorkCenterLoad {}

    @Request(
        uri = "ReportsHub",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "ReportsHub")
    public interface ReportsHub {}

}
