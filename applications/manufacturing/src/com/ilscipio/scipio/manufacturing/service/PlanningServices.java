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
package com.ilscipio.scipio.manufacturing.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Manufacturing capacity planning service definitions.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class PlanningServices {

    /**
     * Returns work center capacity and load for a date range
     */
    @Service(
        name = "getWorkCenterLoad",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.planning.PlanningServices",
        invoke = "getWorkCenterLoad",
        description = "Returns work center capacity and load for a date range",
        auth = "true",
        attributes = {
            @Attribute(name = "fixedAssetId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "includeClosed", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "workCenters", type = "List", mode = "OUT"),
            @Attribute(name = "days", type = "List", mode = "OUT"),
            @Attribute(name = "loadRows", type = "List", mode = "OUT"),
            @Attribute(name = "tasks", type = "List", mode = "OUT")
        },
        permissions = {@Permissions(joinType = "AND", permissions = {@Permission(permission = "MANUFACTURING", action = "_VIEW")})}
    )
    public interface GetWorkCenterLoad {}

    /**
     * Returns a summary dashboard of production run status, work center load and material shortages
     */
    @Service(
        name = "getManufacturingDashboard",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.planning.PlanningServices",
        invoke = "getManufacturingDashboard",
        description = "Returns a summary dashboard of production run status, work center load and material shortages",
        auth = "true",
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "days", type = "Integer", mode = "IN", optional = "true", defaultValue = "7"),
            @Attribute(name = "runCounts", type = "Map", mode = "OUT"),
            @Attribute(name = "lateRuns", type = "List", mode = "OUT"),
            @Attribute(name = "runningTasks", type = "List", mode = "OUT"),
            @Attribute(name = "upcomingRuns", type = "List", mode = "OUT"),
            @Attribute(name = "shortages", type = "List", mode = "OUT"),
            @Attribute(name = "mrpProposals", type = "Map", mode = "OUT"),
            @Attribute(name = "lastMrpRun", type = "Map", mode = "OUT", optional = "true"),
            @Attribute(name = "workCenterLoad", type = "List", mode = "OUT")
        },
        permissions = {@Permissions(joinType = "AND", permissions = {@Permission(permission = "MANUFACTURING", action = "_VIEW")})}
    )
    public interface GetManufacturingDashboard {}

}
