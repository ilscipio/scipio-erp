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
 * Fabrication order service definitions: a fabrication order (WorkEffort of type FAB_ORDER) groups
 * several production runs so a planner can see their combined quantity, planned time, machines, and
 * status, and move them through statuses together.
 *
 * <p>SCIPIO: 4.0.0: New.</p>
 */
public class FabricationServices {

    /**
     * Creates a fabrication order header.
     */
    @Service(
        name = "createFabricationOrder",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.fabrication.FabricationServices",
        invoke = "createFabricationOrder",
        description = "Creates a fabrication order header",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortName", type = "String", mode = "IN"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "estimatedStartDate", type = "java.sql.Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "estimatedCompletionDate", type = "java.sql.Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "fabricationOrderId", type = "String", mode = "OUT")
        }
    )
    public interface CreateFabricationOrder {}

    /**
     * Updates a fabrication order header.
     */
    @Service(
        name = "updateFabricationOrder",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.fabrication.FabricationServices",
        invoke = "updateFabricationOrder",
        description = "Updates a fabrication order header",
        auth = "true",
        attributes = {
            @Attribute(name = "fabricationOrderId", type = "String", mode = "IN"),
            @Attribute(name = "workEffortName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "estimatedStartDate", type = "java.sql.Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "estimatedCompletionDate", type = "java.sql.Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface UpdateFabricationOrder {}

    /**
     * Adds an existing production run to a fabrication order by setting the run header's parent.
     */
    @Service(
        name = "addProductionRunToFabricationOrder",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.fabrication.FabricationServices",
        invoke = "addProductionRunToFabricationOrder",
        description = "Adds an existing production run to a fabrication order",
        auth = "true",
        attributes = {
            @Attribute(name = "fabricationOrderId", type = "String", mode = "IN"),
            @Attribute(name = "productionRunId", type = "String", mode = "IN")
        }
    )
    public interface AddProductionRunToFabricationOrder {}

    /**
     * Removes a production run from its fabrication order (clears the run header's parent).
     */
    @Service(
        name = "removeProductionRunFromFabricationOrder",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.fabrication.FabricationServices",
        invoke = "removeProductionRunFromFabricationOrder",
        description = "Removes a production run from its fabrication order",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN")
        }
    )
    public interface RemoveProductionRunFromFabricationOrder {}

    /**
     * Returns the fabrication order header, its production runs, and totals computed over them.
     */
    @Service(
        name = "getFabricationOrder",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.fabrication.FabricationServices",
        invoke = "getFabricationOrder",
        description = "Returns the fabrication order header, its production runs, and totals",
        auth = "true",
        attributes = {
            @Attribute(name = "fabricationOrderId", type = "String", mode = "IN"),
            @Attribute(name = "header", type = "java.util.Map", mode = "OUT", optional = "true"),
            @Attribute(name = "runs", type = "java.util.List", mode = "OUT", optional = "true"),
            @Attribute(name = "totals", type = "java.util.Map", mode = "OUT", optional = "true")
        }
    )
    public interface GetFabricationOrder {}

    /**
     * Moves every non-closed, non-cancelled production run of a fabrication order to the given status,
     * using the existing quickChangeProductionRunStatus service; collects errors and continues.
     */
    @Service(
        name = "changeFabricationOrderStatus",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.fabrication.FabricationServices",
        invoke = "changeFabricationOrderStatus",
        description = "Moves every open production run of a fabrication order to the given status",
        auth = "true",
        attributes = {
            @Attribute(name = "fabricationOrderId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "updatedCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "errorMessageList", type = "java.util.List", mode = "OUT", optional = "true")
        }
    )
    public interface ChangeFabricationOrderStatus {}

}
