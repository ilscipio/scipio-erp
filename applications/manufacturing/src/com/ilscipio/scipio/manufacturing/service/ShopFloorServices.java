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
 * Shop floor service definitions: task reject declaration and shop floor task listing.
 *
 * <p>SCIPIO: 4.0.0: New.</p>
 */
public class ShopFloorServices {

    /**
     * Declares a rejected (scrapped) quantity on a running production run task
     */
    @Service(
        name = "declareProductionRunTaskReject",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.shopfloor.ShopFloorServices",
        invoke = "declareProductionRunTaskReject",
        description = "Declares a rejected (scrapped) quantity on a running production run task",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN"),
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "reasonEnumId", type = "String", mode = "IN"),
            @Attribute(name = "lotId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "rejectSeqId", type = "String", mode = "OUT")
        }
    )
    public interface DeclareProductionRunTaskReject {}

    /**
     * Returns the rejected-quantity records for a production run and/or task
     */
    @Service(
        name = "getProductionRunRejects",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.shopfloor.ShopFloorServices",
        invoke = "getProductionRunRejects",
        description = "Returns the rejected-quantity records for a production run and/or task",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "workEffortId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "rejects", type = "List", mode = "OUT"),
            @Attribute(name = "totalRejected", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface GetProductionRunRejects {}

    /**
     * Returns the active production run tasks for the shop floor, optionally filtered by work center or facility
     */
    @Service(
        name = "getShopFloorTasks",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.shopfloor.ShopFloorServices",
        invoke = "getShopFloorTasks",
        description = "Returns the active production run tasks for the shop floor, optionally filtered by work center or facility",
        auth = "true",
        attributes = {
            @Attribute(name = "fixedAssetId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "includeCompleted", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "tasks", type = "List", mode = "OUT"),
            @Attribute(name = "workCenters", type = "List", mode = "OUT")
        }
    )
    public interface GetShopFloorTasks {}

    /**
     * Switches the machine (or line) assigned to a production run task
     */
    @Service(
        name = "switchProductionRunTaskMachine",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.shopfloor.ShopFloorServices",
        invoke = "switchProductionRunTaskMachine",
        description = "Switches the machine or production line assigned to a production run task",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "fixedAssetId", type = "String", mode = "IN")
        }
    )
    public interface SwitchProductionRunTaskMachine {}

}
