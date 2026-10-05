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
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class RoutingServices {

    /**
     * Only used to defined the query lookup for Routing Task
     */
    @Service(
        name = "lookupRoutingTask",
        engine = "java",
        location = "org.ofbiz.manufacturing.techdata.TechDataServices",
        invoke = "lookupRoutingTask",
        description = "Only used to defined the query lookup for Routing Task",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortName", type = "String", mode = "IN", optional = "true", formLabel = "${uiLabelMap.ManufacturingTaskName}"),
            @Attribute(name = "fixedAssetId", type = "String", mode = "IN", optional = "true", formLabel = "${uiLabelMap.ManufacturingMachineGroup}"),
            @Attribute(name = "lookupResult", type = "List", mode = "OUT")
        }
    )
    public interface LookupRoutingTask {}

    /**
     * Check if a new routingTaskAssoc is ok or not (same SeqId and Date)
     */
    @Service(
        name = "checkRoutingTaskAssoc",
        engine = "java",
        location = "org.ofbiz.manufacturing.techdata.TechDataServices",
        invoke = "checkRoutingTaskAssoc",
        description = "Check if a new routingTaskAssoc is ok or not (same SeqId and Date)",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "WorkEffortAssoc", mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "sequenceNum", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "java.sql.Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "create", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceNumNotOk", type = "String", mode = "OUT")
        }
    )
    public interface CheckRoutingTaskAssoc {}

    /**
     * Get the product's routing and routing tasks
     */
    @Service(
        name = "getProductRouting",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.RoutingServices",
        invoke = "getProductRouting",
        description = "Get the product's routing and routing tasks",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "workEffortId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "applicableDate", type = "java.sql.Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "ignoreDefaultRouting", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "routing", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "tasks", type = "java.util.List", mode = "OUT", optional = "true")
        }
    )
    public interface GetProductRouting {}

    /**
     * Get the routing task assocs of a given routing
     */
    @Service(
        name = "getRoutingTaskAssocs",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.RoutingServices",
        invoke = "getRoutingTaskAssocs",
        description = "Get the routing task assocs of a given routing",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "routingTaskAssocs", type = "java.util.List", mode = "OUT", optional = "true")
        }
    )
    public interface GetRoutingTaskAssocs {}

    /**
     * Computes the estimated time needed to perform the task
     */
    @Service(
        name = "getEstimatedTaskTime",
        engine = "java",
        location = "org.ofbiz.manufacturing.routing.RoutingServices",
        invoke = "getEstimatedTaskTime",
        description = "Computes the estimated time needed to perform the task",
        auth = "true",
        attributes = {
            @Attribute(name = "taskId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "routingId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "estimatedTaskTime", type = "Long", mode = "OUT"),
            @Attribute(name = "setupTime", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "taskUnitTime", type = "BigDecimal", mode = "OUT", optional = "true")
        }
    )
    public interface GetEstimatedTaskTime {}

}
