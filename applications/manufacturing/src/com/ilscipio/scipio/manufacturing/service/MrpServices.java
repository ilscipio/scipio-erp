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
public class MrpServices {

    /**
     * Performs a run of Mrp
     */
    @Service(
        name = "executeMrp",
        engine = "java",
        location = "org.ofbiz.manufacturing.mrp.MrpServices",
        invoke = "executeMrp",
        description = "Performs a run of Mrp",
        auth = "true",
        transactionTimeout = "7200",
        maxRetry = "0",
        attributes = {
            @Attribute(name = "facilityGroupId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "manufacturingFacilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mrpName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "defaultYearsOffset", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "msgResult", type = "List", mode = "OUT"),
            @Attribute(name = "mrpId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface ExecuteMrp {}

    /**
     * Initialize data for the MRP
     */
    @Service(
        name = "initMrpEvents",
        engine = "java",
        location = "org.ofbiz.manufacturing.mrp.MrpServices",
        invoke = "initMrpEvents",
        description = "Initialize data for the MRP",
        auth = "true",
        attributes = {
            @Attribute(name = "mrpId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "manufacturingFacilityId", type = "String", mode = "IN"),
            @Attribute(name = "reInitialize", type = "Boolean", mode = "IN"),
            @Attribute(name = "defaultYearsOffset", type = "Integer", mode = "IN", optional = "true")
        }
    )
    public interface InitMrpEvents {}

    /**
     * create an MrpEvent
     */
    @Service(
        name = "createMrpEvent",
        engine = "java",
        location = "org.ofbiz.manufacturing.mrp.InventoryEventPlannedServices",
        invoke = "createMrpEvent",
        description = "create an MrpEvent",
        attributes = {
            @Attribute(name = "mrpId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "eventDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "mrpEventTypeId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "eventName", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateMrpEvent {}

    /**
     * Set estimated ship dates for order items based on outstanding production runs
     */
    @Service(
        name = "setEstimatedDeliveryDates",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "setEstimatedDeliveryDates",
        description = "Set estimated ship dates for order items based on outstanding production runs"
    )
    public interface SetEstimatedDeliveryDates {}

    /**
     * Create a MrpEventType record
     */
    @Service(
        name = "createMrpEventType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a MrpEventType record",
        defaultEntityName = "MrpEventType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateMrpEventType {}

    /**
     * Update a MrpEventType record
     */
    @Service(
        name = "updateMrpEventType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a MrpEventType record",
        defaultEntityName = "MrpEventType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateMrpEventType {}

    /**
     * Delete a MrpEventType record
     */
    @Service(
        name = "deleteMrpEventType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a MrpEventType record",
        defaultEntityName = "MrpEventType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteMrpEventType {}

}
