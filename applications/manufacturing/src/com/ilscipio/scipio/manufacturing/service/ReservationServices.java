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
 * Lot reservation service definitions: a reservation holds a whole inventory lot for a production run task
 * by pushing its available-to-promise down (quantity on hand is untouched), so the lot is not picked for
 * anything else while the task is open.
 *
 * <p>SCIPIO: 4.0.0: New.</p>
 */
public class ReservationServices {

    /**
     * Holds a lot for a production run task: resolves the InventoryItem for the given lotId (or inventoryItemId)
     * and productId in the run's facility, checks the product is a component of the task, then reserves the
     * item's whole available-to-promise for the task.
     */
    @Service(
        name = "reserveProductionRunLot",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.reservation.ReservationServices",
        invoke = "reserveProductionRunLot",
        description = "Holds a whole inventory lot for a production run task by reserving its available-to-promise",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "productionRunId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "lotId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "OUT"),
            @Attribute(name = "quantityReserved", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface ReserveProductionRunLot {}

    /**
     * Releases one open lot reservation of a production run task: gives the reserved quantity back to
     * available-to-promise and marks the reservation released.
     */
    @Service(
        name = "releaseProductionRunLot",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.reservation.ReservationServices",
        invoke = "releaseProductionRunLot",
        description = "Releases a lot reservation of a production run task back to available-to-promise",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN")
        }
    )
    public interface ReleaseProductionRunLot {}

    /**
     * Releases every open lot reservation of every task of a production run; called when the run closes.
     */
    @Service(
        name = "releaseProductionRunReservations",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.reservation.ReservationServices",
        invoke = "releaseProductionRunReservations",
        description = "Releases every open lot reservation of a production run's tasks",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN"),
            @Attribute(name = "releasedCount", type = "Integer", mode = "OUT", optional = "true")
        }
    )
    public interface ReleaseProductionRunReservations {}

    /**
     * Gives back to available-to-promise the quantity just issued from a reserved inventory item, and lowers
     * the reservation's held quantity by the same amount, so the issued quantity is not deducted from
     * available-to-promise twice (once by the reservation, once by the issuance). A no-op when the item
     * carries no open reservation for the task.
     */
    @Service(
        name = "adjustProductionRunLotReservation",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.reservation.ReservationServices",
        invoke = "adjustProductionRunLotReservation",
        description = "Gives an issued quantity back to available-to-promise on a lot reservation and lowers the reserved quantity by it",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItem", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "quantityIssued", type = "BigDecimal", mode = "IN", optional = "true")
        }
    )
    public interface AdjustProductionRunLotReservation {}

    /**
     * Returns the open lot reservations of a production run's tasks, with lot, product, item, quantity, unit
     * and task info for display.
     */
    @Service(
        name = "getProductionRunReservations",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.reservation.ReservationServices",
        invoke = "getProductionRunReservations",
        description = "Returns the open lot reservations of a production run's tasks",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN"),
            @Attribute(name = "reservations", type = "List", mode = "OUT")
        }
    )
    public interface GetProductionRunReservations {}

}
