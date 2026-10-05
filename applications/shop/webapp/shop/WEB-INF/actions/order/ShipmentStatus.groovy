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

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;

shipmentId = parameters.shipmentId;
if (shipmentId) {
    shipment = from("Shipment").where("shipmentId", shipmentId).queryOne();
    shipmentItems = from("ShipmentItem").where("shipmentId", shipmentId).queryList();

    // get Shipment tracking info
    orderShipmentInfoSummaryList = select("shipmentId", "shipmentRouteSegmentId", "shipmentPackageSeqId", "carrierPartyId", "trackingCode")
                                    .from("OrderShipmentInfoSummary")
                                    .where("shipmentId", shipmentId)
                                    .orderBy("shipmentId", "shipmentRouteSegmentId", "shipmentPackageSeqId")
                                    .distinct()
                                    .queryList();

    context.shipment = shipment;
    context.shipmentItems = shipmentItems;
    context.orderShipmentInfoSummaryList = orderShipmentInfoSummaryList;
}
