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
package com.ilscipio.scipio.product.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ShipmentgatewayServices {

    /**
     * Update Shipment Gateway Config DHL
     */
    @Service(
        name = "updateShipmentGatewayConfigDhl",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentGatewayConfigServices.xml",
        invoke = "updateShipmentGatewayConfigDhl",
        description = "Update Shipment Gateway Config DHL",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentGatewayDhl", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "ShipmentGatewayDhl", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateShipmentGatewayConfigDhl {}

    /**
     * Update Shipment Gateway Config FedEx
     */
    @Service(
        name = "updateShipmentGatewayConfigFedex",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentGatewayConfigServices.xml",
        invoke = "updateShipmentGatewayConfigFedex",
        description = "Update Shipment Gateway Config FedEx",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentGatewayFedex", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "ShipmentGatewayFedex", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateShipmentGatewayConfigFedex {}

    /**
     * Update Shipment Gateway Config UPS
     */
    @Service(
        name = "updateShipmentGatewayConfigUps",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentGatewayConfigServices.xml",
        invoke = "updateShipmentGatewayConfigUps",
        description = "Update Shipment Gateway Config UPS",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentGatewayUps", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "ShipmentGatewayUps", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateShipmentGatewayConfigUps {}

    /**
     * Update Shipment Gateway Config USPS
     */
    @Service(
        name = "updateShipmentGatewayConfigUsps",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentGatewayConfigServices.xml",
        invoke = "updateShipmentGatewayConfigUsps",
        description = "Update Shipment Gateway Config USPS",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentGatewayUsps", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "ShipmentGatewayUsps", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateShipmentGatewayConfigUsps {}

    /**
     * Create a ShipmentGatewayConfig record
     */
    @Service(
        name = "createShipmentGatewayConfig",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ShipmentGatewayConfig record",
        defaultEntityName = "ShipmentGatewayConfig",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateShipmentGatewayConfig {}

    /**
     * Update Shipment Gateway Config
     */
    @Service(
        name = "updateShipmentGatewayConfig",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentGatewayConfigServices.xml",
        invoke = "updateShipmentGatewayConfig",
        description = "Update Shipment Gateway Config",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentGatewayConfig", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "ShipmentGatewayConfig", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateShipmentGatewayConfig {}

    /**
     * Delete a ShipmentGatewayConfig record
     */
    @Service(
        name = "deleteShipmentGatewayConfig",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ShipmentGatewayConfig record",
        defaultEntityName = "ShipmentGatewayConfig",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteShipmentGatewayConfig {}

    /**
     * Create ShipmentGatewayConfigType
     */
    @Service(
        name = "createShipmentGatewayConfigType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create ShipmentGatewayConfigType",
        defaultEntityName = "ShipmentGatewayConfigType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateShipmentGatewayConfigType {}

    /**
     * Update Shipment Gateway Config Type
     */
    @Service(
        name = "updateShipmentGatewayConfigType",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentGatewayConfigServices.xml",
        invoke = "updateShipmentGatewayConfigType",
        description = "Update Shipment Gateway Config Type",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentGatewayConfigType", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "ShipmentGatewayConfigType", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateShipmentGatewayConfigType {}

    /**
     * Delete ShipmentGatewayConfigType
     */
    @Service(
        name = "deleteShipmentGatewayConfigType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete ShipmentGatewayConfigType",
        defaultEntityName = "ShipmentGatewayConfigType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteShipmentGatewayConfigType {}

    /**
     * Create ShipmentType
     */
    @Service(
        name = "createShipmentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create ShipmentType",
        defaultEntityName = "ShipmentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateShipmentType {}

    /**
     * Update ShipmentType
     */
    @Service(
        name = "updateShipmentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update ShipmentType",
        defaultEntityName = "ShipmentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateShipmentType {}

    /**
     * Delete ShipmentType
     */
    @Service(
        name = "deleteShipmentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete ShipmentType",
        defaultEntityName = "ShipmentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteShipmentType {}

}
