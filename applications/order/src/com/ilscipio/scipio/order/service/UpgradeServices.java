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
package com.ilscipio.scipio.order.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class UpgradeServices {

    /**
     *              Migrate data from OldOrderItemAssociation to OrderItemAssoc.             Since revision 485144 (2006-12-10) the entity OrderItemAssociation has been deprecated.             This service can be used to upgrade existing data from the OrderItemAssociation entity to the new             OrderItemAssoc entity.             Before running this service, load the seed data for the OrderItemAssocType entity from the file:             order/data/OrderTypeData.xml         
     */
    @Service(
        name = "migrateOrderItemAssociation",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/UpgradeServices.xml",
        invoke = "migrateOrderItemAssociation",
        description = "\n            Migrate data from OldOrderItemAssociation to OrderItemAssoc.\n            Since revision 485144 (2006-12-10) the entity OrderItemAssociation has been deprecated.\n            This service can be used to upgrade existing data from the OrderItemAssociation entity to the new\n            OrderItemAssoc entity.\n            Before running this service, load the seed data for the OrderItemAssocType entity from the file:\n            order/data/OrderTypeData.xml\n        "
    )
    public interface MigrateOrderItemAssociation {}

    /**
     *              Migrate data from OldCustRequestRole to CustRequestParty.             Since revision 684647 (2008-08-11) the entity CustRequestRole has been deprecated.             This service can be used to upgrade existing data from the OldCustRequestRole entity to the new             CustRequestParty entity.             Before running this service, load the seed data for the RoleType entity from the file:             party/data/PartyTypeData.xml         
     */
    @Service(
        name = "migrateCustRequestRole",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/UpgradeServices.xml",
        invoke = "migrateCustRequestRole",
        description = "\n            Migrate data from OldCustRequestRole to CustRequestParty.\n            Since revision 684647 (2008-08-11) the entity CustRequestRole has been deprecated.\n            This service can be used to upgrade existing data from the OldCustRequestRole entity to the new\n            CustRequestParty entity.\n            Before running this service, load the seed data for the RoleType entity from the file:\n            party/data/PartyTypeData.xml\n        "
    )
    public interface MigrateCustRequestRole {}

    /**
     *              Since revision 895250 (2010-01-02) the entity OrderShipment is used to record purchase order items that             will be received as part of a purchase shipment. Previously ItemIssuance was used with an empty inventoryId.             This service will replace ItemIssuaces with OrderShipment records for the required shipments.         
     */
    @Service(
        name = "migrateOrderShipment",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/UpgradeServices.xml",
        invoke = "migrateOrderShipment",
        description = "\n            Since revision 895250 (2010-01-02) the entity OrderShipment is used to record purchase order items that\n            will be received as part of a purchase shipment. Previously ItemIssuance was used with an empty inventoryId.\n            This service will replace ItemIssuaces with OrderShipment records for the required shipments.\n        "
    )
    public interface MigrateOrderShipment {}

}
