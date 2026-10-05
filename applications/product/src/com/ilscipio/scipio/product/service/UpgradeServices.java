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
public class UpgradeServices {

    /**
     *              Migrate data from OldFacilityRole to FacilityParty.             Since revision 698159 (2008-09-23) the entity FacilityRole has been deprecated.             This service can be used to upgrade existing data from the FacilityRole entity to the new             FacilityParty entity.             Before running this service, load the seed data for the RoleType entity from the file:             party/data/PartyTypeData.xml         
     */
    @Service(
        name = "migrateFacilityRole",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/UpgradeServices.xml",
        invoke = "migrateFacilityRole",
        description = "\n            Migrate data from OldFacilityRole to FacilityParty.\n            Since revision 698159 (2008-09-23) the entity FacilityRole has been deprecated.\n            This service can be used to upgrade existing data from the FacilityRole entity to the new\n            FacilityParty entity.\n            Before running this service, load the seed data for the RoleType entity from the file:\n            party/data/PartyTypeData.xml\n        "
    )
    public interface MigrateFacilityRole {}

    /**
     *              Migrate data from Facility.oldSquareFootage to Facility.facilitySize.             The field Facility.squareFootageSince has been deprecated since revision 929912 (2010-04-01)         
     */
    @Service(
        name = "migrateFacilitySquareFootage",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/UpgradeServices.xml",
        invoke = "migrateFacilitySquareFootage",
        description = "\n            Migrate data from Facility.oldSquareFootage to Facility.facilitySize.\n            The field Facility.squareFootageSince has been deprecated since revision 929912 (2010-04-01)\n        "
    )
    public interface MigrateFacilitySquareFootage {}

    /**
     *              Migrate data from oldProductKeyword to ProductKeyword.             The entity oldProductKeyword has been deprecated.             This service can be used to upgrade existing data from the oldProductKeyword entity to the new             ProductKeyword entity.             Before running this service, load the seed data for the KeywordType entity from the file:             common/data/CommonTypeData.xml         
     */
    @Service(
        name = "migrateProductKeyword",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/UpgradeServices.xml",
        invoke = "migrateProductKeyword",
        description = "\n            Migrate data from oldProductKeyword to ProductKeyword.\n            The entity oldProductKeyword has been deprecated.\n            This service can be used to upgrade existing data from the oldProductKeyword entity to the new\n            ProductKeyword entity.\n            Before running this service, load the seed data for the KeywordType entity from the file:\n            common/data/CommonTypeData.xml\n        "
    )
    public interface MigrateProductKeyword {}

}
