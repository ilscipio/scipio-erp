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
package com.ilscipio.scipio.accounting.service;

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
     *              Migrate statusId to GlReconciliation entity,             this service can be used to upgrade existing data i.e it sets the statusId(new field in entity) to "Created" if reconciledBalance found empty otherwise sets "Reconciled".             Before running this service, load the seed data for StatusType and StatusItem from the file :             accounting/data/AccountingTypeData.xml         
     */
    @Service(
        name = "migrateStatusToGlReconciliation",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/UpgradeServices.xml",
        invoke = "migrateStatusToGlReconciliation",
        description = "\n            Migrate statusId to GlReconciliation entity,\n            this service can be used to upgrade existing data i.e it sets the statusId(new field in entity) to \"Created\" if reconciledBalance found empty otherwise sets \"Reconciled\".\n            Before running this service, load the seed data for StatusType and StatusItem from the file :\n            accounting/data/AccountingTypeData.xml\n        "
    )
    public interface MigrateStatusToGlReconciliation {}

    /**
     *              Migrate statusId to FinAccountTrans entity,             this service can be used to upgrade existing data i.e it sets the statusId(new field in entity) to "Approved" if found empty.             Before running this service, load the seed data for StatusType and StatusItem from the file :             accounting/data/AccountingTypeData.xml         
     */
    @Service(
        name = "migrateStatusToFinAccountTrans",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/UpgradeServices.xml",
        invoke = "migrateStatusToFinAccountTrans",
        description = "\n            Migrate statusId to FinAccountTrans entity,\n            this service can be used to upgrade existing data i.e it sets the statusId(new field in entity) to \"Approved\" if found empty.\n            Before running this service, load the seed data for StatusType and StatusItem from the file :\n            accounting/data/AccountingTypeData.xml\n        "
    )
    public interface MigrateStatusToFinAccountTrans {}

    /**
     * Copy the FixedAssetMaintMeter entity to FixedAssetMeter. FixedAssetMeter.readingDate will be replaced with FixedAssetMaintMeter.createdStamp.
     */
    @Service(
        name = "migrateFixedAssetMaintMeter",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/UpgradeServices.xml",
        invoke = "migrateFixedAssetMaintMeter",
        description = "Copy the FixedAssetMaintMeter entity to FixedAssetMeter. FixedAssetMeter.readingDate will be replaced with FixedAssetMaintMeter.createdStamp.",
        auth = "true",
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "CREATE")
    )
    public interface MigrateFixedAssetMaintMeter {}

    /**
     * Copy the AgreementWorkEffortAppl entity to AgreementWorkEffortApplic
     */
    @Service(
        name = "migrateAgreementWorkEffortAppl",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/UpgradeServices.xml",
        invoke = "migrateAgreementWorkEffortAppl",
        description = "Copy the AgreementWorkEffortAppl entity to AgreementWorkEffortApplic",
        auth = "true",
        permissionService = @PermissionService(service = "acctgAgreementPermissionCheck", mainAction = "CREATE")
    )
    public interface MigrateAgreementWorkEffortAppl {}

}
