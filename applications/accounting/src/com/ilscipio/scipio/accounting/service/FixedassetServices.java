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
public class FixedassetServices {

    /**
     * Create a Fixed Asset
     */
    @Service(
        name = "createFixedAsset",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "createFixedAsset",
        description = "Create a Fixed Asset",
        defaultEntityName = "FixedAsset",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fixedAssetTypeId", optional = "false")
        }
    )
    public interface CreateFixedAsset {}

    /**
     * Update a Fixed Asset
     */
    @Service(
        name = "updateFixedAsset",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "updateFixedAsset",
        description = "Update a Fixed Asset",
        defaultEntityName = "FixedAsset",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fixedAssetTypeId", optional = "false")
        }
    )
    public interface UpdateFixedAsset {}

    /**
     * Add Product To Fixed Asset
     */
    @Service(
        name = "addFixedAssetProduct",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "addFixedAssetProduct",
        description = "Add Product To Fixed Asset",
        defaultEntityName = "FixedAssetProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface AddFixedAssetProduct {}

    /**
     * Update the Product to Fixed Asset information
     */
    @Service(
        name = "updateFixedAssetProduct",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "updateFixedAssetProduct",
        description = "Update the Product to Fixed Asset information",
        defaultEntityName = "FixedAssetProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateFixedAssetProduct {}

    /**
     * Remove Product From Fixed Asset
     */
    @Service(
        name = "removeFixedAssetProduct",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "removeFixedAssetProduct",
        description = "Remove Product From Fixed Asset",
        defaultEntityName = "FixedAssetProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveFixedAssetProduct {}

    /**
     * Create a Fixed Asset Standard Cost
     */
    @Service(
        name = "createFixedAssetStdCost",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "createFixedAssetStdCost",
        description = "Create a Fixed Asset Standard Cost",
        defaultEntityName = "FixedAssetStdCost",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateFixedAssetStdCost {}

    /**
     * Update a Fixed Asset Standard Cost
     */
    @Service(
        name = "updateFixedAssetStdCost",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "updateFixedAssetStdCost",
        description = "Update a Fixed Asset Standard Cost",
        defaultEntityName = "FixedAssetStdCost",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateFixedAssetStdCost {}

    /**
     * Cancel a Fixed Asset Standard Cost
     */
    @Service(
        name = "cancelFixedAssetStdCost",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "cancelFixedAssetStdCost",
        description = "Cancel a Fixed Asset Standard Cost",
        defaultEntityName = "FixedAssetStdCost",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "UPDATE")
    )
    public interface CancelFixedAssetStdCost {}

    /**
     * Create a Fixed Asset Identification
     */
    @Service(
        name = "createFixedAssetIdent",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "createFixedAssetIdent",
        description = "Create a Fixed Asset Identification",
        defaultEntityName = "FixedAssetIdent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateFixedAssetIdent {}

    /**
     * Update a Fixed Asset Identification
     */
    @Service(
        name = "updateFixedAssetIdent",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "updateFixedAssetIdent",
        description = "Update a Fixed Asset Identification",
        defaultEntityName = "FixedAssetIdent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateFixedAssetIdent {}

    /**
     * Remove a Fixed Asset Identification
     */
    @Service(
        name = "removeFixedAssetIdent",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "removeFixedAssetIdent",
        description = "Remove a Fixed Asset Identification",
        defaultEntityName = "FixedAssetIdent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveFixedAssetIdent {}

    /**
     * Create a Fixed Asset Registration
     */
    @Service(
        name = "createFixedAssetRegistration",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "createFixedAssetRegistration",
        description = "Create a Fixed Asset Registration",
        defaultEntityName = "FixedAssetRegistration",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateFixedAssetRegistration {}

    /**
     * Update a Fixed Asset Registration
     */
    @Service(
        name = "updateFixedAssetRegistration",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "updateFixedAssetRegistration",
        description = "Update a Fixed Asset Registration",
        defaultEntityName = "FixedAssetRegistration",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateFixedAssetRegistration {}

    /**
     * Delete a Fixed Asset Registration
     */
    @Service(
        name = "deleteFixedAssetRegistration",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "deleteFixedAssetRegistration",
        description = "Delete a Fixed Asset Registration",
        defaultEntityName = "FixedAssetRegistration",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteFixedAssetRegistration {}

    /**
     * Create a Fixed Asset Maintenance
     */
    @Service(
        name = "createFixedAssetMaint",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "createFixedAssetMaint",
        description = "Create a Fixed Asset Maintenance",
        defaultEntityName = "FixedAssetMaint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "estimatedStartDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "estimatedCompletionDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "maintTemplateWorkEffortId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "maintHistSeqId", mode = "OUT")
        }
    )
    public interface CreateFixedAssetMaint {}

    /**
     * Update a Fixed Asset Maintenance
     */
    @Service(
        name = "updateFixedAssetMaint",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "updateFixedAssetMaint",
        description = "Update a Fixed Asset Maintenance",
        defaultEntityName = "FixedAssetMaint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateFixedAssetMaint {}

    /**
     * Remove a Fixed Asset Maintenance
     */
    @Service(
        name = "deleteFixedAssetMaint",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "deleteFixedAssetMaint",
        description = "Remove a Fixed Asset Maintenance",
        defaultEntityName = "FixedAssetMaint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteFixedAssetMaint {}

    /**
     * Create Fixed Asset Maintenances from ProductMaint time intervals. Currently works         with day, month, and year interval types. This service is intended to be run as a regularly         scheduled job.
     */
    @Service(
        name = "createMaintsFromTimeInterval",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "createMaintsFromTimeInterval",
        description = "Create Fixed Asset Maintenances from ProductMaint time intervals. Currently works\n        with day, month, and year interval types. This service is intended to be run as a regularly\n        scheduled job.",
        auth = "true",
        useTransaction = "false",
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateMaintsFromTimeInterval {}

    /**
     * Create a Fixed Asset Maintenance Meter
     */
    @Service(
        name = "createFixedAssetMeter",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "createFixedAssetMeter",
        description = "Create a Fixed Asset Maintenance Meter",
        defaultEntityName = "FixedAssetMeter",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateFixedAssetMeter {}

    /**
     * Update a Fixed Asset Maintenance Meter
     */
    @Service(
        name = "updateFixedAssetMeter",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "updateFixedAssetMeter",
        description = "Update a Fixed Asset Maintenance Meter",
        defaultEntityName = "FixedAssetMeter",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateFixedAssetMeter {}

    /**
     * Remove a Fixed Asset Maintenance Meter
     */
    @Service(
        name = "deleteFixedAssetMeter",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "deleteFixedAssetMeter",
        description = "Remove a Fixed Asset Maintenance Meter",
        defaultEntityName = "FixedAssetMeter",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteFixedAssetMeter {}

    /**
     * Create a Fixed Asset Maintenance Order
     */
    @Service(
        name = "createFixedAssetMaintOrder",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "createFixedAssetMaintOrder",
        description = "Create a Fixed Asset Maintenance Order",
        defaultEntityName = "FixedAssetMaintOrder",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "fixedAssetId", type = "String", mode = "IN"),
            @Attribute(name = "maintHistSeqId", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateFixedAssetMaintOrder {}

    /**
     * Remove a Fixed Asset Maintenance Order
     */
    @Service(
        name = "deleteFixedAssetMaintOrder",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "deleteFixedAssetMaintOrder",
        description = "Remove a Fixed Asset Maintenance Order",
        defaultEntityName = "FixedAssetMaintOrder",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteFixedAssetMaintOrder {}

    /**
     * Add Party to a Fixed Asset
     */
    @Service(
        name = "createPartyFixedAssetAssignment",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "createPartyFixedAssetAssignment",
        description = "Add Party to a Fixed Asset",
        defaultEntityName = "PartyFixedAssetAssignment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreatePartyFixedAssetAssignment {}

    /**
     * Update Party to Fixed Asset
     */
    @Service(
        name = "updatePartyFixedAssetAssignment",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "updatePartyFixedAssetAssignment",
        description = "Update Party to Fixed Asset",
        defaultEntityName = "PartyFixedAssetAssignment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdatePartyFixedAssetAssignment {}

    /**
     * Delete Party to Fixed Asset
     */
    @Service(
        name = "deletePartyFixedAssetAssignment",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "deletePartyFixedAssetAssignment",
        description = "Delete Party to Fixed Asset",
        defaultEntityName = "PartyFixedAssetAssignment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "DELETE")
    )
    public interface DeletePartyFixedAssetAssignment {}

    /**
     * Create a Fixed Asset Depreciation Method
     */
    @Service(
        name = "createFixedAssetDepMethod",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Fixed Asset Depreciation Method",
        defaultEntityName = "FixedAssetDepMethod",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateFixedAssetDepMethod {}

    /**
     * Create a Fixed Asset Depreciation Method
     */
    @Service(
        name = "updateFixedAssetDepMethod",
        engine = "entity-auto",
        invoke = "update",
        description = "Create a Fixed Asset Depreciation Method",
        defaultEntityName = "FixedAssetDepMethod",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateFixedAssetDepMethod {}

    /**
     * Delete a Fixed Asset Depreciation Method
     */
    @Service(
        name = "deleteFixedAssetDepMethod",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Fixed Asset Depreciation Method",
        defaultEntityName = "FixedAssetDepMethod",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteFixedAssetDepMethod {}

    /**
     * If the accounting transaction is a depreciation transaction for a fixed asset, update the depreciation amount in the FixedAsset entity.
     */
    @Service(
        name = "checkUpdateFixedAssetDepreciation",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "checkUpdateFixedAssetDepreciation",
        description = "If the accounting transaction is a depreciation transaction for a fixed asset, update the depreciation amount in the FixedAsset entity.",
        defaultEntityName = "AcctgTrans",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "UPDATE")
    )
    public interface CheckUpdateFixedAssetDepreciation {}

    /**
     * Interface to describe base parameters for Depreciation Calculation Services
     */
    @Service(
        name = "fixedAssetDepCalcInterface",
        engine = "interface",
        description = "Interface to describe base parameters for Depreciation Calculation Services",
        attributes = {
            @Attribute(name = "expEndOfLifeYear", type = "Integer", mode = "IN"),
            @Attribute(name = "assetAcquiredYear", type = "Integer", mode = "IN"),
            @Attribute(name = "purchaseCost", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "salvageValue", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "usageYears", type = "Integer", mode = "IN"),
            @Attribute(name = "assetDepreciationTillDate", type = "List", mode = "OUT"),
            @Attribute(name = "assetNBVAfterDepreciation", type = "List", mode = "OUT"),
            @Attribute(name = "assetDepreciationInfoList", type = "List", mode = "OUT"),
            @Attribute(name = "nextDepreciationAmount", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "plannedPastDepreciationTotal", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface FixedAssetDepCalcInterface {}

    /**
     * Straight line depreciation service to Fixed Asset
     */
    @Service(
        name = "straightLineDepreciation",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "straightLineDepreciation",
        description = "Straight line depreciation service to Fixed Asset",
        defaultEntityName = "FixedAsset",
        auth = "true",
        implemented = {@Implements(service = "fixedAssetDepCalcInterface")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface StraightLineDepreciation {}

    /**
     * Double declining balance depreciation service to Fixed Asset
     */
    @Service(
        name = "doubleDecliningBalanceDepreciation",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "doubleDecliningBalanceDepreciation",
        description = "Double declining balance depreciation service to Fixed Asset",
        defaultEntityName = "FixedAsset",
        auth = "true",
        implemented = {@Implements(service = "fixedAssetDepCalcInterface")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DoubleDecliningBalanceDepreciation {}

    /**
     * Select the depreciation method according to the entry in FixedAssetDepMethod
     */
    @Service(
        name = "calculateFixedAssetDepreciation",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "calculateFixedAssetDepreciation",
        description = "Select the depreciation method according to the entry in FixedAssetDepMethod",
        defaultEntityName = "FixedAssetDepMethod",
        auth = "true",
        attributes = {
            @Attribute(name = "fixedAssetId", type = "String", mode = "IN"),
            @Attribute(name = "assetDepreciationTillDate", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "assetNBVAfterDepreciation", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "assetDepreciationInfoList", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "nextDepreciationAmount", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "plannedPastDepreciationTotal", type = "BigDecimal", mode = "OUT", optional = "true")
        }
    )
    public interface CalculateFixedAssetDepreciation {}

    /**
     * Create a Fixed Asset Type Gl Account Mapping
     */
    @Service(
        name = "createFixedAssetTypeGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml",
        invoke = "createFixedAssetTypeGlAccount",
        description = "Create a Fixed Asset Type Gl Account Mapping",
        defaultEntityName = "FixedAssetTypeGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "fixedAssetTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fixedAssetId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateFixedAssetTypeGlAccount {}

    /**
     * Update a Fixed Asset Type Gl Account Mapping
     */
    @Service(
        name = "updateFixedAssetTypeGlAccount",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Fixed Asset Type Gl Account Mapping",
        defaultEntityName = "FixedAssetTypeGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateFixedAssetTypeGlAccount {}

    /**
     * Delete a Fixed Asset Type Gl Account Mapping
     */
    @Service(
        name = "deleteFixedAssetTypeGlAccount",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Fixed Asset Type Gl Account Mapping",
        defaultEntityName = "FixedAssetTypeGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteFixedAssetTypeGlAccount {}

    /**
     * Create a FixedAssetGeoPoint
     */
    @Service(
        name = "createFixedAssetGeoPoint",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FixedAssetGeoPoint",
        defaultEntityName = "FixedAssetGeoPoint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateFixedAssetGeoPoint {}

    /**
     * Update a FixedAssetGeoPoint
     */
    @Service(
        name = "updateFixedAssetGeoPoint",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a FixedAssetGeoPoint",
        defaultEntityName = "FixedAssetGeoPoint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateFixedAssetGeoPoint {}

    /**
     * Delete a FixedAssetGeoPoint
     */
    @Service(
        name = "deleteFixedAssetGeoPoint",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a FixedAssetGeoPoint",
        defaultEntityName = "FixedAssetGeoPoint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "fixedAssetPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteFixedAssetGeoPoint {}

    /**
     * Create an AccommodationClass
     */
    @Service(
        name = "createAccommodationClass",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an AccommodationClass",
        defaultEntityName = "AccommodationClass",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateAccommodationClass {}

    /**
     * Update an AccommodationClass
     */
    @Service(
        name = "updateAccommodationClass",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an AccommodationClass",
        defaultEntityName = "AccommodationClass",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAccommodationClass {}

    /**
     * Delete an AccommodationClass
     */
    @Service(
        name = "deleteAccommodationClass",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an AccommodationClass",
        defaultEntityName = "AccommodationClass",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteAccommodationClass {}

    /**
     * Create a AccommodationMapType entry
     */
    @Service(
        name = "createAccommodationMapType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a AccommodationMapType entry",
        defaultEntityName = "AccommodationMapType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateAccommodationMapType {}

    /**
     * Update a AccommodationMapType record
     */
    @Service(
        name = "updateAccommodationMapType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a AccommodationMapType record",
        defaultEntityName = "AccommodationMapType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAccommodationMapType {}

    /**
     * Delete a AccommodationMapType record
     */
    @Service(
        name = "deleteAccommodationMapType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a AccommodationMapType record",
        defaultEntityName = "AccommodationMapType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteAccommodationMapType {}

    /**
     * Create a AccommodationMap
     */
    @Service(
        name = "createAccommodationMap",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a AccommodationMap",
        defaultEntityName = "AccommodationMap",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateAccommodationMap {}

    /**
     * Update a AccommodationMap
     */
    @Service(
        name = "updateAccommodationMap",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a AccommodationMap",
        defaultEntityName = "AccommodationMap",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAccommodationMap {}

    /**
     * Delete a AccommodationMap
     */
    @Service(
        name = "deleteAccommodationMap",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a AccommodationMap",
        defaultEntityName = "AccommodationMap",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteAccommodationMap {}

    /**
     * Create a FixedAssetAttribute
     */
    @Service(
        name = "createFixedAssetAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FixedAssetAttribute",
        defaultEntityName = "FixedAssetAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateFixedAssetAttribute {}

    /**
     * Update a FixedAssetAttribute
     */
    @Service(
        name = "updateFixedAssetAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a FixedAssetAttribute",
        defaultEntityName = "FixedAssetAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFixedAssetAttribute {}

    /**
     * Delete a FixedAssetAttribute
     */
    @Service(
        name = "deleteFixedAssetAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a FixedAssetAttribute",
        defaultEntityName = "FixedAssetAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFixedAssetAttribute {}

    /**
     * Create a FixedAssetIdentType
     */
    @Service(
        name = "createFixedAssetIdentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FixedAssetIdentType",
        defaultEntityName = "FixedAssetIdentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateFixedAssetIdentType {}

    /**
     * Update a FixedAssetIdentType
     */
    @Service(
        name = "updateFixedAssetIdentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a FixedAssetIdentType",
        defaultEntityName = "FixedAssetIdentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFixedAssetIdentType {}

    /**
     * Delete a FixedAssetIdentType
     */
    @Service(
        name = "deleteFixedAssetIdentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a FixedAssetIdentType",
        defaultEntityName = "FixedAssetIdentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFixedAssetIdentType {}

    /**
     * Create a FixedAssetProductType
     */
    @Service(
        name = "createFixedAssetProductType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FixedAssetProductType",
        defaultEntityName = "FixedAssetProductType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateFixedAssetProductType {}

    /**
     * Update a FixedAssetProductType
     */
    @Service(
        name = "updateFixedAssetProductType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a FixedAssetProductType",
        defaultEntityName = "FixedAssetProductType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFixedAssetProductType {}

    /**
     * Delete a FixedAssetProductType
     */
    @Service(
        name = "deleteFixedAssetProductType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a FixedAssetProductType",
        defaultEntityName = "FixedAssetProductType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFixedAssetProductType {}

    /**
     * Create a FixedAssetStdCostType
     */
    @Service(
        name = "createFixedAssetStdCostType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FixedAssetStdCostType",
        defaultEntityName = "FixedAssetStdCostType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateFixedAssetStdCostType {}

    /**
     * Update a FixedAssetStdCostType
     */
    @Service(
        name = "updateFixedAssetStdCostType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a FixedAssetStdCostType",
        defaultEntityName = "FixedAssetStdCostType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFixedAssetStdCostType {}

    /**
     * Delete a FixedAssetStdCostType
     */
    @Service(
        name = "deleteFixedAssetStdCostType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a FixedAssetStdCostType",
        defaultEntityName = "FixedAssetStdCostType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFixedAssetStdCostType {}

    /**
     * Create a FixedAssetType
     */
    @Service(
        name = "createFixedAssetType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FixedAssetType",
        defaultEntityName = "FixedAssetType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateFixedAssetType {}

    /**
     * Update a FixedAssetType
     */
    @Service(
        name = "updateFixedAssetType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a FixedAssetType",
        defaultEntityName = "FixedAssetType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFixedAssetType {}

    /**
     * Delete a FixedAssetType
     */
    @Service(
        name = "deleteFixedAssetType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a FixedAssetType",
        defaultEntityName = "FixedAssetType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFixedAssetType {}

    /**
     * Create a FixedAssetTypeAttr
     */
    @Service(
        name = "createFixedAssetTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FixedAssetTypeAttr",
        defaultEntityName = "FixedAssetTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateFixedAssetTypeAttr {}

    /**
     * Update a FixedAssetTypeAttr
     */
    @Service(
        name = "updateFixedAssetTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a FixedAssetTypeAttr",
        defaultEntityName = "FixedAssetTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFixedAssetTypeAttr {}

    /**
     * Delete a FixedAssetTypeAttr
     */
    @Service(
        name = "deleteFixedAssetTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a FixedAssetTypeAttr",
        defaultEntityName = "FixedAssetTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFixedAssetTypeAttr {}

}
