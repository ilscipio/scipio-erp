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
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class RateServices {

    /**
     * Create/update Rate Amount
     */
    @Service(
        name = "updateRateAmount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/rate/RateServices.xml",
        invoke = "updateRateAmount",
        description = "Create/update Rate Amount",
        defaultEntityName = "RateAmount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgBasePermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "rateTypeId", optional = "false"),
            @OverrideAttribute(name = "rateAmount", optional = "false")
        }
    )
    public interface UpdateRateAmount {}

    /**
     * Expire Rate Amount
     */
    @Service(
        name = "expireRateAmount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/rate/RateServices.xml",
        invoke = "expireRateAmount",
        description = "Expire Rate Amount",
        defaultEntityName = "RateAmount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgBasePermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "rateTypeId", optional = "false"),
            @OverrideAttribute(name = "fromDate", optional = "false")
        }
    )
    public interface ExpireRateAmount {}

    /**
     * Delete Rate Amount (SCIPIO: NOTE: this actually expires it; always has)
     */
    @Service(
        name = "deleteRateAmount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/rate/RateServices.xml",
        invoke = "deleteRateAmount",
        description = "Delete Rate Amount (SCIPIO: NOTE: this actually expires it; always has)",
        defaultEntityName = "RateAmount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgBasePermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "rateTypeId", optional = "false"),
            @OverrideAttribute(name = "fromDate", optional = "false")
        }
    )
    public interface DeleteRateAmount {}

    /**
     * Get Rate Amount
     */
    @Service(
        name = "getRateAmount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/rate/RateServices.xml",
        invoke = "getRateAmount",
        description = "Get Rate Amount",
        defaultEntityName = "RateAmount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true")
        },
        attributes = {
            @Attribute(name = "level", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "rateAmount", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "periodTypeId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "rateCurrencyUomId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgBasePermissionCheck", mainAction = "VIEW"),
        overrideAttributes = {
            @OverrideAttribute(name = "rateTypeId", optional = "false")
        }
    )
    public interface GetRateAmount {}

    /**
     * Get all Rates Amounts for a given workEffortId
     */
    @Service(
        name = "getRatesAmountsFromWorkEffortId",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/rate/RateServices.xml",
        invoke = "getRatesAmountsFromWorkEffortId",
        description = "Get all Rates Amounts for a given workEffortId",
        defaultEntityName = "RateAmount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true")
        },
        attributes = {
            @Attribute(name = "periodTypeId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "rateCurrencyUomId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "OUT", optional = "true"),
            @Attribute(name = "ratesList", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgBasePermissionCheck", mainAction = "VIEW"),
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface GetRatesAmountsFromWorkEffortId {}

    /**
     * Get all Rates Amounts for a given partyId
     */
    @Service(
        name = "getRatesAmountsFromPartyId",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/rate/RateServices.xml",
        invoke = "getRatesAmountsFromPartyId",
        description = "Get all Rates Amounts for a given partyId",
        defaultEntityName = "RateAmount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true")
        },
        attributes = {
            @Attribute(name = "periodTypeId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "rateCurrencyUomId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "OUT", optional = "true"),
            @Attribute(name = "ratesList", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgBasePermissionCheck", mainAction = "VIEW"),
        overrideAttributes = {
            @OverrideAttribute(name = "partyId", optional = "false")
        }
    )
    public interface GetRatesAmountsFromPartyId {}

    /**
     * Get all Rates Amounts for a given emplPositionTypeId
     */
    @Service(
        name = "getRatesAmountsFromEmplPositionTypeId",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/rate/RateServices.xml",
        invoke = "getRatesAmountsFromEmplPositionTypeId",
        description = "Get all Rates Amounts for a given emplPositionTypeId",
        defaultEntityName = "RateAmount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true")
        },
        attributes = {
            @Attribute(name = "periodTypeId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "rateCurrencyUomId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "OUT", optional = "true"),
            @Attribute(name = "ratesList", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgBasePermissionCheck", mainAction = "VIEW"),
        overrideAttributes = {
            @OverrideAttribute(name = "emplPositionTypeId", optional = "false")
        }
    )
    public interface GetRatesAmountsFromEmplPositionTypeId {}

    /**
     * Get the most specific non-empty Rate Amount list from a list of Rate Amount, given the input parameters :         workEffortId, partyId, emplPositionTypeId and rateTypeId
     */
    @Service(
        name = "filterRateAmountList",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/rate/RateServices.xml",
        invoke = "filterRateAmountList",
        description = "Get the most specific non-empty Rate Amount list from a list of Rate Amount, given the input parameters :\n        workEffortId, partyId, emplPositionTypeId and rateTypeId",
        defaultEntityName = "RateAmount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "ratesList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "filteredRatesList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface FilterRateAmountList {}

    /**
     * Creates PartyRate
     */
    @Service(
        name = "updatePartyRate",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/rate/RateServices.xml",
        invoke = "updatePartyRate",
        description = "Creates PartyRate",
        defaultEntityName = "PartyRate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "rateAmount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "rateCurrencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "periodTypeId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "acctgBasePermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface UpdatePartyRate {}

    /**
     * Deletes (expires) PartyRate (SCIPIO: NOTE: This expires the PartyRate; it cannot be deleted)
     */
    @Service(
        name = "deletePartyRate",
        engine = "group",
        description = "Deletes (expires) PartyRate (SCIPIO: NOTE: This expires the PartyRate; it cannot be deleted)",
        invokes = {@GroupInvoke(name = "expirePartyRate", resultToContext = "false")}
    )
    public interface DeletePartyRate {}

    /**
     * Expire PartyRate and expire related rateAmount
     */
    @Service(
        name = "expirePartyRate",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/rate/RateServices.xml",
        invoke = "expirePartyRate",
        description = "Expire PartyRate and expire related rateAmount",
        defaultEntityName = "PartyRate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "rateAmountFromDate", type = "Timestamp", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgBasePermissionCheck", mainAction = "UPDATE")
    )
    public interface ExpirePartyRate {}

    /**
     * Create a RateType
     */
    @Service(
        name = "createRateType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a RateType",
        defaultEntityName = "RateType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateRateType {}

    /**
     * Update a RateType
     */
    @Service(
        name = "updateRateType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a RateType",
        defaultEntityName = "RateType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateRateType {}

    /**
     * Delete a RateType
     */
    @Service(
        name = "deleteRateType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a RateType",
        defaultEntityName = "RateType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteRateType {}

}
