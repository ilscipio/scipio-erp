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
package com.ilscipio.scipio.marketing.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class OpportunityServices {

    /**
     * Creates a Sales Forecast for the userLogin. Requires ORDERMGR_4C_CREATE permission.             This will save the forecast into the history as well. Note that this service does not compute             the forecast. That must be done in a higher level service.
     */
    @Service(
        name = "createSalesForecast",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/opportunity/OpportunityServices.xml",
        invoke = "createSalesForecast",
        description = "Creates a Sales Forecast for the userLogin. Requires ORDERMGR_4C_CREATE permission.\n            This will save the forecast into the history as well. Note that this service does not compute\n            the forecast. That must be done in a higher level service.",
        defaultEntityName = "SalesForecast",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateSalesForecast {}

    /**
     * Updates a Sales Forecast and marks it as modified by the userLogin. Requires ORDERMGR_4C_UPDATE             permission. This will save the current forecast into the history before overwritting it.             Note that this service does not compute the forecast. That must be done in a higher level service.
     */
    @Service(
        name = "updateSalesForecast",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/opportunity/OpportunityServices.xml",
        invoke = "updateSalesForecast",
        description = "Updates a Sales Forecast and marks it as modified by the userLogin. Requires ORDERMGR_4C_UPDATE\n            permission. This will save the current forecast into the history before overwritting it.\n            Note that this service does not compute the forecast. That must be done in a higher level service.",
        defaultEntityName = "SalesForecast",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "changeNote", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateSalesForecast {}

    /**
     * Creates a Sales Forecast Detail
     */
    @Service(
        name = "createSalesForecastDetail",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/opportunity/OpportunityServices.xml",
        invoke = "createSalesForecastDetail",
        description = "Creates a Sales Forecast Detail",
        defaultEntityName = "SalesForecastDetail",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "salesForecastDetailId", mode = "OUT")
        }
    )
    public interface CreateSalesForecastDetail {}

    /**
     * Updates a Sales Forecast Detail
     */
    @Service(
        name = "updateSalesForecastDetail",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/opportunity/OpportunityServices.xml",
        invoke = "updateSalesForecastDetail",
        description = "Updates a Sales Forecast Detail",
        defaultEntityName = "SalesForecastDetail",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateSalesForecastDetail {}

    /**
     * Delete a Sales Forecast Detail
     */
    @Service(
        name = "deleteSalesForecastDetail",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/opportunity/OpportunityServices.xml",
        invoke = "deleteSalesForecastDetail",
        description = "Delete a Sales Forecast Detail",
        defaultEntityName = "SalesForecastDetail",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSalesForecastDetail {}

    /**
     * Create an sales opportunity
     */
    @Service(
        name = "createSalesOpportunity",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/opportunity/OpportunityServices.xml",
        invoke = "createSalesOpportunity",
        description = "Create an sales opportunity",
        defaultEntityName = "SalesOpportunity",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"createdByUserLogin"})
        },
        attributes = {
            @Attribute(name = "accountPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "leadPartyId", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "opportunityName", allowHtml = "any"),
            @OverrideAttribute(name = "description", allowHtml = "any"),
            @OverrideAttribute(name = "nextStep", allowHtml = "any")
        }
    )
    public interface CreateSalesOpportunity {}

    /**
     * Update an sales opportunity
     */
    @Service(
        name = "updateSalesOpportunity",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/opportunity/OpportunityServices.xml",
        invoke = "updateSalesOpportunity",
        description = "Update an sales opportunity",
        defaultEntityName = "SalesOpportunity",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "accountPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "leadPartyId", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "opportunityName", allowHtml = "any"),
            @OverrideAttribute(name = "description", allowHtml = "any"),
            @OverrideAttribute(name = "nextStep", allowHtml = "any")
        }
    )
    public interface UpdateSalesOpportunity {}

    /**
     * Create sales opportunity role
     */
    @Service(
        name = "createSalesOpportunityRole",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/opportunity/OpportunityServices.xml",
        invoke = "createSalesOpportunityRole",
        description = "Create sales opportunity role",
        defaultEntityName = "SalesOpportunityRole",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateSalesOpportunityRole {}

    /**
     * Create sales opportunity account role
     */
    @Service(
        name = "createSalesOpportunityAccountRole",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/opportunity/OpportunityServices.xml",
        invoke = "createSalesOpportunityAccountRole",
        description = "Create sales opportunity account role",
        defaultEntityName = "SalesOpportunityRole",
        attributes = {
            @Attribute(name = "accountPartyId", type = "String", mode = "IN"),
            @Attribute(name = "salesOpportunityId", type = "String", mode = "IN")
        }
    )
    public interface CreateSalesOpportunityAccountRole {}

    /**
     * Create sales opportunity lead role
     */
    @Service(
        name = "createSalesOpportunityLeadRole",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/opportunity/OpportunityServices.xml",
        invoke = "createSalesOpportunityLeadRole",
        description = "Create sales opportunity lead role",
        defaultEntityName = "SalesOpportunityRole",
        attributes = {
            @Attribute(name = "leadPartyId", type = "String", mode = "IN"),
            @Attribute(name = "salesOpportunityId", type = "String", mode = "IN")
        }
    )
    public interface CreateSalesOpportunityLeadRole {}

    /**
     * find sales opportunity role party
     */
    @Service(
        name = "findPartyInSalesOpportunityRole",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/opportunity/OpportunityServices.xml",
        invoke = "findPartyInSalesOpportunityRole",
        description = "find sales opportunity role party",
        defaultEntityName = "SalesOpportunityRole",
        attributes = {
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "salesOpportunityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface FindPartyInSalesOpportunityRole {}

    /**
     * Create a SalesOpportunityCompetitor
     */
    @Service(
        name = "createSalesOpportunityCompetitor",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SalesOpportunityCompetitor",
        defaultEntityName = "SalesOpportunityCompetitor",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateSalesOpportunityCompetitor {}

    /**
     * Update a SalesOpportunityCompetitor
     */
    @Service(
        name = "updateSalesOpportunityCompetitor",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SalesOpportunityCompetitor",
        defaultEntityName = "SalesOpportunityCompetitor",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateSalesOpportunityCompetitor {}

    /**
     * Delete a SalesOpportunityCompetitor
     */
    @Service(
        name = "deleteSalesOpportunityCompetitor",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SalesOpportunityCompetitor",
        defaultEntityName = "SalesOpportunityCompetitor",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSalesOpportunityCompetitor {}

    /**
     * Delete a SalesOpportunityRole
     */
    @Service(
        name = "deleteSalesOpportunityRole",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SalesOpportunityRole",
        defaultEntityName = "SalesOpportunityRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSalesOpportunityRole {}

    /**
     * Create a SalesOpportunityStage
     */
    @Service(
        name = "createSalesOpportunityStage",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SalesOpportunityStage",
        defaultEntityName = "SalesOpportunityStage",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateSalesOpportunityStage {}

    /**
     * Update a SalesOpportunityStage
     */
    @Service(
        name = "updateSalesOpportunityStage",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SalesOpportunityStage",
        defaultEntityName = "SalesOpportunityStage",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateSalesOpportunityStage {}

    /**
     * Delete a SalesOpportunityStage
     */
    @Service(
        name = "deleteSalesOpportunityStage",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SalesOpportunityStage",
        defaultEntityName = "SalesOpportunityStage",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSalesOpportunityStage {}

    /**
     * Create a SalesOpportunityTrckCode
     */
    @Service(
        name = "createSalesOpportunityTrckCode",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SalesOpportunityTrckCode",
        defaultEntityName = "SalesOpportunityTrckCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateSalesOpportunityTrckCode {}

    /**
     * Update a SalesOpportunityTrckCode
     */
    @Service(
        name = "updateSalesOpportunityTrckCode",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SalesOpportunityTrckCode",
        defaultEntityName = "SalesOpportunityTrckCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateSalesOpportunityTrckCode {}

    /**
     * Delete a SalesOpportunityTrckCode
     */
    @Service(
        name = "deleteSalesOpportunityTrckCode",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SalesOpportunityTrckCode",
        defaultEntityName = "SalesOpportunityTrckCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSalesOpportunityTrckCode {}

    /**
     * Create a SalesOpportunityWorkEffort
     */
    @Service(
        name = "createSalesOpportunityWorkEffort",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SalesOpportunityWorkEffort",
        defaultEntityName = "SalesOpportunityWorkEffort",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateSalesOpportunityWorkEffort {}

    /**
     * Delete a SalesOpportunityWorkEffort
     */
    @Service(
        name = "deleteSalesOpportunityWorkEffort",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SalesOpportunityWorkEffort",
        defaultEntityName = "SalesOpportunityWorkEffort",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSalesOpportunityWorkEffort {}

    /**
     * Create a SalesOpportunityQuote
     */
    @Service(
        name = "createSalesOpportunityQuote",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SalesOpportunityQuote",
        defaultEntityName = "SalesOpportunityQuote",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateSalesOpportunityQuote {}

    /**
     * Delete a SalesOpportunityQuote
     */
    @Service(
        name = "deleteSalesOpportunityQuote",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SalesOpportunityQuote",
        defaultEntityName = "SalesOpportunityQuote",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSalesOpportunityQuote {}

}
