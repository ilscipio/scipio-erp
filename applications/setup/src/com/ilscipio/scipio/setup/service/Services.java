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
package com.ilscipio.scipio.setup.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Services {

    /**
     *              SCIPIO: Update accounting preferences for a party (organization). This is a Scipio custom service based on the accounting updatePartyAcctgPreference service              intended for the setup component. The default one doesn't let update almost any of the fields, which from a point of view of a setup application is quite weird.             In any case, We should determine with care, what can be updated and in which circumstances.          
     */
    @Service(
        name = "setupUpdatePartyAcctgPreference",
        engine = "java",
        location = "com.ilscipio.scipio.accounting.ledger.AcctgAdminServices",
        invoke = "updatePartyAcctgPreference",
        description = "\n            SCIPIO: Update accounting preferences for a party (organization). This is a Scipio custom service based on the accounting updatePartyAcctgPreference service \n            intended for the setup component. The default one doesn't let update almost any of the fields, which from a point of view of a setup application is quite weird.\n            In any case, We should determine with care, what can be updated and in which circumstances. \n        ",
        defaultEntityName = "PartyAcctgPreference",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgPrefPermissionCheck", mainAction = "UPDATE")
    )
    public interface SetupUpdatePartyAcctgPreference {}

    @Service(
        name = "createSetupTaxAuthority",
        engine = "group",
        defaultEntityName = "TaxAuthority",
        auth = "true",
        invokes = {@GroupInvoke(name = "createPartyGroup", resultToContext = "false"), @GroupInvoke(name = "createPartyRole", resultToContext = "false"), @GroupInvoke(name = "createTaxAuthority", resultToContext = "false")}
    )
    public interface CreateSetupTaxAuthority {}

    /**
     * SCIPIO: 4.0.0: Headless setup wizard step: create the organization (PartyGroup, roles, address,
     * telecom, email, accounting preference). Ported from commonext SetupEvents.xml#createOrganization.
     */
    @Service(
        name = "setupCreateOrganization",
        engine = "java",
        location = "com.ilscipio.scipio.setup.service.SetupServices",
        invoke = "setupCreateOrganization",
        description = "Create an organization (PartyGroup) with roles, address, telecom, email and accounting currency",
        auth = "true",
        attributes = {
            @Attribute(name = "groupName", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "countryGeoId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "address1", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "address2", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "city", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "postalCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "stateProvinceGeoId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "countryCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "emailAddress", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fiscalYearStartMonth", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fiscalYearStartDay", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "taxIdNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "OUT"),
            @Attribute(name = "contactMechIds", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface SetupCreateOrganization {}

    /**
     * SCIPIO: 4.0.0: Headless setup wizard step: create a facility with its shipping address.
     * Ported from commonext SetupEvents.xml#createFacilityAndContactMech.
     */
    @Service(
        name = "setupCreateFacility",
        engine = "java",
        location = "com.ilscipio.scipio.setup.service.SetupServices",
        invoke = "setupCreateFacility",
        description = "Create a facility with an optional shipping address and default location",
        auth = "true",
        attributes = {
            @Attribute(name = "facilityName", type = "String", mode = "IN"),
            @Attribute(name = "facilityTypeId", type = "String", mode = "IN", optional = "true", defaultValue = "WAREHOUSE"),
            @Attribute(name = "ownerPartyId", type = "String", mode = "IN"),
            @Attribute(name = "defaultInventoryItemTypeId", type = "String", mode = "IN", optional = "true", defaultValue = "NON_SERIAL_INV_ITEM"),
            @Attribute(name = "address1", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "address2", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "city", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "postalCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "stateProvinceGeoId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "countryGeoId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "facilityId", type = "String", mode = "OUT"),
            @Attribute(name = "locationSeqId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SetupCreateFacility {}

    /**
     * SCIPIO: 4.0.0: Headless setup wizard step: create a product store with an optional web site.
     * Ported from setup SetupEvents.xml#createProductStoreAndWebSite / commonext#createProductStoreWithDefaultSetting.
     */
    @Service(
        name = "setupCreateStore",
        engine = "java",
        location = "com.ilscipio.scipio.setup.service.SetupServices",
        invoke = "setupCreateStore",
        description = "Create a product store, optionally with a web site, and assign the owner role",
        auth = "true",
        attributes = {
            @Attribute(name = "storeName", type = "String", mode = "IN"),
            @Attribute(name = "ownerPartyId", type = "String", mode = "IN"),
            @Attribute(name = "inventoryFacilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "defaultCurrencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "defaultLocaleString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "siteName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "hostname", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "visualThemeSetId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "OUT"),
            @Attribute(name = "webSiteId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SetupCreateStore {}

    /**
     * SCIPIO: 4.0.0: Headless setup wizard step: create a catalog, attach it to a store and create a browse-root category.
     */
    @Service(
        name = "setupCreateCatalog",
        engine = "java",
        location = "com.ilscipio.scipio.setup.service.SetupServices",
        invoke = "setupCreateCatalog",
        description = "Create a catalog, attach it to a product store and optionally create a root category",
        auth = "true",
        attributes = {
            @Attribute(name = "catalogName", type = "String", mode = "IN"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "rootCategoryName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "prodCatalogId", type = "String", mode = "OUT"),
            @Attribute(name = "productCategoryId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SetupCreateCatalog {}

    /**
     * SCIPIO: 4.0.0: Headless setup wizard step: create the owner/admin user (Person + UserLogin).
     * Ported from setup SetupEvents.xml#createUser.
     */
    @Service(
        name = "setupCreateUser",
        engine = "java",
        location = "com.ilscipio.scipio.setup.service.SetupServices",
        invoke = "setupCreateUser",
        description = "Create an owner user (Person and UserLogin), with an optional link to an organization",
        auth = "true",
        attributes = {
            @Attribute(name = "userLoginId", type = "String", mode = "IN"),
            @Attribute(name = "password", type = "String", mode = "IN"),
            @Attribute(name = "firstName", type = "String", mode = "IN"),
            @Attribute(name = "lastName", type = "String", mode = "IN"),
            @Attribute(name = "emailAddress", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true", description = "Organization party to link the new user to"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN", optional = "true", defaultValue = "EMPLOYEE"),
            @Attribute(name = "partyRelationshipTypeId", type = "String", mode = "IN", optional = "true", defaultValue = "EMPLOYMENT"),
            @Attribute(name = "securityGroupId", type = "String", mode = "IN", optional = "true", defaultValue = "FULLADMIN"),
            @Attribute(name = "partyId", type = "String", mode = "OUT"),
            @Attribute(name = "userLoginId", type = "String", mode = "OUT")
        }
    )
    public interface SetupCreateUser {}

    /**
     * SCIPIO: 4.0.0: Headless setup wizard step: load the chart of accounts for an organization.
     * The setup component's own importDefaultGL/importGlAccounts (SetupEvents.xml) are unimplemented stubs;
     * this ports the actual working logic from commonext SetupEvents.xml#setupDefaultGeneralLedger.
     */
    @Service(
        name = "setupLoadAccounting",
        engine = "java",
        location = "com.ilscipio.scipio.setup.service.SetupServices",
        invoke = "setupLoadAccounting",
        description = "Load the general chart of accounts for an organization",
        auth = "true",
        attributes = {
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN"),
            @Attribute(name = "countryGeoId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "baseCurrencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fiscalYearStartMonth", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fiscalYearStartDay", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "glAccountCount", type = "Long", mode = "OUT"),
            @Attribute(name = "taxAuthorityCreated", type = "Boolean", mode = "OUT"),
            @Attribute(name = "note", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SetupLoadAccounting {}

    /**
     * SCIPIO: 4.0.0: Headless setup wizard step: thin wrapper of createSetupTaxAuthority that also
     * links the tax authority to the organization via createPartyTaxAuthInfo.
     * Ported from setup SetupEvents.xml#createTaxAuthority.
     */
    @Service(
        name = "setupCreateTaxAuthority",
        engine = "java",
        location = "com.ilscipio.scipio.setup.service.SetupServices",
        invoke = "setupCreateTaxAuthority",
        description = "Create a tax authority (PartyGroup, PartyRole, TaxAuthority) and link it to an organization",
        auth = "true",
        attributes = {
            @Attribute(name = "taxAuthGeoId", type = "String", mode = "IN"),
            @Attribute(name = "taxAuthPartyId", type = "String", mode = "IN"),
            @Attribute(name = "groupName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "requireTaxIdForExemption", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "includeTaxInPrice", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orgPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "taxIdNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "taxAuthGeoId", type = "String", mode = "OUT"),
            @Attribute(name = "taxAuthPartyId", type = "String", mode = "OUT")
        }
    )
    public interface SetupCreateTaxAuthority {}

}
