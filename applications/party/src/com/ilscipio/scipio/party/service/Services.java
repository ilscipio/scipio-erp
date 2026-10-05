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
package com.ilscipio.scipio.party.service;

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
     * Create an AddressMatchMap record
     */
    @Service(
        name = "createAddressMatchMap",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "createAddressMatchMap",
        description = "Create an AddressMatchMap record",
        defaultEntityName = "AddressMatchMap",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "CREATE")
    )
    public interface CreateAddressMatchMap {}

    /**
     * Import a CSV (name,value) of AddressMatchMap records
     */
    @Service(
        name = "importAddressMatchMapCsv",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "importAddressMatchMapCsv",
        description = "Import a CSV (name,value) of AddressMatchMap records",
        auth = "true",
        attributes = {
            @Attribute(name = "uploadedFile", type = "java.nio.ByteBuffer", mode = "IN"),
            @Attribute(name = "_uploadedFile_fileName", type = "String", mode = "IN"),
            @Attribute(name = "_uploadedFile_contentType", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "CREATE")
    )
    public interface ImportAddressMatchMapCsv {}

    /**
     * Delete an AddressMatchMap record
     */
    @Service(
        name = "removeAddressMatchMap",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "deleteAddressMatchMap",
        description = "Delete an AddressMatchMap record",
        defaultEntityName = "AddressMatchMap",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveAddressMatchMap {}

    /**
     * Delete an AddressMatchMap record
     */
    @Service(
        name = "clearAddressMatchMap",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "clearAddressMatchMap",
        description = "Delete an AddressMatchMap record",
        defaultEntityName = "AddressMatchMap",
        auth = "true",
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "DELETE")
    )
    public interface ClearAddressMatchMap {}

    /**
     * Delete a Party
     */
    @Service(
        name = "deleteParty",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "deleteParty",
        description = "Delete a Party",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface DeleteParty {}

    /**
     * Set the party status. Requires PARTYMGR_UPDATE or PARTYMGR_STS_UPDATE permission. The change to statusId must be defined in StatusValidChange, otherwise             this service will fail. The result is the original statusId, so that ECA conditions can check if a status has actually changed.
     */
    @Service(
        name = "setPartyStatus",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "setPartyStatus",
        description = "Set the party status. Requires PARTYMGR_UPDATE or PARTYMGR_STS_UPDATE permission. The change to statusId must be defined in StatusValidChange, otherwise\n            this service will fail. The result is the original statusId, so that ECA conditions can check if a status has actually changed.",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "statusDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "partyStatusPermissionCheck", mainAction = "UPDATE")
    )
    public interface SetPartyStatus {}

    /**
     * Create a Person
     */
    @Service(
        name = "createPerson",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "createPerson",
        description = "Create a Person",
        defaultEntityName = "Person",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "preferredCurrencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "externalId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreatePerson {}

    /**
     * Create a Person and UserLogin
     */
    @Service(
        name = "createPersonAndUserLogin",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml",
        invoke = "createPersonAndUserLogin",
        description = "Create a Person and UserLogin",
        requireNewTransaction = "true",
        implemented = {@Implements(service = "createUserLogin")},
        entityAttributes = {
            @EntityAttributes(entityName = "Person", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Party", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "INOUT", optional = "true", entityName = "Person"),
            @Attribute(name = "newUserLogin", type = "Map", mode = "OUT")
        }
    )
    public interface CreatePersonAndUserLogin {}

    /**
     * Update a Person
     */
    @Service(
        name = "updatePerson",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "updatePerson",
        description = "Update a Person",
        defaultEntityName = "Person",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "preferredCurrencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "externalId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "partyGroupPermissionCheck", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "firstName", optional = "false"),
            @OverrideAttribute(name = "lastName", optional = "false")
        }
    )
    public interface UpdatePerson {}

    /**
     * Create a PartyGroup
     */
    @Service(
        name = "createPartyGroup",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "createPartyGroup",
        description = "Create a PartyGroup",
        defaultEntityName = "PartyGroup",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "partyTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "preferredCurrencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "externalId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "groupName", optional = "false"),
            @OverrideAttribute(name = "comments", allowHtml = "any")
        }
    )
    public interface CreatePartyGroup {}

    /**
     * Update a PartyGroup
     */
    @Service(
        name = "updatePartyGroup",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "updatePartyGroup",
        description = "Update a PartyGroup",
        defaultEntityName = "PartyGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "preferredCurrencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "externalId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "partyGroupPermissionCheck", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "comments", allowHtml = "any")
        }
    )
    public interface UpdatePartyGroup {}

    /**
     * Save Party Name Change
     */
    @Service(
        name = "savePartyNameChange",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "savePartyNameChange",
        description = "Save Party Name Change",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "groupName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "firstName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "middleName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "lastName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "personalTitle", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "suffix", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SavePartyNameChange {}

    /**
     * Get Party Name For Date
     */
    @Service(
        name = "getPartyNameForDate",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "getPartyNameForDate",
        description = "Get Party Name For Date",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "compareDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "lastNameFirst", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "groupName", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "firstName", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "middleName", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "lastName", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "personalTitle", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "suffix", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "fullName", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "gender", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface GetPartyNameForDate {}

    /**
     * Create an Affiliate
     */
    @Service(
        name = "createAffiliate",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "createAffiliate",
        description = "Create an Affiliate",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "INOUT"),
            @Attribute(name = "affiliateName", type = "String", mode = "IN"),
            @Attribute(name = "affiliateDescription", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "yearEstablished", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "siteType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sitePageViews", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "siteVisitors", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateAffiliate {}

    /**
     * Update an Affiliate
     */
    @Service(
        name = "updateAffiliate",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "updateAffiliate",
        description = "Update an Affiliate",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "affiliateName", type = "String", mode = "IN"),
            @Attribute(name = "affiliateDescription", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "yearEstablished", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "siteType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sitePageViews", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "siteVisitors", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateAffiliate {}

    /**
     * Create a note item and associate with a party. If a noteId is passed, creates an assoication to that note instead.
     */
    @Service(
        name = "createPartyNote",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "createPartyNote",
        description = "Create a note item and associate with a party. If a noteId is passed, creates an assoication to that note instead.",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "noteName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "noteId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "note", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreatePartyNote {}

    /**
     * Create a new role type
     */
    @Service(
        name = "createRoleType",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "createRoleType",
        description = "Create a new role type",
        auth = "true",
        attributes = {
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "parentTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN"),
            @Attribute(name = "roleType", type = "org.ofbiz.entity.GenericValue", mode = "OUT")
        }
    )
    public interface CreateRoleType {}

    /**
     * Sets the party (customer) profile defaults
     */
    @Service(
        name = "setPartyProfileDefaults",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "setPartyProfileDefaults",
        description = "Sets the party (customer) profile defaults",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyProfileDefault", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "partyIdPermissionCheck", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "productStoreId", optional = "false")
        }
    )
    public interface SetPartyProfileDefaults {}

    /**
     * Updates PartyProfileDefault defaultBillAddr and defaultShipAddr if ID changed (no perm check)
     */
    @Service(
        name = "updatePartyProfileDefaultPostalAddressIds",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "updatePartyProfileDefaultPostalAddressIds",
        description = "Updates PartyProfileDefault defaultBillAddr and defaultShipAddr if ID changed (no perm check)",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true", description = "Note: If empty, uses current userLogin partyId"),
            @Attribute(name = "oldContactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdatePartyProfileDefaultPostalAddressIds {}

    /**
     * Updates party postal address and then updates PartyProfileDefault defaultBillAddr and defaultShipAddr if ID changed (no perm check)
     */
    @Service(
        name = "updatePartyPostalAddressAndProfileIds",
        engine = "group",
        description = "Updates party postal address and then updates PartyProfileDefault defaultBillAddr and defaultShipAddr if ID changed (no perm check)",
        auth = "true",
        invokes = {@GroupInvoke(name = "updatePartyPostalAddress", resultToContext = "false")}
    )
    public interface UpdatePartyPostalAddressAndProfileIds {}

    /**
     * create a party attribute record
     */
    @Service(
        name = "createPartyAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "create a party attribute record",
        defaultEntityName = "PartyAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "CREATE")
    )
    public interface CreatePartyAttribute {}

    /**
     * updates a party attribute record
     */
    @Service(
        name = "updatePartyAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "updates a party attribute record",
        defaultEntityName = "PartyAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdatePartyAttribute {}

    /**
     * removes a party attribute record
     */
    @Service(
        name = "removePartyAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "removes a party attribute record",
        defaultEntityName = "PartyAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "DELETE")
    )
    public interface RemovePartyAttribute {}

    /**
     * Merges customer accounts and disabled the duplicate
     */
    @Service(
        name = "linkPartyRecord",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "linkParty",
        description = "Merges customer accounts and disabled the duplicate",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "INOUT"),
            @Attribute(name = "partyIdTo", type = "String", mode = "IN")
        }
    )
    public interface LinkPartyRecord {}

    /**
     * Performs a lookup for parties
     */
    @Service(
        name = "lookupParty",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/LookupServices.xml",
        invoke = "lookupParty",
        description = "Performs a lookup for parties",
        auth = "true",
        attributes = {
            @Attribute(name = "firstName", type = "String", mode = "IN", optional = "true", formLabel = "First name"),
            @Attribute(name = "lastName", type = "String", mode = "IN", optional = "true", formLabel = "Last name"),
            @Attribute(name = "lookupResult", type = "List", mode = "OUT")
        }
    )
    public interface LookupParty {}

    /**
     * Find the partyId corresponding to a reference and a reference type
     */
    @Service(
        name = "findPartiesById",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "findPartyById",
        description = "Find the partyId corresponding to a reference and a reference type",
        auth = "true",
        attributes = {
            @Attribute(name = "idToFind", type = "String", mode = "IN"),
            @Attribute(name = "partyIdentificationTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "searchPartyFirst", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "searchAllId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "party", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "partiesFound", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface FindPartiesById {}

    /**
     * Create a Party Role (add a Role to a Party). The logged in user must have PARTYMGR_CREATE or have             permission to change the role of this partyId
     */
    @Service(
        name = "createPartyRole",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Party Role (add a Role to a Party). The logged in user must have PARTYMGR_CREATE or have\n            permission to change the role of this partyId",
        defaultEntityName = "PartyRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "partyRolePermissionCheck", mainAction = "CREATE")
    )
    public interface CreatePartyRole {}

    /**
     * Delete a Party Role (remove a Role from a Party). The logged in user must have PARTYMGR_DELETE or have             permission to change the role of this partyId
     */
    @Service(
        name = "deletePartyRole",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Party Role (remove a Role from a Party). The logged in user must have PARTYMGR_DELETE or have\n            permission to change the role of this partyId",
        defaultEntityName = "PartyRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "partyRolePermissionCheck", mainAction = "DELETE")
    )
    public interface DeletePartyRole {}

    /**
     * Ensure that the party is in the specified role.
     */
    @Service(
        name = "ensurePartyRole",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml",
        invoke = "ensureNaPartyRole",
        description = "Ensure that the party is in the specified role.",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN")
        }
    )
    public interface EnsurePartyRole {}

    /**
     * Ensure that the party is in the _NA_ role.
     */
    @Service(
        name = "ensureNaPartyRole",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml",
        invoke = "ensureNaPartyRole",
        description = "Ensure that the party is in the _NA_ role.",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN")
        }
    )
    public interface EnsureNaPartyRole {}

    /**
     * Ensure that the party indicate by partyIdFrom is in the roleTypeIdFrom specifc role. If roleTypeIdFrom isn't present use _NA_
     */
    @Service(
        name = "ensurePartyRoleFrom",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml",
        invoke = "ensureNaPartyRole",
        description = "Ensure that the party indicate by partyIdFrom is in the roleTypeIdFrom specifc role. If roleTypeIdFrom isn't present use _NA_",
        auth = "true",
        attributes = {
            @Attribute(name = "partyIdFrom", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeIdFrom", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface EnsurePartyRoleFrom {}

    /**
     * Ensure that the party indicate by partyIdTo is in the roleTypeIdTo specific role. If roleTypeIdTo isn't present use _NA_
     */
    @Service(
        name = "ensurePartyRoleTo",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml",
        invoke = "ensureNaPartyRole",
        description = "Ensure that the party indicate by partyIdTo is in the roleTypeIdTo specific role. If roleTypeIdTo isn't present use _NA_",
        auth = "true",
        attributes = {
            @Attribute(name = "partyIdTo", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeIdTo", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface EnsurePartyRoleTo {}

    /**
     *              Create a Relationship between two Parties;             if partyIdFrom is not specified the partyId of the current userLogin will be used;             if roleTypeIds are not specified they will default to "_NA_".             If a partyIdFrom is passed in, it will be used if the userLogin has PARTYMGR_REL_CREATE permission.         
     */
    @Service(
        name = "createPartyRelationship",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "createPartyRelationship",
        description = "\n            Create a Relationship between two Parties;\n            if partyIdFrom is not specified the partyId of the current userLogin will be used;\n            if roleTypeIds are not specified they will default to \"_NA_\".\n            If a partyIdFrom is passed in, it will be used if the userLogin has PARTYMGR_REL_CREATE permission.\n        ",
        defaultEntityName = "PartyRelationship",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyRelationshipPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "partyIdTo", optional = "false")
        }
    )
    public interface CreatePartyRelationship {}

    /**
     *              Update a Relationship between two Parties;             if partyIdFrom is not specified the partyId of the current userLogin will be used;             if roleTypeIds are not specified they will default to "_NA_".             If a partyIdFrom is passed in, it will be used if the userLogin has PARTYMGR_REL_UPDATE permission.         
     */
    @Service(
        name = "updatePartyRelationship",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "updatePartyRelationship",
        description = "\n            Update a Relationship between two Parties;\n            if partyIdFrom is not specified the partyId of the current userLogin will be used;\n            if roleTypeIds are not specified they will default to \"_NA_\".\n            If a partyIdFrom is passed in, it will be used if the userLogin has PARTYMGR_REL_UPDATE permission.\n        ",
        defaultEntityName = "PartyRelationship",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyRelationshipPermissionCheck", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "partyIdFrom", optional = "true"),
            @OverrideAttribute(name = "roleTypeIdFrom", optional = "true"),
            @OverrideAttribute(name = "roleTypeIdTo", optional = "true")
        }
    )
    public interface UpdatePartyRelationship {}

    /**
     *              Delete a Relationship between two Parties;             if partyIdFrom is not specified the partyId of the current userLogin will be used;             if roleTypeIds are not specified they will default to "_NA_".         
     */
    @Service(
        name = "deletePartyRelationship",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "deletePartyRelationship",
        description = "\n            Delete a Relationship between two Parties;\n            if partyIdFrom is not specified the partyId of the current userLogin will be used;\n            if roleTypeIds are not specified they will default to \"_NA_\".\n        ",
        defaultEntityName = "PartyRelationship",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "partyRelationshipPermissionCheck", mainAction = "DELETE"),
        overrideAttributes = {
            @OverrideAttribute(name = "partyIdFrom", optional = "true"),
            @OverrideAttribute(name = "roleTypeIdFrom", optional = "true"),
            @OverrideAttribute(name = "roleTypeIdTo", optional = "true")
        }
    )
    public interface DeletePartyRelationship {}

    /**
     * Create party's roles and party's relationship
     */
    @Service(
        name = "createPartyRelationshipAndRole",
        engine = "group",
        description = "Create party's roles and party's relationship",
        auth = "true",
        invokes = {@GroupInvoke(name = "ensurePartyRoleFrom", resultToContext = "false"), @GroupInvoke(name = "ensurePartyRoleTo", resultToContext = "false"), @GroupInvoke(name = "createPartyRelationship", resultToContext = "false")}
    )
    public interface CreatePartyRelationshipAndRole {}

    /**
     * Create a new Party Relationship type
     */
    @Service(
        name = "createPartyRelationshipType",
        engine = "java",
        location = "org.ofbiz.party.party.PartyRelationshipServices",
        invoke = "createPartyRelationshipType",
        description = "Create a new Party Relationship type",
        defaultEntityName = "PartyRelationshipType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "partyRelationshipName", optional = "false")
        }
    )
    public interface CreatePartyRelationshipType {}

    /**
     *              Create or update both parties roles and parties relationship, partyRelationshipTypeId being mandatory.             The relationship is considered from one side or another (partyId is checked internally against partyIdFrom)             If a type of parties relationship exists PartyIdTo or PartyIdFrom are updated.             The history is maintained, allowing to track changes.         
     */
    @Service(
        name = "createUpdatePartyRelationshipAndRoles",
        engine = "java",
        location = "org.ofbiz.party.party.PartyRelationshipServices",
        invoke = "createUpdatePartyRelationshipAndRoles",
        description = "\n            Create or update both parties roles and parties relationship, partyRelationshipTypeId being mandatory.\n            The relationship is considered from one side or another (partyId is checked internally against partyIdFrom)\n            If a type of parties relationship exists PartyIdTo or PartyIdFrom are updated.\n            The history is maintained, allowing to track changes.\n        ",
        defaultEntityName = "PartyRelationship",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateUpdatePartyRelationshipAndRoles {}

    /**
     * create a company/contact relationship and add the related roles
     */
    @Service(
        name = "createPartyRelationshipContactAccount",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "createPartyRelationshipContactAccount",
        description = "create a company/contact relationship and add the related roles",
        auth = "true",
        attributes = {
            @Attribute(name = "accountPartyId", type = "String", mode = "IN"),
            @Attribute(name = "contactPartyId", type = "String", mode = "IN"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true", allowHtml = "any")
        }
    )
    public interface CreatePartyRelationshipContactAccount {}

    /**
     * Create a ContactMech
     */
    @Service(
        name = "createContactMech",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.xml",
        invoke = "createContactMech",
        description = "Create a ContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ContactMech", mode = "IN", include = "nonpk"),
            @EntityAttributes(entityName = "ContactMech", mode = "INOUT", include = "pk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "infoString", optional = "true")
        }
    )
    public interface CreateContactMech {}

    /**
     * Create a PartyContactMech
     */
    @Service(
        name = "createPartyContactMech",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "createPartyContactMech",
        description = "Create a PartyContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ContactMech", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "PartyContactMech", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "contactMechTypeId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "partyContactMechPermissionCheck", mainAction = "CREATE")
    )
    public interface CreatePartyContactMech {}

    /**
     * Update a ContactMech
     */
    @Service(
        name = "updateContactMech",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.xml",
        invoke = "updateContactMech",
        description = "Update a ContactMech",
        auth = "true",
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT"),
            @Attribute(name = "contactMechTypeId", type = "String", mode = "IN"),
            @Attribute(name = "infoString", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateContactMech {}

    /**
     * Update a PartyContactMech
     */
    @Service(
        name = "updatePartyContactMech",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "updatePartyContactMech",
        description = "Update a PartyContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyContactMech", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT"),
            @Attribute(name = "contactMechTypeId", type = "String", mode = "IN"),
            @Attribute(name = "infoString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "newContactMechId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "partyContactMechPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdatePartyContactMech {}

    /**
     * Update a PartyContactMech, where the record has already been looked up
     */
    @Service(
        name = "updatePartyContactMechGiven",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "updatePartyContactMech",
        description = "Update a PartyContactMech, where the record has already been looked up",
        auth = "true",
        implemented = {@Implements(service = "updatePartyContactMech")},
        attributes = {
            @Attribute(name = "partyContactMech", type = "GenericValue", mode = "IN")
        },
        permissionService = @PermissionService(service = "partyContactMechPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdatePartyContactMechGiven {}

    /**
     * Delete (expire) a PartyContactMech (SCIPIO: 2018-10-30: Now also expires PartyContactMechPurpose records)
     */
    @Service(
        name = "deletePartyContactMech",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "deletePartyContactMechAndPurposes",
        description = "Delete (expire) a PartyContactMech (SCIPIO: 2018-10-30: Now also expires PartyContactMechPurpose records)",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "partyContactMechPermissionCheck", mainAction = "DELETE")
    )
    public interface DeletePartyContactMech {}

    /**
     * Delete (expire) a PartyContactMech only (SCIPIO: Not its purposes)
     */
    @Service(
        name = "deletePartyContactMechOnly",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "deletePartyContactMechOnly",
        description = "Delete (expire) a PartyContactMech only (SCIPIO: Not its purposes)",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "partyContactMechPermissionCheck", mainAction = "DELETE")
    )
    public interface DeletePartyContactMechOnly {}

    /**
     * Create a Postal Address
     */
    @Service(
        name = "createPostalAddress",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.xml",
        invoke = "createPostalAddress",
        description = "Create a Postal Address",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "PostalAddress", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "OUT")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "address1", optional = "false"),
            @OverrideAttribute(name = "city", optional = "false"),
            @OverrideAttribute(name = "postalCode", optional = "false")
        }
    )
    public interface CreatePostalAddress {}

    /**
     * Find the partyId/contactMechId for a specific email address, if not found do not return a value
     */
    @Service(
        name = "findPartyFromEmailAddress",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "findPartyFromEmailAddress",
        description = "Find the partyId/contactMechId for a specific email address, if not found do not return a value",
        auth = "true",
        attributes = {
            @Attribute(name = "address", type = "String", mode = "IN"),
            @Attribute(name = "caseInsensitive", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "personal", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface FindPartyFromEmailAddress {}

    /**
     * Find the partyId/contactMechId for a specific telephone number, if not found do not return a value
     */
    @Service(
        name = "findPartyFromTelephone",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "findPartyFromTelephone",
        description = "Find the partyId/contactMechId for a specific telephone number, if not found do not return a value",
        auth = "true",
        attributes = {
            @Attribute(name = "telno", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface FindPartyFromTelephone {}

    /**
     *              Find the partyId/contactMechId for a specific telephone number, if not found do not return a value.             Same than above but keep the number complete internally.         
     */
    @Service(
        name = "findPartyFromTelephoneComplete",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "findPartyFromTelephoneComplete",
        description = "\n            Find the partyId/contactMechId for a specific telephone number, if not found do not return a value.\n            Same than above but keep the number complete internally.\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "telno", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface FindPartyFromTelephoneComplete {}

    /**
     * Create a Postal Address
     */
    @Service(
        name = "createPartyPostalAddress",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "createPartyPostalAddress",
        description = "Create a Postal Address",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyContactMech", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "PostalAddress", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "partyContactMechPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "address1", optional = "false"),
            @OverrideAttribute(name = "city", optional = "false"),
            @OverrideAttribute(name = "postalCode", optional = "false")
        }
    )
    public interface CreatePartyPostalAddress {}

    /**
     * Update a Postal Address
     */
    @Service(
        name = "updatePostalAddress",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.xml",
        invoke = "updatePostalAddress",
        description = "Update a Postal Address",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "PostalAddress", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT"),
            @Attribute(name = "directions", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "oldContactMechId", type = "String", mode = "OUT"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "address1", optional = "false"),
            @OverrideAttribute(name = "city", optional = "false"),
            @OverrideAttribute(name = "postalCode", optional = "false")
        }
    )
    public interface UpdatePostalAddress {}

    /**
     * Update a Postal Address
     */
    @Service(
        name = "updatePartyPostalAddress",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "updatePartyPostalAddress",
        description = "Update a Postal Address",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyContactMech", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "PostalAddress", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT"),
            @Attribute(name = "oldContactMechId", type = "String", mode = "OUT"),
            @Attribute(name = "directions", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "updatePartyProfileIds", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "SCIPIO: If set to false, will prevent trying to update PartyProfileDefault.\n                Currently (2018-11-01), this is true by default")
        },
        permissionService = @PermissionService(service = "partyContactMechPermissionCheck", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "address1", optional = "false"),
            @OverrideAttribute(name = "city", optional = "false"),
            @OverrideAttribute(name = "postalCode", optional = "false")
        }
    )
    public interface UpdatePartyPostalAddress {}

    /**
     * Create a Telecommunications Number
     */
    @Service(
        name = "createTelecomNumber",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.xml",
        invoke = "createTelecomNumber",
        description = "Create a Telecommunications Number",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "TelecomNumber", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "OUT")
        }
    )
    public interface CreateTelecomNumber {}

    /**
     * Create a Telecommunications Number
     */
    @Service(
        name = "createPartyTelecomNumber",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "createPartyTelecomNumber",
        description = "Create a Telecommunications Number",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyContactMech", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "TelecomNumber", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "partyContactMechPermissionCheck", mainAction = "CREATE")
    )
    public interface CreatePartyTelecomNumber {}

    /**
     * Update a Telecommunications Number
     */
    @Service(
        name = "updateTelecomNumber",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.xml",
        invoke = "updateTelecomNumber",
        description = "Update a Telecommunications Number",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "TelecomNumber", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT"),
            @Attribute(name = "forceNewRecord", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "SCIPIO: If true, bypasses change detection and creates a new record even if no changes (added 2018-11-02)"),
            @Attribute(name = "oldContactMechId", type = "String", mode = "OUT")
        }
    )
    public interface UpdateTelecomNumber {}

    /**
     * Update a Telecommunications Number
     */
    @Service(
        name = "updatePartyTelecomNumber",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "updatePartyTelecomNumber",
        description = "Update a Telecommunications Number",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyContactMech", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "TelecomNumber", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT")
        },
        permissionService = @PermissionService(service = "partyContactMechPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdatePartyTelecomNumber {}

    /**
     * Create an Email Address
     */
    @Service(
        name = "createEmailAddress",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.xml",
        invoke = "createEmailAddress",
        description = "Create an Email Address",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ContactMech", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "ContactMech", mode = "OUT", include = "pk")
        },
        attributes = {
            @Attribute(name = "emailAddress", type = "String", mode = "IN")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "infoString", optional = "true")
        }
    )
    public interface CreateEmailAddress {}

    /**
     * Create an Email Address
     */
    @Service(
        name = "createPartyEmailAddress",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "createPartyEmailAddress",
        description = "Create an Email Address",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ContactMech", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "PartyContactMech", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "emailAddress", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "partyContactMechPermissionCheck", mainAction = "CREATE")
    )
    public interface CreatePartyEmailAddress {}

    /**
     * Update an Email Address
     */
    @Service(
        name = "updateEmailAddress",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.xml",
        invoke = "updateEmailAddress",
        description = "Update an Email Address",
        auth = "true",
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT"),
            @Attribute(name = "emailAddress", type = "String", mode = "IN")
        }
    )
    public interface UpdateEmailAddress {}

    /**
     * Update an Email Address
     */
    @Service(
        name = "updatePartyEmailAddress",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "updatePartyEmailAddress",
        description = "Update an Email Address",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyContactMech", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT"),
            @Attribute(name = "emailAddress", type = "String", mode = "IN"),
            @Attribute(name = "oldContactMechId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "partyContactMechPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdatePartyEmailAddress {}

    /**
     * Copies all contact mechs from the partyIdFrom to the partyIdTo. Does not delete or overwrite any contact mechs.
     */
    @Service(
        name = "copyPartyContactMechs",
        engine = "java",
        location = "org.ofbiz.party.contact.ContactMechServices",
        invoke = "copyPartyContactMechs",
        description = "Copies all contact mechs from the partyIdFrom to the partyIdTo. Does not delete or overwrite any contact mechs.",
        auth = "true",
        attributes = {
            @Attribute(name = "partyIdFrom", type = "String", mode = "IN"),
            @Attribute(name = "partyIdTo", type = "String", mode = "IN")
        }
    )
    public interface CopyPartyContactMechs {}

    /**
     * Create an Ftp Address associated to a party
     */
    @Service(
        name = "createPartyFtpAddress",
        engine = "groovy",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.groovy",
        invoke = "createPartyFtpAddress",
        description = "Create an Ftp Address associated to a party",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ContactMech", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "PartyContactMech", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "FtpAddress", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT", optional = "true")
        },
        permissionService = @PermissionService(service = "partyContactMechPermissionCheck", mainAction = "CREATE")
    )
    public interface CreatePartyFtpAddress {}

    /**
     * Update an Ftp Address associated to a party
     */
    @Service(
        name = "updatePartyFtpAddress",
        engine = "groovy",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.groovy",
        invoke = "updatePartyFtpAddress",
        description = "Update an Ftp Address associated to a party",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyContactMech", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "FtpAddress", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT")
        },
        permissionService = @PermissionService(service = "partyContactMechPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdatePartyFtpAddress {}

    /**
     * create FtpAddress
     */
    @Service(
        name = "createFtpAddress",
        engine = "groovy",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.groovy",
        invoke = "createFtpAddress",
        description = "create FtpAddress",
        defaultEntityName = "FtpAddress",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "CREATE")
    )
    public interface CreateFtpAddress {}

    /**
     * update FtpAddress
     */
    @Service(
        name = "updateFtpAddressWithHistory",
        engine = "groovy",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.groovy",
        invoke = "updateFtpAddressWithHistory",
        description = "update FtpAddress",
        defaultEntityName = "FtpAddress",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT"),
            @Attribute(name = "oldContactMechId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateFtpAddressWithHistory {}

    /**
     * create a contact mech attribute record
     */
    @Service(
        name = "createContactMechAttribute",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.xml",
        invoke = "createContactMechAttribute",
        description = "create a contact mech attribute record",
        defaultEntityName = "ContactMechAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "CREATE")
    )
    public interface CreateContactMechAttribute {}

    /**
     * updates a contact mech attribute record
     */
    @Service(
        name = "updateContactMechAttribute",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.xml",
        invoke = "updateContactMechAttribute",
        description = "updates a contact mech attribute record",
        defaultEntityName = "ContactMechAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateContactMechAttribute {}

    /**
     * removes a contact mech attribute record
     */
    @Service(
        name = "removeContactMechAttribute",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.xml",
        invoke = "removeContactMechAttribute",
        description = "removes a contact mech attribute record",
        defaultEntityName = "ContactMechAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveContactMechAttribute {}

    /**
     * Create a Party ContactMech Purpose
     */
    @Service(
        name = "createPartyContactMechPurpose",
        engine = "java",
        location = "org.ofbiz.party.contact.ContactMechServices",
        invoke = "createPartyContactMechPurpose",
        description = "Create a Party ContactMech Purpose",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "IN"),
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "INOUT", optional = "true")
        }
    )
    public interface CreatePartyContactMechPurpose {}

    /**
     * Delete a Party ContactMech Purpose (Note: actually expires the purpose using thruDate)
     */
    @Service(
        name = "deletePartyContactMechPurpose",
        engine = "java",
        location = "org.ofbiz.party.contact.ContactMechServices",
        invoke = "deletePartyContactMechPurpose",
        description = "Delete a Party ContactMech Purpose (Note: actually expires the purpose using thruDate)",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "IN"),
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN")
        }
    )
    public interface DeletePartyContactMechPurpose {}

    /**
     * Delete a Party ContactMech Purpose (Note: actually expires the purpose using thruDate)
     */
    @Service(
        name = "deletePartyContactMechPurposeIfExists",
        engine = "java",
        location = "org.ofbiz.party.contact.ContactMechServices",
        invoke = "deletePartyContactMechPurposeIfExists",
        description = "Delete a Party ContactMech Purpose (Note: actually expires the purpose using thruDate)",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "IN"),
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN")
        }
    )
    public interface DeletePartyContactMechPurposeIfExists {}

    /**
     * Expires a Party Contact Mech Purpose (Note: same as deletePartyContactMechPurpose except requires partyId)
     */
    @Service(
        name = "expirePartyContactMechPurpose",
        engine = "entity-auto",
        invoke = "expire",
        description = "Expires a Party Contact Mech Purpose (Note: same as deletePartyContactMechPurpose except requires partyId)",
        defaultEntityName = "PartyContactMechPurpose",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "partyContactMechPermissionCheck", mainAction = "DELETE")
    )
    public interface ExpirePartyContactMechPurpose {}

    /**
     * Interface for services similar to ensurePartyContactMechPurposes (SCIPIO)
     */
    @Service(
        name = "ensureContactMechPurposesInterface",
        engine = "interface",
        description = "Interface for services similar to ensurePartyContactMechPurposes (SCIPIO)",
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "IN"),
            @Attribute(name = "contactMechPurposeTypeIds", type = "List", mode = "IN"),
            @Attribute(name = "exact", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, other purposes not in the list are removed."),
            @Attribute(name = "conflictMode", type = "String", mode = "IN", optional = "true", defaultValue = "error", description = "What to do when a role is already in use by another active contactMechId:\n                * error: return an error (safe mode)\n                * skip: leave old one, don't create new one\n                * steal: steal the purpose from the other contact mech (add to ours, remove from the old)\n                * dup: create a duplicate purpose for the new one, leave old intact (NOT RECOMMENDED)\n                * dup-warn: same as duplicate but also leaves a warning in the log (NOT RECOMMENDED)")
        }
    )
    public interface EnsureContactMechPurposesInterface {}

    /**
     * Ensures Party ContactMech has the requested purposes (SCIPIO)
     */
    @Service(
        name = "ensurePartyContactMechPurposes",
        engine = "java",
        location = "org.ofbiz.party.contact.ContactMechServices",
        invoke = "ensurePartyContactMechPurposes",
        description = "Ensures Party ContactMech has the requested purposes (SCIPIO)",
        auth = "true",
        implemented = {@Implements(service = "ensureContactMechPurposesInterface")},
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "partyContactMechPermissionCheck", mainAction = "UPDATE")
    )
    public interface EnsurePartyContactMechPurposes {}

    @Service(
        name = "createContactMechLink",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.xml",
        invoke = "createContactMechLink",
        defaultEntityName = "ContactMechLink",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface CreateContactMechLink {}

    @Service(
        name = "deleteContactMechLink",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.xml",
        invoke = "deleteContactMechLink",
        defaultEntityName = "ContactMechLink",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteContactMechLink {}

    /**
     * Create a Postal Address Boundary
     */
    @Service(
        name = "createPostalAddressBoundary",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Postal Address Boundary",
        defaultEntityName = "PostalAddressBoundary",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "CREATE")
    )
    public interface CreatePostalAddressBoundary {}

    /**
     * Delete a Postal Address Boundary
     */
    @Service(
        name = "deletePostalAddressBoundary",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Postal Address Boundary",
        defaultEntityName = "PostalAddressBoundary",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "DELETE")
    )
    public interface DeletePostalAddressBoundary {}

    /**
     * create PartyClassification
     */
    @Service(
        name = "createPartyClassification",
        engine = "entity-auto",
        invoke = "create",
        description = "create PartyClassification",
        defaultEntityName = "PartyClassification",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreatePartyClassification {}

    /**
     * update PartyClassification
     */
    @Service(
        name = "updatePartyClassification",
        engine = "entity-auto",
        invoke = "update",
        description = "update PartyClassification",
        defaultEntityName = "PartyClassification",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdatePartyClassification {}

    /**
     * delete PartyClassification
     */
    @Service(
        name = "deletePartyClassification",
        engine = "entity-auto",
        invoke = "delete",
        description = "delete PartyClassification",
        defaultEntityName = "PartyClassification",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "DELETE")
    )
    public interface DeletePartyClassification {}

    /**
     * create PartyClassificationGroup
     */
    @Service(
        name = "createPartyClassificationGroup",
        engine = "entity-auto",
        invoke = "create",
        description = "create PartyClassificationGroup",
        defaultEntityName = "PartyClassificationGroup",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "CREATE")
    )
    public interface CreatePartyClassificationGroup {}

    /**
     * update PartyClassificationGroup
     */
    @Service(
        name = "updatePartyClassificationGroup",
        engine = "entity-auto",
        invoke = "update",
        description = "update PartyClassificationGroup",
        defaultEntityName = "PartyClassificationGroup",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdatePartyClassificationGroup {}

    /**
     * delete PartyClassificationGroup
     */
    @Service(
        name = "deletePartyClassificationGroup",
        engine = "entity-auto",
        invoke = "delete",
        description = "delete PartyClassificationGroup",
        defaultEntityName = "PartyClassificationGroup",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "DELETE")
    )
    public interface DeletePartyClassificationGroup {}

    /**
     * create PartyIdentification entity
     */
    @Service(
        name = "createPartyIdentification",
        engine = "entity-auto",
        invoke = "create",
        description = "create PartyIdentification entity",
        defaultEntityName = "PartyIdentification",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "CREATE")
    )
    public interface CreatePartyIdentification {}

    /**
     * update PartyIdentification entity
     */
    @Service(
        name = "updatePartyIdentification",
        engine = "entity-auto",
        invoke = "update",
        description = "update PartyIdentification entity",
        defaultEntityName = "PartyIdentification",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdatePartyIdentification {}

    /**
     * delete PartyClassificationGroup
     */
    @Service(
        name = "deletePartyIdentification",
        engine = "entity-auto",
        invoke = "delete",
        description = "delete PartyClassificationGroup",
        defaultEntityName = "PartyIdentification",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "DELETE")
    )
    public interface DeletePartyIdentification {}

    /**
     * create many identifications with format in map identifications : [partyType : TYPE, TYPE : value]
     */
    @Service(
        name = "createPartyIdentifications",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "createPartyIdentifications",
        description = "create many identifications with format in map identifications : [partyType : TYPE, TYPE : value]",
        defaultEntityName = "PartyIdentification",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "identifications", type = "Map", mode = "IN")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "CREATE")
    )
    public interface CreatePartyIdentifications {}

    /**
     * Create Vendor Information
     */
    @Service(
        name = "createVendor",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Vendor Information",
        defaultEntityName = "Vendor",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "Vendor", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "Vendor", mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "CREATE")
    )
    public interface CreateVendor {}

    /**
     * Update Vendor Information
     */
    @Service(
        name = "updateVendor",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Vendor Information",
        defaultEntityName = "Vendor",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "Vendor", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "Vendor", mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateVendor {}

    /**
     * Remove Vendor Information
     */
    @Service(
        name = "deleteVendor",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove Vendor Information",
        defaultEntityName = "Vendor",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "Vendor", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteVendor {}

    /**
     * Creates a relation between a Party and a DataSource using PartyDataSource. The userLogin must have PARTYMGR_SRC_CREATE permission.
     */
    @Service(
        name = "createPartyDataSource",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "createPartyDataSource",
        description = "Creates a relation between a Party and a DataSource using PartyDataSource. The userLogin must have PARTYMGR_SRC_CREATE permission.",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "dataSourceId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "partyDatasourcePermissionCheck", mainAction = "CREATE")
    )
    public interface CreatePartyDataSource {}

    /**
     * Set the Communication event Status
     */
    @Service(
        name = "setCommunicationEventStatus",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "setCommunicationEventStatus",
        description = "Set the Communication event Status",
        defaultEntityName = "CommunicationEvent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "setRoleStatusToComplete", type = "String", mode = "IN", defaultValue = "N"),
            @Attribute(name = "partyIdFrom", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "partyCommunicationEventPermissionCheck", mainAction = "UPDATE")
    )
    public interface SetCommunicationEventStatus {}

    /**
     * Set the Communication event  Status for a specific role
     */
    @Service(
        name = "setCommunicationEventRoleStatus",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "setCommunicationEventRoleStatus",
        description = "Set the Communication event  Status for a specific role",
        defaultEntityName = "CommunicationEventRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "partyCommunicationEventPermissionCheck", mainAction = "UPDATE")
    )
    public interface SetCommunicationEventRoleStatus {}

    /**
     * Create a Communication Event with or w/o permission check
     */
    @Service(
        name = "createCommunicationEventInterface",
        engine = "interface",
        description = "Create a Communication Event with or w/o permission check",
        entityAttributes = {
            @EntityAttributes(entityName = "CommunicationEvent", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "CommunicationEvent", mode = "INOUT", include = "pk", optional = "true")
        },
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "custRequestId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "action", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "headerString", allowHtml = "any"),
            @OverrideAttribute(name = "content", allowHtml = "any"),
            @OverrideAttribute(name = "messageId", allowHtml = "any"),
            @OverrideAttribute(name = "subject", allowHtml = "any")
        }
    )
    public interface CreateCommunicationEventInterface {}

    /**
     * Create a Communication Event with permission check
     */
    @Service(
        name = "createCommunicationEvent",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "createCommunicationEventWithPermission",
        description = "Create a Communication Event with permission check",
        auth = "true",
        implemented = {@Implements(service = "createCommunicationEventInterface")},
        permissionService = @PermissionService(service = "partyCommunicationEventPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateCommunicationEvent {}

    /**
     * Create a Communication Event without permission check
     */
    @Service(
        name = "createCommunicationEventWithoutPermission",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "createCommunicationEventWithoutPermission",
        description = "Create a Communication Event without permission check",
        auth = "true",
        implemented = {@Implements(service = "createCommunicationEventInterface")}
    )
    public interface CreateCommunicationEventWithoutPermission {}

    /**
     * Update a Communication Event
     */
    @Service(
        name = "updateCommunicationEvent",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "updateCommunicationEvent",
        description = "Update a Communication Event",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CommunicationEvent", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "CommunicationEvent", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechPurposeTypeIdFrom", type = "String", mode = "IN", optional = "true", description = "Set a specific purpose for the originator email"),
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "partyCommunicationEventPermissionCheck", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "messageId", allowHtml = "any"),
            @OverrideAttribute(name = "content", allowHtml = "any"),
            @OverrideAttribute(name = "subject", allowHtml = "any")
        }
    )
    public interface UpdateCommunicationEvent {}

    /**
     * Delete a Communication Event, optionally delete the attached content and dataresource
     */
    @Service(
        name = "deleteCommunicationEvent",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "deleteCommunicationEvent",
        description = "Delete a Communication Event, optionally delete the attached content and dataresource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CommunicationEvent", mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "delContentDataResource", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "partyCommunicationEventPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteCommunicationEvent {}

    /**
     * Delete a Communication Event, optionally delete the attached content and dataresource             and when this is the only communication event connected to a workeffort delete the workeffort too.
     */
    @Service(
        name = "deleteCommunicationEventWorkEffort",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "deleteCommunicationEventWorkEffort",
        description = "Delete a Communication Event, optionally delete the attached content and dataresource\n            and when this is the only communication event connected to a workeffort delete the workeffort too.",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CommunicationEvent", mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "delContentDataResource", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "partyCommunicationEventPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteCommunicationEventWorkEffort {}

    /**
     * Create a Communication Event Purpose
     */
    @Service(
        name = "createCommunicationEventPurpose",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "createCommunicationEventPurpose",
        description = "Create a Communication Event Purpose",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CommunicationEventPurpose", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "CommunicationEventPurpose", mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyCommunicationEventPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateCommunicationEventPurpose {}

    /**
     * Update a CommunicationEventPurpose
     */
    @Service(
        name = "updateCommunicationEventPurpose",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a CommunicationEventPurpose",
        defaultEntityName = "CommunicationEventPurpose",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCommunicationEventPurpose {}

    /**
     * Remove a Communication Event Purpose
     */
    @Service(
        name = "removeCommunicationEventPurpose",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "removeCommunicationEventPurpose",
        description = "Remove a Communication Event Purpose",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CommunicationEventPurpose", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "partyCommunicationEventPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveCommunicationEventPurpose {}

    /**
     * Create a CustRequestCommEvent
     */
    @Service(
        name = "createCustRequestCommEvent",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "createCustRequestCommEvent",
        description = "Create a CustRequestCommEvent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CustRequestCommEvent", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "partyCommunicationEventPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateCustRequestCommEvent {}

    /**
     * Delete a CustRequestCommEvent
     */
    @Service(
        name = "deleteCustRequestCommEvent",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a CustRequestCommEvent",
        defaultEntityName = "CustRequestCommEvent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "partyCommunicationEventPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteCustRequestCommEvent {}

    /**
     * Create a Communication Event Role with or w/o permission check
     */
    @Service(
        name = "createCommunicationEventRoleInterface",
        engine = "interface",
        description = "Create a Communication Event Role with or w/o permission check",
        entityAttributes = {
            @EntityAttributes(entityName = "CommunicationEventRole", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "CommunicationEventRole", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCommunicationEventRoleInterface {}

    /**
     * Create a Communication Event Role with permission check
     */
    @Service(
        name = "createCommunicationEventRole",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "createCommunicationEventRole",
        description = "Create a Communication Event Role with permission check",
        auth = "true",
        implemented = {@Implements(service = "createCommunicationEventRoleInterface")},
        permissionService = @PermissionService(service = "partyCommunicationEventPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateCommunicationEventRole {}

    /**
     * Create a Communication Event Role without permission check
     */
    @Service(
        name = "createCommunicationEventRoleWithoutPermission",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "createCommunicationEventRole",
        description = "Create a Communication Event Role without permission check",
        auth = "true",
        implemented = {@Implements(service = "createCommunicationEventRoleInterface")}
    )
    public interface CreateCommunicationEventRoleWithoutPermission {}

    /**
     * Update a Communication Event Role
     */
    @Service(
        name = "updateCommunicationEventRole",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "updateCommunicationEventRole",
        description = "Update a Communication Event Role",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CommunicationEventRole", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "CommunicationEventRole", mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "partyCommunicationEventPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateCommunicationEventRole {}

    /**
     * Remove a Communication Event Role
     */
    @Service(
        name = "removeCommunicationEventRole",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "removeCommunicationEventRole",
        description = "Remove a Communication Event Role",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CommunicationEventRole", mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "deleteCommEventIfLast", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "delContentDataResource", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "partyCommunicationEventPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveCommunicationEventRole {}

    /**
     * Creates a WorkEffort entity and CommunicationEventWorkEff
     */
    @Service(
        name = "createCommEventWorkEffort",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "makeCommunicationEventWorkEffort",
        description = "Creates a WorkEffort entity and CommunicationEventWorkEff",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "WorkEffort", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "WorkEffort", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "CommunicationEventWorkEff", mode = "INOUT", include = "pk"),
            @EntityAttributes(entityName = "CommunicationEventWorkEff", mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortId", optional = "true"),
            @OverrideAttribute(name = "currentStatusId", optional = "false"),
            @OverrideAttribute(name = "workEffortName", optional = "false"),
            @OverrideAttribute(name = "workEffortTypeId", optional = "false")
        }
    )
    public interface CreateCommEventWorkEffort {}

    /**
     * Marks a communication event as read
     */
    @Service(
        name = "setCommEventRoleToRead",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "setCommEventRoleToRead",
        description = "Marks a communication event as read",
        defaultEntityName = "CommunicationEventRole",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "communicationEventId", optional = "false")
        }
    )
    public interface SetCommEventRoleToRead {}

    /**
     * Sends a communication event as a single-part email using sendMail.  All parameters come from CommunicationEvent, which must             be of type EMAIL_COMMUNICATION. Will look for a contactMechIdTo to send the emails
     */
    @Service(
        name = "sendCommEventAsEmail",
        engine = "java",
        location = "org.ofbiz.party.communication.CommunicationEventServices",
        invoke = "sendCommEventAsEmail",
        description = "Sends a communication event as a single-part email using sendMail.  All parameters come from CommunicationEvent, which must\n            be of type EMAIL_COMMUNICATION. Will look for a contactMechIdTo to send the emails",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "communicationEventId", type = "String", mode = "IN")
        }
    )
    public interface SendCommEventAsEmail {}

    /**
     * Sends communication event associated contents to a ftp server. All parameters come from CommunicationEvent, which must             be of type FILE_TRANSFER_COMM. Will look for a contactMechIdTo to connect to Ftp
     */
    @Service(
        name = "sendCommEventAsFtp",
        engine = "java",
        location = "org.ofbiz.party.communication.CommunicationEventServices",
        invoke = "sendCommEventAsFtp",
        description = "Sends communication event associated contents to a ftp server. All parameters come from CommunicationEvent, which must\n            be of type FILE_TRANSFER_COMM. Will look for a contactMechIdTo to connect to Ftp",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "communicationEventId", type = "String", mode = "IN")
        }
    )
    public interface SendCommEventAsFtp {}

    /**
     * Creates a CommunicationEvent record based on information before             running a sendContentToFtp service (to be used via ECA)         
     */
    @Service(
        name = "createCommEventFromFtpTransfer",
        engine = "java",
        location = "org.ofbiz.party.communication.CommunicationEventServices",
        invoke = "createCommEventFromFtpTransfer",
        description = "Creates a CommunicationEvent record based on information before\n            running a sendContentToFtp service (to be used via ECA)\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "contactMechId", type = "String", mode = "IN"),
            @Attribute(name = "communicationEventId", type = "String", mode = "OUT")
        }
    )
    public interface CreateCommEventFromFtpTransfer {}

    /**
     *                  Creates a CommunicationEvent record based on information before running a sendMail service (to be used via ECA)         
     */
    @Service(
        name = "createCommEventFromEmail",
        engine = "java",
        location = "org.ofbiz.party.communication.CommunicationEventServices",
        invoke = "createCommEventFromEmail",
        description = "\n                Creates a CommunicationEvent record based on information before running a sendMail service (to be used via ECA)\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "subject", type = "String", mode = "IN"),
            @Attribute(name = "sendFrom", type = "String", mode = "IN"),
            @Attribute(name = "sendTo", type = "String", mode = "IN"),
            @Attribute(name = "contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "communicationEventId", type = "String", mode = "OUT")
        }
    )
    public interface CreateCommEventFromEmail {}

    /**
     *                  Updates a CommunicationEvent record after running a sendMail service (to be used via ECA)         
     */
    @Service(
        name = "updateCommEventAfterEmail",
        engine = "java",
        location = "org.ofbiz.party.communication.CommunicationEventServices",
        invoke = "updateCommEventAfterEmail",
        description = "\n                Updates a CommunicationEvent record after running a sendMail service (to be used via ECA)\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "communicationEventId", type = "String", mode = "IN"),
            @Attribute(name = "messageWrapper", type = "org.ofbiz.service.mail.MimeMessageWrapper", mode = "IN")
        }
    )
    public interface UpdateCommEventAfterEmail {}

    /**
     *              Process incoming email. Try to determine partyIdFrom from the first SendFrom email address. datetimeStarted and datetimeEnded are the             sent and received dates respectively, partyIdTo is from the first SendTo email address or the delivered-to address. If the parties are not found,             the email addresses are stored in CommunicationEvent.note             If however it is detected as spam (external) or when the 'from' email address is missing, the service will not return a communicationEventId.             If the party cannot be found the status of the communicationEvent will be set to: COM_UNKNOWN_PARTY.             If the parties are found the status is set to COM_ENTERED         
     */
    @Service(
        name = "storeIncomingEmail",
        engine = "java",
        location = "org.ofbiz.party.communication.CommunicationEventServices",
        invoke = "storeIncomingEmail",
        description = "\n            Process incoming email. Try to determine partyIdFrom from the first SendFrom email address. datetimeStarted and datetimeEnded are the\n            sent and received dates respectively, partyIdTo is from the first SendTo email address or the delivered-to address. If the parties are not found,\n            the email addresses are stored in CommunicationEvent.note\n            If however it is detected as spam (external) or when the 'from' email address is missing, the service will not return a communicationEventId.\n            If the party cannot be found the status of the communicationEvent will be set to: COM_UNKNOWN_PARTY.\n            If the parties are found the status is set to COM_ENTERED\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "messageWrapper", type = "org.ofbiz.service.mail.MimeMessageWrapper", mode = "IN"),
            @Attribute(name = "communicationEventId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface StoreIncomingEmail {}

    /**
     * Send emails to members of a contact list, wrapping each email in its own transaction and tagging each member                 that has been sent, so if the whole effort is aborted, it can start over from the middle.  The max-retry is important because if this service is                 and some emails cannot sent, it will start again later and try again
     */
    @Service(
        name = "sendEmailToContactList",
        engine = "java",
        location = "org.ofbiz.party.communication.CommunicationEventServices",
        invoke = "sendEmailToContactList",
        description = "Send emails to members of a contact list, wrapping each email in its own transaction and tagging each member\n                that has been sent, so if the whole effort is aborted, it can start over from the middle.  The max-retry is important because if this service is\n                and some emails cannot sent, it will start again later and try again",
        auth = "true",
        transactionTimeout = "300",
        maxRetry = "3",
        attributes = {
            @Attribute(name = "contactListId", type = "String", mode = "IN"),
            @Attribute(name = "communicationEventId", type = "String", mode = "IN")
        }
    )
    public interface SendEmailToContactList {}

    /**
     * Sets the status of a communication event to COM_COMPLETE using the updateCommunicationEvent service,             set datetimeEnded to now if not defined
     */
    @Service(
        name = "setCommEventComplete",
        engine = "java",
        location = "org.ofbiz.party.communication.CommunicationEventServices",
        invoke = "setCommEventComplete",
        description = "Sets the status of a communication event to COM_COMPLETE using the updateCommunicationEvent service,\n            set datetimeEnded to now if not defined",
        auth = "true",
        attributes = {
            @Attribute(name = "communicationEventId", type = "String", mode = "IN"),
            @Attribute(name = "partyIdFrom", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SetCommEventComplete {}

    /**
     * Checks for email communication events with the status COM_IN_PROGRESS and a startdate which is expired, then send the email
     */
    @Service(
        name = "sendEmailDated",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "sendEmailDated",
        description = "Checks for email communication events with the status COM_IN_PROGRESS and a startdate which is expired, then send the email",
        auth = "true",
        useTransaction = "false"
    )
    public interface SendEmailDated {}

    /**
     * Create a PartyContent record
     */
    @Service(
        name = "createPartyContent",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/content/PartyContentServices.xml",
        invoke = "createPartyContent",
        description = "Create a PartyContent record",
        defaultEntityName = "PartyContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreatePartyContent {}

    /**
     * Update a PartyContent record
     */
    @Service(
        name = "updatePartyContent",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/content/PartyContentServices.xml",
        invoke = "updatePartyContent",
        description = "Update a PartyContent record",
        defaultEntityName = "PartyContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePartyContent {}

    /**
     * Remove a PartyContent record
     */
    @Service(
        name = "removePartyContent",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/content/PartyContentServices.xml",
        invoke = "removePartyContent",
        description = "Remove a PartyContent record",
        defaultEntityName = "PartyContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemovePartyContent {}

    /**
     * Remove a PartyContent record and the Content record itself (SCIPIO)             WARN: Does not delete files stored in the filesystem!
     */
    @Service(
        name = "removePartyContentAndRelated",
        engine = "group",
        description = "Remove a PartyContent record and the Content record itself (SCIPIO)\n            WARN: Does not delete files stored in the filesystem!",
        auth = "true",
        invokes = {@GroupInvoke(name = "removePartyContent", resultToContext = "false"), @GroupInvoke(name = "removeContentAndRelated", resultToContext = "false")}
    )
    public interface RemovePartyContentAndRelated {}

    /**
     * Creates a Text Document DataResource and Content Records
     */
    @Service(
        name = "createPartyTextContent",
        engine = "group",
        description = "Creates a Text Document DataResource and Content Records",
        auth = "true",
        transactionTimeout = "300",
        invokes = {@GroupInvoke(name = "createTextContent", resultToContext = "true"), @GroupInvoke(name = "createPartyContent", resultToContext = "false")}
    )
    public interface CreatePartyTextContent {}

    /**
     * Upload and attach a file to a party
     */
    @Service(
        name = "uploadPartyContentFile",
        engine = "group",
        description = "Upload and attach a file to a party",
        transactionTimeout = "300",
        invokes = {@GroupInvoke(name = "createContentFromUploadedFile", resultToContext = "true"), @GroupInvoke(name = "createPartyContent", resultToContext = "false")}
    )
    public interface UploadPartyContentFile {}

    /**
     * Get the main party Email address
     */
    @Service(
        name = "getPartyEmail",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "getPartyEmail",
        description = "Get the main party Email address",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN", defaultValue = "PRIMARY_EMAIL"),
            @Attribute(name = "emailAddress", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface GetPartyEmail {}

    /**
     * Get the party Email Telephone
     */
    @Service(
        name = "getPartyTelephone",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "getPartyTelephone",
        description = "Get the party Email Telephone",
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "countryCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "areaCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "contactNumber", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "extension", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface GetPartyTelephone {}

    /**
     * Get the party postal address
     */
    @Service(
        name = "getPartyPostalAddress",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "getPartyPostalAddress",
        description = "Get the party postal address",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "address1", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "address2", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "directions", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "city", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "postalCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "stateProvinceGeoId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "countyGeoId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "countryGeoId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface GetPartyPostalAddress {}

    /**
     * Create a PartyCarrierAccount record
     */
    @Service(
        name = "createPartyCarrierAccount",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PartyCarrierAccount record",
        defaultEntityName = "PartyCarrierAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @OverrideAttribute(name = "accountNumber", type = "String", mode = "IN", optional = "false")
        }
    )
    public interface CreatePartyCarrierAccount {}

    /**
     * Update a PartyCarrierAccount record
     */
    @Service(
        name = "updatePartyCarrierAccount",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PartyCarrierAccount record",
        defaultEntityName = "PartyCarrierAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePartyCarrierAccount {}

    @Service(
        name = "sendUpdatePersonalInfoEmailNotification",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "sendUpdatePersonalInfoEmailNotification",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "updatedUserLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SendUpdatePersonalInfoEmailNotification {}

    @Service(
        name = "sendCreatePartyEmailNotification",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "sendCreatePartyEmailNotification",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "emailAddress", type = "String", mode = "IN"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN")
        }
    )
    public interface SendCreatePartyEmailNotification {}

    @Service(
        name = "createEmailAddressVerification",
        engine = "java",
        location = "org.ofbiz.party.contact.ContactMechServices",
        invoke = "createEmailAddressVerification",
        defaultEntityName = "EmailAddressVerification",
        attributes = {
            @Attribute(name = "emailAddress", type = "String", mode = "IN"),
            @Attribute(name = "verifyHash", type = "String", mode = "OUT"),
            @Attribute(name = "expireDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sendVerificationEmail", type = "Boolean", mode = "INOUT", optional = "true", defaultValue = "true")
        }
    )
    public interface CreateEmailAddressVerification {}

    @Service(
        name = "sendVerifyEmailAddressNotification",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.xml",
        invoke = "sendVerifyEmailAddressNotification",
        attributes = {
            @Attribute(name = "emailAddress", type = "String", mode = "IN")
        }
    )
    public interface SendVerifyEmailAddressNotification {}

    @Service(
        name = "verifyEmailAddress",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/ContactMechServices.xml",
        invoke = "verifyEmailAddress",
        attributes = {
            @Attribute(name = "verifyHash", type = "String", mode = "IN")
        }
    )
    public interface VerifyEmailAddress {}

    /**
     * Create Party Invitation
     */
    @Service(
        name = "createPartyInvitation",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyInvitationServices.xml",
        invoke = "createPartyInvitation",
        description = "Create Party Invitation",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyInvitation", mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "PartyInvitation", mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "partyInvitationId", mode = "OUT", optional = "false")
        }
    )
    public interface CreatePartyInvitation {}

    /**
     * Update Party Invitation
     */
    @Service(
        name = "updatePartyInvitation",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyInvitationServices.xml",
        invoke = "updatePartyInvitation",
        description = "Update Party Invitation",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyInvitation", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "PartyInvitation", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePartyInvitation {}

    /**
     * Remove Party Invitation
     */
    @Service(
        name = "deletePartyInvitation",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyInvitationServices.xml",
        invoke = "deletePartyInvitation",
        description = "Remove Party Invitation",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyInvitation", mode = "IN", include = "pk")
        }
    )
    public interface DeletePartyInvitation {}

    /**
     * Create PartyInvitationGroupAssoc
     */
    @Service(
        name = "createPartyInvitationGroupAssoc",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyInvitationServices.xml",
        invoke = "createPartyInvitationGroupAssoc",
        description = "Create PartyInvitationGroupAssoc",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyInvitationGroupAssoc", mode = "IN", include = "pk")
        }
    )
    public interface CreatePartyInvitationGroupAssoc {}

    /**
     * Remove PartyInvitationGroupAssoc
     */
    @Service(
        name = "deletePartyInvitationGroupAssoc",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyInvitationServices.xml",
        invoke = "deletePartyInvitationGroupAssoc",
        description = "Remove PartyInvitationGroupAssoc",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyInvitationGroupAssoc", mode = "IN", include = "pk")
        }
    )
    public interface DeletePartyInvitationGroupAssoc {}

    /**
     * Create PartyInvitationRoleAssoc
     */
    @Service(
        name = "createPartyInvitationRoleAssoc",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyInvitationServices.xml",
        invoke = "createPartyInvitationRoleAssoc",
        description = "Create PartyInvitationRoleAssoc",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyInvitationRoleAssoc", mode = "IN", include = "pk")
        }
    )
    public interface CreatePartyInvitationRoleAssoc {}

    /**
     * Remove PartyInvitationRoleAssoc
     */
    @Service(
        name = "deletePartyInvitationRoleAssoc",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyInvitationServices.xml",
        invoke = "deletePartyInvitationRoleAssoc",
        description = "Remove PartyInvitationRoleAssoc",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyInvitationRoleAssoc", mode = "IN", include = "pk")
        }
    )
    public interface DeletePartyInvitationRoleAssoc {}

    @Service(
        name = "acceptPartyInvitation",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyInvitationServices.xml",
        invoke = "acceptPartyInvitation",
        attributes = {
            @Attribute(name = "partyInvitationId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "accAndDecPartyInvitationPermissionCheck")
    )
    public interface AcceptPartyInvitation {}

    @Service(
        name = "declinePartyInvitation",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyInvitationServices.xml",
        invoke = "declinePartyInvitation",
        attributes = {
            @Attribute(name = "partyInvitationId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "accAndDecPartyInvitationPermissionCheck")
    )
    public interface DeclinePartyInvitation {}

    @Service(
        name = "cancelPartyInvitation",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyInvitationServices.xml",
        invoke = "cancelPartyInvitation",
        attributes = {
            @Attribute(name = "partyInvitationId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "cancelPartyInvitationPermissionCheck")
    )
    public interface CancelPartyInvitation {}

    /**
     *              Performs a basic Party Manager security check. The user must have one of the base PARTYMGR             CRUD+ADMIN permissions.         
     */
    @Service(
        name = "partyBasePermissionCheck",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyPermissionServices.xml",
        invoke = "basePermissionCheck",
        description = "\n            Performs a basic Party Manager security check. The user must have one of the base PARTYMGR\n            CRUD+ADMIN permissions.\n        ",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface PartyBasePermissionCheck {}

    /**
     *              Performs a party ID security check. The userLogin partyId must equal             the partyId parameter, or the logged-in user must have the correct permission             to perform the operation.         
     */
    @Service(
        name = "partyIdPermissionCheck",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyPermissionServices.xml",
        invoke = "basePlusPartyIdPermissionCheck",
        description = "\n            Performs a party ID security check. The userLogin partyId must equal\n            the partyId parameter, or the logged-in user must have the correct permission\n            to perform the operation.\n        ",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface PartyIdPermissionCheck {}

    /**
     *              Performs a party status security check. The userLogin partyId must equal the partyId parameter OR             the user must have one of the base PARTYMGR or PARTYMGR_STS CRUD+ADMIN permissions.         
     */
    @Service(
        name = "partyStatusPermissionCheck",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyPermissionServices.xml",
        invoke = "partyStatusPermissionCheck",
        description = "\n            Performs a party status security check. The userLogin partyId must equal the partyId parameter OR\n            the user must have one of the base PARTYMGR or PARTYMGR_STS CRUD+ADMIN permissions.\n        ",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface PartyStatusPermissionCheck {}

    /**
     *              Performs a party group security check. The userLogin partyId must equal the partyId parameter OR             the user has one of the base PARTYMGR or PARTYMGR_GRP CRUD+ADMIN permissions.         
     */
    @Service(
        name = "partyGroupPermissionCheck",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyPermissionServices.xml",
        invoke = "partyGroupPermissionCheck",
        description = "\n            Performs a party group security check. The userLogin partyId must equal the partyId parameter OR\n            the user has one of the base PARTYMGR or PARTYMGR_GRP CRUD+ADMIN permissions.\n        ",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface PartyGroupPermissionCheck {}

    /**
     *              Performs a party datasource security check. The user must have one of the base PARTYMGR or             PARTYMGR_SRC CRUD+ADMIN permissions.         
     */
    @Service(
        name = "partyDatasourcePermissionCheck",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyPermissionServices.xml",
        invoke = "partyDatasourcePermissionCheck",
        description = "\n            Performs a party datasource security check. The user must have one of the base PARTYMGR or\n            PARTYMGR_SRC CRUD+ADMIN permissions.\n        ",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface PartyDatasourcePermissionCheck {}

    /**
     *              Performs a party role security check. The user must have one of the base PARTYMGR or             PARTYMGR_ROLE CRUD+ADMIN permissions.         
     */
    @Service(
        name = "partyRolePermissionCheck",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyPermissionServices.xml",
        invoke = "partyRolePermissionCheck",
        description = "\n            Performs a party role security check. The user must have one of the base PARTYMGR or\n            PARTYMGR_ROLE CRUD+ADMIN permissions.\n        ",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface PartyRolePermissionCheck {}

    /**
     *              Performs a party relationship security check. The user must have one of the base PARTYMGR or             PARTYMGR_REL CRUD+ADMIN permissions.         
     */
    @Service(
        name = "partyRelationshipPermissionCheck",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyPermissionServices.xml",
        invoke = "partyRelationshipPermissionCheck",
        description = "\n            Performs a party relationship security check. The user must have one of the base PARTYMGR or\n            PARTYMGR_REL CRUD+ADMIN permissions.\n        ",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "partyIdFrom", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface PartyRelationshipPermissionCheck {}

    /**
     *              Performs a party contact mech security check. The userLogin partyId must equal the partyId parameter OR             the user must have one of the base PARTYMGR or PARTYMGR_PCM CRUD+ADMIN permissions.         
     */
    @Service(
        name = "partyContactMechPermissionCheck",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyPermissionServices.xml",
        invoke = "partyContactMechPermissionCheck",
        description = "\n            Performs a party contact mech security check. The userLogin partyId must equal the partyId parameter OR\n            the user must have one of the base PARTYMGR or PARTYMGR_PCM CRUD+ADMIN permissions.\n        ",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface PartyContactMechPermissionCheck {}

    /**
     *              Performs accept and decline PartyInvitation security check. The userLogin partyId must equal the             partyIdTo in PartyInvitation OR partyId fetched using emailAdress in PartyInvitation.             The user with PARTYMGR_UPDATE permission can also perform this function.         
     */
    @Service(
        name = "accAndDecPartyInvitationPermissionCheck",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyPermissionServices.xml",
        invoke = "accAndDecPartyInvitationPermissionCheck",
        description = "\n            Performs accept and decline PartyInvitation security check. The userLogin partyId must equal the\n            partyIdTo in PartyInvitation OR partyId fetched using emailAdress in PartyInvitation.\n            The user with PARTYMGR_UPDATE permission can also perform this function.\n        ",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "partyInvitationId", type = "String", mode = "IN")
        }
    )
    public interface AccAndDecPartyInvitationPermissionCheck {}

    /**
     *              Performs cancel PartyInvitation security check. The userLogin partyId must equal the             partyId/partyIdFrom in PartyInvitation OR partyId fetched using emailAdress in PartyInvitation.             The user with PARTYMGR_UPDATE permission can also perform this function.         
     */
    @Service(
        name = "cancelPartyInvitationPermissionCheck",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyPermissionServices.xml",
        invoke = "cancelPartyInvitationPermissionCheck",
        description = "\n            Performs cancel PartyInvitation security check. The userLogin partyId must equal the\n            partyId/partyIdFrom in PartyInvitation OR partyId fetched using emailAdress in PartyInvitation.\n            The user with PARTYMGR_UPDATE permission can also perform this function.\n        ",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "partyInvitationId", type = "String", mode = "IN")
        }
    )
    public interface CancelPartyInvitationPermissionCheck {}

    /**
     * Party CommunicationEvents Permission Checking Logic
     */
    @Service(
        name = "partyCommunicationEventPermissionCheck",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyPermissionServices.xml",
        invoke = "partyCommunicationEventPermissionCheck",
        description = "Party CommunicationEvents Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "partyIdFrom", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyIdTo", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface PartyCommunicationEventPermissionCheck {}

    /**
     * Create postal address, purposes and set them defaults
     */
    @Service(
        name = "createPostalAddressAndPurposes",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "createPostalAddressAndPurposes",
        description = "Create postal address, purposes and set them defaults",
        implemented = {@Implements(service = "createPartyPostalAddress")},
        attributes = {
            @Attribute(name = "setShippingPurpose", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "setBillingPurpose", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreatePostalAddressAndPurposes {}

    /**
     * Update postal address, purposes and set them defaults. The setShippingPurpose and setBillingPurpose enable the service to create purposes for PostalAddress and make them default addresses of party             (SCIPIO: Note: This is a PartyProfileDefault- and UI-oriented old stock service; see implementation for quirks)
     */
    @Service(
        name = "updatePostalAddressAndPurposes",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "updatePostalAddressAndPurposes",
        description = "Update postal address, purposes and set them defaults. The setShippingPurpose and setBillingPurpose enable the service to create purposes for PostalAddress and make them default addresses of party\n            (SCIPIO: Note: This is a PartyProfileDefault- and UI-oriented old stock service; see implementation for quirks)",
        implemented = {@Implements(service = "updatePartyPostalAddress")},
        attributes = {
            @Attribute(name = "setShippingPurpose", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "setBillingPurpose", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "contactMechId", optional = "false")
        }
    )
    public interface UpdatePostalAddressAndPurposes {}

    /**
     * Update postal address, telecom number and purposes. The setShippingPurpose and setBillingPurpose enable the service to create purposes for TelecomNumber             (SCIPIO: Note: This is a PartyProfileDefault- and UI-oriented old stock service; see implementation for quirks)
     */
    @Service(
        name = "updateContactMechAndPurposes",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "updateContactMechAndPurposes",
        description = "Update postal address, telecom number and purposes. The setShippingPurpose and setBillingPurpose enable the service to create purposes for TelecomNumber\n            (SCIPIO: Note: This is a PartyProfileDefault- and UI-oriented old stock service; see implementation for quirks)",
        implemented = {@Implements(service = "updatePostalAddressAndPurposes")},
        entityAttributes = {
            @EntityAttributes(entityName = "TelecomNumber", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "phoneContactMechId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateContactMechAndPurposes {}

    /**
     * Create and Update a person
     */
    @Service(
        name = "createUpdatePerson",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "createUpdatePerson",
        description = "Create and Update a person",
        defaultEntityName = "Person",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "firstName", optional = "false"),
            @OverrideAttribute(name = "lastName", optional = "false")
        }
    )
    public interface CreateUpdatePerson {}

    /**
     * Create and Update telecom number
     */
    @Service(
        name = "createUpdatePartyTelecomNumber",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "createUpdatePartyTelecomNumber",
        description = "Create and Update telecom number",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyContactMech", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "TelecomNumber", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateUpdatePartyTelecomNumber {}

    /**
     * Create and Update email address
     */
    @Service(
        name = "createUpdatePartyEmailAddress",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "createUpdatePartyEmailAddress",
        description = "Create and Update email address",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "emailAddress", type = "String", mode = "INOUT"),
            @Attribute(name = "contactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "OUT")
        }
    )
    public interface CreateUpdatePartyEmailAddress {}

    /**
     * Create or Update a postal address
     */
    @Service(
        name = "createUpdatePartyPostalAddress",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml",
        invoke = "createUpdatePartyPostalAddress",
        description = "Create or Update a postal address",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyContactMech", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "PostalAddress", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateUpdatePartyPostalAddress {}

    @Service(
        name = "processBouncedMessage",
        engine = "java",
        location = "org.ofbiz.party.communication.CommunicationEventServices",
        invoke = "processBouncedMessage",
        implemented = {@Implements(service = "mailProcessInterface")}
    )
    public interface ProcessBouncedMessage {}

    @Service(
        name = "logIncomingMessage",
        engine = "java",
        location = "org.ofbiz.party.communication.CommunicationEventServices",
        invoke = "logIncomingMessage",
        implemented = {@Implements(service = "mailProcessInterface")}
    )
    public interface LogIncomingMessage {}

    /**
     * Create customer profile on basis of First Name ,Last Name and Email Address
     */
    @Service(
        name = "quickCreateCustomer",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "quickCreateCustomer",
        description = "Create customer profile on basis of First Name ,Last Name and Email Address",
        attributes = {
            @Attribute(name = "firstName", type = "String", mode = "IN"),
            @Attribute(name = "lastName", type = "String", mode = "IN"),
            @Attribute(name = "emailAddress", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "OUT"),
            @Attribute(name = "contactListId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "subscribeContactList", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface QuickCreateCustomer {}

    /**
     * Get the main role of this party which is a child of the MAIN_ROLE roletypeId
     */
    @Service(
        name = "getPartyMainRole",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "getPartyMainRole",
        description = "Get the main role of this party which is a child of the MAIN_ROLE roletypeId",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface GetPartyMainRole {}

    /**
     * Create communication event and send mail to company
     */
    @Service(
        name = "sendContactUsEmailToCompany",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml",
        invoke = "sendContactUsEmailToCompany",
        description = "Create communication event and send mail to company",
        implemented = {@Implements(service = "createCommunicationEventWithoutPermission")},
        attributes = {
            @Attribute(name = "emailAddress", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "firstName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "lastName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "postalCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "countryCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "emailType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "replyTo", type = "java.util.List", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "partyIdTo", optional = "false")
        }
    )
    public interface SendContactUsEmailToCompany {}

    @Service(
        name = "sendAccountActivatedEmailNotification",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "sendAccountActivatedEmailNotification",
        attributes = {
            @Attribute(name = "userLoginId", type = "String", mode = "IN"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN")
        }
    )
    public interface SendAccountActivatedEmailNotification {}

    /**
     * Create a AgreementAttribute entry
     */
    @Service(
        name = "createAgreementAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a AgreementAttribute entry",
        defaultEntityName = "AgreementAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateAgreementAttribute {}

    /**
     * Update a AgreementAttribute record
     */
    @Service(
        name = "updateAgreementAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a AgreementAttribute record",
        defaultEntityName = "AgreementAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAgreementAttribute {}

    /**
     * Delete a AgreementAttribute record
     */
    @Service(
        name = "deleteAgreementAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a AgreementAttribute record",
        defaultEntityName = "AgreementAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteAgreementAttribute {}

    /**
     * Import an party with related main role, company and contact info in csv format, will ignore parties already entered
     */
    @Service(
        name = "importParty",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "importParty",
        description = "Import an party with related main role, company and contact info in csv format, will ignore parties already entered",
        auth = "true",
        attributes = {
            @Attribute(name = "uploadedFile", type = "java.nio.ByteBuffer", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "partyGroupPermissionCheck", mainAction = "CREATE")
    )
    public interface ImportParty {}

    /**
     * Create a AgreementItemTypeAttr entry
     */
    @Service(
        name = "createAgreementItemTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a AgreementItemTypeAttr entry",
        defaultEntityName = "AgreementItemTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateAgreementItemTypeAttr {}

    /**
     * Update a AgreementItemTypeAttr record
     */
    @Service(
        name = "updateAgreementItemTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a AgreementItemTypeAttr record",
        defaultEntityName = "AgreementItemTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAgreementItemTypeAttr {}

    /**
     * Delete a AgreementItemTypeAttr record
     */
    @Service(
        name = "deleteAgreementItemTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a AgreementItemTypeAttr record",
        defaultEntityName = "AgreementItemTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteAgreementItemTypeAttr {}

    /**
     * Update a RoleType Record
     */
    @Service(
        name = "updateRoleType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a RoleType Record",
        defaultEntityName = "RoleType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateRoleType {}

    /**
     * Delete a RoleType Record
     */
    @Service(
        name = "deleteRoleType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a RoleType Record",
        defaultEntityName = "RoleType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteRoleType {}

    /**
     * Create a RoleTypeAttr Record
     */
    @Service(
        name = "createRoleTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a RoleTypeAttr Record",
        defaultEntityName = "RoleTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateRoleTypeAttr {}

    /**
     * Update a RoleTypeAttr Record
     */
    @Service(
        name = "updateRoleTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a RoleTypeAttr Record",
        defaultEntityName = "RoleTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateRoleTypeAttr {}

    /**
     * Delete a RoleTypeAttr Record
     */
    @Service(
        name = "deleteRoleTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a RoleTypeAttr Record",
        defaultEntityName = "RoleTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteRoleTypeAttr {}

    /**
     * Create a NeedType
     */
    @Service(
        name = "createNeedType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a NeedType",
        defaultEntityName = "NeedType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateNeedType {}

    /**
     * Update a NeedType
     */
    @Service(
        name = "updateNeedType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a NeedType",
        defaultEntityName = "NeedType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface UpdateNeedType {}

    /**
     * Delete a NeedType
     */
    @Service(
        name = "deleteNeedType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a NeedType",
        defaultEntityName = "NeedType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteNeedType {}

    /**
     * Create a CommunicationEventPrpTyp
     */
    @Service(
        name = "createCommunicationEventPrpTyp",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a CommunicationEventPrpTyp",
        defaultEntityName = "CommunicationEventPrpTyp",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateCommunicationEventPrpTyp {}

    /**
     * Update a CommunicationEventPrpTyp
     */
    @Service(
        name = "updateCommunicationEventPrpTyp",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a CommunicationEventPrpTyp",
        defaultEntityName = "CommunicationEventPrpTyp",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCommunicationEventPrpTyp {}

    /**
     * Delete a CommunicationEventPrpTyp
     */
    @Service(
        name = "deleteCommunicationEventPrpTyp",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a CommunicationEventPrpTyp",
        defaultEntityName = "CommunicationEventPrpTyp",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCommunicationEventPrpTyp {}

    /**
     * Service to create a person, a UserLogin and add standard security groups to the new user
     */
    @Service(
        name = "createPartyLoginAndSecurityGroups",
        engine = "java",
        location = "com.ilscipio.scipio.party.PartyServices",
        invoke = "createPartyLoginAndSecurityGroups",
        description = "Service to create a person, a UserLogin and add standard security groups to the new user",
        auth = "true",
        implemented = {@Implements(service = "createPersonAndUserLogin"), @Implements(service = "addUserLoginToSecurityGroup")}
    )
    public interface CreatePartyLoginAndSecurityGroups {}

    /**
     * Service to check, if a UserLogin exists for a given userLoginId
     */
    @Service(
        name = "findUserLogin",
        engine = "java",
        location = "com.ilscipio.scipio.party.PartyServices",
        invoke = "findUserLogin",
        description = "Service to check, if a UserLogin exists for a given userLoginId",
        auth = "true",
        attributes = {
            @Attribute(name = "userLoginId", type = "String", mode = "IN"),
            @Attribute(name = "responseMessage", type = "String", mode = "OUT"),
            @Attribute(name = "exists", type = "Boolean", mode = "OUT"),
            @Attribute(name = "userLogin", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface FindUserLogin {}

    /**
     * Count PartyContactMechPurpose and FacilityContactMechPurpose records that are not expired but whose associated XxxContactMech records are expired
     */
    @Service(
        name = "countOldUnexpiredContactMechPurposes",
        engine = "java",
        location = "com.ilscipio.scipio.party.PartyServices",
        invoke = "countOldUnexpiredContactMechPurposes",
        description = "Count PartyContactMechPurpose and FacilityContactMechPurpose records that are not expired but whose associated XxxContactMech records are expired",
        auth = "true",
        attributes = {
            @Attribute(name = "partyPurposeCount", type = "Long", mode = "OUT"),
            @Attribute(name = "facilityPurposeCount", type = "Long", mode = "OUT"),
            @Attribute(name = "totalPurposeCount", type = "Long", mode = "OUT")
        }
    )
    public interface CountOldUnexpiredContactMechPurposes {}

    /**
     * Expire PartyContactMechPurpose and FacilityContactMechPurpose records that are not expired but whose associated XxxContactMech records are expired
     */
    @Service(
        name = "expireOldUnexpiredContactMechPurposes",
        engine = "java",
        location = "com.ilscipio.scipio.party.PartyServices",
        invoke = "expireOldUnexpiredContactMechPurposes",
        description = "Expire PartyContactMechPurpose and FacilityContactMechPurpose records that are not expired but whose associated XxxContactMech records are expired",
        auth = "true",
        attributes = {
            @Attribute(name = "previewOnly", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If set to true, print values found to log instead of committing"),
            @Attribute(name = "partyPurposeCount", type = "Long", mode = "OUT"),
            @Attribute(name = "facilityPurposeCount", type = "Long", mode = "OUT"),
            @Attribute(name = "totalPurposeCount", type = "Long", mode = "OUT")
        }
    )
    public interface ExpireOldUnexpiredContactMechPurposes {}

    /**
     * Sends request data to listening websockets on channel. Automatically calls removeHitBinLiveData. Config, defaults in webtools.properties.
     */
    @Service(
        name = "sendHitBinLiveData",
        engine = "java",
        location = "com.ilscipio.scipio.party.web.PartyWebServices",
        invoke = "sendHitBinLiveData",
        description = "Sends request data to listening websockets on channel. Automatically calls removeHitBinLiveData. Config, defaults in webtools.properties.",
        requireNewTransaction = "true",
        transactionTimeout = "7260",
        maxRetry = "0",
        semaphore = "fail",
        log = "quiet",
        logEca = "quiet",
        attributes = {
            @Attribute(name = "serverHostName", type = "String", mode = "IN", optional = "true", description = "By default, gotten from GeneralConfig.getLocalhostAddress().getHostName()"),
            @Attribute(name = "execMode", type = "String", mode = "IN", optional = "true", description = "Supports: \"full\", \"report\" (FIXME: same as \"off\" for now), \"off\". Default in webtools.properties."),
            @Attribute(name = "processAllServers", type = "Boolean", mode = "IN", optional = "true", description = "If true, handle the data for all servers. Default in webtools.properties."),
            @Attribute(name = "reportAllServers", type = "Boolean", mode = "IN", optional = "true", description = "If true, data sent via websocket includes counts for all servers, not just this host. Defaults to processAllServers if set, otherwise webtools.properties."),
            @Attribute(name = "expireAllServers", type = "Boolean", mode = "IN", optional = "true", description = "If true, when clears happens, clears from all servers, not just serverHostName. Defaults to processAllServers if set, otherwise webtools.properties."),
            @Attribute(name = "expireMinutes", type = "Integer", mode = "IN", optional = "true", description = "Default in webtools.properties"),
            @Attribute(name = "channel", type = "String", mode = "IN"),
            @Attribute(name = "bucketMinutes", type = "Integer", mode = "IN", optional = "true", description = "Causes final results to be bucketed every 5, 10, 15, etc. minutes as specified (0-based). Default in webtools.properties."),
            @Attribute(name = "interval", type = "String", mode = "IN", optional = "true", description = "TODO: REVIEW: unused in current form"),
            @Attribute(name = "sendEmpty", type = "Boolean", mode = "IN", optional = "true", description = "Default in webtools.properties")
        }
    )
    public interface SendHitBinLiveData {}

    /**
     * Expires old ServerHitBucketStats whose thruDate are older than given days (thus containing no records for that date). Config, defaults in webtools.properties.             Currently only expires whole buckets (usually not an issue).
     */
    @Service(
        name = "removeHitBinLiveData",
        engine = "java",
        location = "com.ilscipio.scipio.party.web.PartyWebServices",
        invoke = "removeHitBinLiveData",
        description = "Expires old ServerHitBucketStats whose thruDate are older than given days (thus containing no records for that date). Config, defaults in webtools.properties.\n            Currently only expires whole buckets (usually not an issue).",
        requireNewTransaction = "true",
        transactionTimeout = "7200",
        maxRetry = "0",
        attributes = {
            @Attribute(name = "serverHostName", type = "String", mode = "IN", optional = "true", description = "By default, gotten from GeneralConfig.getLocalhostAddress().getHostName()"),
            @Attribute(name = "expireAllServers", type = "Boolean", mode = "IN", optional = "true", description = "If true, clears from all servers, not just serverHostName. Default in webtools.properties."),
            @Attribute(name = "expireMinutes", type = "Integer", mode = "IN", optional = "true", description = "Default configured in webtools.properties"),
            @Attribute(name = "nowTimestamp", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface RemoveHitBinLiveData {}

    /**
     * Removes all ServerHitBucketStats records for a server or all. Currently only expires whole buckets (usually not an issue).
     */
    @Service(
        name = "removeHitBinLiveDataAll",
        engine = "java",
        location = "com.ilscipio.scipio.party.web.PartyWebServices",
        invoke = "removeHitBinLiveDataAll",
        description = "Removes all ServerHitBucketStats records for a server or all. Currently only expires whole buckets (usually not an issue).",
        requireNewTransaction = "true",
        transactionTimeout = "7200",
        maxRetry = "0",
        attributes = {
            @Attribute(name = "serverHostName", type = "String", mode = "IN", optional = "true", description = "By default, gotten from GeneralConfig.getLocalhostAddress().getHostName()"),
            @Attribute(name = "expireAllServers", type = "Boolean", mode = "IN", optional = "true", description = "If true, clears from all servers, not just serverHostName. Default in webtools.properties.")
        }
    )
    public interface RemoveHitBinLiveDataAll {}

    /**
     * Returns the requests saved in ServerHitBucketStats as if they'd been returned by getServerRequests
     */
    @Service(
        name = "getSavedHitBinLiveData",
        engine = "java",
        location = "com.ilscipio.scipio.party.web.PartyWebServices",
        invoke = "getSavedHitBinLiveData",
        description = "Returns the requests saved in ServerHitBucketStats as if they'd been returned by getServerRequests",
        attributes = {
            @Attribute(name = "serverHostName", type = "String", mode = "IN", optional = "true", description = "By default, gotten from GeneralConfig.getLocalhostAddress().getHostName()"),
            @Attribute(name = "allServers", type = "Boolean", mode = "IN", optional = "true", description = "If true, the serverRequests map is populated with all servers, otherwise only requests map is returned. Defaults in webtools.properties."),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "maxRequests", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "requests", type = "Map", mode = "OUT", optional = "true"),
            @Attribute(name = "serverRequests", type = "Map", mode = "OUT", optional = "true", description = "Maps serverHostName to requests maps - only set if allServers true")
        }
    )
    public interface GetSavedHitBinLiveData {}

    /**
     * Fetch the gravatar url from gravatar.com
     */
    @Service(
        name = "getGravatarImage",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "getGravatarImage",
        description = "Fetch the gravatar url from gravatar.com",
        auth = "true",
        attributes = {
            @Attribute(name = "emailAddress", type = "String", mode = "IN"),
            @Attribute(name = "size", type = "Integer", mode = "IN", optional = "true", defaultValue = "50"),
            @Attribute(name = "gravatarImageUrl", type = "String", mode = "OUT")
        }
    )
    public interface GetGravatarImage {}

}
