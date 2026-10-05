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

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ViewServices {

    /**
     * General Party Find Service, Used in the findparty page in the Party Manager, etc
     */
    @Service(
        name = "findParty",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "findParty",
        description = "General Party Find Service, Used in the findparty page in the Party Manager, etc",
        attributes = {
            @Attribute(name = "extInfo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "VIEW_INDEX", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "VIEW_SIZE", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "lookupFlag", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "showAll", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userLoginId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "groupName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "firstName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "lastName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "address1", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "address2", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "city", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "stateProvinceGeoId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "postalCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "infoString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "countryCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "areaCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "serialNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "softIdentifier", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyRelationshipTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "ownerPartyIds", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "sortField", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypes", type = "List", mode = "OUT"),
            @Attribute(name = "partyTypes", type = "List", mode = "OUT"),
            @Attribute(name = "currentRole", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "currentPartyType", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "currentStateGeo", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "viewIndex", type = "Integer", mode = "OUT"),
            @Attribute(name = "viewSize", type = "Integer", mode = "OUT"),
            @Attribute(name = "partyList", type = "List", mode = "OUT"),
            @Attribute(name = "partyListSize", type = "Integer", mode = "OUT"),
            @Attribute(name = "paramList", type = "String", mode = "OUT"),
            @Attribute(name = "highIndex", type = "Integer", mode = "OUT"),
            @Attribute(name = "lowIndex", type = "Integer", mode = "OUT"),
            @Attribute(name = "sortField", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface FindParty {}

    /**
     * General Party Find Service, duplicated for screen widget purpose, Used in the new findparty page in the Party Manager, etc
     */
    @Service(
        name = "performFindParty",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "performFindParty",
        description = "General Party Find Service, duplicated for screen widget purpose, Used in the new findparty page in the Party Manager, etc",
        attributes = {
            @Attribute(name = "roleTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "noConditionFind", type = "String", mode = "IN", optional = "true", defaultValue = "N"),
            @Attribute(name = "extInfo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "extCond", type = "org.ofbiz.entity.condition.EntityCondition", mode = "IN", optional = "true", description = "EntityCondition that can be send to this service to manage complex search case"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userLoginId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "externalId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "groupName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "firstName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "lastName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "address1", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "address2", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "city", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "stateProvinceGeoId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "postalCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "infoString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "countryCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "areaCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "idValue", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyIdentificationTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "serialNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "softIdentifier", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyRelationshipTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "ownerPartyIds", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "sortField", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "listIt", type = "org.ofbiz.entity.util.EntityListIterator", mode = "OUT", optional = "true")
        }
    )
    public interface PerformFindParty {}

    /**
     * Get Contact Mechs associated with party. It produces a list of Maps (valueMaps) which can contain a list of contactMechPurposes, a partyContactMech Map, contactMechtype Map and a contactMech Map
     */
    @Service(
        name = "getPartyContactMechValueMaps",
        engine = "java",
        location = "org.ofbiz.party.contact.ContactMechServices",
        invoke = "getPartyContactMechValueMaps",
        description = "Get Contact Mechs associated with party. It produces a list of Maps (valueMaps) which can contain a list of contactMechPurposes, a partyContactMech Map, contactMechtype Map and a contactMech Map",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "showOld", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "valueMaps", type = "java.util.List", mode = "OUT", optional = "true")
        }
    )
    public interface GetPartyContactMechValueMaps {}

    /**
     * Gets a person entity from the cache/database
     */
    @Service(
        name = "getPerson",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "getPerson",
        description = "Gets a person entity from the cache/database",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "lookupPerson", type = "org.ofbiz.entity.GenericValue", mode = "OUT")
        }
    )
    public interface GetPerson {}

    /**
     * Gets a collection of parties from an email address
     */
    @Service(
        name = "getPartyFromEmail",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "getPartyFromEmail",
        description = "Gets a collection of parties from an email address",
        attributes = {
            @Attribute(name = "email", type = "String", mode = "IN"),
            @Attribute(name = "parties", type = "java.util.Collection", mode = "OUT", optional = "true")
        }
    )
    public interface GetPartyFromEmail {}

    /**
     * Gets a collection of parties from an email address
     */
    @Service(
        name = "getPartyFromUserLogin",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "getPartyFromUserLogin",
        description = "Gets a collection of parties from an email address",
        attributes = {
            @Attribute(name = "userLoginId", type = "String", mode = "IN"),
            @Attribute(name = "parties", type = "java.util.Collection", mode = "OUT", optional = "true")
        }
    )
    public interface GetPartyFromUserLogin {}

    /**
     * Gets a collection of parties from a first/last name
     */
    @Service(
        name = "getPartyFromName",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "getPartyFromPerson",
        description = "Gets a collection of parties from a first/last name",
        attributes = {
            @Attribute(name = "firstName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "lastName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "parties", type = "java.util.Collection", mode = "OUT", optional = "true")
        }
    )
    public interface GetPartyFromName {}

    /**
     * Gets a collection of parties from a party group namee
     */
    @Service(
        name = "getPartyFromGroupName",
        engine = "java",
        location = "org.ofbiz.party.party.PartyServices",
        invoke = "getPartyFromPartyGroup",
        description = "Gets a collection of parties from a party group namee",
        attributes = {
            @Attribute(name = "groupName", type = "String", mode = "IN"),
            @Attribute(name = "parties", type = "java.util.Collection", mode = "OUT", optional = "true")
        }
    )
    public interface GetPartyFromGroupName {}

    /**
     * Gets all parties related to partyIdFrom through the PartyRelationship entity
     */
    @Service(
        name = "getPartiesByRelationship",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "getPartiesByRelationship",
        description = "Gets all parties related to partyIdFrom through the PartyRelationship entity",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyRelationship", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "parties", type = "java.util.Collection", mode = "OUT", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "partyIdFrom", optional = "false")
        }
    )
    public interface GetPartiesByRelationship {}

    /**
     *              Gets Parent Organizations for an Organization Party.             This uses the PartyRelationship table with partyRelationshipTypeId="GROUP_ROLLUP".             The Parent Organization will be in the relationship on either side with roleTypeId="PARENT_ORGANIZATION".             The Child Organization will be in the relationship on either side with roleTypeId="ORGANIZATION_UNIT", or any child of that type.             The getParentsOfParents attribute defaults to Y.             The parentOrganizationPartyIdList coming out will contain the original organizationPartyId.         
     */
    @Service(
        name = "getParentOrganizations",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "getParentOrganizations",
        description = "\n            Gets Parent Organizations for an Organization Party.\n            This uses the PartyRelationship table with partyRelationshipTypeId=\"GROUP_ROLLUP\".\n            The Parent Organization will be in the relationship on either side with roleTypeId=\"PARENT_ORGANIZATION\".\n            The Child Organization will be in the relationship on either side with roleTypeId=\"ORGANIZATION_UNIT\", or any child of that type.\n            The getParentsOfParents attribute defaults to Y.\n            The parentOrganizationPartyIdList coming out will contain the original organizationPartyId.\n        ",
        attributes = {
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN"),
            @Attribute(name = "getParentsOfParents", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "parentOrganizationPartyIdList", type = "List", mode = "OUT")
        }
    )
    public interface GetParentOrganizations {}

    /**
     *              Get Child RoleTypes.             The childRoleTypeIdList coming out will contain the original roleTypeId.         
     */
    @Service(
        name = "getChildRoleTypes",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "getChildRoleTypes",
        description = "\n            Get Child RoleTypes.\n            The childRoleTypeIdList coming out will contain the original roleTypeId.\n        ",
        attributes = {
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "childRoleTypeIdList", type = "List", mode = "OUT")
        }
    )
    public interface GetChildRoleTypes {}

    /**
     * Get all Postal Address Boundaries
     */
    @Service(
        name = "getPostalAddressBoundary",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "getPostalAddressBoundary",
        description = "Get all Postal Address Boundaries",
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "IN"),
            @Attribute(name = "geos", type = "java.util.List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "VIEW")
    )
    public interface GetPostalAddressBoundary {}

    /**
     *              Get Parties Related to a Party             - The relatedPartyIdList coming out will include the original partyIdFrom             - The includeFromToSwitched and recurse attributes should by "Y" or "N" and default to N.             - The useCache attribute should be "true" or "false", defaults to "false"         
     */
    @Service(
        name = "getRelatedParties",
        engine = "simple",
        location = "component://party/script/org/ofbiz/party/party/PartyServices.xml",
        invoke = "getRelatedParties",
        description = "\n            Get Parties Related to a Party\n            - The relatedPartyIdList coming out will include the original partyIdFrom\n            - The includeFromToSwitched and recurse attributes should by \"Y\" or \"N\" and default to N.\n            - The useCache attribute should be \"true\" or \"false\", defaults to \"false\"\n        ",
        attributes = {
            @Attribute(name = "partyIdFrom", type = "String", mode = "IN"),
            @Attribute(name = "partyRelationshipTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeIdFrom", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeIdFromInclueAllChildTypes", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeIdToIncludeAllChildTypes", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "includeFromToSwitched", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "recurse", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "useCache", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "relatedPartyIdList", type = "List", mode = "OUT")
        }
    )
    public interface GetRelatedParties {}

}
