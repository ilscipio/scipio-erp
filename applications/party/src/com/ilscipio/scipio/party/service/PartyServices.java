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
public class PartyServices {

    /**
     * Create a PartyClassificationType Record
     */
    @Service(
        name = "createPartyClassificationType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PartyClassificationType Record",
        defaultEntityName = "PartyClassificationType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePartyClassificationType {}

    /**
     * Update a PartyClassificationType Record
     */
    @Service(
        name = "updatePartyClassificationType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PartyClassificationType Record",
        defaultEntityName = "PartyClassificationType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePartyClassificationType {}

    /**
     * Delete a PartyClassificationType Record
     */
    @Service(
        name = "deletePartyClassificationType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PartyClassificationType Record",
        defaultEntityName = "PartyClassificationType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePartyClassificationType {}

    /**
     * Create a PartyContentType Record
     */
    @Service(
        name = "createPartyContentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PartyContentType Record",
        defaultEntityName = "PartyContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePartyContentType {}

    /**
     * Update a PartyContentType Record
     */
    @Service(
        name = "updatePartyContentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PartyContentType Record",
        defaultEntityName = "PartyContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePartyContentType {}

    /**
     * Delete a PartyContentType Record
     */
    @Service(
        name = "deletePartyContentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PartyContentType Record",
        defaultEntityName = "PartyContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePartyContentType {}

    /**
     * Create a PartyGeoPoint Record
     */
    @Service(
        name = "createPartyGeoPoint",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PartyGeoPoint Record",
        defaultEntityName = "PartyGeoPoint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePartyGeoPoint {}

    /**
     * Update a PartyGeoPoint Record
     */
    @Service(
        name = "updatePartyGeoPoint",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PartyGeoPoint Record",
        defaultEntityName = "PartyGeoPoint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePartyGeoPoint {}

    /**
     * Expire a PartyGeoPoint Record
     */
    @Service(
        name = "expirePartyGeoPoint",
        engine = "entity-auto",
        invoke = "expire",
        description = "Expire a PartyGeoPoint Record",
        defaultEntityName = "PartyGeoPoint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface ExpirePartyGeoPoint {}

    /**
     * Create a PartyIcsAvsOverride Record
     */
    @Service(
        name = "createPartyIcsAvsOverride",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PartyIcsAvsOverride Record",
        defaultEntityName = "PartyIcsAvsOverride",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePartyIcsAvsOverride {}

    /**
     * Update a PartyIcsAvsOverride Record
     */
    @Service(
        name = "updatePartyIcsAvsOverride",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PartyIcsAvsOverride Record",
        defaultEntityName = "PartyIcsAvsOverride",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePartyIcsAvsOverride {}

    /**
     * Delete a PartyIcsAvsOverride Record
     */
    @Service(
        name = "deletePartyIcsAvsOverride",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PartyIcsAvsOverride Record",
        defaultEntityName = "PartyIcsAvsOverride",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePartyIcsAvsOverride {}

    /**
     * Create a PartyIdentificationType Record
     */
    @Service(
        name = "createPartyIdentificationType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PartyIdentificationType Record",
        defaultEntityName = "PartyIdentificationType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePartyIdentificationType {}

    /**
     * Update a PartyIdentificationType Record
     */
    @Service(
        name = "updatePartyIdentificationType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PartyIdentificationType Record",
        defaultEntityName = "PartyIdentificationType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePartyIdentificationType {}

    /**
     * Delete a PartyIdentificationType Record
     */
    @Service(
        name = "deletePartyIdentificationType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PartyIdentificationType Record",
        defaultEntityName = "PartyIdentificationType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePartyIdentificationType {}

    /**
     * Create a PartyType Record
     */
    @Service(
        name = "createPartyType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PartyType Record",
        defaultEntityName = "PartyType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePartyType {}

    /**
     * Update a PartyType Record
     */
    @Service(
        name = "updatePartyType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PartyType Record",
        defaultEntityName = "PartyType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePartyType {}

    /**
     * Delete a PartyType Record
     */
    @Service(
        name = "deletePartyType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PartyType Record",
        defaultEntityName = "PartyType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePartyType {}

    /**
     * Create a PartyTypeAttr Record
     */
    @Service(
        name = "createPartyTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PartyTypeAttr Record",
        defaultEntityName = "PartyTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePartyTypeAttr {}

    /**
     * Update a PartyTypeAttr Record
     */
    @Service(
        name = "updatePartyTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PartyTypeAttr Record",
        defaultEntityName = "PartyTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePartyTypeAttr {}

    /**
     * Delete a PartyTypeAttr Record
     */
    @Service(
        name = "deletePartyTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PartyTypeAttr Record",
        defaultEntityName = "PartyTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePartyTypeAttr {}

    /**
     * Create a PriorityType Record
     */
    @Service(
        name = "createPriorityType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PriorityType Record",
        defaultEntityName = "PriorityType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePriorityType {}

    /**
     * Update a PriorityType Record
     */
    @Service(
        name = "updatePriorityType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PriorityType Record",
        defaultEntityName = "PriorityType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePriorityType {}

    /**
     * Delete a PriorityType Record
     */
    @Service(
        name = "deletePriorityType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PriorityType Record",
        defaultEntityName = "PriorityType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePriorityType {}

    /**
     * Checks the party contactMechType. Meant to be used in *ecas
     */
    @Service(
        name = "checkEmailPartyContactMech",
        engine = "java",
        location = "com.ilscipio.scipio.party.PartyServices",
        invoke = "checkEmailPartyContactMech",
        description = "Checks the party contactMechType. Meant to be used in *ecas",
        attributes = {
            @Attribute(name = "serviceContext", type = "Map", mode = "IN"),
            @Attribute(name = "conditionReply", type = "Boolean", mode = "OUT")
        }
    )
    public interface CheckEmailPartyContactMech {}

}
