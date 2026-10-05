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
public class ContactServices {

    /**
     * Create a new ContactMech Type Record
     */
    @Service(
        name = "createContactMechType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new ContactMech Type Record",
        defaultEntityName = "ContactMechType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateContactMechType {}

    /**
     * Update a ContactMech Type Record
     */
    @Service(
        name = "updateContactMechType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ContactMech Type Record",
        defaultEntityName = "ContactMechType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateContactMechType {}

    /**
     * Delete a ContactMech Type Record
     */
    @Service(
        name = "deleteContactMechType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ContactMech Type Record",
        defaultEntityName = "ContactMechType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface DeleteContactMechType {}

    /**
     * Create a new ContactMech Purpose Type Record
     */
    @Service(
        name = "createContactMechPurposeType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new ContactMech Purpose Type Record",
        defaultEntityName = "ContactMechPurposeType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateContactMechPurposeType {}

    /**
     * Update a ContactMech Purpose Type Record
     */
    @Service(
        name = "updateContactMechPurposeType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ContactMech Purpose Type Record",
        defaultEntityName = "ContactMechPurposeType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateContactMechPurposeType {}

    /**
     * Delete a ContactMech Purpose Type Record
     */
    @Service(
        name = "deleteContactMechPurposeType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ContactMech Purpose Type Record",
        defaultEntityName = "ContactMechPurposeType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface DeleteContactMechPurposeType {}

    /**
     * Create a new ContactMech Type Attr Record
     */
    @Service(
        name = "createContactMechTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new ContactMech Type Attr Record",
        defaultEntityName = "ContactMechTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateContactMechTypeAttr {}

    /**
     * Update a ContactMech Type Attr Record
     */
    @Service(
        name = "updateContactMechTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ContactMech Type Attr Record",
        defaultEntityName = "ContactMechTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateContactMechTypeAttr {}

    /**
     * Delete an existing ContactMech Type Attr Record
     */
    @Service(
        name = "deleteContactMechTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an existing ContactMech Type Attr Record",
        defaultEntityName = "ContactMechTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteContactMechTypeAttr {}

    /**
     * Create a new ContactMech Type Purpose Record
     */
    @Service(
        name = "createContactMechTypePurpose",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new ContactMech Type Purpose Record",
        defaultEntityName = "ContactMechTypePurpose",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateContactMechTypePurpose {}

    /**
     * Update a ContactMech Type Purpose Record
     */
    @Service(
        name = "updateContactMechTypePurpose",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ContactMech Type Purpose Record",
        defaultEntityName = "ContactMechTypePurpose",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateContactMechTypePurpose {}

    /**
     * Delete an existing ContactMech Type Purpose Record
     */
    @Service(
        name = "deleteContactMechTypePurpose",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an existing ContactMech Type Purpose Record",
        defaultEntityName = "ContactMechTypePurpose",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteContactMechTypePurpose {}

}
