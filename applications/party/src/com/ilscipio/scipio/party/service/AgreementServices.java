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
public class AgreementServices {

    @Service(
        name = "createAgreementTermAttribute",
        engine = "entity-auto",
        invoke = "create",
        defaultEntityName = "AgreementTermAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateAgreementTermAttribute {}

    @Service(
        name = "updateAgreementTermAttribute",
        engine = "entity-auto",
        invoke = "update",
        defaultEntityName = "AgreementTermAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAgreementTermAttribute {}

    @Service(
        name = "deleteAgreementTermAttribute",
        engine = "entity-auto",
        invoke = "delete",
        defaultEntityName = "AgreementTermAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteAgreementTermAttribute {}

    /**
     * Create a AgreementItemType record
     */
    @Service(
        name = "createAgreementItemType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a AgreementItemType record",
        defaultEntityName = "AgreementItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateAgreementItemType {}

    /**
     * Update a AgreementItemType record
     */
    @Service(
        name = "updateAgreementItemType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a AgreementItemType record",
        defaultEntityName = "AgreementItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAgreementItemType {}

    /**
     * Delete an AgreementItemType record
     */
    @Service(
        name = "deleteAgreementItemType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an AgreementItemType record",
        defaultEntityName = "AgreementItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteAgreementItemType {}

    /**
     * Create TermType record
     */
    @Service(
        name = "createTermType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create TermType record",
        defaultEntityName = "TermType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateTermType {}

    /**
     * Update TermType record
     */
    @Service(
        name = "updateTermType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update TermType record",
        defaultEntityName = "TermType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateTermType {}

    /**
     * Delete TermType record
     */
    @Service(
        name = "deleteTermType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete TermType record",
        defaultEntityName = "TermType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteTermType {}

    /**
     * Create TermTypeAttr record
     */
    @Service(
        name = "createTermTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create TermTypeAttr record",
        defaultEntityName = "TermTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateTermTypeAttr {}

    /**
     * Update TermTypeAttr record
     */
    @Service(
        name = "updateTermTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update TermTypeAttr record",
        defaultEntityName = "TermTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateTermTypeAttr {}

    /**
     * Delete TermTypeAttr record
     */
    @Service(
        name = "deleteTermTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete TermTypeAttr record",
        defaultEntityName = "TermTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteTermTypeAttr {}

    /**
     * Create Addendum Record
     */
    @Service(
        name = "createAddendum",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Addendum Record",
        defaultEntityName = "Addendum",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateAddendum {}

    /**
     * Update Addendum Record
     */
    @Service(
        name = "updateAddendum",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Addendum Record",
        defaultEntityName = "Addendum",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAddendum {}

    /**
     * Delete Addendum record
     */
    @Service(
        name = "deleteAddendum",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Addendum record",
        defaultEntityName = "Addendum",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteAddendum {}

    /**
     * Create AgreementContentType Record
     */
    @Service(
        name = "createAgreementContentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create AgreementContentType Record",
        defaultEntityName = "AgreementContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateAgreementContentType {}

    /**
     * Update AgreementContentType Record
     */
    @Service(
        name = "updateAgreementContentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update AgreementContentType Record",
        defaultEntityName = "AgreementContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateAgreementContentType {}

    /**
     * Delete AgreementContentType record
     */
    @Service(
        name = "deleteAgreementContentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete AgreementContentType record",
        defaultEntityName = "AgreementContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteAgreementContentType {}

}
