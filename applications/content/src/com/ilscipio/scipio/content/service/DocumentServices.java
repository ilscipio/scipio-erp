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
package com.ilscipio.scipio.content.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class DocumentServices {

    /**
     * Create a DocumentAttribute record
     */
    @Service(
        name = "createDocumentAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a DocumentAttribute record",
        defaultEntityName = "DocumentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateDocumentAttribute {}

    /**
     * Update a DocumentAttribute record
     */
    @Service(
        name = "updateDocumentAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a DocumentAttribute record",
        defaultEntityName = "DocumentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateDocumentAttribute {}

    /**
     * Delete a DocumentAttribute record
     */
    @Service(
        name = "deleteDocumentAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a DocumentAttribute record",
        defaultEntityName = "DocumentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteDocumentAttribute {}

    /**
     * Create a Document record
     */
    @Service(
        name = "createDocument",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Document record",
        defaultEntityName = "Document",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateDocument {}

    /**
     * Update a Document record
     */
    @Service(
        name = "updateDocument",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Document record",
        defaultEntityName = "Document",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateDocument {}

    /**
     * Delete a Document record
     */
    @Service(
        name = "deleteDocument",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Document record",
        defaultEntityName = "Document",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteDocument {}

    /**
     * Create a DocumentType record
     */
    @Service(
        name = "createDocumentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a DocumentType record",
        defaultEntityName = "DocumentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateDocumentType {}

    /**
     * Update a DocumentType record
     */
    @Service(
        name = "updateDocumentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a DocumentType record",
        defaultEntityName = "DocumentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateDocumentType {}

    /**
     * Delete a DocumentType record
     */
    @Service(
        name = "deleteDocumentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a DocumentType record",
        defaultEntityName = "DocumentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteDocumentType {}

    /**
     * Create a DocumentTypeAttr record
     */
    @Service(
        name = "createDocumentTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a DocumentTypeAttr record",
        defaultEntityName = "DocumentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateDocumentTypeAttr {}

    /**
     * Update a DocumentTypeAttr record
     */
    @Service(
        name = "updateDocumentTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a DocumentTypeAttr record",
        defaultEntityName = "DocumentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateDocumentTypeAttr {}

    /**
     * Delete a DocumentTypeAttr record
     */
    @Service(
        name = "deleteDocumentTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a DocumentTypeAttr record",
        defaultEntityName = "DocumentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteDocumentTypeAttr {}

}
