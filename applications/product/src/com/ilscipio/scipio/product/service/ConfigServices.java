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
package com.ilscipio.scipio.product.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ConfigServices {

    /**
     * Create a new ConfigOptionProductOption Record
     */
    @Service(
        name = "createConfigOptionProductOption",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new ConfigOptionProductOption Record",
        defaultEntityName = "ConfigOptionProductOption",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateConfigOptionProductOption {}

    /**
     * Update a ConfigOptionProductOption record
     */
    @Service(
        name = "updateConfigOptionProductOption",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ConfigOptionProductOption record",
        defaultEntityName = "ConfigOptionProductOption",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateConfigOptionProductOption {}

    /**
     * Delete an existing ConfigOptionProductOption Record
     */
    @Service(
        name = "deleteConfigOptionProductOption",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an existing ConfigOptionProductOption Record",
        defaultEntityName = "ConfigOptionProductOption",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteConfigOptionProductOption {}

    /**
     * Create a ProdConfItemContentType
     */
    @Service(
        name = "createProdConfItemContentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProdConfItemContentType",
        defaultEntityName = "ProdConfItemContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateProdConfItemContentType {}

    /**
     * Update a ProdConfItemContentType
     */
    @Service(
        name = "updateProdConfItemContentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProdConfItemContentType",
        defaultEntityName = "ProdConfItemContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface UpdateProdConfItemContentType {}

    /**
     * Delete a ProdConfItemContentType
     */
    @Service(
        name = "deleteProdConfItemContentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProdConfItemContentType",
        defaultEntityName = "ProdConfItemContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProdConfItemContentType {}

}
