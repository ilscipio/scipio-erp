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
package com.ilscipio.scipio.common.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class MethodServices {

    /**
     * Create a Custom Method
     */
    @Service(
        name = "createCustomMethod",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Custom Method",
        defaultEntityName = "CustomMethod",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCustomMethod {}

    /**
     * Update a Custom Method
     */
    @Service(
        name = "updateCustomMethod",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Custom Method",
        defaultEntityName = "CustomMethod",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCustomMethod {}

    /**
     * Delete a Custom Method
     */
    @Service(
        name = "deleteCustomMethod",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Custom Method",
        defaultEntityName = "CustomMethod",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCustomMethod {}

    /**
     * Create a Custom Method Type
     */
    @Service(
        name = "createCustomMethodType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Custom Method Type",
        defaultEntityName = "CustomMethodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCustomMethodType {}

    /**
     * Update a Custom Method Type
     */
    @Service(
        name = "updateCustomMethodType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Custom Method Type",
        defaultEntityName = "CustomMethodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCustomMethodType {}

    /**
     * Delete a Custom Method Type
     */
    @Service(
        name = "deleteCustomMethodType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Custom Method Type",
        defaultEntityName = "CustomMethodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCustomMethodType {}

}
