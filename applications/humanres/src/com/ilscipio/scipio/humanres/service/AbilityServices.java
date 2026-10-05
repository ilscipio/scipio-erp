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
package com.ilscipio.scipio.humanres.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class AbilityServices {

    /**
     * Create a PerfRatingType record
     */
    @Service(
        name = "createPerfRatingType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PerfRatingType record",
        defaultEntityName = "PerfRatingType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePerfRatingType {}

    /**
     * Update a PerfRatingType record
     */
    @Service(
        name = "updatePerfRatingType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PerfRatingType record",
        defaultEntityName = "PerfRatingType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePerfRatingType {}

    /**
     * Delete a PerfRatingType record
     */
    @Service(
        name = "deletePerfRatingType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PerfRatingType record",
        defaultEntityName = "PerfRatingType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePerfRatingType {}

    /**
     * Create a PerfReviewItemType record
     */
    @Service(
        name = "createPerfReviewItemType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PerfReviewItemType record",
        defaultEntityName = "PerfReviewItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePerfReviewItemType {}

    /**
     * Update a PerfReviewItemType record
     */
    @Service(
        name = "updatePerfReviewItemType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PerfReviewItemType record",
        defaultEntityName = "PerfReviewItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePerfReviewItemType {}

    /**
     * Delete a PerfReviewItemType record
     */
    @Service(
        name = "deletePerfReviewItemType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PerfReviewItemType record",
        defaultEntityName = "PerfReviewItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePerfReviewItemType {}

}
