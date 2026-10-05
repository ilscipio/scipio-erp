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
package com.ilscipio.scipio.entity.def;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a key mapping for an entity relationship.
 *
 * <p>Used within {@link Relation} to specify how fields are mapped between entities.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * // Field names are the same in both entities
 * {@literal @}KeyMap(fieldName = "productTypeId")
 *
 * // Field names are different
 * {@literal @}KeyMap(fieldName = "createdByUserLogin", relFieldName = "partyId")
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface KeyMap {

    /**
     * Field name in the current entity; required.
     */
    String fieldName();

    /**
     * Field name in the related entity; optional.
     *
     * <p>If not specified, defaults to the same as fieldName.</p>
     */
    String relFieldName() default "";
}
