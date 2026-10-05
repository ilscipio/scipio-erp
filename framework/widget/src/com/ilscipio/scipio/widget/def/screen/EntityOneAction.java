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
package com.ilscipio.scipio.widget.def.screen;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines an entity-one action for screen widgets.
 *
 * <p>Retrieves a single entity value based on primary key fields.</p>
 *
 * <p>Example XML equivalent:</p>
 * <pre>{@code
 * <entity-one entity-name="Party" value-field="party" auto-field-map="true"/>
 * }</pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Repeatable(EntityOneActionList.class)
public @interface EntityOneAction {

    /**
     * The entity name to query.
     */
    String entityName();

    /**
     * The field name to store the result value.
     */
    String valueField() default "";  // SCIPIO: 4.0.0: Default to empty, derives from entityName

    /**
     * Whether to automatically map fields from parameters/context to primary key fields.
     * Default is true.
     */
    boolean autoFieldMap() default true;

    /**
     * Whether to use the entity cache.
     * Default is false.
     */
    boolean useCache() default false;

    /**
     * Field mappings for the primary key lookup.
     */
    FieldMap[] fieldMaps() default {};
}
