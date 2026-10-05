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
 * Imports all fields from a member entity as aliases in a view-entity.
 *
 * <p>Corresponds to alias-all element in entitymodel.xsd.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface AliasAll {

    /**
     * Entity alias to import fields from; required.
     */
    String entityAlias();

    /**
     * Prefix to add to all field names; optional.
     */
    String prefix() default "";

    /**
     * If true, include in GROUP BY clause; default false.
     */
    boolean groupBy() default false;

    /**
     * Aggregate function to apply to all fields; optional.
     */
    AggregateFunction function() default AggregateFunction.NONE;

    /**
     * Field set for grouping fields together; optional.
     */
    String fieldSet() default "";

    /**
     * If set to "false", exclude from default SELECT; optional.
     */
    String select() default "";

    /**
     * Fields to exclude from the alias-all; optional.
     */
    String[] excludes() default {};

    /**
     * Description; optional.
     */
    String description() default "";
}
