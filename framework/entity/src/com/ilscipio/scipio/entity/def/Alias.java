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

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines an alias (field) in a view-entity.
 *
 * <p>Corresponds to alias element in entitymodel.xsd.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Repeatable(AliasList.class)
public @interface Alias {

    /**
     * Alias name (exposed field name); required.
     */
    String name();

    /**
     * Entity alias this field comes from; optional if using complex-alias.
     */
    String entityAlias() default "";

    /**
     * Field name in the source entity; optional, defaults to name.
     */
    String field() default "";

    /**
     * Column alias in SQL; optional.
     */
    String colAlias() default "";

    /**
     * If "true", this is a primary key field; optional.
     */
    String primKey() default "";

    /**
     * If true, include in GROUP BY clause; default false.
     */
    boolean groupBy() default false;

    /**
     * Aggregate function to apply; optional.
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
     * Complex alias definition for computed fields; optional.
     */
    ComplexAlias complexAlias() default @ComplexAlias(operator = "");

    /**
     * Description; optional.
     */
    String description() default "";
}
