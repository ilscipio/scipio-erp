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
package com.ilscipio.scipio.widget.def.form;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Defines entity-based options for dropdown/radio/check fields, equivalent to widget-form.xsd entity-options element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface EntityOptions {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Entity name to get options from; required when UNSET is false.
     */
    String entityName() default "";

    /**
     * Field name to use as the key/value.
     * Defaults to the field's entry-name.
     */
    String keyFieldName() default "";

    /**
     * Description template using ${} syntax.
     */
    String description() default "${description}";

    /**
     * Whether to cache the entity lookup.
     */
    boolean cache() default true;

    /**
     * Filter by date: "true", "false", or "by-name".
     */
    String filterByDate() default "by-name";

    /**
     * Entity constraints to filter options.
     */
    EntityConstraint[] constraints() default {};

    /**
     * Order-by fields for the options.
     */
    EntityOrderBy[] orderBy() default {};
}
