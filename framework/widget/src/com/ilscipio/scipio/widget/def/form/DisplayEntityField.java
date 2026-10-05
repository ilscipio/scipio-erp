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
 * Defines a display field that looks up description from an entity,
 * equivalent to widget-form.xsd display-entity element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface DisplayEntityField {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Entity name to look up; required when UNSET is false.
     */
    String entityName() default "";

    /**
     * Key field name in the entity.
     * Defaults to the field's entry-name.
     */
    String keyFieldName() default "";

    /**
     * Description template using ${} syntax.
     */
    String description() default "${description}";

    /**
     * Size limit for display (truncates if exceeded).
     * 0 means no limit.
     */
    int size() default 0;

    /**
     * Whether to cache the entity lookup.
     */
    boolean cache() default true;

    /**
     * Whether to also render a hidden field.
     * Note: uses the key value, not the description.
     */
    boolean alsoHidden() default true;

    /**
     * Sub-hyperlink to display next to the field.
     */
    SubHyperlink subHyperlink() default @SubHyperlink(UNSET = true);

    /**
     * In-place editor configuration.
     */
    InPlaceEditor inPlaceEditor() default @InPlaceEditor(UNSET = true);
}
