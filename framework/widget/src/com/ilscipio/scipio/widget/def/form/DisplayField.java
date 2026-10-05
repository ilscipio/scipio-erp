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
 * Defines a display-only field, equivalent to widget-form.xsd display element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface DisplayField {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Whether to also render a hidden field with the value.
     */
    boolean alsoHidden() default true;

    /**
     * Description/text to display.
     * Supports flexible expressions like "${fieldValue}".
     */
    String description() default "";

    /**
     * Size limit for display (truncates if exceeded).
     * 0 means no limit.
     */
    int size() default 0;

    /**
     * Display type: "text", "currency", "date", "date-time", "image", "accounting-number".
     */
    String type() default "text";

    /**
     * Currency UOM ID for type="currency".
     */
    String currency() default "";

    /**
     * Image location for type="image".
     */
    String imageLocation() default "";

    /**
     * Default value when field is empty.
     */
    String defaultValue() default "";

    /**
     * Date format pattern for type="date" or type="date-time".
     */
    String dateFormat() default "";

    /**
     * In-place editor configuration.
     */
    InPlaceEditor inPlaceEditor() default @InPlaceEditor(UNSET = true);
}
