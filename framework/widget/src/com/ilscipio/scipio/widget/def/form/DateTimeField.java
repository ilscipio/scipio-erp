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
 * Defines a date-time input field, equivalent to widget-form.xsd date-time element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface DateTimeField {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Type of date-time field: "timestamp", "date", or "time".
     */
    String type() default "timestamp";

    /**
     * Default value for the field.
     * Supports flexible expressions.
     */
    String defaultValue() default "";

    /**
     * Input method: "text" or "time-dropdown".
     */
    String inputMethod() default "text";

    /**
     * Clock format: "12" or "24".
     */
    String clock() default "24";

    /**
     * Step interval for time selection: "1", "5", "10", "15", or "30".
     */
    String step() default "1";

    /**
     * Whether to use input mask: "Y" or "N".
     */
    String mask() default "N";

    /**
     * Placeholder text displayed when field is empty.
     */
    String placeholder() default "";
}
