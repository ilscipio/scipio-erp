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
 * Defines a set action for screen widgets, equivalent to widget-common.xsd set element.
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}SetAction(field = "titleProperty", value = "PageTitle")
 * {@literal @}SetAction(field = "parameters.orderId", fromField = "orderId", global = true)
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Repeatable(SetActionList.class)
public @interface SetAction {

    /**
     * Field name to set; required.
     */
    String field();

    /**
     * Literal value to set; optional.
     *
     * <p>Supports flexible expressions like "${parameters.orderId}".</p>
     */
    String value() default "";

    /**
     * Field to copy value from; optional.
     */
    String fromField() default "";

    /**
     * Default value if source is empty; optional.
     */
    String defaultValue() default "";

    /**
     * Type to convert value to; optional.
     *
     * <p>Common types: String, Integer, Long, Double, BigDecimal, Timestamp, Date, Boolean, List, Map</p>
     */
    String type() default "";

    /**
     * Whether to set in global context; optional.
     */
    boolean global() default false;

    /**
     * Whether to set only if field is empty; optional.
     */
    boolean setIfEmpty() default true;

    /**
     * Whether to set only if field is null; optional.
     */
    boolean setIfNull() default true;

    /**
     * Scope to get the from-field value from; optional.
     *
     * <p>Valid values: "user", "application", empty (default context).</p>
     */
    String fromScope() default "";
}
