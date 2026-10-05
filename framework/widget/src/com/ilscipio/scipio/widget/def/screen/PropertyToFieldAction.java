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
 * Defines a property-to-field action for screen widgets.
 *
 * <p>Reads a property from a properties file and sets it to a context field.</p>
 *
 * <p>Example XML equivalent:</p>
 * <pre>{@code
 * <property-to-field field="defaultCountryGeoId" resource="general" property="country.geo.id.default" default="USA"/>
 * }</pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Repeatable(PropertyToFieldActionList.class)
public @interface PropertyToFieldAction {

    /**
     * The context field to set.
     */
    String field();

    /**
     * The properties resource name (e.g., "general").
     */
    String resource();

    /**
     * The property key to read.
     */
    String property();

    /**
     * Default value if property is not found.
     */
    String defaultValue() default "";

    /**
     * Whether to not override if field already exists.
     */
    boolean noLocale() default false;

    /**
     * The argument field for property value substitution.
     */
    String argListName() default "";

    /**
     * Whether to use global scope.
     */
    boolean global() default false;
}
