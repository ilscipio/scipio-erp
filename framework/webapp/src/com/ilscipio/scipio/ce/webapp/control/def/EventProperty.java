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
package com.ilscipio.scipio.ce.webapp.control.def;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a name-value property for an event, analogous to property element in site-conf.xsd event.
 *
 * <p>Properties can be used by the event handler depending on support by its type.
 * Currently these are load-time properties (not runtime).</p>
 *
 * <p>Supported event properties by type:</p>
 * <ul>
 * <li>service: mode-parameter = Name of a request attribute/parameter that overrides default service invocation mode</li>
 * </ul>
 *
 * <p>SCIPIO: 4.0.0: Added for controller annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface EventProperty {

    /**
     * Property name.
     */
    String name();

    /**
     * Property value.
     */
    String value();

    /**
     * Property type. Default: "String".
     * Values: PlainString, String, BigDecimal, Double, Float, Long, Integer, Date, Time, Timestamp, Boolean, Object
     */
    String type() default "String";
}
