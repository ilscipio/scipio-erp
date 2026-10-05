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
package com.ilscipio.scipio.service.def.seca;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a Scipio service ECA (field) set, equivalent to services-eca.xsd ECA set element.
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface SecaSet {

    String fieldName();

    String envName() default "";

    String value() default "";

    /**
     * Special formatting operation.
     *
     * <ul>Values:
     * <li><code>append</code></li>
     * <li><code>to-upper</code></li>
     * <li><code>to-lower</code></li>
     * <li><code>hash-code</code></li>
     * <li><code>long</code></li>
     * <li><code>double</code></li>
     * <li><code>upper-first-char</code></li>
     * <li><code>lower-first-char</code></li>
     * <li><code>db-to-java</code></li>
     * <li><code>java-to-db</code></li>
     * </ul>
     */
    String format() default "";

}
