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
 * Defines a redirect parameter for a response, analogous to redirect-parameter element in site-conf.xsd.
 *
 * <p>Adds a parameter with the given name to the redirect. Value is found in request attribute
 * if exists, or request parameter if no attribute is found.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for controller annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface RedirectParameter {

    /**
     * Name of the parameter to redirect.
     */
    String name();

    /**
     * If specified, used instead of the value of name for the key to find
     * a request attribute or parameter.
     */
    String from() default "";

    /**
     * Set a string value directly for the parameter.
     */
    String value() default "";

    /**
     * If set to "exclude", the parameter is excluded instead of included.
     */
    String mode() default "include";
}
