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
 * Defines parameter-to-attribute mapping for an event, analogous to param-to-attr element in site-conf.xsd.
 *
 * <p>Transfers request parameters to request attributes before the event.
 * The source parameters are by default from the parameters map (UtilHttp.getParameterMap)
 * and support application/json body parameters.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for controller annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface ParamToAttr {

    /**
     * Comma-separated list of parameter names.
     */
    String name();

    /**
     * Comma-separated list of target attribute names. If multiple, should correspond to list of names.
     */
    String toName() default "";

    /**
     * Whether to override existing (non-null) request attributes. Required.
     */
    boolean override();

    /**
     * Whether to set attribute if parameter value is null. Default: true.
     */
    boolean setIfNull() default true;

    /**
     * Whether to set attribute if parameter value is empty. Default: true.
     */
    boolean setIfEmpty() default true;
}
