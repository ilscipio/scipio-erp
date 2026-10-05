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
 * Name filtering for allow-view-save-default, analogous to name-filter element in site-conf.xsd.
 *
 * <p>Used to filter based on name patterns (prefix, suffix, or regexp).</p>
 *
 * <p>SCIPIO: 4.0.0: Added for controller annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface NameFilter {

    /**
     * The field to filter on (e.g., "view-name").
     */
    String field();

    /**
     * Use filter if the name starts with this string.
     */
    String prefix() default "";

    /**
     * Use filter if the name ends with this string.
     */
    String suffix() default "";

    /**
     * Use filter if the name matches this regexp (from beginning).
     */
    String regexp() default "";

    /**
     * If the name matches this filter, sets this value.
     */
    boolean useValue();
}
