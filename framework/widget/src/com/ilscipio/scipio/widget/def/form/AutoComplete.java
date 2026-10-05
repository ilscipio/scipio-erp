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
 * Defines autocomplete configuration for dropdown fields, equivalent to widget-form.xsd auto-complete element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface AutoComplete {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Whether to auto-select first match.
     */
    boolean autoSelect() default false;

    /**
     * Delay between keystrokes before search (in seconds).
     */
    double frequency() default 0.4;

    /**
     * Minimum characters before autocomplete activates.
     */
    int minChars() default 1;

    /**
     * Maximum number of choices to display.
     */
    int choices() default 10;

    /**
     * Whether to search for partial matches within words.
     */
    boolean partialSearch() default true;

    /**
     * Minimum characters for partial search.
     */
    int partialChars() default 1;

    /**
     * Whether to ignore case when matching.
     */
    boolean ignoreCase() default true;

    /**
     * Whether to search entire text for matches.
     */
    boolean fullSearch() default true;
}
