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
 * Defines a text search field with find options, equivalent to widget-form.xsd text-find element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface TextFindField {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Size of the input field in characters.
     */
    int size() default 25;

    /**
     * Maximum length of input.
     * 0 means no limit.
     */
    int maxlength() default 0;

    /**
     * Default value for the field.
     */
    String defaultValue() default "";

    /**
     * Whether to ignore case in search.
     */
    boolean ignoreCase() default true;

    /**
     * Default search option: "equals", "like", "contains", "empty", "notEqual",
     * "lessThan", "lessThanEqualTo", "greaterThan", "greaterThanEqualTo".
     */
    String defaultOption() default "contains";

    /**
     * Whether browser autocomplete is enabled.
     */
    boolean clientAutocompleteField() default true;

    /**
     * Which options to hide: "true", "false", "ignore-case", "options".
     */
    String hideOptions() default "false";

    /**
     * Sub-hyperlink to display next to the field.
     */
    SubHyperlink subHyperlink() default @SubHyperlink(UNSET = true);
}
