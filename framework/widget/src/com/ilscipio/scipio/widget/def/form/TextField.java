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
 * Defines a text input field, equivalent to widget-form.xsd text element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface TextField {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Size of the input field in characters.
     */
    int size() default 25;

    /**
     * Maximum length of input allowed.
     * 0 means no limit.
     */
    int maxlength() default 0;

    /**
     * Default value for the field.
     * Supports flexible expressions like "${parameters.value}".
     */
    String defaultValue() default "";

    /**
     * Whether the field is disabled.
     */
    boolean disabled() default false;

    /**
     * Whether the field is read-only.
     */
    boolean readonly() default false;

    /**
     * Placeholder text displayed when field is empty.
     */
    String placeholder() default "";

    /**
     * Input mask pattern.
     * Use 9 for numeric, a for alpha, * for alphanumeric.
     */
    String mask() default "";

    /**
     * Whether browser autocomplete is enabled for this field.
     */
    boolean clientAutocompleteField() default true;

    /**
     * Sub-hyperlink to display next to the field.
     */
    SubHyperlink subHyperlink() default @SubHyperlink(UNSET = true);

    /**
     * Auto-complete configuration.
     */
    AutoComplete autoComplete() default @AutoComplete(UNSET = true);
}
