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
 * Defines a dropdown select field, equivalent to widget-form.xsd drop-down element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface DropDownField {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Whether to allow empty/no selection.
     */
    boolean allowEmpty() default false;

    /**
     * Whether to allow multiple selections.
     */
    boolean allowMulti() default false;

    /**
     * How to handle current value: "first-in-list" or "selected".
     */
    String current() default "first-in-list";

    /**
     * Key to select when there is no current value.
     * SCIPIO: Enhanced to apply mainly to "new" record screens.
     */
    String noCurrentSelectedKey() default "";

    /**
     * Number of visible options (for multi-select).
     */
    int size() default 1;

    /**
     * Description for current selection.
     */
    String currentDescription() default "";

    /**
     * Size of "other" text field for combo box (0 disables).
     */
    int otherFieldSize() default 0;

    /**
     * Size for text truncation display.
     */
    int textSize() default 0;

    /**
     * Options from an entity.
     */
    EntityOptions entityOptions() default @EntityOptions(UNSET = true);

    /**
     * Options from a list in context.
     */
    ListOptions listOptions() default @ListOptions(UNSET = true);

    /**
     * Static options.
     */
    Option[] options() default {};

    /**
     * Autocomplete configuration.
     */
    AutoComplete autoComplete() default @AutoComplete(UNSET = true);

    /**
     * Sub-hyperlink to display next to the field.
     */
    SubHyperlink subHyperlink() default @SubHyperlink(UNSET = true);
}
