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
 * Defines a lookup field with popup form, equivalent to widget-form.xsd lookup element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface LookupField {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Name of the lookup form; required when UNSET is false.
     */
    String targetFormName() default "";

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
     * Field to populate with description from lookup.
     */
    String descriptionFieldName() default "";

    /**
     * Parameters to pass to the lookup form (comma-separated).
     */
    String targetParameter() default "";

    /**
     * Whether browser autocomplete is enabled.
     */
    boolean clientAutocompleteField() default true;

    /**
     * Whether the input is read-only.
     */
    boolean readOnly() default false;

    /**
     * Presentation mode: "layer", "window", or "none".
     */
    String presentation() default "layer";

    /**
     * Layer height (e.g., "250px", "12%").
     */
    String height() default "";

    /**
     * Layer width (e.g., "250px", "12%").
     */
    String width() default "";

    /**
     * Layer position: "center", "left", "right", "topleft", "topright", "topcenter".
     */
    String position() default "topleft";

    /**
     * Whether to fade background when layer is shown.
     */
    boolean fadeBackground() default true;

    /**
     * Whether the search screenlet is initially collapsed.
     */
    boolean initiallyCollapsed() default false;

    /**
     * Whether to show description tooltip.
     */
    boolean showDescription() default false;

    /**
     * Sub-hyperlink to display next to the field.
     */
    SubHyperlink subHyperlink() default @SubHyperlink(UNSET = true);

    /**
     * Field to use for initial value.
     */
    String initialValueField() default "";
}
