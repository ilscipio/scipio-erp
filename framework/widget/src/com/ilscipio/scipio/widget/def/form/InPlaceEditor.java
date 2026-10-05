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
 * Defines in-place editing configuration, equivalent to widget-form.xsd in-place-editor element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface InPlaceEditor {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * URL to submit edits to; required when UNSET is false.
     */
    String url() default "";

    /**
     * Cancel control type: "link", "button", or "false".
     */
    String cancelControl() default "";

    /**
     * Text for cancel control.
     */
    String cancelText() default "";

    /**
     * Text to show to indicate the field is editable.
     */
    String clickToEditText() default "";

    /**
     * Field post creation action: "activate", "focus", or "false".
     */
    String fieldPostCreation() default "";

    /**
     * CSS class for the form.
     */
    String formClassName() default "";

    /**
     * ID for the form.
     */
    String formId() default "";

    /**
     * Highlight color during edit.
     */
    String highlightColor() default "";

    /**
     * End color for highlight transition.
     */
    String highlightEndColor() default "";

    /**
     * CSS class for hover state.
     */
    String hoverClassName() default "";

    /**
     * Whether the response is HTML.
     */
    boolean htmlResponse() default false;

    /**
     * CSS class during loading.
     */
    String loadingClassName() default "";

    /**
     * Text during loading.
     */
    String loadingText() default "";

    /**
     * OK control type: "link", "button", or "false".
     */
    String okControl() default "";

    /**
     * Text for OK control.
     */
    String okText() default "";

    /**
     * Parameter name for the value.
     */
    String paramName() default "";

    /**
     * CSS class during saving.
     */
    String savingClassName() default "";

    /**
     * Text during saving.
     */
    String savingText() default "";

    /**
     * Whether to submit when field loses focus.
     */
    boolean submitOnBlur() default false;

    /**
     * Text to show after controls.
     */
    String textAfterControls() default "";

    /**
     * Text to show before controls.
     */
    String textBeforeControls() default "";

    /**
     * Text to show between controls.
     */
    String textBetweenControls() default "";

    /**
     * Whether to update display after request.
     */
    boolean updateAfterRequestCall() default true;

    /**
     * Simple editor configuration.
     */
    SimpleEditor simpleEditor() default @SimpleEditor(UNSET = true);

    /**
     * Field mappings for the request.
     */
    FieldMap[] fieldMaps() default {};
}
