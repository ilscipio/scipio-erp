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
 * Defines a submit button field, equivalent to widget-form.xsd submit element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface SubmitField {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Button type: "button", "text-link", or "image".
     */
    String buttonType() default "button";

    /**
     * Image location when buttonType is "image".
     */
    String imageLocation() default "";

    /**
     * Whether to show a confirmation dialog before submit.
     */
    boolean requestConfirmation() default false;

    /**
     * Confirmation message to display.
     */
    String confirmationMessage() default "";

    /**
     * Confirmation message (legacy).
     */
    String confirmation() default "";

    /**
     * AJAX submit refresh target.
     */
    String backgroundSubmitRefreshTarget() default "";

    /**
     * Image to display for the button.
     */
    ImageElement image() default @ImageElement(UNSET = true);
}
