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
 * Defines a hyperlink field, equivalent to widget-form.xsd hyperlink element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface HyperlinkField {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Target URL or request URI.
     */
    String target() default "";

    /**
     * URL mode for the target.
     */
    UrlMode urlMode() default UrlMode.INTRA_APP;

    /**
     * Description/text to display for the link.
     * Supports flexible expressions.
     */
    String description() default "";

    /**
     * Whether to also render a hidden field.
     */
    boolean alsoHidden() default true;

    /**
     * CSS style class.
     */
    String style() default "";

    /**
     * Target window for the link.
     */
    String targetWindow() default "";

    /**
     * Whether to URL-encode the link.
     */
    boolean encode() default true;

    /**
     * Whether to use full path for the URL.
     */
    boolean fullPath() default false;

    /**
     * Whether to use HTTPS.
     */
    boolean secure() default false;

    /**
     * Parameters to pass to the target.
     */
    ParameterDef[] parameters() default {};

    /**
     * Auto-parameters from a service.
     */
    AutoParametersService autoParametersService() default @AutoParametersService(UNSET = true);

    /**
     * Auto-parameters from an entity.
     */
    AutoParametersEntity autoParametersEntity() default @AutoParametersEntity(UNSET = true);

    /**
     * Image to display for the link.
     */
    ImageElement image() default @ImageElement(UNSET = true);

    /**
     * CSS class for the link.
     */
    String linkStyle() default "";

    /**
     * Type of link: "auto", "anchor", "hidden-form".
     */
    String linkType() default "";

    /**
     * Confirmation message (legacy, use requestConfirmation + confirmationMessage).
     */
    String confirmation() default "";

    /**
     * Whether to show a confirmation dialog.
     */
    String requestConfirmation() default "";

    /**
     * Confirmation message to display.
     */
    String confirmationMessage() default "";

    /**
     * Condition for when to use this hyperlink.
     */
    String useWhen() default "";
}
