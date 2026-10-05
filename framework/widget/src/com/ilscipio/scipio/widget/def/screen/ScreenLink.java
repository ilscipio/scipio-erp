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
package com.ilscipio.scipio.widget.def.screen;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a link widget for screens, equivalent to widget-screen.xsd link element.
 *
 * <p>Renders a hyperlink with optional parameters and image.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}ScreenLink(text = "Click Here", target = "ViewProduct", urlMode = "intra-app")
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
@Repeatable(ScreenLinkList.class)
public @interface ScreenLink {

    /**
     * The link text.
     */
    String text() default "";

    /**
     * HTML id attribute.
     */
    String id() default "";

    /**
     * CSS style/class.
     */
    String style() default "";

    /**
     * Link name attribute.
     */
    String name() default "";

    /**
     * Link title attribute.
     */
    String title() default "";

    /**
     * Text size limit.
     */
    int size() default -1;

    /**
     * Link target (href).
     */
    String target() default "";

    /**
     * Target window for the link.
     */
    String targetWindow() default "";

    /**
     * URL prefix.
     */
    String prefix() default "";

    /**
     * Link width.
     */
    String width() default "";

    /**
     * Link height.
     */
    String height() default "";

    /**
     * Link type: auto, anchor, hidden-form, ajax-window, layered-modal.
     */
    String linkType() default "auto";

    /**
     * URL mode: intra-app, inter-app, content, plain.
     */
    String urlMode() default "intra-app";

    /**
     * Whether to use full path.
     */
    String fullPath() default "";

    /**
     * Whether to use secure (HTTPS).
     */
    String secure() default "";

    /**
     * Whether to URL-encode.
     */
    String encode() default "";

    /**
     * Whether to show confirmation dialog.
     */
    boolean requestConfirmation() default false;

    /**
     * Confirmation message text.
     */
    String confirmationMessage() default "";

    /**
     * Condition for when to display the link.
     */
    String useWhen() default "";

    /**
     * Parameters to include with the link.
     */
    LinkParameter[] parameters() default {};
}
