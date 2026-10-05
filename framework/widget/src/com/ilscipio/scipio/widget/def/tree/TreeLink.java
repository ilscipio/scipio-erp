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
package com.ilscipio.scipio.widget.def.tree;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

import com.ilscipio.scipio.widget.def.menu.UrlMode;

/**
 * Defines link for tree nodes, equivalent to widget-tree.xsd link element.
 *
 * <p>SCIPIO: 4.0.0: Added for tree annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface TreeLink {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Link text.
     */
    String text() default "";

    /**
     * HTML id attribute.
     */
    String id() default "";

    /**
     * CSS class.
     */
    String style() default "";

    /**
     * Link name.
     */
    String name() default "";

    /**
     * Link title/tooltip.
     */
    String title() default "";

    /**
     * Link target URL.
     */
    String target() default "";

    /**
     * Target window.
     */
    String targetWindow() default "";

    /**
     * URL prefix.
     */
    String prefix() default "";

    /**
     * Link type: auto, anchor, hidden-form.
     */
    String linkType() default "auto";

    /**
     * URL mode: intra-app, inter-app, content, plain.
     */
    UrlMode urlMode() default UrlMode.INTRA_APP;

    /**
     * Whether to use full path.
     */
    String fullPath() default "";

    /**
     * Whether to use secure connection.
     */
    String secure() default "";

    /**
     * Whether to encode URL.
     */
    String encode() default "";

    /**
     * Whether to request confirmation.
     */
    boolean requestConfirmation() default false;

    /**
     * Confirmation message.
     */
    String confirmationMessage() default "";

    /**
     * Use-when condition.
     */
    String useWhen() default "";

    /**
     * Link parameters.
     */
    TreeParameter[] parameters() default {};

    /**
     * Image for the link.
     */
    TreeImage image() default @TreeImage(UNSET = true);
}
