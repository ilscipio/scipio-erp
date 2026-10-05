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
package com.ilscipio.scipio.widget.def.menu;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Defines a link in a menu item, equivalent to widget-menu.xsd link element.
 *
 * <p>SCIPIO: 4.0.0: Added for menu annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface MenuLink {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Link text/description.
     */
    String text() default "";

    /**
     * HTML id attribute.
     */
    String id() default "";

    /**
     * CSS style class.
     */
    String style() default "";

    /**
     * Link name attribute.
     */
    String name() default "";

    /**
     * Link title attribute (for tooltip).
     */
    String title() default "";

    /**
     * Text size limit.
     */
    int size() default 0;

    /**
     * Target URL/request.
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
    LinkType linkType() default LinkType.AUTO;

    /**
     * URL mode: intra-app, inter-app, content, plain.
     */
    UrlMode urlMode() default UrlMode.INTRA_APP;

    /**
     * Whether to use full path.
     */
    String fullPath() default "";

    /**
     * Whether to use secure (HTTPS).
     */
    String secure() default "";

    /**
     * Whether to encode the URL.
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
     * SCIPIO: Condition for when to use this link.
     */
    String useWhen() default "";

    /**
     * Link parameters.
     */
    MenuParameter[] parameters() default {};

    /**
     * Auto parameters from service.
     */
    AutoParametersService autoParametersService() default @AutoParametersService(UNSET = true);

    /**
     * Auto parameters from entity.
     */
    AutoParametersEntity autoParametersEntity() default @AutoParametersEntity(UNSET = true);

    /**
     * Image in link.
     */
    MenuImage image() default @MenuImage(UNSET = true);
}
