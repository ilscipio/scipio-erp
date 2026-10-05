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
 * Defines an image element for hyperlinks, equivalent to widget-common.xsd image element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface ImageElement {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Image source URL; required when UNSET is false.
     */
    String src() default "";

    /**
     * Element ID.
     */
    String id() default "";

    /**
     * CSS style class.
     */
    String style() default "";

    /**
     * Image width.
     */
    String width() default "";

    /**
     * Image height.
     */
    String height() default "";

    /**
     * Image border.
     */
    String border() default "";

    /**
     * Alt text for accessibility.
     */
    String alt() default "";

    /**
     * Title/tooltip text.
     */
    String title() default "";

    /**
     * URL mode for the image source.
     */
    String urlMode() default "content";
}
