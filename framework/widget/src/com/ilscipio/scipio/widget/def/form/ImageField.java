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
 * Defines an image display field, equivalent to widget-form.xsd image element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface ImageField {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * CSS style class.
     */
    String style() default "";

    /**
     * Image value/path.
     */
    String value() default "";

    /**
     * Default value when field is empty.
     */
    String defaultValue() default "";

    /**
     * Description/tooltip text.
     */
    String description() default "";

    /**
     * Alt text for accessibility.
     */
    String alternate() default "";

    /**
     * Sub-hyperlink to wrap the image.
     */
    SubHyperlink subHyperlink() default @SubHyperlink(UNSET = true);

    /**
     * Border width in pixels.
     */
    String border() default "";

    /**
     * Image width (e.g., "100", "50%").
     */
    String width() default "";

    /**
     * Image height (e.g., "100", "50%").
     */
    String height() default "";
}
