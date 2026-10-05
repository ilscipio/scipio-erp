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
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a column within a column-container, equivalent to the column child element in widget-screen.xsd.
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
public @interface Column {

    /**
     * HTML id attribute for the column.
     */
    String id() default "";

    /**
     * CSS style/class for the column.
     */
    String style() default "";

    /**
     * Screens to include in this column.
     */
    IncludeScreen[] includeScreens() default {};

    /**
     * Forms to include in this column.
     */
    IncludeForm[] includeForms() default {};

    /**
     * Menus to include in this column.
     */
    IncludeMenu[] includeMenus() default {};

    /**
     * Labels to render in this column.
     */
    Label[] labels() default {};

    /**
     * Containers to render in this column.
     */
    Container[] containers() default {};

    /**
     * HTML templates to render in this column.
     */
    HtmlTemplate[] htmlTemplates() default {};
}
