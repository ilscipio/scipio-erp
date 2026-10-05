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
 * Defines a second-level container widget for use inside ContainerInScreenlet.
 *
 * <p>This is a leaf-level container type (no further nesting). Used inside
 * {@link ContainerInScreenlet} to provide two levels of container nesting
 * inside screenlets.</p>
 *
 * <p>SCIPIO: 4.0.0: Added to support containers inside screenlets without cyclic references.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
public @interface ContainerInScreenlet2 {

    /**
     * The container ID.
     */
    String id() default "";

    /**
     * CSS style/class for the container.
     */
    String style() default "";

    /**
     * Include-screens directly in this container.
     */
    IncludeScreen[] includeScreens() default {};

    /**
     * Include-forms directly in this container.
     */
    IncludeForm[] includeForms() default {};

    /**
     * Generic ordered widgets (link, image, content, sub-content, horizontal-separator, ...).
     *
     * <p>SCIPIO: 4.0.0: Added; these widget types were silently dropped by the converter
     * because the container annotations had no member able to hold them.</p>
     */
    Widget[] widgets() default {};

    /**
     * Sections at the end of the chain, where no SectionNested level is left.
     *
     * <p>SCIPIO: 4.0.0: Added; such a section was dropped or flattened, losing its condition
     * and merging its actions with its siblings'.</p>
     */
    SectionLeaf[] sections() default {};


    /**
     * Labels.
     *
     * <p>SCIPIO: 4.0.0: Added; label children were silently dropped by the converter.</p>
     */
    Label[] labels() default {};

    /**
     * Included menus.
     *
     * <p>SCIPIO: 4.0.0: Added; include-menu children were silently dropped by the converter.</p>
     */
    IncludeMenu[] includeMenus() default {};



    /**
     * HTML templates directly in this container.
     */
    HtmlTemplate[] htmlTemplates() default {};

    /** SCIPIO: 4.0.0: decorator-section-include children. */
    DecoratorSectionInclude[] decoratorSectionIncludes() default {};

    /** SCIPIO: 4.0.0: The index of this container among its siblings, or -1 to keep the declaration order. */
    int position() default -1;

    // NOTE: No nested containers - this is the leaf level.
}
