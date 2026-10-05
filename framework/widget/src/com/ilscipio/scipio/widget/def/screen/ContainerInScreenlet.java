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
 * Defines a container widget for use inside ScreenletNested.
 *
 * <p>This is a special container type that can be nested inside ScreenletNested
 * without creating cyclic type references. It supports one level of nested
 * containers via {@link ContainerInScreenlet2}.</p>
 *
 * <p>SCIPIO: 4.0.0: Added to support containers inside screenlets without cyclic references.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
public @interface ContainerInScreenlet {

    /**
     * The container ID.
     */
    String id() default "";

    /**
     * CSS style/class for the container.
     */
    String style() default "";

    /**
     * Nested containers (second level).
     */
    ContainerInScreenlet2[] containers() default {};

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

    /**
     * SCIPIO: 4.0.0: Slot of this child among all children of its widgets block, counting every typed
     * array. -1 (the default) keeps the declaration order of its own array. Set it when a child of
     * another type must render between two children of this type; every widgets block honours it.
     */
    int position() default -1;
}
