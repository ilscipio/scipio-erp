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
 * Defines a second-level nested container widget for screens.
 *
 * <p>This annotation is used for containers nested inside {@link Container}.
 * It is identical to Container but can nest {@link Container3} instead of Container2
 * to avoid cyclic type references.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for preserving container nesting hierarchy.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
@Repeatable(Container2List.class)
public @interface Container2 {

    /**
     * CSS style class for the container.
     */
    String style() default "";

    /**
     * ID attribute for the container element.
     */
    String id() default "";

    /**
     * Auto update target area name.
     */
    String autoUpdateTargetId() default "";

    /**
     * Auto update interval in seconds.
     */
    int autoUpdateInterval() default 0;

    /**
     * Container type (div, span, etc.).
     */
    String type() default "";

    /**
     * Scipio targeting expression for conditional rendering.
     */
    String contains() default "";

    /**
     * Forms to include inside the container.
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
     * Screens to include inside the container.
     */
    IncludeScreen[] includeScreens() default {};

    /**
     * Labels to include inside the container.
     */
    Label[] labels() default {};

    /**
     * HTML templates to include inside the container.
     */
    HtmlTemplate[] htmlTemplates() default {};

    /**
     * Menus to include inside the container.
     */
    IncludeMenu[] includeMenus() default {};

    /**
     * Nested containers (level 3) inside this container.
     * Use Container3 for third-level nesting to avoid cyclic type references.
     *
     * <p>SCIPIO: 4.0.0: Added to preserve container nesting hierarchy.</p>
     */
    Container3[] containers() default {};

    /**
     * Screenlets nested inside this container.
     * Uses ScreenletNested to avoid cyclic type references.
     *
     * <p>SCIPIO: 4.0.0: Added to support screenlets inside containers.</p>
     */
    ScreenletNested[] screenlets() default {};

    SectionNested2[] sections() default {};

    /**
     * Decorator section includes nested directly in this container.
     *
     * <p>SCIPIO: 4.0.0: Required for XML screens that put decorator-section-include inside a container.</p>
     */
    DecoratorSectionInclude[] decoratorSectionIncludes() default {};

    /**
     * SCIPIO: 4.0.0: Slot of this child among all children of its widgets block, counting every typed
     * array. -1 (the default) keeps the declaration order of its own array. Set it when a child of
     * another type must render between two children of this type; every widgets block honours it.
     */
    int position() default -1;
}
