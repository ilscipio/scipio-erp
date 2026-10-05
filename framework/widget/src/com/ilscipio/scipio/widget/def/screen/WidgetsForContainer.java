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
 * Defines the widget content for a container or section at nesting depth 1.
 *
 * <p>Contains arrays of different widget types that can appear inside
 * a container. Uses depth 2 types (Container2, SectionNested2) for nested content.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
public @interface WidgetsForContainer {

    Widget[] value() default {};

    /**
     * Decorator screen wrapping this nested section's widgets.
     *
     * <p>SCIPIO: 4.0.0: Added; a decorator-screen inside a nested section was silently dropped.</p>
     */
    DecoratorScreenNested decorator() default @DecoratorScreenNested(name = "");



    IncludeScreen[] includeScreens() default {};

    IncludeForm[] includeForms() default {};

    IncludeMenu[] includeMenus() default {};

    Label[] labels() default {};

    HtmlTemplate[] htmlTemplates() default {};

    ScreenletNested[] screenlets() default {};

    Container2[] containers() default {};

    SectionNested2[] sections() default {};
}
