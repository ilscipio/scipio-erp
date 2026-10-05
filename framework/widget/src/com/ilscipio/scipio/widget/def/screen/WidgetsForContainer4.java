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
 * Defines the widget content for a container or section at nesting depth 4 (terminal).
 *
 * <p>Contains arrays of different widget types that can appear inside
 * a container. This is the maximum nesting depth, so it uses Container4
 * for containers but has no sections method (no deeper nesting).</p>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
public @interface WidgetsForContainer4 {

    Widget[] value() default {};

    /**
     * Sections at the end of the chain, where no SectionNested level is left.
     *
     * <p>SCIPIO: 4.0.0: Added; such a section was dropped or flattened, losing its condition
     * and merging its actions with its siblings'.</p>
     */
    SectionLeaf[] sections() default {};



    IncludeScreen[] includeScreens() default {};

    IncludeForm[] includeForms() default {};

    IncludeMenu[] includeMenus() default {};

    Label[] labels() default {};

    HtmlTemplate[] htmlTemplates() default {};

    ScreenletNested[] screenlets() default {};

    Container4[] containers() default {};
}
