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
 * Defines fail-widgets that render when a section's condition evaluates to false.
 *
 * <p>This is critical for decorator sections that have conditional rendering
 * with fallback content.</p>
 *
 * <p>Example XML equivalent:</p>
 * <pre>{@code
 * <section>
 *     <condition>
 *         <if-empty-section section-name="left-column"/>
 *     </condition>
 *     <widgets>
 *         <decorator-section-include name="left-column"/>
 *     </widgets>
 *     <fail-widgets>
 *         <include-screen name="DefMainSideBarMenu"/>
 *     </fail-widgets>
 * </section>
 * }</pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
public @interface FailWidgets {

    /**
     * Screens to include in the fail-widgets block.
     */
    IncludeScreen[] includeScreens() default {};

    /**
     * Forms to include in the fail-widgets block.
     */
    IncludeForm[] includeForms() default {};

    /**
     * Menus to include in the fail-widgets block.
     */
    IncludeMenu[] includeMenus() default {};

    /**
     * Labels to render in the fail-widgets block.
     */
    Label[] labels() default {};

    /**
     * Screenlets to render in the fail-widgets block.
     */
    Screenlet[] screenlets() default {};

    /**
     * Containers to render in the fail-widgets block.
     */
    Container[] containers() default {};

    /**
     * HTML templates to render in the fail-widgets block.
     */
    HtmlTemplate[] htmlTemplates() default {};

    /**
     * Decorator section includes in the fail-widgets block.
     */
    DecoratorSectionInclude[] decoratorSectionIncludes() default {};

    /**
     * Images to render in the fail-widgets block.
     */
    Image[] images() default {};

    /**
     * Horizontal separators to render in the fail-widgets block.
     */
    HorizontalSeparator[] horizontalSeparators() default {};

    /**
     * Content widgets to render in the fail-widgets block.
     */
    Content[] contents() default {};
}
