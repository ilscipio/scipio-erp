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

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * A container inside a {@link SectionLeaf}; holds content but no further container,
 * screenlet or section.
 *
 * <p>That restriction is what keeps {@link SectionLeaf} acyclic, and so lets a section
 * appear where the Container/Screenlet chains have run out of levels.</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 *
 * @see SectionLeaf
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({}) // Used as nested annotation only
public @interface ContainerLeaf {

    String style() default "";

    String id() default "";

    String type() default "";

    String contains() default "";

    String autoUpdateTargetId() default "";

    int autoUpdateInterval() default 0;

    Widget[] widgets() default {};

    IncludeForm[] includeForms() default {};

    IncludeScreen[] includeScreens() default {};

    IncludeMenu[] includeMenus() default {};

    Label[] labels() default {};

    HtmlTemplate[] htmlTemplates() default {};

    DecoratorSectionInclude[] decoratorSectionIncludes() default {};

    /**
     * SCIPIO: 4.0.0: Slot of this child among all children of its widgets block, counting every typed
     * array. -1 (the default) keeps the declaration order of its own array. Set it when a child of
     * another type must render between two children of this type; every widgets block honours it.
     */
    int position() default -1;
}
