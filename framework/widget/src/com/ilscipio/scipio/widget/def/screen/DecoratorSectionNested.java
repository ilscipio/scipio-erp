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
 * A decorator-section of a {@link DecoratorScreenNested}.
 *
 * <p>Its content uses {@link WidgetsForContainer4}, the leaf widget holder, which is what keeps
 * the annotation types acyclic. Content deeper than that leaf level cannot be expressed here.</p>
 *
 * <p>SCIPIO: 4.0.0: Added; a decorator-screen inside a nested section was silently dropped.</p>
 *
 * @see DecoratorScreenNested
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({}) // Used as nested annotation only
public @interface DecoratorSectionNested {

    /**
     * Decorator section name; required.
     */
    String name();

    /**
     * Renders the section only when this expression is true (Scipio extension).
     */
    String useWhen() default "";

    /**
     * Falls back to the auto-included section when this one is empty (Scipio extension).
     */
    boolean fallbackAutoInclude() default false;

    /**
     * Lets an auto-included section override this one (Scipio extension).
     */
    boolean overrideByAutoInclude() default false;

    /**
     * Content type hint (Scipio extension).
     */
    String contains() default "";

    /**
     * The widgets of this decorator section.
     */
    WidgetsForContainer4 widgets() default @WidgetsForContainer4;
}
