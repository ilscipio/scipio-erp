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
 * A decorator-screen inside a nested section (a {@link WidgetsForContainer} chain member).
 *
 * <p>{@link DecoratorScreen} cannot be used there: its sections reach back to the container
 * chain and make the annotation types cyclic. This variant stops the chain by holding
 * {@link DecoratorSectionNested} sections instead.</p>
 *
 * <p>SCIPIO: 4.0.0: Added; a decorator-screen inside a nested section was silently dropped.</p>
 *
 * @see DecoratorScreen
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({}) // Used as nested annotation only
public @interface DecoratorScreenNested {

    /**
     * Decorator screen name; empty means no decorator.
     */
    String name();

    /**
     * Decorator screen location; optional.
     */
    String location() default "";

    /**
     * Fallback decorator screen name (Scipio extension).
     */
    String fallbackName() default "";

    /**
     * Fallback decorator screen location (Scipio extension).
     */
    String fallbackLocation() default "";

    /**
     * Uses the fallback when the decorator is empty (Scipio extension).
     */
    boolean fallbackIfEmpty() default false;

    /**
     * Automatically includes matching decorator sections (Scipio extension).
     */
    boolean autoDecoratorSectionInclude() default false;

    /**
     * The decorator sections to define.
     */
    DecoratorSectionNested[] sections() default {};

    /**
     * SCIPIO: 4.0.0: The index of this decorator among its siblings, or -1 to keep the declaration order.
     */
    int position() default -1;
}
