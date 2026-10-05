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
 * Defines a decorator-screen widget, equivalent to widget-screen.xsd decorator-screen element.
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}Screen(name = "MyScreen")
 * {@literal @}DecoratorScreen(name = "main-decorator", location = "${parameters.mainDecoratorLocation}")
 * public interface MyScreenDef {}
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface DecoratorScreen {

    /**
     * Decorator screen name; required.
     *
     * <p>Can be empty string to indicate no decorator (used as default).</p>
     */
    String name();

    /**
     * Decorator screen location; optional.
     *
     * <p>Supports flexible expressions like "${parameters.mainDecoratorLocation}".
     * If not specified, looks in current screen file.</p>
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
     * Whether to use fallback if decorator is empty (Scipio extension).
     */
    boolean fallbackIfEmpty() default false;

    /**
     * Automatically include matching decorator sections (Scipio extension).
     */
    boolean autoDecoratorSectionInclude() default false;

    /**
     * Decorator sections to define.
     */
    DecoratorSection[] sections() default {};
}
