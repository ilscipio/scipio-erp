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
 * Defines a conditional section for screens.
 *
 * <p>A section can have conditions that determine whether to render
 * its widgets or fail-widgets.</p>
 *
 * <p>Example XML equivalent:</p>
 * <pre>{@code
 * <section>
 *     <condition>
 *         <if-empty field="parties"/>
 *     </condition>
 *     <widgets>
 *         <include-form name="NewForm" location="..."/>
 *     </widgets>
 *     <fail-widgets>
 *         <include-screen name="ExistingView"/>
 *     </fail-widgets>
 * </section>
 * }</pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
@Repeatable(SectionList.class)
public @interface Section {

    /**
     * Section name (optional, for debugging/identification).
     */
    String name() default "";

    /**
     * Whether to share the scope with the parent. Default false.
     */
    boolean shareScope() default false;

    /**
     * Scipio targeting expression for selective rendering optimization.
     * Uses syntax like $, #, ~, %, ^ for matching types.
     */
    String contains() default "";

    /**
     * CSS ID for the section.
     */
    String id() default "";

    /**
     * CSS style/class for the section.
     */
    String style() default "";

    /**
     * The condition that must be true for widgets to render.
     * If the condition is false, fail-widgets render instead.
     */
    Condition condition() default @Condition;

    /**
     * Widgets to render when condition is true (or no condition).
     */
    Widgets widgets() default @Widgets;

    /**
     * Widgets to render when condition is false.
     */
    Widgets failWidgets() default @Widgets;

    /**
     * Fail-widgets structure for rendering when condition is false.
     * Alternative to failWidgets() - use FailWidgets annotation for more control.
     */
    FailWidgets failWidgetsBlock() default @FailWidgets;

    /**
     * Actions to execute for this section.
     */
    Actions actions() default @Actions;

    /**
     * Actions to execute if an exception occurs during section processing.
     * The exception will be available in the 'scpException' field.
     */
    Actions catchActions() default @Actions;

    /**
     * Actions that always execute at the end of section processing,
     * regardless of success or failure.
     */
    Actions finallyActions() default @Actions;
}
