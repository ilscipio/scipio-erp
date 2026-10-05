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
 * Defines an inline section for use within decorator-sections.
 *
 * <p>This annotation allows conditional rendering inside decorator-sections without
 * creating a cyclic type reference. It uses {@link InlineWidgets} instead of
 * {@link Widgets} to exclude {@link DecoratorScreen}.</p>
 *
 * <p>Common use case - conditional pass-through pattern:</p>
 * <pre>{@code
 * <decorator-section name="body">
 *   <section>
 *     <condition>
 *       <not><if-empty-section section-name="body"/></not>
 *     </condition>
 *     <widgets>
 *       <decorator-section-include name="body"/>
 *     </widgets>
 *     <fail-widgets>
 *       <screenlet title="Default Content">...</screenlet>
 *     </fail-widgets>
 *   </section>
 * </decorator-section>
 * }</pre>
 *
 * <p>Equivalent annotation:</p>
 * <pre>
 * {@literal @}DecoratorSection(
 *     name = "body",
 *     sections = @InlineSection(
 *         condition = @Condition(not = true, ifEmptySection = @IfEmptySection(sectionName = "body")),
 *         widgets = @InlineWidgets(decoratorSectionIncludes = @DecoratorSectionInclude(name = "body")),
 *         failWidgets = @InlineWidgets(screenlets = @Screenlet(title = "Default Content", ...))
 *     )
 * )
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for nested section support in decorator-sections.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
@Repeatable(InlineSectionList.class)
public @interface InlineSection {

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
     * If the condition is false, failWidgets render instead.
     */
    Condition condition() default @Condition;

    /**
     * Actions to execute for this section.
     */
    Actions actions() default @Actions;

    /**
     * Widgets to render when condition is true (or no condition).
     * Uses {@link InlineWidgets} to avoid cyclic type references.
     */
    InlineWidgets widgets() default @InlineWidgets;

    /**
     * Widgets to render when condition is false.
     * Uses {@link InlineWidgets} to avoid cyclic type references.
     */
    InlineWidgets failWidgets() default @InlineWidgets;

    /**
     * SCIPIO: 4.0.0: The index of this section among its siblings, or -1 to keep the declaration order.
     */
    int position() default -1;

    // NOTE: Nested InlineSection[] sections() removed because Java annotations
    // cannot have self-referential types. For complex multi-level conditionals,
    // use separate screens or nested include-screen widgets.
}
