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
 * Defines a widgets container for inline sections.
 *
 * <p>This annotation is similar to {@link Widgets} but excludes {@link DecoratorScreen}
 * to break the cyclic type reference that would otherwise occur with nested sections
 * inside decorator-sections.</p>
 *
 * <p>Widgets can be specified in two ways:</p>
 *
 * <h3>Recommended: Unified Widget Array (preserves order)</h3>
 * <pre>
 * {@literal @}InlineWidgets(value = {
 *     {@literal @}Widget(type = WidgetType.INCLUDE_SCREEN, name = "Header"),
 *     {@literal @}Widget(type = WidgetType.LABEL, text = "Content"),
 *     {@literal @}Widget(type = WidgetType.INCLUDE_FORM, name = "MyForm", location = "...")
 * })
 * </pre>
 *
 * <h3>Legacy: Type-specific Arrays (order NOT guaranteed)</h3>
 * <pre>
 * {@literal @}InlineWidgets(
 *     includeScreens = {{@literal @}IncludeScreen(...)},
 *     labels = {{@literal @}Label(...)}
 * )
 * </pre>
 *
 * <p><strong>Important:</strong> When using the unified {@link #value()} array, widgets are
 * processed in array order, which is critical for proper rendering sequence.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for nested section support in decorator-sections.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
public @interface InlineWidgets {

    // ========== Unified Widget Array (RECOMMENDED) ==========

    /**
     * Unified widget array - widgets are rendered in array order.
     *
     * <p>This is the recommended way to specify widgets as it preserves rendering order.
     * Use this instead of the type-specific arrays below.</p>
     */
    Widget[] value() default {};

    // ========== Legacy Type-specific Arrays (for backward compatibility) ==========
    // NOTE: These do NOT guarantee rendering order.

    /**
     * Screens to include.
     * @deprecated Use {@link #value()} with {@link WidgetType#INCLUDE_SCREEN} for order-preserved widgets.
     */
    @Deprecated
    IncludeScreen[] includeScreens() default {};

    /**
     * Forms to include.
     * @deprecated Use {@link #value()} with {@link WidgetType#INCLUDE_FORM} for order-preserved widgets.
     */
    @Deprecated
    IncludeForm[] includeForms() default {};

    /**
     * Menus to include.
     * @deprecated Use {@link #value()} with {@link WidgetType#INCLUDE_MENU} for order-preserved widgets.
     */
    @Deprecated
    IncludeMenu[] includeMenus() default {};

    /**
     * Labels to render.
     * @deprecated Use {@link #value()} with {@link WidgetType#LABEL} for order-preserved widgets.
     */
    @Deprecated
    Label[] labels() default {};

    /**
     * Screenlets to render.
     * @deprecated Use {@link #value()} with {@link WidgetType#SCREENLET} for order-preserved widgets.
     */
    @Deprecated
    Screenlet[] screenlets() default {};

    /**
     * Containers to render.
     * @deprecated Use {@link #value()} with {@link WidgetType#CONTAINER} for order-preserved widgets.
     */
    @Deprecated
    Container[] containers() default {};

    /**
     * HTML templates to render.
     * @deprecated Use {@link #value()} with {@link WidgetType#HTML_TEMPLATE} for order-preserved widgets.
     */
    @Deprecated
    HtmlTemplate[] htmlTemplates() default {};

    /**
     * Images to render.
     * @deprecated Use {@link #value()} with {@link WidgetType#IMAGE} for order-preserved widgets.
     */
    @Deprecated
    Image[] images() default {};

    /**
     * Horizontal separators to render.
     * @deprecated Use {@link #value()} with {@link WidgetType#HORIZONTAL_SEPARATOR} for order-preserved widgets.
     */
    @Deprecated
    HorizontalSeparator[] horizontalSeparators() default {};

    /**
     * Content widgets to render.
     * @deprecated Use {@link #value()} with {@link WidgetType#CONTENT} for order-preserved widgets.
     */
    @Deprecated
    Content[] contents() default {};

    /**
     * Decorator section includes.
     * @deprecated Use {@link #value()} with {@link WidgetType#DECORATOR_SECTION_INCLUDE} for order-preserved widgets.
     */
    @Deprecated
    DecoratorSectionInclude[] decoratorSectionIncludes() default {};

    /**
     * Grids to include.
     * @deprecated Use {@link #value()} with {@link WidgetType#INCLUDE_GRID} for order-preserved widgets.
     */
    @Deprecated
    IncludeGrid[] includeGrids() default {};

    /**
     * Trees to include.
     * @deprecated Use {@link #value()} with {@link WidgetType#INCLUDE_TREE} for order-preserved widgets.
     */
    @Deprecated
    IncludeTreeWidget[] includeTrees() default {};

    /**
     * Links to render.
     * @deprecated Use {@link #value()} with {@link WidgetType#LINK} for order-preserved widgets.
     */
    @Deprecated
    ScreenLink[] links() default {};

    /**
     * Sub-content widgets to render.
     * @deprecated Use {@link #value()} with {@link WidgetType#SUB_CONTENT} for order-preserved widgets.
     */
    @Deprecated
    SubContent[] subContents() default {};

    /**
     * Column containers to render.
     * @deprecated Use {@link #value()} with {@link WidgetType#COLUMN_CONTAINER} for order-preserved widgets.
     */
    @Deprecated
    ColumnContainer[] columnContainers() default {};

    /**
     * Iterate sections.
     * @deprecated Use {@link #value()} with {@link WidgetType#ITERATE_SECTION} for order-preserved widgets.
     */
    @Deprecated
    IterateSection[] iterateSections() default {};

    /**
     * A decorator screen nested in these widgets.
     *
     * <p>SCIPIO: 4.0.0: DecoratorScreen would close a type cycle; the nested form does not.</p>
     */
    DecoratorScreenNested decorator() default @DecoratorScreenNested(name = "");

    /**
     * Sections with a condition, actions, name or contains nested in these widgets.
     *
     * <p>SCIPIO: 4.0.0: The converter skipped them, which emptied e.g. CommonInvoicesDecorator.
     * InlineSection would close a type cycle; SectionNested does not.</p>
     */
    SectionNested[] sections() default {};

    // NOTE: Section is intentionally excluded to prevent infinite nesting.
    // Use InlineSection[] in the parent InlineSection if nested conditionals are needed.
}
