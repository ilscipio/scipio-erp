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
 * Defines a decorator-section widget, equivalent to widget-screen.xsd decorator-section element.
 *
 * <p>Widgets can be specified in two ways:</p>
 *
 * <h3>Recommended: Unified Widget Array (preserves order)</h3>
 * <pre>
 * {@literal @}DecoratorSection(name = "body", value = {
 *     {@literal @}Widget(type = WidgetType.LABEL, text = "Header"),
 *     {@literal @}Widget(type = WidgetType.INCLUDE_FORM, name = "MyForm", location = "..."),
 *     {@literal @}Widget(type = WidgetType.HTML_TEMPLATE, location = "...")
 * })
 * </pre>
 *
 * <h3>Legacy: Type-specific Arrays (order NOT guaranteed)</h3>
 * <pre>
 * {@literal @}DecoratorSection(name = "body",
 *     includeForms = {{@literal @}IncludeForm(...)},
 *     htmlTemplates = {{@literal @}HtmlTemplate(...)}
 * )
 * </pre>
 *
 * <p><strong>Important:</strong> When using the unified {@link #value()} array, widgets are
 * processed in array order, which is critical for proper rendering sequence. The legacy
 * type-specific arrays process widgets by type, not by declaration order.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Repeatable(DecoratorSectionList.class)
public @interface DecoratorSection {

    /**
     * Section name; required.
     *
     * <p>Must match a decorator-section-include name in the decorator.</p>
     */
    String name();

    /**
     * Conditional rendering expression (Scipio flexible expression).
     *
     * <p>When specified, this section only renders if the expression evaluates to true.
     * This is critical for conditional section rendering like:</p>
     * <pre>{@code
     * <decorator-section name="left-column" use-when="${context.widePage != true}">
     * }</pre>
     */
    String useWhen() default "";

    /**
     * Whether to automatically include content from the parent decorator section
     * if this section has no content. Default false.
     */
    boolean fallbackAutoInclude() default false;

    /**
     * Whether auto-included content should override this section's content.
     * Default false.
     */
    boolean overrideByAutoInclude() default false;

    /**
     * Scipio targeting expression for selective rendering optimization.
     * Uses syntax like $, #, ~, %, ^ for matching types.
     */
    String contains() default "";

    // ========== Unified Widget Array (RECOMMENDED) ==========

    /**
     * Unified widget array - widgets are rendered in array order.
     *
     * <p>This is the recommended way to specify widgets as it preserves rendering order.
     * Use this instead of the type-specific arrays below.</p>
     *
     * <p>SCIPIO: 4.0.0: Added for order-preserved widget rendering.</p>
     */
    Widget[] value() default {};

    // ========== Legacy Type-specific Arrays (for backward compatibility) ==========
    // NOTE: These do NOT guarantee rendering order.

    /**
     * Widgets to include in this section.
     * @deprecated Use {@link #value()} with {@link WidgetType#INCLUDE_SCREEN} for order-preserved widgets.
     */
    @Deprecated
    IncludeScreen[] includeScreens() default {};

    /**
     * Forms to include in this section.
     * @deprecated Use {@link #value()} with {@link WidgetType#INCLUDE_FORM} for order-preserved widgets.
     */
    @Deprecated
    IncludeForm[] includeForms() default {};

    /**
     * Menus to include in this section.
     * @deprecated Use {@link #value()} with {@link WidgetType#INCLUDE_MENU} for order-preserved widgets.
     */
    @Deprecated
    IncludeMenu[] includeMenus() default {};

    /**
     * Labels to render in this section.
     * @deprecated Use {@link #value()} with {@link WidgetType#LABEL} for order-preserved widgets.
     */
    @Deprecated
    Label[] labels() default {};

    /**
     * Screenlets to render in this section.
     * @deprecated Use {@link #value()} with {@link WidgetType#SCREENLET} for order-preserved widgets.
     */
    @Deprecated
    Screenlet[] screenlets() default {};

    /**
     * Containers to render in this section.
     * @deprecated Use {@link #value()} with {@link WidgetType#CONTAINER} for order-preserved widgets.
     */
    @Deprecated
    Container[] containers() default {};

    /**
     * HTML templates to render in this section.
     * @deprecated Use {@link #value()} with {@link WidgetType#HTML_TEMPLATE} for order-preserved widgets.
     */
    @Deprecated
    HtmlTemplate[] htmlTemplates() default {};

    /**
     * Decorator section includes in this section.
     * Used to include content passed from the calling screen.
     * @deprecated Use {@link #value()} with {@link WidgetType#DECORATOR_SECTION_INCLUDE} for order-preserved widgets.
     */
    @Deprecated
    DecoratorSectionInclude[] decoratorSectionIncludes() default {};

    /**
     * Grids to include in this section.
     * @deprecated Use {@link #value()} with {@link WidgetType#INCLUDE_GRID} for order-preserved widgets.
     */
    @Deprecated
    IncludeGrid[] includeGrids() default {};

    /**
     * Trees to include in this section.
     * @deprecated Use {@link #value()} with {@link WidgetType#INCLUDE_TREE} for order-preserved widgets.
     */
    @Deprecated
    IncludeTreeWidget[] includeTrees() default {};

    /**
     * Links to render in this section.
     * @deprecated Use {@link #value()} with {@link WidgetType#LINK} for order-preserved widgets.
     */
    @Deprecated
    ScreenLink[] links() default {};

    /**
     * Sub-content widgets to render in this section.
     * @deprecated Use {@link #value()} with {@link WidgetType#SUB_CONTENT} for order-preserved widgets.
     */
    @Deprecated
    SubContent[] subContents() default {};

    /**
     * Column containers to render in this section.
     * @deprecated Use {@link #value()} with {@link WidgetType#COLUMN_CONTAINER} for order-preserved widgets.
     */
    @Deprecated
    ColumnContainer[] columnContainers() default {};

    /**
     * Images to render in this section.
     * @deprecated Use {@link #value()} with {@link WidgetType#IMAGE} for order-preserved widgets.
     */
    @Deprecated
    Image[] images() default {};

    /**
     * Content widgets to render in this section.
     * @deprecated Use {@link #value()} with {@link WidgetType#CONTENT} for order-preserved widgets.
     */
    @Deprecated
    Content[] contents() default {};

    /**
     * Horizontal separators to render in this section.
     * @deprecated Use {@link #value()} with {@link WidgetType#HORIZONTAL_SEPARATOR} for order-preserved widgets.
     */
    @Deprecated
    HorizontalSeparator[] horizontalSeparators() default {};

    /**
     * Inline sections with conditions inside this decorator section.
     *
     * <p>Use for conditional pass-through patterns like if-empty-section checks:</p>
     * <pre>{@code
     * <decorator-section name="body">
     *   <section>
     *     <condition><if-empty-section section-name="body"/></condition>
     *     <widgets><label text="No content"/></widgets>
     *     <fail-widgets><decorator-section-include name="body"/></fail-widgets>
     *   </section>
     * </decorator-section>
     * }</pre>
     *
     * <p>InlineSection uses {@link InlineWidgets} instead of {@link Widgets}
     * to break the cyclic type reference that would otherwise occur.</p>
     */
    InlineSection[] sections() default {};

    /**
     * Decorator screens nested directly in this section, for example a FindScreenDecorator in a body.
     *
     * <p>SCIPIO: 4.0.0: The converter dropped these, so every such screen rendered an empty body.</p>
     */
    DecoratorScreenNested[] decorators() default {};

    /**
     * Label text to display (shorthand for a single label).
     * @deprecated Use labels() instead for consistency.
     */
    @Deprecated
    String label() default "";

    /**
     * HTML to include (shorthand).
     * @deprecated Use htmlTemplates() instead.
     */
    @Deprecated
    String html() default "";
}
