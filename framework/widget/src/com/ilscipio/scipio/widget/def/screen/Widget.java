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
 * Unified widget annotation for screen widgets.
 *
 * <p>This annotation consolidates all widget types into a single annotation with a {@link WidgetType}
 * discriminator. This design ensures widgets are processed in array order, preserving the rendering
 * sequence that was defined in the original XML.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}Widget(type = WidgetType.INCLUDE_SCREEN, name = "Header", location = "...")
 * {@literal @}Widget(type = WidgetType.LABEL, text = "Welcome")
 * {@literal @}Widget(type = WidgetType.INCLUDE_FORM, name = "MyForm", location = "...")
 * </pre>
 *
 * <p>The above widgets will render in order: Header screen, label, then MyForm.</p>
 *
 * <p>Note: Container and Screenlet widgets with nested content should use the specific
 * annotations (@Container, @Screenlet) as they have complex nested structures that cannot
 * be represented in a single @Widget due to Java annotation cycle limitations.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for unified widget annotation support with order preservation.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
@Repeatable(WidgetList.class)
public @interface Widget {

    /**
     * The type of widget. Required.
     */
    WidgetType type();

    // ========== Common Attributes ==========

    /**
     * Widget name - used by INCLUDE_SCREEN, INCLUDE_FORM, INCLUDE_MENU, INCLUDE_GRID, INCLUDE_TREE,
     * DECORATOR_SECTION_INCLUDE.
     */
    String name() default "";

    /**
     * Widget location - used by INCLUDE_SCREEN, INCLUDE_FORM, INCLUDE_MENU, INCLUDE_GRID, INCLUDE_TREE,
     * HTML_TEMPLATE.
     */
    String location() default "";

    /**
     * Whether to share scope - used by INCLUDE_SCREEN, INCLUDE_FORM.
     */
    boolean shareScope() default false;

    // ========== LABEL Attributes ==========

    /**
     * Text content - used by LABEL.
     */
    String text() default "";

    /**
     * CSS style class - used by LABEL, CONTAINER, IMAGE, LINK.
     */
    String style() default "";

    /**
     * Element ID - used by LABEL, CONTAINER.
     */
    String id() default "";

    // ========== HTML_TEMPLATE Attributes ==========

    /**
     * Template language - used by HTML_TEMPLATE. Default is "ftl".
     */
    String lang() default "ftl";

    /**
     * Whether to trim lines - used by HTML_TEMPLATE.
     */
    boolean trimLines() default true;

    /**
     * Inline content - used by HTML_TEMPLATE, CONTENT.
     */
    String content() default "";

    // ========== IMAGE Attributes ==========

    /**
     * Image source URL - used by IMAGE.
     */
    String src() default "";

    /**
     * Alt text - used by IMAGE.
     */
    String alt() default "";

    /**
     * Title/tooltip text - used by IMAGE, LINK.
     */
    String title() default "";

    /**
     * Image width - used by IMAGE.
     */
    String width() default "";

    /**
     * Image height - used by IMAGE.
     */
    String height() default "";

    /**
     * Image border - used by IMAGE.
     */
    String border() default "";

    /**
     * URL mode - used by IMAGE.
     */
    String urlMode() default "";

    // ========== LINK Attributes ==========

    /**
     * Link target URL - used by LINK.
     */
    String target() default "";

    /**
     * Link target window - used by LINK.
     */
    String targetWindow() default "";

    /**
     * Link type - used by LINK.
     */
    String linkType() default "";

    // ========== CONTENT Attributes ==========

    /**
     * Content ID - used by CONTENT, SUB_CONTENT.
     */
    String contentId() default "";

    /**
     * Data resource ID - used by CONTENT.
     */
    String dataResourceId() default "";

    /**
     * Edit request - used by CONTENT.
     */
    String editRequest() default "";

    /**
     * Edit container style - used by CONTENT.
     */
    String editContainerStyle() default "";

    /**
     * Enable edit value - used by CONTENT.
     */
    String enableEditValue() default "";

    /**
     * Enable edit name - used by CONTENT, SUB_CONTENT.
     *
     * <p>SCIPIO: 4.0.0: Added; the enable-edit-name attribute was dropped by the converter.</p>
     */
    String enableEditName() default "";

    /**
     * XML escape - used by CONTENT.
     */
    boolean xmlEscape() default false;

    // ========== SUB_CONTENT Attributes ==========

    /**
     * Map key - used by SUB_CONTENT.
     */
    String mapKey() default "";

    /**
     * Association name - used by SUB_CONTENT.
     */
    String assocName() default "";

    // ========== CONTAINER Attributes ==========

    /**
     * Container type (div, span, etc.) - used by CONTAINER.
     */
    String containerType() default "";

    /**
     * Auto update target ID - used by CONTAINER.
     */
    String autoUpdateTargetId() default "";

    /**
     * Auto update interval in seconds - used by CONTAINER.
     */
    int autoUpdateInterval() default 0;

    /**
     * Scipio contains targeting expression - used by CONTAINER.
     */
    String contains() default "";

    // ========== ITERATE_SECTION Attributes ==========

    /**
     * List field name - used by ITERATE_SECTION.
     */
    String list() default "";

    /**
     * Entry field name - used by ITERATE_SECTION.
     */
    String entry() default "";

    /**
     * Key field name - used by ITERATE_SECTION.
     */
    String key() default "";

    /**
     * View size - used by ITERATE_SECTION.
     */
    int viewSize() default -1;

    /**
     * Paginate flag - used by ITERATE_SECTION.
     */
    boolean paginate() default true;

    /**
     * SCIPIO: 4.0.0: Slot of this child among all children of its widgets block, counting every typed
     * array. -1 (the default) keeps the declaration order of its own array. Set it when a child of
     * another type must render between two children of this type; every widgets block honours it.
     */
    int position() default -1;

    /**
     * SCIPIO: 4.0.0: The platform-specific branch this template renders in: "html" (default), "xsl-fo" for
     * PDF output through FOP, "text" or "xml". Maps to the child element of &lt;platform-specific&gt;.
     */
    String platform() default "html";

    /** SCIPIO: 4.0.0: conf-mode of an include-portal-page. */
    String confMode() default "";

    /** SCIPIO: 4.0.0: use-private of an include-portal-page. */
    String usePrivate() default "";

    /** SCIPIO: 4.0.0: paginate-target of an iterate-section. */
    String paginateTarget() default "";
}
