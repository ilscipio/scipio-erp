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
 * Defines a content widget for rendering CMS content.
 *
 * <p>Example XML equivalent:</p>
 * <pre>{@code
 * <content content-id="CONTENT_ID" edit-request="EditContent"/>
 * }</pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
@Repeatable(ContentList.class)
public @interface Content {

    /**
     * The content ID.
     */
    String contentId() default "";

    /**
     * The data resource ID.
     */
    String dataResourceId() default "";

    /**
     * The edit request URL.
     */
    String editRequest() default "";

    /**
     * The CSS style for the edit container.
     */
    String editContainerStyle() default "editWrapper";

    /**
     * The name of the parameter to enable editing.
     */
    String enableEditName() default "enableEdit";

    /**
     * Whether to XML-escape the content.
     */
    boolean xmlEscape() default false;

    /**
     * Width of iframe for non-HTML content.
     */
    String width() default "";

    /**
     * Height of iframe for non-HTML content.
     */
    String height() default "";

    /**
     * Border of iframe for non-HTML content.
     */
    String border() default "";

    /**
     * SCIPIO: 4.0.0: Slot of this child among all children of its widgets block, counting every typed
     * array. -1 (the default) keeps the declaration order of its own array. Set it when a child of
     * another type must render between two children of this type; every widgets block honours it.
     */
    int position() default -1;
}
