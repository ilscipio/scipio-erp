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
 * Defines a sub-content widget, equivalent to widget-screen.xsd sub-content element.
 *
 * <p>Renders content from a content management source using a map key.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}SubContent(contentId = "WEBSITE_CONTENT", mapKey = "HEADER_TEXT")
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
@Repeatable(SubContentList.class)
public @interface SubContent {

    /**
     * The content ID to retrieve; required.
     */
    String contentId();

    /**
     * The map key within the content; required.
     */
    String mapKey();

    /**
     * Request for edit functionality; optional.
     */
    String editRequest() default "";

    /**
     * Style for the edit container; defaults to "editWrapper".
     */
    String editContainerStyle() default "editWrapper";

    /**
     * Name for enabling edit mode; defaults to "enableEdit".
     */
    String enableEditName() default "enableEdit";

    /**
     * Whether to XML-escape the content; defaults to false.
     */
    boolean xmlEscape() default false;
}
