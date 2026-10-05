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
 * Defines an iterate-section widget, equivalent to widget-screen.xsd iterate-section element.
 *
 * <p>Iterates over a list and renders sections for each entry.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}IterateSection(entry = "item", list = "items", sections = {
 *     {@literal @}Section(name = "itemSection", ...)
 * })
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
@Repeatable(IterateSectionList.class)
public @interface IterateSection {

    /**
     * Variable name for the current entry; required.
     */
    String entry();

    /**
     * The list to iterate over; required.
     */
    String list();

    /**
     * Optional key variable name for maps.
     */
    String key() default "";

    /**
     * Number of items to display per page.
     */
    String viewSize() default "";

    /**
     * Target for pagination links.
     */
    String paginateTarget() default "";

    /**
     * Whether to paginate; defaults to "${paginate}".
     */
    String paginate() default "${paginate}";

    // NOTE: Section[] sections() not supported due to Java annotation cyclic reference limitations.
    // For iterate-section with sections, use XML definition or separate annotated classes.
}
