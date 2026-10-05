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
 * Defines an include-tree widget, equivalent to widget-screen.xsd include-tree element.
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}IncludeTreeWidget(name = "CategoryTree", location = "component://product/widget/ProductTrees.xml")
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
@Repeatable(IncludeTreeWidgetList.class)
public @interface IncludeTreeWidget {

    /**
     * Tree name to include; required.
     */
    String name();

    /**
     * Tree location; required.
     *
     * <p>Component-style location (e.g., "component://product/widget/ProductTrees.xml").</p>
     */
    String location();

    /**
     * Whether to share scope with tree; optional.
     */
    boolean shareScope() default false;
}
