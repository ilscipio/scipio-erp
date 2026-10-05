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
 * Defines an include-screen widget, equivalent to widget-screen.xsd include-screen element.
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}IncludeScreen(name = "ProductDetail", location = "component://product/widget/ProductScreens.xml")
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Repeatable(IncludeScreenList.class)
public @interface IncludeScreen {

    /**
     * Screen name to include; required.
     *
     * <p>Can be empty string to indicate no screen (used as default).</p>
     */
    String name();

    /**
     * Screen location; optional.
     *
     * <p>Component-style location (e.g., "component://product/widget/ProductScreens.xml").
     * If not specified, looks in current screen file.</p>
     */
    String location() default "";

    /**
     * Whether to share scope with included screen; optional.
     */
    boolean shareScope() default false;

    /**
     * SCIPIO: 4.0.0: Slot of this child among all children of its widgets block, counting every typed
     * array. -1 (the default) keeps the declaration order of its own array. Set it when a child of
     * another type must render between two children of this type; every widgets block honours it.
     */
    int position() default -1;
}
