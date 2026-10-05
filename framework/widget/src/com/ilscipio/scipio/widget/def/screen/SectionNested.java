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
 * Defines a nested section within a WidgetsForContainer context (depth 1).
 *
 * <p>Used inside container/screenlet widget definitions to represent
 * conditional sections with their own widgets and fail-widgets at nesting depth 1.
 * Uses {@link WidgetsForContainer} for its widgets/failWidgets.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
@Repeatable(SectionNestedList.class)
public @interface SectionNested {

    String name() default "";

    boolean shareScope() default false;

    String contains() default "";

    String id() default "";

    String style() default "";

    Condition condition() default @Condition;

    WidgetsForContainer widgets() default @WidgetsForContainer;

    WidgetsForContainer failWidgets() default @WidgetsForContainer;

    Actions actions() default @Actions;

    Actions catchActions() default @Actions;

    Actions finallyActions() default @Actions;

    /**
     * SCIPIO: 4.0.0: Slot of this child among all children of its widgets block, counting every typed
     * array. -1 (the default) keeps the declaration order of its own array. Set it when a child of
     * another type must render between two children of this type; every widgets block honours it.
     */
    int position() default -1;
}
