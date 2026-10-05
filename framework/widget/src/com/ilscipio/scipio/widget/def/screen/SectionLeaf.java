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

import com.ilscipio.scipio.widget.def.condition.Condition;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * A section at the end of a container or screenlet chain, where no SectionNested level is left.
 *
 * <p>Its content uses {@link WidgetsLeaf}, which holds no screenlet and no further section;
 * that is what keeps the annotation types acyclic. Before this existed, such a section was
 * either dropped outright (inside a screenlet container) or flattened, which merged its
 * actions with its siblings' and silently changed what each branch rendered.</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({}) // Used as nested annotation only
public @interface SectionLeaf {

    String name() default "";

    boolean shareScope() default false;

    String contains() default "";

    String id() default "";

    String style() default "";

    Condition condition() default @Condition(type = com.ilscipio.scipio.widget.def.condition.impl.Always.class);

    Actions actions() default @Actions;

    WidgetsLeaf widgets() default @WidgetsLeaf;

    WidgetsLeaf failWidgets() default @WidgetsLeaf;

    /**
     * SCIPIO: 4.0.0: Slot of this child among all children of its widgets block, counting every typed
     * array. -1 (the default) keeps the declaration order of its own array. Set it when a child of
     * another type must render between two children of this type; every widgets block honours it.
     */
    int position() default -1;
}
