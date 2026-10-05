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
 * Includes a form's top section actions in this widget's actions.
 *
 * <p>This directive includes the actions from the fully-built target widget model,
 * with full recursion implied. Name and location support flexible expressions
 * and are resolved at runtime.</p>
 *
 * <p>Example XML equivalent:</p>
 * <pre>{@code
 * <include-form-actions name="EditProduct" location="component://product/widget/ProductForms.xml"/>
 * }</pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
@Repeatable(IncludeFormActionsActionList.class)
public @interface IncludeFormActionsAction {

    /**
     * The name of the form whose actions to include.
     */
    String name();

    /**
     * The location of the form definition file.
     * Optional - defaults to current location if not specified.
     */
    String location() default "";
}
