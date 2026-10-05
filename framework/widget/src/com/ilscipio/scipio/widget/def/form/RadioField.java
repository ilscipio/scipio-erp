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
package com.ilscipio.scipio.widget.def.form;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Defines a radio button field, equivalent to widget-form.xsd radio element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface RadioField {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Key to select when there is no current value.
     * SCIPIO: Enhanced to apply mainly to "new" record screens.
     */
    String noCurrentSelectedKey() default "";

    /**
     * Options from an entity.
     */
    EntityOptions entityOptions() default @EntityOptions(UNSET = true);

    /**
     * Options from a list in context.
     */
    ListOptions listOptions() default @ListOptions(UNSET = true);

    /**
     * Static options.
     */
    Option[] options() default {};
}
