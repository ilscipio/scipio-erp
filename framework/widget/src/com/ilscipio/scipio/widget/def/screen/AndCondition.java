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
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Compound AND condition: all sub-conditions must be true.
 *
 * <p>Equivalent to the XML &lt;and&gt; element.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
public @interface AndCondition {

    /**
     * If-empty conditions in this group.
     */
    String[] ifEmpty() default {};

    /**
     * If-not-empty conditions in this group.
     */
    String[] ifNotEmpty() default {};

    /**
     * If-true conditions in this group.
     */
    String[] ifTrue() default {};

    /**
     * If-false conditions in this group.
     */
    String[] ifFalse() default {};

    /**
     * If-compare conditions in this group.
     */
    IfCompare[] ifCompare() default {};

    /**
     * If-compare-field conditions in this group.
     */
    IfCompareField[] ifCompareField() default {};

    /**
     * If-has-permission conditions in this group.
     */
    IfHasPermission[] ifHasPermission() default {};

    /**
     * If-service-permission conditions in this group.
     */
    IfServicePermission[] ifServicePermission() default {};

    /**
     * If-validate-method conditions in this group.
     */
    IfValidateMethod[] ifValidateMethod() default {};

    /**
     * If-regexp conditions in this group.
     */
    IfRegexp[] ifRegexp() default {};

    /**
     * If-empty-section conditions in this group (screen-specific).
     */
    IfEmptySection[] ifEmptySection() default {};

    /**
     * If-entity-permission conditions in this group.
     */
    IfEntityPermission[] ifEntityPermission() default {};

    /**
     * If-widget conditions in this group (Scipio extension).
     */
    IfWidget[] ifWidget() default {};

    /**
     * If-component conditions in this group (Scipio extension).
     */
    IfComponent[] ifComponent() default {};

    /**
     * If-entity conditions in this group (Scipio extension).
     */
    IfEntity[] ifEntity() default {};

    /**
     * If-service conditions in this group (Scipio extension).
     */
    IfServiceDef[] ifService() default {};

    // NOTE: Nested compound conditions (and/or/xor within and/or/xor) are not directly
    // supported due to Java annotation cyclic type restrictions. For deeply nested conditions,
    // use the Condition annotation with its compound condition arrays, or use separate
    // annotated methods/classes for each condition group.
}
