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
 * Defines conditions for screen sections.
 *
 * <p>Supports various condition types: if-empty, if-compare, if-has-permission,
 * if-service-permission, and compound conditions via separate group annotations.</p>
 *
 * <p>Example XML equivalents:</p>
 * <pre>{@code
 * <condition>
 *     <if-empty field="parties"/>
 * </condition>
 *
 * <condition>
 *     <if-has-permission permission="PARTYMGR" action="_CREATE"/>
 * </condition>
 *
 * <condition>
 *     <if-compare field="showScreen" operator="equals" value="origin"/>
 * </condition>
 * }</pre>
 *
 * <p>For compound conditions (and, or, xor), use the separate group
 * annotations: {@link AndCondition}, {@link OrCondition}, {@link XorCondition}.</p>
 *
 * <p>Alternatively, use the {@link #functionalConditions()} attribute for complex conditions
 * with the new functional interface pattern:</p>
 * <pre>{@code
 * @Condition(functionalConditions = {
 *     @com.ilscipio.scipio.widget.def.condition.Condition(
 *         type = HasPermission.class,
 *         params = {"PARTYMGR", "_ADMIN"}
 *     )
 * })
 * }</pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support. Enhanced with functional conditions.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
public @interface Condition {

    /**
     * Functional condition(s) using the {@link com.ilscipio.scipio.widget.def.condition.Condition} annotation.
     * If multiple conditions are provided, they are AND-ed together.
     * This takes precedence over all other condition attributes when non-empty.
     */
    com.ilscipio.scipio.widget.def.condition.Condition[] functionalConditions() default {};

    /**
     * If-empty condition: checks if a field is empty/null.
     */
    String ifEmpty() default "";

    /**
     * If-not-empty condition: checks if a field is NOT empty/null.
     */
    String ifNotEmpty() default "";

    /**
     * If-true condition: checks if a field is Boolean true.
     */
    String ifTrue() default "";

    /**
     * If-false condition: checks if a field is Boolean false.
     */
    String ifFalse() default "";

    /**
     * If-compare conditions (field, operator, value).
     */
    IfCompare[] ifCompare() default {};

    /**
     * If-compare-field conditions (field compared to another field).
     */
    IfCompareField[] ifCompareField() default {};

    /**
     * If-has-permission conditions.
     */
    IfHasPermission[] ifHasPermission() default {};

    /**
     * If-service-permission conditions.
     */
    IfServicePermission[] ifServicePermission() default {};

    /**
     * If-validate-method condition.
     */
    IfValidateMethod[] ifValidateMethod() default {};

    /**
     * If-regexp condition.
     */
    IfRegexp[] ifRegexp() default {};

    /**
     * If-empty-section condition (screen-specific).
     * Checks if a decorator section is empty/not provided.
     */
    IfEmptySection[] ifEmptySection() default {};

    /**
     * If-entity-permission condition.
     * Checks entity-level permissions.
     */
    IfEntityPermission[] ifEntityPermission() default {};

    /**
     * If-widget condition (Scipio extension).
     * Checks if a widget (screen/form/menu/tree) exists.
     */
    IfWidget[] ifWidget() default {};

    /**
     * If-component condition (Scipio extension).
     * Checks if a component is loaded.
     */
    IfComponent[] ifComponent() default {};

    /**
     * If-entity condition (Scipio extension).
     * Checks if an entity definition exists.
     */
    IfEntity[] ifEntity() default {};

    /**
     * If-service condition (Scipio extension).
     * Checks if a service definition exists.
     */
    IfServiceDef[] ifService() default {};

    /**
     * Compound AND conditions - all basic conditions from this group must be true.
     */
    AndCondition[] and() default {};

    /**
     * Compound OR conditions - at least one basic condition from this group must be true.
     */
    OrCondition[] or() default {};

    /**
     * Compound XOR conditions - exactly one basic condition from this group must be true.
     */
    XorCondition[] xor() default {};

    /**
     * NOT condition (negates the entire condition).
     */
    boolean not() default false;
}
