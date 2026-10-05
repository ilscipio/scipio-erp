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
 * Defines an actions container for sections.
 *
 * <p>Contains various action types that can be executed. Actions can be specified in two ways:</p>
 *
 * <h3>Recommended: Unified Action Array (preserves order)</h3>
 * <pre>
 * {@literal @}Actions(value = {
 *     {@literal @}Action(type = ActionType.SET, field = "a", value = "1"),
 *     {@literal @}Action(type = ActionType.SERVICE, serviceName = "foo"),
 *     {@literal @}Action(type = ActionType.SET, field = "b", value = "2")
 * })
 * </pre>
 *
 * <h3>Legacy: Type-specific Arrays (order NOT guaranteed)</h3>
 * <pre>
 * {@literal @}Actions(
 *     set = {{@literal @}SetAction(field = "a", value = "1")},
 *     service = {{@literal @}ServiceAction(serviceName = "foo")}
 * )
 * </pre>
 *
 * <p><strong>Important:</strong> When using the unified {@link #value()} array, actions are processed
 * in array order, which is critical for proper execution sequence. The legacy type-specific arrays
 * do NOT guarantee order and should only be used for simple cases where order doesn't matter.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
public @interface Actions {

    // ========== Unified Action Array (RECOMMENDED) ==========

    /**
     * Unified action array - actions are executed in array order.
     *
     * <p>This is the recommended way to specify actions as it preserves execution order.
     * Use this instead of the type-specific arrays below.</p>
     */
    Action[] value() default {};

    /** SCIPIO: 4.0.0: Nested conditional blocks (XML: &lt;if&gt; inside an if branch); merged with {@link #value()} by order. */
    IfAction2[] ifs() default {};

    // ========== Legacy Type-specific Arrays (for backward compatibility) ==========
    // NOTE: These methods are deprecated. Use value() with unified @Action annotations instead.
    // The legacy arrays do NOT guarantee execution order.

    /**
     * Set actions.
     * @deprecated Use {@link #value()} with {@link ActionType#SET} for order-preserved actions.
     */
    @Deprecated
    SetAction[] set() default {};

    /**
     * Clear-field actions.
     * @deprecated Use {@link #value()} with {@link ActionType#CLEAR_FIELD} for order-preserved actions.
     */
    @Deprecated
    ClearFieldAction[] clearField() default {};

    /**
     * Service actions.
     * @deprecated Use {@link #value()} with {@link ActionType#SERVICE} for order-preserved actions.
     */
    @Deprecated
    ServiceAction[] service() default {};

    /**
     * Entity-one actions.
     * @deprecated Use {@link #value()} with {@link ActionType#ENTITY_ONE} for order-preserved actions.
     */
    @Deprecated
    EntityOneAction[] entityOne() default {};

    /**
     * Entity-and actions.
     * @deprecated Use {@link #value()} with {@link ActionType#ENTITY_AND} for order-preserved actions.
     */
    @Deprecated
    EntityAndAction[] entityAnd() default {};

    /**
     * Entity-condition actions.
     * @deprecated Use {@link #value()} with {@link ActionType#ENTITY_CONDITION} for order-preserved actions.
     */
    @Deprecated
    EntityConditionAction[] entityCondition() default {};

    /**
     * Get-related-one actions.
     * @deprecated Use {@link #value()} with {@link ActionType#GET_RELATED_ONE} for order-preserved actions.
     */
    @Deprecated
    GetRelatedOneAction[] getRelatedOne() default {};

    /**
     * Get-related actions.
     * @deprecated Use {@link #value()} with {@link ActionType#GET_RELATED} for order-preserved actions.
     */
    @Deprecated
    GetRelatedAction[] getRelated() default {};

    /**
     * Script actions.
     * @deprecated Use {@link #value()} with {@link ActionType#SCRIPT} for order-preserved actions.
     */
    @Deprecated
    ScriptAction[] script() default {};

    /**
     * Property-to-field actions.
     * @deprecated Use {@link #value()} with {@link ActionType#PROPERTY_TO_FIELD} for order-preserved actions.
     */
    @Deprecated
    PropertyToFieldAction[] propertyToField() default {};

    /**
     * Property-map actions.
     * @deprecated Use {@link #value()} with {@link ActionType#PROPERTY_MAP} for order-preserved actions.
     */
    @Deprecated
    PropertyMapAction[] propertyMap() default {};

    /**
     * Include-screen-actions actions.
     * @deprecated Use {@link #value()} with {@link ActionType#INCLUDE_SCREEN_ACTIONS} for order-preserved actions.
     */
    @Deprecated
    IncludeScreenActionsAction[] includeScreenActions() default {};

    /**
     * Include-form-actions actions.
     * @deprecated Use {@link #value()} with {@link ActionType#INCLUDE_FORM_ACTIONS} for order-preserved actions.
     */
    @Deprecated
    IncludeFormActionsAction[] includeFormActions() default {};

    /**
     * Include-form-row-actions actions.
     * @deprecated Use {@link #value()} with {@link ActionType#INCLUDE_FORM_ROW_ACTIONS} for order-preserved actions.
     */
    @Deprecated
    IncludeFormRowActionsAction[] includeFormRowActions() default {};

    /**
     * Include-menu-actions actions.
     * @deprecated Use {@link #value()} with {@link ActionType#INCLUDE_MENU_ACTIONS} for order-preserved actions.
     */
    @Deprecated
    IncludeMenuActionsAction[] includeMenuActions() default {};

    /**
     * Include-tree-actions actions.
     * @deprecated Use {@link #value()} with {@link ActionType#INCLUDE_TREE_ACTIONS} for order-preserved actions.
     */
    @Deprecated
    IncludeTreeActionsAction[] includeTreeActions() default {};

    /**
     * Condition-to-field actions.
     * @deprecated Use {@link #value()} with {@link ActionType#CONDITION_TO_FIELD} for order-preserved actions.
     */
    @Deprecated
    ConditionToFieldAction[] conditionToField() default {};

    /**
     * Close-object actions.
     * @deprecated Use {@link #value()} with {@link ActionType#CLOSE_OBJECT} for order-preserved actions.
     */
    @Deprecated
    CloseObjectAction[] closeObject() default {};

    /**
     * Throw-exception actions.
     * @deprecated Use {@link #value()} with {@link ActionType#THROW_EXCEPTION} for order-preserved actions.
     */
    @Deprecated
    ThrowExceptionAction[] throwException() default {};

    // NOTE: IfAction cannot be nested within Actions due to Java annotation cyclic type restrictions.
    // IfAction references Actions (for then/else blocks), and Actions cannot reference IfAction back.
    // Use IfAction directly at the class/method level with @IfAction annotation.
}
