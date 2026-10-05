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
 * Unified action annotation for screen widgets.
 *
 * <p>This annotation consolidates all action types into a single annotation with an {@link ActionType}
 * discriminator. This design ensures actions are processed in array order, preserving the execution
 * sequence that was defined in the original XML.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}Action(type = ActionType.SET, field = "titleProperty", value = "PageTitle")
 * {@literal @}Action(type = ActionType.SERVICE, serviceName = "getPartyList", resultMapName = "partyList")
 * {@literal @}Action(type = ActionType.SET, field = "processed", value = "true")
 * </pre>
 *
 * <p>The above actions will execute in order: set titleProperty, call service, set processed.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for unified action annotation support with order preservation.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Repeatable(ActionList.class)
public @interface Action {

    /**
     * The type of action to perform. Required.
     */
    ActionType type();

    // ========== Common Attributes ==========

    /**
     * Field name - used by SET, CLEAR_FIELD, CLOSE_OBJECT, THROW_EXCEPTION, PROPERTY_TO_FIELD, CONDITION_TO_FIELD.
     */
    String field() default "";

    /**
     * Whether to use global scope.
     * Used by: SET, PROPERTY_TO_FIELD, PROPERTY_MAP, CONDITION_TO_FIELD.
     */
    boolean global() default false;

    /**
     * Name of included widget - used by INCLUDE_SCREEN_ACTIONS, INCLUDE_FORM_ACTIONS,
     * INCLUDE_FORM_ROW_ACTIONS, INCLUDE_MENU_ACTIONS, INCLUDE_TREE_ACTIONS.
     */
    String name() default "";

    /**
     * Location of external resource - used by SCRIPT, INCLUDE_* actions.
     */
    String location() default "";

    // ========== SET Action Attributes ==========

    /**
     * Literal value to set. Supports flexible expressions like "${parameters.orderId}".
     * Used by: SET.
     */
    String value() default "";

    /**
     * Field to copy value from.
     * Used by: SET.
     */
    String fromField() default "";

    /**
     * Default value if source is empty.
     * Used by: SET, PROPERTY_TO_FIELD.
     */
    String defaultValue() default "";

    /**
     * Type to convert value to (String, Integer, Long, Double, BigDecimal, Timestamp, Date, Boolean, List, Map).
     * Used by: SET, CONDITION_TO_FIELD.
     */
    String valueType() default "";

    /**
     * Whether to set only if field is empty.
     * Used by: SET.
     */
    boolean setIfEmpty() default true;

    /**
     * Whether to set only if field is null.
     * Used by: SET.
     */
    boolean setIfNull() default true;

    /**
     * Scope to get the from-field value from ("user", "application", or empty for default context).
     * Used by: SET.
     */
    String fromScope() default "";

    // ========== SERVICE Action Attributes ==========

    /**
     * Service name to invoke.
     * Used by: SERVICE.
     */
    String serviceName() default "";

    /**
     * Field to store service result map.
     * Used by: SERVICE.
     */
    String resultMapName() default "";

    /**
     * Field name to store list results from service.
     * Used by: SERVICE.
     */
    String resultMapList() default "";

    /**
     * Whether to auto-map context fields to service/entity parameters.
     * Used by: SERVICE, ENTITY_ONE.
     */
    boolean autoFieldMap() default true;

    /**
     * Field containing the input map.
     * Used by: SERVICE.
     */
    String resultMapField() default "";

    /**
     * Field assignments for service/entity input.
     * Used by: SERVICE, ENTITY_ONE, ENTITY_AND, ENTITY_CONDITION.
     */
    FieldMap[] fieldMaps() default {};

    // ========== ENTITY Action Attributes ==========

    /**
     * Entity name to query.
     * Used by: ENTITY_ONE, ENTITY_AND, ENTITY_CONDITION.
     */
    String entityName() default "";

    /**
     * Field name to store single entity result.
     * Used by: ENTITY_ONE, GET_RELATED_ONE.
     */
    String valueField() default "";

    /**
     * Field name to store list results.
     * Used by: ENTITY_AND, ENTITY_CONDITION, GET_RELATED.
     */
    String list() default "";

    /**
     * Whether to use entity cache.
     * Used by: ENTITY_ONE, ENTITY_AND, ENTITY_CONDITION, GET_RELATED_ONE, GET_RELATED.
     */
    boolean useCache() default false;

    /**
     * Whether to filter by date (from/thru dates).
     * Used by: ENTITY_AND, ENTITY_CONDITION.
     */
    boolean filterByDate() default false;

    /**
     * Whether to return distinct results.
     * Used by: ENTITY_CONDITION.
     */
    boolean distinct() default false;

    /**
     * Condition expressions for entity queries.
     * Used by: ENTITY_CONDITION.
     */
    ConditionExpr[] conditions() default {};

    /**
     * Fields to select (if empty, all fields are selected).
     * Used by: ENTITY_AND, ENTITY_CONDITION.
     */
    String[] selectFields() default {};

    /**
     * Order by field names.
     * Used by: ENTITY_AND, ENTITY_CONDITION.
     */
    String[] orderBy() default {};

    /**
     * Result set type: "forward" or "scroll".
     * Used by: ENTITY_AND.
     */
    String resultSetType() default "scroll";

    /**
     * Limit range start (for pagination).
     * Used by: ENTITY_AND.
     */
    int limitStart() default -1;

    /**
     * Limit range size (for pagination).
     * Used by: ENTITY_AND.
     */
    int limitSize() default -1;

    /**
     * Whether to use an iterator instead of loading all results.
     * Used by: ENTITY_AND.
     */
    boolean useIterator() default false;

    /**
     * Delegator name (optional).
     * Used by: ENTITY_CONDITION.
     */
    String delegatorName() default "";

    // ========== GET_RELATED Action Attributes ==========

    /**
     * The name of the relation to follow.
     * Used by: GET_RELATED_ONE, GET_RELATED.
     */
    String relationName() default "";

    /**
     * The field to store the related entity value.
     * Used by: GET_RELATED_ONE.
     */
    String toValueField() default "";

    /**
     * Optional map field for additional filter conditions.
     * Used by: GET_RELATED.
     */
    String map() default "";

    /**
     * Optional list field containing order-by field names.
     * Used by: GET_RELATED.
     */
    String orderByList() default "";

    // ========== SCRIPT Action Attributes ==========

    /**
     * Inline script content.
     * Used by: SCRIPT.
     */
    String script() default "";

    /**
     * Scripting language (groovy, bsh, javascript, etc.).
     * Used by: SCRIPT.
     */
    String lang() default "groovy";

    // ========== PROPERTY Action Attributes ==========

    /**
     * Properties resource name (e.g., "general", "CommonUiLabels").
     * Used by: PROPERTY_TO_FIELD, PROPERTY_MAP.
     */
    String resource() default "";

    /**
     * Property key to read.
     * Used by: PROPERTY_TO_FIELD.
     */
    String property() default "";

    /**
     * Whether to skip locale-based property lookup.
     * Used by: PROPERTY_TO_FIELD.
     */
    boolean noLocale() default false;

    /**
     * Argument field for property value substitution.
     * Used by: PROPERTY_TO_FIELD.
     */
    String argListName() default "";

    /**
     * Context map name to store properties in.
     * Used by: PROPERTY_MAP.
     */
    String mapName() default "";

    /**
     * If true, missing property-map will not generate an error.
     * Used by: PROPERTY_MAP.
     */
    boolean optional() default false;

    // ========== CONDITION_TO_FIELD Action Attributes ==========

    /**
     * The scope to set the field in (e.g., "screen", "request").
     * Used by: CONDITION_TO_FIELD.
     */
    String toScope() default "screen";

    /**
     * Only evaluate the condition if the field matches this state.
     * Values: "empty" (only if field is empty/null), "not-empty" (only if field has value).
     * Used by: CONDITION_TO_FIELD.
     */
    String onlyIfField() default "";

    /**
     * The condition to evaluate.
     * Used by: CONDITION_TO_FIELD.
     */
    Condition condition() default @Condition;

    /**
     * Explicit execution order index across type-level action annotations.
     * Java reflection cannot recover declaration order across DIFFERENT annotation
     * types (e.g. @Action vs @IfAction); the converter numbers all screen-level
     * actions sequentially when a screen mixes them. -1 = unordered (legacy).
     */
    int order() default -1;
}
