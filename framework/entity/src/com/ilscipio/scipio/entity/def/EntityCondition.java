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
package com.ilscipio.scipio.entity.def;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines entity-level condition configuration for a view-entity.
 *
 * <p>Corresponds to entity-condition element in entitymodel.xsd.</p>
 *
 * <p>This annotation can specify filter-by-date, distinct, WHERE conditions,
 * HAVING conditions, and default ORDER BY.</p>
 *
 * <p>Example:</p>
 * <pre>
 * {@literal @}ViewEntity(name = "ActiveProducts",
 *     condition = {@literal @}EntityCondition(
 *         filterByDate = "true",
 *         distinct = true,
 *         conditionExpr = {@literal @}ConditionExpr(fieldName = "statusId", operator = ConditionOperator.EQUALS, value = "ACTIVE"),
 *         orderBy = {{@literal @}OrderBy(fieldName = "productName")}
 *     ))
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface EntityCondition {

    /**
     * Filter by date using standard date range fields (fromDate, thruDate).
     *
     * <p>Can be "true", "false", or a specific date field name.</p>
     */
    String filterByDate() default "";

    /**
     * If true, use SELECT DISTINCT; default false.
     */
    boolean distinct() default false;

    /**
     * Single condition expression for simple WHERE clauses; optional.
     */
    ConditionExpr conditionExpr() default @ConditionExpr(fieldName = "", operator = ConditionOperator.EQUALS);

    /**
     * Condition list for complex WHERE clauses with AND/OR; optional.
     */
    ConditionList conditionList() default @ConditionList;

    /**
     * Having condition list for aggregate filtering; optional.
     */
    ConditionList havingConditionList() default @ConditionList;

    /**
     * Default ORDER BY fields; optional.
     */
    OrderBy[] orderBy() default {};
}
