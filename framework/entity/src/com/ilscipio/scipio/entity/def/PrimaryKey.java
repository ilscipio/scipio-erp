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

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a field as part of the primary key, equivalent to entitymodel.xsd prim-key element.
 *
 * <p>This annotation is repeatable for composite primary keys.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * // Single primary key
 * {@literal @}PrimaryKey(field = "productId")
 *
 * // Composite primary key
 * {@literal @}PrimaryKey(field = "orderId")
 * {@literal @}PrimaryKey(field = "orderItemSeqId")
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Repeatable(PrimaryKeyList.class)
public @interface PrimaryKey {

    /**
     * Field name that is part of the primary key; required.
     *
     * <p>Must reference a field defined on the same entity.</p>
     */
    String field();
}
