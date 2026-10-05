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
 * Defines a join (view-link) between member entities in a view-entity.
 *
 * <p>Corresponds to view-link element in entitymodel.xsd.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface ViewLink {

    /**
     * Entity alias of the "from" entity; required.
     */
    String entityAlias();

    /**
     * Entity alias of the "to" entity; required.
     */
    String relEntityAlias();

    /**
     * If true, use LEFT OUTER JOIN; default false (INNER JOIN).
     */
    boolean relOptional() default false;

    /**
     * Key mappings for the join; at least one required.
     */
    KeyMap[] keyMaps();

    /**
     * Additional join condition; optional.
     */
    EntityCondition condition() default @EntityCondition;

    /**
     * Description; optional.
     */
    String description() default "";
}
