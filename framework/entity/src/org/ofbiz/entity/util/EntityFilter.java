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
package org.ofbiz.entity.util;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.GenericEntity;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

/** SCIPIO: Interface for entity matching. */
public interface EntityFilter {

    EntityFilter ANY = new EntityFilter() {
        @Override
        public boolean matches(GenericEntity entity, Map<String, Object> context) {
            return true;
        }

        @Override
        public String toString() {
            return "any";
        }
    };

    EntityFilter NONE = new EntityFilter() {
        @Override
        public boolean matches(GenericEntity entity, Map<String, Object> context) {
            return false;
        }

        @Override
        public boolean matchesNone() {
            return true;
        }

        @Override
        public String toString() {
            return "none";
        }
    };

    boolean matches(GenericEntity entity, Map<String, Object> context);

    default boolean matches(GenericEntity entity) {
        return matches(entity, Collections.emptyMap());
    }

    default boolean matchesAny(Collection<? extends GenericEntity> entities, Map<String, Object> context) {
        for (GenericEntity entity : entities) {
            if (matches(entity, context)) {
                return true;
            }
        }
        return false;
    }

    default boolean matchesAny(Collection<? extends GenericEntity> entities) {
        return matchesAny(entities, Collections.emptyMap());
    }

    default boolean matchesAll(Collection<? extends GenericEntity> entities, Map<String, Object> context) {
        for (GenericEntity entity : entities) {
            if (!matches(entity, context)) {
                return false;
            }
        }
        return true;
    }

    default boolean matchesAll(Collection<? extends GenericEntity> entities) {
        return matchesAll(entities, Collections.emptyMap());
    }

    default boolean matchesNone() {
        return false;
    }

    /** Returns {@link EntityFilter#NONE} or {@link EntityFilter#ANY} if the expression is unset or corresponds to none or any; if other, returns null. */
    static EntityFilter checkAnyNoneFromExprOrNull(String expr) {
        if (UtilValidate.isEmpty(expr) || "none".equals(expr)) {
            return NONE;
        } else if ("any".equals(expr)) {
            return ANY;
        } else {
            return null;
        }
    }
}
