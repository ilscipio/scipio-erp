package org.ofbiz.entity.util;

import org.ofbiz.entity.GenericEntity;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

/** SCIPIO: Interface for entity matching. */
public interface EntityFilter {

    public static final EntityFilter ANY = new EntityFilter() {
        @Override
        public boolean matches(GenericEntity entity, Map<String, Object> context) {
            return true;
        }
    };

    public static final EntityFilter NONE = new EntityFilter() {
        @Override
        public boolean matches(GenericEntity entity, Map<String, Object> context) {
            return false;
        }

        @Override
        public boolean matchesNone() {
            return true;
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
}
