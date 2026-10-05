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

import org.ofbiz.base.util.UtilValidate;

import java.util.ArrayList;
import java.util.List;

/**
 * Utility methods for entity annotation processing.
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
public class EntityDefUtil {

    private EntityDefUtil() {}

    /**
     * Gets the entity name from the annotation, with fallback to the class name.
     */
    public static String getEntityName(Entity entity, Class<?> entityClass) {
        String name = entity.name();
        if (UtilValidate.isEmpty(name)) {
            // Use class simple name as default
            name = entityClass.getSimpleName();
            // Remove common suffixes
            if (name.endsWith("Entity")) {
                name = name.substring(0, name.length() - 6);
            }
        }
        return name;
    }

    /**
     * Gets all Field annotations from the entity class, combining those from
     * the @Entity.fields() attribute and repeatable @Field annotations.
     */
    public static List<Field> getAllFields(Entity entity, Class<?> entityClass) {
        List<Field> result = new ArrayList<>();

        // Fields from @Entity.fields() attribute
        Field[] inlineFields = entity.fields();
        if (inlineFields != null) {
            for (Field field : inlineFields) {
                result.add(field);
            }
        }

        // Repeatable @Field annotations
        Field[] repeatableFields = entityClass.getAnnotationsByType(Field.class);
        if (repeatableFields != null) {
            for (Field field : repeatableFields) {
                result.add(field);
            }
        }

        return result;
    }

    /**
     * Gets all PrimaryKey annotations from the entity class, combining those from
     * the @Entity.primaryKeys() attribute and repeatable @PrimaryKey annotations.
     */
    public static List<PrimaryKey> getAllPrimaryKeys(Entity entity, Class<?> entityClass) {
        List<PrimaryKey> result = new ArrayList<>();

        // PrimaryKeys from @Entity.primaryKeys() attribute
        PrimaryKey[] inlineKeys = entity.primaryKeys();
        if (inlineKeys != null) {
            for (PrimaryKey pk : inlineKeys) {
                result.add(pk);
            }
        }

        // Repeatable @PrimaryKey annotations
        PrimaryKey[] repeatableKeys = entityClass.getAnnotationsByType(PrimaryKey.class);
        if (repeatableKeys != null) {
            for (PrimaryKey pk : repeatableKeys) {
                result.add(pk);
            }
        }

        return result;
    }

    /**
     * Gets all Relation annotations from the entity class, combining those from
     * the @Entity.relations() attribute and repeatable @Relation annotations.
     */
    public static List<Relation> getAllRelations(Entity entity, Class<?> entityClass) {
        List<Relation> result = new ArrayList<>();

        // Relations from @Entity.relations() attribute
        Relation[] inlineRelations = entity.relations();
        if (inlineRelations != null) {
            for (Relation rel : inlineRelations) {
                result.add(rel);
            }
        }

        // Repeatable @Relation annotations
        Relation[] repeatableRelations = entityClass.getAnnotationsByType(Relation.class);
        if (repeatableRelations != null) {
            for (Relation rel : repeatableRelations) {
                result.add(rel);
            }
        }

        return result;
    }

    /**
     * Gets all Index annotations from the entity class, combining those from
     * the @Entity.indexes() attribute and repeatable @Index annotations.
     */
    public static List<Index> getAllIndexes(Entity entity, Class<?> entityClass) {
        List<Index> result = new ArrayList<>();

        // Indexes from @Entity.indexes() attribute
        Index[] inlineIndexes = entity.indexes();
        if (inlineIndexes != null) {
            for (Index idx : inlineIndexes) {
                result.add(idx);
            }
        }

        // Repeatable @Index annotations
        Index[] repeatableIndexes = entityClass.getAnnotationsByType(Index.class);
        if (repeatableIndexes != null) {
            for (Index idx : repeatableIndexes) {
                result.add(idx);
            }
        }

        return result;
    }

    /**
     * Checks if a string annotation value is set (not empty).
     */
    public static boolean isSet(String value) {
        return UtilValidate.isNotEmpty(value);
    }

    /**
     * Returns the value if set, otherwise returns the default.
     */
    public static String valueOrDefault(String value, String defaultValue) {
        return isSet(value) ? value : defaultValue;
    }
}
