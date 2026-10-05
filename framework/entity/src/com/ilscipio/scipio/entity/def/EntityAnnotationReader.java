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

import com.ilscipio.scipio.ce.base.component.ComponentReflectInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilTimer;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelField;
import org.ofbiz.entity.model.ModelIndex;
import org.ofbiz.entity.model.ModelKeyMap;
import org.ofbiz.entity.model.ModelReader;
import org.ofbiz.entity.model.ModelRelation;
import org.ofbiz.entity.model.ModelUtil;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Entity annotation reader - creates ModelEntity objects from @Entity annotations.
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@SuppressWarnings("serial")
public class EntityAnnotationReader implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final ComponentReflectInfo reflectInfo;
    protected final ModelReader modelReader;

    public EntityAnnotationReader(ComponentReflectInfo reflectInfo, ModelReader modelReader) {
        this.reflectInfo = reflectInfo;
        this.modelReader = modelReader;
    }

    /**
     * Reads all @Entity annotated classes and returns a map of entity names to ModelEntity objects.
     */
    public Map<String, ModelEntity> getModelEntities() {
        UtilTimer utilTimer = new UtilTimer();
        utilTimer.timerString("Before start of entity loop in entity annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "]");

        Map<String, ModelEntity> modelEntities = new LinkedHashMap<>();
        int entityCount = 0;

        for (Class<?> entityClass : reflectInfo.getReflectQuery().getAnnotatedClasses(Entity.class)) {
            Entity entityDef = entityClass.getAnnotation(Entity.class);
            if (entityDef == null) {
                continue;
            }

            String entityName = EntityDefUtil.getEntityName(entityDef, entityClass);

            // Check for duplicate entity definitions
            if (modelEntities.containsKey(entityName) && !entityDef.redefinition()) {
                Debug.logWarning("Entity " + entityName + " is defined more than once, " +
                        "most recent will over-write previous definition(s)", module);
            }

            try {
                ModelEntity modelEntity = createModelEntity(entityName, entityDef, entityClass);
                if (modelEntity != null) {
                    modelEntities.put(entityName, modelEntity);
                    entityCount++;
                }
            } catch (Exception e) {
                Debug.logError(e, "Error creating entity from annotation: " + entityName +
                        " in class " + entityClass.getName(), module);
            }
        }

        utilTimer.timerString("Finished entity annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "] - Total Entities: " + entityCount + " FINISHED");
        Debug.logInfo("Loaded [" + entityCount + "] Entities from annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "]", module);

        return modelEntities;
    }

    /**
     * Creates a ModelEntity from an @Entity annotation.
     */
    protected ModelEntity createModelEntity(String entityName, Entity entityDef, Class<?> entityClass) {
        // Collect primary key field names
        List<PrimaryKey> primaryKeys = EntityDefUtil.getAllPrimaryKeys(entityDef, entityClass);
        List<String> pkFieldNames = new ArrayList<>();
        for (PrimaryKey pk : primaryKeys) {
            pkFieldNames.add(pk.field());
        }

        // Collect fields
        List<Field> fields = EntityDefUtil.getAllFields(entityDef, entityClass);
        Map<String, ModelField> fieldsMap = new LinkedHashMap<>();

        // Create a temporary ModelEntity for field creation (fields need reference to their entity)
        ModelEntity tempEntity = ModelEntity.createForAnnotation(modelReader, entityName,
                getTableName(entityDef, entityName), entityDef.packageName());

        // Set entity attributes
        tempEntity.setDescription(entityDef.description());
        tempEntity.setDoLock(entityDef.enableLock());
        tempEntity.setNoAutoStamp(entityDef.noAutoStamp());
        tempEntity.setNeverCache(entityDef.neverCache());
        tempEntity.setNeverCheck(entityDef.neverCheck());
        tempEntity.setAutoClearCache(entityDef.autoClearCache());
        tempEntity.setDependentOn(entityDef.dependentOn());
        if (entityDef.sequenceBankSize() > 0) {
            tempEntity.setSequenceBankSize(entityDef.sequenceBankSize());
        }
        tempEntity.setLocation("class://" + entityClass.getName());

        // Create fields
        for (Field fieldDef : fields) {
            boolean isPk = pkFieldNames.contains(fieldDef.name());
            ModelField modelField = createModelField(tempEntity, fieldDef, isPk);
            fieldsMap.put(modelField.getName(), modelField);
        }

        // Add automatic stamp fields if needed
        addAutoStampFields(tempEntity, fieldsMap, entityDef);

        // Set fields on entity
        tempEntity.setFieldsFromAnnotation(fieldsMap, pkFieldNames);

        // Create relations
        List<Relation> relations = EntityDefUtil.getAllRelations(entityDef, entityClass);
        for (Relation relationDef : relations) {
            ModelRelation modelRelation = createModelRelation(tempEntity, relationDef);
            if (modelRelation != null) {
                tempEntity.addRelation(modelRelation);
            }
        }

        // Create indexes
        List<Index> indexes = EntityDefUtil.getAllIndexes(entityDef, entityClass);
        for (Index indexDef : indexes) {
            ModelIndex modelIndex = createModelIndex(tempEntity, indexDef);
            if (modelIndex != null) {
                tempEntity.addIndex(modelIndex);
            }
        }

        return tempEntity;
    }

    /**
     * Creates a ModelField from a @Field annotation.
     */
    protected ModelField createModelField(ModelEntity modelEntity, Field fieldDef, boolean isPk) {
        String description = fieldDef.description();
        String name = fieldDef.name();
        String type = fieldDef.type();
        String colName = UtilValidate.isNotEmpty(fieldDef.colName()) ?
                fieldDef.colName() : ModelUtil.javaNameToDbName(name);
        String fieldSet = fieldDef.fieldSet();
        boolean isNotNull = fieldDef.notNull() || isPk;

        // Parse encrypt mode
        ModelField.EncryptMethod encryptMethod = ModelField.EncryptMethod.FALSE;
        String encrypt = fieldDef.encrypt();
        if ("true".equalsIgnoreCase(encrypt)) {
            encryptMethod = ModelField.EncryptMethod.TRUE;
        } else if ("salt".equalsIgnoreCase(encrypt)) {
            encryptMethod = ModelField.EncryptMethod.SALT;
        }

        boolean enableAuditLog = fieldDef.enableAuditLog();

        // Parse validators
        List<String> validators = new ArrayList<>();
        for (Validate v : fieldDef.validators()) {
            validators.add(v.name());
        }

        // Parse select attribute
        Boolean select = null;
        if (UtilValidate.isNotEmpty(fieldDef.select())) {
            select = "true".equalsIgnoreCase(fieldDef.select());
        }

        return ModelField.create(modelEntity, description, name, type, colName, null, fieldSet,
                isNotNull, isPk, encryptMethod, false, enableAuditLog, validators, select);
    }

    /**
     * Creates a ModelRelation from a @Relation annotation.
     */
    protected ModelRelation createModelRelation(ModelEntity modelEntity, Relation relationDef) {
        String type = relationDef.type().getXmlValue();
        String title = relationDef.title();
        String relEntityName = relationDef.relEntityName();
        String fkName = relationDef.fkName();
        String description = relationDef.description();

        // Create key maps
        List<ModelKeyMap> keyMaps = new ArrayList<>();
        for (KeyMap keyMapDef : relationDef.keyMaps()) {
            String fieldName = keyMapDef.fieldName();
            String relFieldName = UtilValidate.isNotEmpty(keyMapDef.relFieldName()) ?
                    keyMapDef.relFieldName() : fieldName;
            keyMaps.add(new ModelKeyMap(fieldName, relFieldName));
        }

        return ModelRelation.create(modelEntity, description, type, title, relEntityName, fkName, keyMaps, false);
    }

    /**
     * Creates a ModelIndex from an @Index annotation.
     */
    protected ModelIndex createModelIndex(ModelEntity modelEntity, Index indexDef) {
        String name = indexDef.name();
        boolean unique = indexDef.unique();

        // Create index fields
        List<ModelIndex.Field> indexFields = new ArrayList<>();
        for (IndexField fieldDef : indexDef.fields()) {
            ModelIndex.Function function = null;
            if (fieldDef.function() == IndexFunction.LOWER) {
                function = ModelIndex.Function.LOWER;
            } else if (fieldDef.function() == IndexFunction.UPPER) {
                function = ModelIndex.Function.UPPER;
            }
            indexFields.add(new ModelIndex.Field(fieldDef.name(), function));
        }

        return ModelIndex.create(modelEntity, indexDef.description(), name, indexFields, unique);
    }

    /**
     * Gets the table name from annotation or generates from entity name.
     */
    protected String getTableName(Entity entityDef, String entityName) {
        if (UtilValidate.isNotEmpty(entityDef.tableName())) {
            return entityDef.tableName();
        }
        return ModelUtil.javaNameToDbName(entityName);
    }

    /**
     * Adds automatic timestamp fields if needed.
     */
    protected void addAutoStampFields(ModelEntity entity, Map<String, ModelField> fieldsMap, Entity entityDef) {
        boolean doLock = entityDef.enableLock();
        boolean noAutoStamp = entityDef.noAutoStamp();

        // Add lastUpdatedStamp
        if ((doLock || !noAutoStamp) && !fieldsMap.containsKey(ModelEntity.STAMP_FIELD)) {
            ModelField stampField = ModelField.create(entity, "", ModelEntity.STAMP_FIELD, "date-time",
                    null, null, null, false, false, false, true, false, null);
            fieldsMap.put(stampField.getName(), stampField);
        }

        // Add lastUpdatedTxStamp
        if (!noAutoStamp && !fieldsMap.containsKey(ModelEntity.STAMP_TX_FIELD)) {
            ModelField stampTxField = ModelField.create(entity, "", ModelEntity.STAMP_TX_FIELD, "date-time",
                    null, null, null, false, false, false, true, false, null);
            fieldsMap.put(stampTxField.getName(), stampTxField);
        }

        // Add createdStamp
        if ((doLock || !noAutoStamp) && !fieldsMap.containsKey(ModelEntity.CREATE_STAMP_FIELD)) {
            ModelField createStampField = ModelField.create(entity, "", ModelEntity.CREATE_STAMP_FIELD, "date-time",
                    null, null, null, false, false, false, true, false, null);
            fieldsMap.put(createStampField.getName(), createStampField);
        }

        // Add createdTxStamp
        if (!noAutoStamp && !fieldsMap.containsKey(ModelEntity.CREATE_STAMP_TX_FIELD)) {
            ModelField createStampTxField = ModelField.create(entity, "", ModelEntity.CREATE_STAMP_TX_FIELD, "date-time",
                    null, null, null, false, false, false, true, false, null);
            fieldsMap.put(createStampTxField.getName(), createStampTxField);
        }
    }
}
