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
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilTimer;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.model.ModelKeyMap;
import org.ofbiz.entity.model.ModelReader;
import org.ofbiz.entity.model.ModelRelation;
import org.ofbiz.entity.model.ModelViewEntity;
import org.ofbiz.entity.model.ModelViewEntity.ComplexAlias;
import org.ofbiz.entity.model.ModelViewEntity.ComplexAliasField;
import org.ofbiz.entity.model.ModelViewEntity.ModelAlias;
import org.ofbiz.entity.model.ModelViewEntity.ModelAliasAll;
import org.ofbiz.entity.model.ModelViewEntity.ModelMemberEntity;
import org.ofbiz.entity.model.ModelViewEntity.ModelViewLink;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * View entity annotation reader - creates ModelViewEntity objects from @ViewEntity annotations.
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@SuppressWarnings("serial")
public class ViewEntityAnnotationReader implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final ComponentReflectInfo reflectInfo;
    protected final ModelReader modelReader;

    public ViewEntityAnnotationReader(ComponentReflectInfo reflectInfo, ModelReader modelReader) {
        this.reflectInfo = reflectInfo;
        this.modelReader = modelReader;
    }

    /**
     * Reads all @ViewEntity annotated classes and returns a map of entity names to ModelViewEntity objects.
     */
    public Map<String, ModelViewEntity> getModelViewEntities() {
        UtilTimer utilTimer = new UtilTimer();
        utilTimer.timerString("Before start of view-entity loop in entity annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "]");

        Map<String, ModelViewEntity> modelViewEntities = new LinkedHashMap<>();
        int entityCount = 0;

        for (Class<?> entityClass : reflectInfo.getReflectQuery().getAnnotatedClasses(ViewEntity.class)) {
            ViewEntity viewEntityDef = entityClass.getAnnotation(ViewEntity.class);
            if (viewEntityDef == null) {
                continue;
            }

            String entityName = getViewEntityName(viewEntityDef, entityClass);

            // Check for duplicate entity definitions
            if (modelViewEntities.containsKey(entityName) && !viewEntityDef.redefinition()) {
                Debug.logWarning("View-entity " + entityName + " is defined more than once, " +
                        "most recent will over-write previous definition(s)", module);
            }

            try {
                ModelViewEntity modelViewEntity = createModelViewEntity(entityName, viewEntityDef, entityClass);
                if (modelViewEntity != null) {
                    modelViewEntities.put(entityName, modelViewEntity);
                    entityCount++;
                }
            } catch (Exception e) {
                Debug.logError(e, "Error creating view-entity from annotation: " + entityName +
                        " in class " + entityClass.getName(), module);
            }
        }

        utilTimer.timerString("Finished view-entity annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "] - Total View-Entities: " + entityCount + " FINISHED");
        Debug.logInfo("Loaded [" + entityCount + "] View-Entities from annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "]", module);

        return modelViewEntities;
    }

    /**
     * Gets the view-entity name from annotation or derives from class name.
     */
    protected String getViewEntityName(ViewEntity viewEntityDef, Class<?> entityClass) {
        if (UtilValidate.isNotEmpty(viewEntityDef.name())) {
            return viewEntityDef.name();
        }
        // Derive from class name, removing common suffixes
        String className = entityClass.getSimpleName();
        if (className.endsWith("ViewEntity")) {
            return className.substring(0, className.length() - 10);
        } else if (className.endsWith("View")) {
            return className.substring(0, className.length() - 4);
        } else if (className.endsWith("Entity")) {
            return className.substring(0, className.length() - 6);
        }
        return className;
    }

    /**
     * Creates a ModelViewEntity from a @ViewEntity annotation.
     */
    protected ModelViewEntity createModelViewEntity(String entityName, ViewEntity viewEntityDef, Class<?> entityClass) {
        // Create the ModelViewEntity using factory method
        ModelViewEntity modelViewEntity = ModelViewEntity.createForAnnotation(
                modelReader,
                entityName,
                viewEntityDef.packageName(),
                viewEntityDef.title(),
                viewEntityDef.description(),
                viewEntityDef.neverCache(),
                viewEntityDef.autoClearCache()
        );

        modelViewEntity.setLocation("class://" + entityClass.getName());

        // Add member entities
        for (MemberEntity memberDef : viewEntityDef.members()) {
            ModelMemberEntity memberEntity = new ModelMemberEntity(
                    memberDef.entityAlias(),
                    memberDef.entityName()
            );
            modelViewEntity.addMemberModelMemberEntity(memberEntity);
        }

        // Add alias-alls
        for (AliasAll aliasAllDef : viewEntityDef.aliasAlls()) {
            String function = aliasAllDef.function() != AggregateFunction.NONE ?
                    aliasAllDef.function().getXmlValue() : "";
            Boolean select = UtilValidate.isNotEmpty(aliasAllDef.select()) ?
                    UtilMisc.booleanValue(aliasAllDef.select()) : null;

            List<String> excludes = aliasAllDef.excludes().length > 0 ?
                    Arrays.asList(aliasAllDef.excludes()) : null;

            ModelAliasAll aliasAll = new ModelAliasAll(
                    aliasAllDef.entityAlias(),
                    aliasAllDef.prefix(),
                    aliasAllDef.groupBy(),
                    function,
                    aliasAllDef.fieldSet(),
                    excludes,
                    select
            );
            modelViewEntity.addAliasAll(aliasAll);
        }

        // Add individual aliases
        for (Alias aliasDef : getAllAliases(viewEntityDef, entityClass)) {
            ModelAlias modelAlias = createModelAlias(aliasDef);
            modelViewEntity.addAlias(modelAlias);
        }

        // Add view links
        for (ViewLink viewLinkDef : viewEntityDef.viewLinks()) {
            ModelViewLink viewLink = createModelViewLink(viewLinkDef, modelViewEntity);
            modelViewEntity.addViewLink(viewLink);
        }

        // Add relations
        for (Relation relationDef : viewEntityDef.relations()) {
            ModelRelation modelRelation = createModelRelation(modelViewEntity, relationDef);
            if (modelRelation != null) {
                modelViewEntity.addRelation(modelRelation);
            }
        }

        // Handle entity condition
        EntityCondition conditionDef = viewEntityDef.condition();
        if (hasCondition(conditionDef)) {
            // Entity conditions are handled during populateFields
            // Store the order-by fields if present
            for (OrderBy orderByDef : conditionDef.orderBy()) {
                modelViewEntity.addGroupByField(orderByDef.fieldName());
            }
        }

        return modelViewEntity;
    }

    /**
     * Gets all alias annotations from the view entity.
     */
    protected List<Alias> getAllAliases(ViewEntity viewEntityDef, Class<?> entityClass) {
        List<Alias> aliases = new ArrayList<>();
        // Add aliases from the ViewEntity annotation
        aliases.addAll(Arrays.asList(viewEntityDef.aliases()));
        // Add repeatable Alias annotations on the class
        Alias[] classAliases = entityClass.getAnnotationsByType(Alias.class);
        aliases.addAll(Arrays.asList(classAliases));
        return aliases;
    }

    /**
     * Creates a ModelAlias from an @Alias annotation.
     */
    protected ModelAlias createModelAlias(Alias aliasDef) {
        String function = aliasDef.function() != AggregateFunction.NONE ?
                aliasDef.function().getXmlValue() : "";
        Boolean isPk = UtilValidate.isNotEmpty(aliasDef.primKey()) ?
                UtilMisc.booleanValue(aliasDef.primKey()) : null;
        Boolean select = UtilValidate.isNotEmpty(aliasDef.select()) ?
                UtilMisc.booleanValue(aliasDef.select()) : null;

        ModelAlias modelAlias = new ModelAlias(
                aliasDef.entityAlias(),
                aliasDef.name(),
                UtilValidate.isNotEmpty(aliasDef.field()) ? aliasDef.field() : aliasDef.name(),
                aliasDef.colAlias(),
                isPk,
                aliasDef.groupBy(),
                function,
                aliasDef.fieldSet(),
                false, // isFromAliasAll
                select
        );

        // Handle complex alias
        com.ilscipio.scipio.entity.def.ComplexAlias complexAliasDef = aliasDef.complexAlias();
        if (UtilValidate.isNotEmpty(complexAliasDef.operator())) {
            ComplexAlias complexAlias = createComplexAlias(complexAliasDef);
            modelAlias.setComplexAliasMember(complexAlias);
        }

        return modelAlias;
    }

    /**
     * Creates a ComplexAlias from a @ComplexAlias annotation.
     */
    protected ComplexAlias createComplexAlias(com.ilscipio.scipio.entity.def.ComplexAlias complexAliasDef) {
        ComplexAlias complexAlias = new ComplexAlias(complexAliasDef.operator());

        // Add fields
        for (com.ilscipio.scipio.entity.def.ComplexAliasField fieldDef : complexAliasDef.fields()) {
            String function = fieldDef.function() != AggregateFunction.NONE ?
                    fieldDef.function().getXmlValue() : "";
            ComplexAliasField field = new ComplexAliasField(
                    fieldDef.entityAlias(),
                    fieldDef.field(),
                    fieldDef.defaultValue(),
                    function,
                    fieldDef.value()
            );
            complexAlias.addComplexAliasMember(field);
        }

        // Add nested complex aliases (one level deep)
        for (NestedComplexAlias nestedDef : complexAliasDef.nested()) {
            ComplexAlias nestedAlias = new ComplexAlias(nestedDef.operator());
            for (com.ilscipio.scipio.entity.def.ComplexAliasField fieldDef : nestedDef.fields()) {
                String function = fieldDef.function() != AggregateFunction.NONE ?
                        fieldDef.function().getXmlValue() : "";
                ComplexAliasField field = new ComplexAliasField(
                        fieldDef.entityAlias(),
                        fieldDef.field(),
                        fieldDef.defaultValue(),
                        function,
                        fieldDef.value()
                );
                nestedAlias.addComplexAliasMember(field);
            }
            complexAlias.addComplexAliasMember(nestedAlias);
        }

        return complexAlias;
    }

    /**
     * Creates a ModelViewLink from a @ViewLink annotation.
     */
    protected ModelViewLink createModelViewLink(ViewLink viewLinkDef, ModelViewEntity modelViewEntity) {
        List<ModelKeyMap> keyMaps = new ArrayList<>();
        for (KeyMap keyMapDef : viewLinkDef.keyMaps()) {
            String relFieldName = UtilValidate.isNotEmpty(keyMapDef.relFieldName()) ?
                    keyMapDef.relFieldName() : keyMapDef.fieldName();
            keyMaps.add(new ModelKeyMap(keyMapDef.fieldName(), relFieldName));
        }

        // TODO: Handle viewLinkDef.condition() when ViewEntityCondition programmatic constructor is added
        return new ModelViewLink(
                viewLinkDef.entityAlias(),
                viewLinkDef.relEntityAlias(),
                viewLinkDef.relOptional(),
                null, // viewEntityCondition - would need programmatic constructor
                keyMaps
        );
    }

    /**
     * Creates a ModelRelation from a @Relation annotation.
     */
    protected ModelRelation createModelRelation(ModelViewEntity modelViewEntity, Relation relationDef) {
        String type = relationDef.type().getXmlValue();
        String title = relationDef.title();
        String relEntityName = relationDef.relEntityName();
        String fkName = relationDef.fkName();
        String description = relationDef.description();

        List<ModelKeyMap> keyMaps = new ArrayList<>();
        for (KeyMap keyMapDef : relationDef.keyMaps()) {
            String fieldName = keyMapDef.fieldName();
            String relFieldName = UtilValidate.isNotEmpty(keyMapDef.relFieldName()) ?
                    keyMapDef.relFieldName() : fieldName;
            keyMaps.add(new ModelKeyMap(fieldName, relFieldName));
        }

        return ModelRelation.create(modelViewEntity, description, type, title, relEntityName, fkName, keyMaps, false);
    }

    /**
     * Checks if the EntityCondition annotation has any actual conditions.
     */
    protected boolean hasCondition(EntityCondition conditionDef) {
        if (UtilValidate.isNotEmpty(conditionDef.filterByDate())) {
            return true;
        }
        if (conditionDef.distinct()) {
            return true;
        }
        if (conditionDef.orderBy().length > 0) {
            return true;
        }
        if (UtilValidate.isNotEmpty(conditionDef.conditionExpr().fieldName())) {
            return true;
        }
        if (conditionDef.conditionList().exprs().length > 0 ||
                conditionDef.conditionList().nested().length > 0) {
            return true;
        }
        return false;
    }
}
