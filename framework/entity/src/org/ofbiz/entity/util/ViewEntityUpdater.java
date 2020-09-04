package org.ofbiz.entity.util;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelViewEntity;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Helper to update the underlying entities of a view-entity (SCIPIO).
 * FIXME: most of the methods don't fully consider the entities on the instance before overriding (call from empty for now).
 * Not thread-safe.
 */
public class ViewEntityUpdater {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final Delegator delegator;
    protected final ModelViewEntity model;
    protected final boolean makeEmptyOptionals;
    protected Map<String, GenericValue> entities;

    protected ViewEntityUpdater(Delegator delegator, ModelViewEntity model, Boolean makeEmptyOptionals) {
        this.delegator = delegator;
        this.model = model;
        this.makeEmptyOptionals = (makeEmptyOptionals != null) ? makeEmptyOptionals : !model.hasOptionalViewLink();
        clear();
    }

    public static ViewEntityUpdater from(Delegator delegator, ModelEntity model, Boolean makeEmptyOptionals) {
        return new ViewEntityUpdater(delegator, (ModelViewEntity) model, makeEmptyOptionals);
    }

    public static ViewEntityUpdater from(Delegator delegator, ModelEntity model) {
        return from(delegator, model, null);
    }

    public static ViewEntityUpdater from(GenericValue initialValue, Boolean makeEmptyOptionals) {
        ViewEntityUpdater updater = new ViewEntityUpdater(initialValue.getDelegator(), (ModelViewEntity) initialValue.getModelEntity(), makeEmptyOptionals);
        updater.setAllFields(initialValue);
        return updater;
    }

    public static ViewEntityUpdater from(GenericValue initialValue) {
        return from(initialValue, null);
    }

    public ViewEntityUpdater makeEmpty() {
        return new ViewEntityUpdater(getDelegator(), getModel(), isMakeEmptyOptionals());
    }

    public Delegator getDelegator() {
        return delegator;
    }

    public ModelViewEntity getModel() {
        return model;
    }

    public boolean isMakeEmptyOptionals() {
        return makeEmptyOptionals;
    }

    public List<String> getEntityAliasCreateOrder() {
        return getModel().getMemberEntityDependencyOrderByAlias();
    }

    public void clear() {
        Map<String, GenericValue> entities = new LinkedHashMap<>();
        for(ModelViewEntity.ModelAlias modelAlias : getModel().getAliases()) {
            entities.put(modelAlias.getEntityAlias(), null);
        }
        this.entities = entities;
    }

    public String getEntityName() {
        return getModel().getEntityName();
    }

    public String getEntityName(String entityAlias) {
        ModelViewEntity.ModelMemberEntity mme = getModel().getMemberModelMemberEntity(entityAlias);
        if (mme == null) {
            throw new IllegalArgumentException("Invalid entity alias [" + entityAlias + "] for view-entity [" + getEntityName() + "]");
        }
        return mme.getEntityName();
    }

    public Map<String, GenericValue> getEntities() {
        return entities;
    }

    public Map<String, GenericValue> getEntities(Map<String, GenericValue> out) {
        out.putAll(getEntities());
        return out;
    }

    public GenericValue getEntity(String entityAlias) {
        return getEntities().get(entityAlias);
    }

    public Map<String, GenericValue> getRequiredEntities(Map<String, GenericValue> out) {
        for(String entityAlias : getModel().getRequiredEntityAliases()) {
            out.put(entityAlias, getEntity(entityAlias));
        }
        return out;
    }

    public Map<String, GenericValue> getOptionalEntities(Map<String, GenericValue> out) {
        for(String entityAlias : getModel().getOptionalEntityAliases()) {
            out.put(entityAlias, getEntity(entityAlias));
        }
        return out;
    }

    public GenericValue getOrMakeEntity(String entityAlias) {
        GenericValue entity = getEntity(entityAlias);
        if (entity == null) {
            entity = makeEntity(entityAlias);
            setEntity(entityAlias, entity);
        }
        return entity;
    }

    public GenericValue makeEntity(String entityAlias) {
        return getDelegator().makeValue(getEntityName(entityAlias));
    }

    public void setEntity(String entityAlias, GenericValue entity) {
        entities.put(entityAlias, entity);
    }

    public void removeEntity(String entityAlias) {
        entities.put(entityAlias, null);
    }

    public GenericValue setEntityField(String entityAlias, String fieldName, Object value) {
        GenericValue entity = getOrMakeEntity(entityAlias);
        entity.set(fieldName, value);
        return entity;
    }

    public void setField(String aliasName, Object value) {
        setField(aliasName, value);
    }

    protected void setField(ModelViewEntity.AliasMappings alias, Object value) {
        for (Map.Entry<String, List<String>> aliasFieldsEntry : alias.getEntityAliasFieldMap().entrySet()) {
            String entityAlias = aliasFieldsEntry.getKey();
            GenericValue entity = getEntity(entityAlias);
            if (entity == null) {
                if (isMakeEmptyOptionals() || getModel().isRequiredEntityAlias(entityAlias)) {
                    entity = makeEntity(entityAlias);
                    setEntity(entityAlias, entity);
                } else {
                    continue;
                }
            }
            for (String field : aliasFieldsEntry.getValue()) {
                entity.set(field, value);
            }
        }
    }

    public void setAllFields(Map<String, Object> fields) {
        setAllFields(fields, null);
    }

    public void setPkFields(Map<String, Object> fields) {
        setAllFields(fields, Boolean.TRUE);
    }

    public void setNonPkFields(Map<String, Object> fields) {
        setAllFields(fields, Boolean.FALSE);
    }

    protected void setAllFields(Map<String, Object> fields, Boolean pkMode) {
        if (isMakeEmptyOptionals()) {
            Map<String, GenericValue> newOptionals = new HashMap<>();
            for (Map.Entry<String, Object> fieldEntry : fields.entrySet()) {
                String aliasName = fieldEntry.getKey();
                Object value = fieldEntry.getValue();
                ModelViewEntity.AliasMappings alias = getModel().getAliasMappings(aliasName);
                if (alias == null) {
                    throw new IllegalArgumentException("Invalid field name [" + aliasName + "] for view-entity [" + getEntityName() + "]");
                }
                for (Map.Entry<String, List<String>> aliasFieldsEntry : alias.getRequiredEntityAliasFieldMap().entrySet()) {
                    ModelEntity targetModel = getModel().getMemberModelEntity(aliasFieldsEntry.getKey());
                    Map<String, Object> newFields = new HashMap<>();
                    for (String field : aliasFieldsEntry.getValue()) {
                        if (pkMode == null || (Boolean.TRUE.equals(pkMode) && targetModel.isPkField(field)) ||
                                (Boolean.FALSE.equals(pkMode) && targetModel.isNoPkField(field))) {
                            newFields.put(field, value);
                        }
                    }
                    if (!newFields.isEmpty()) {
                        GenericValue entity = getOrMakeEntity(aliasFieldsEntry.getKey());
                        entity.setAllFields(newFields);
                    }
                }
                for (Map.Entry<String, List<String>> aliasFieldsEntry : alias.getOptionalEntityAliasFieldMap().entrySet()) {
                    ModelEntity targetModel = getModel().getMemberModelEntity(aliasFieldsEntry.getKey());
                    Map<String, Object> newFields = new HashMap<>();
                    for (String field : aliasFieldsEntry.getValue()) {
                        if (pkMode == null || (Boolean.TRUE.equals(pkMode) && targetModel.isPkField(field)) ||
                                (Boolean.FALSE.equals(pkMode) && targetModel.isNoPkField(field))) {
                            newFields.put(field, value);
                        }
                    }
                    if (!newFields.isEmpty()) {
                        // if the rel-optional entity is already there, update it directly, otherwise update newOptionals
                        GenericValue entity = getEntity(aliasFieldsEntry.getKey());
                        if (entity == null) {
                            entity = makeEntity(aliasFieldsEntry.getKey());
                            newOptionals.put(aliasFieldsEntry.getKey(), entity);
                        }
                    }
                }
            }
            for (Map.Entry<String, GenericValue> entry : newOptionals.entrySet()) {
                GenericValue optEntity = entry.getValue();
                // If any non-pks set, use, otherwise discard
                for (String fieldName : optEntity.getModelEntity().getNoPkFieldNames()) {
                    if (optEntity.containsKey(fieldName)) {
                        setEntity(entry.getKey(), optEntity);
                        break;
                    }
                }
            }
        } else {
            for (Map.Entry<String, Object> fieldEntry : fields.entrySet()) {
                String aliasName = fieldEntry.getKey();
                Object value = fieldEntry.getValue();
                ModelViewEntity.AliasMappings alias = getModel().getAliasMappings(aliasName);
                if (alias == null) {
                    throw new IllegalArgumentException("Invalid field name [" + aliasName + "] for view-entity [" + getEntityName() + "]");
                }
                for (Map.Entry<String, List<String>> aliasFieldsEntry : alias.getEntityAliasFieldMap().entrySet()) {
                    ModelEntity targetModel = getModel().getMemberModelEntity(aliasFieldsEntry.getKey());
                    Map<String, Object> newFields = new HashMap<>();
                    for (String field : aliasFieldsEntry.getValue()) {
                        if (pkMode == null || (Boolean.TRUE.equals(pkMode) && targetModel.isPkField(field)) ||
                                (Boolean.FALSE.equals(pkMode) && targetModel.isNoPkField(field))) {
                            newFields.put(field, value);
                        }
                    }
                    if (!newFields.isEmpty()) {
                        GenericValue entity = getOrMakeEntity(aliasFieldsEntry.getKey());
                        entity.setAllFields(newFields);
                    }
                }
            }
        }
    }

    public void loadFromPkFields(Map<String, Object> fields) throws GenericEntityException {
        ViewEntityUpdater temp = makeEmpty();
        temp.setPkFields(fields);
        for(Map.Entry<String, GenericValue> entry : temp.getEntities().entrySet()) {
            GenericValue tempEntity = entry.getValue();
            if (tempEntity.containsPrimaryKey(true)) {
                GenericValue entity = getDelegator().findOne(tempEntity.getEntityName(), tempEntity.getPrimaryKey(), false);
                if (entity != null) {
                    setEntity(entry.getKey(), entity);
                }
            }
        }
    }

    public List<String> getMissingSinglePkEntityAliases() {
        List<String> entityAliases = new ArrayList<>(getEntities().size());
        for(String entityAlias : getEntityAliasCreateOrder()) {
            GenericValue entity = getEntity(entityAlias);
            if (entity == null) {
                if ((isMakeEmptyOptionals() || getModel().isRequiredEntityAlias(entityAlias)) && getModel().getMemberModelEntity(entityAlias).isSinglePk()) {
                    entityAliases.add(entityAlias);
                }
            } else if (!entity.containsPrimaryKey(true) && entity.getModelEntity().isSinglePk()) {
                entityAliases.add(entityAlias);
            }
        }
        return entityAliases;
    }

    public void createSetNextSeqIdAll() throws GenericEntityException {
        List<String> entityAliases = getMissingSinglePkEntityAliases();
        for(String entityAlias : entityAliases) {
            GenericValue entity = getOrMakeEntity(entityAlias);
            if (!entity.containsPrimaryKey(true)) {
                entity = entity.createSetNextSeqId();
                setEntity(entityAlias, entity);

                // If needed, apply the newly created PK to the view-linked entities
                String pkFieldName = entity.getModelEntity().getFirstPkFieldName();
                Object pkValue = entity.get(pkFieldName);
                ModelViewEntity.AliasMappings aliasMappings = getModel().getAliasMappingFromField(entityAlias, pkFieldName);
                for(Map.Entry<String, List<String>> fieldMapEntry : aliasMappings.getEntityAliasFieldMap().entrySet()) {
                    for(String fieldName : fieldMapEntry.getValue()) {
                        if (entityAlias.equals(fieldMapEntry.getKey()) && pkFieldName.equals(fieldName)) {
                            continue;
                        }
                        setField(aliasMappings, pkValue);
                    }
                }
            }
        }
    }

    public void checkRequiredEntities() throws GenericEntityException {
        for(String entityAlias : getModel().getRequiredEntityAliases()) {
            if (getEntity(entityAlias) == null) {
                throw new GenericEntityException("Missing entity or fields for entity alias [" + entityAlias + "] in view-entity [" + getEntityName() + "]");
            }
        }
    }

    public void createOrStoreAll(boolean createPks) throws GenericEntityException {
        if (createPks) {
            createSetNextSeqIdAll();
        }

        // NOTE: There's never a reason to make missing required entities here because they *should* have been made by setAllFields implicitly,
        // and then createSetNextSeqIdAll for required entities if applicable; if not it implies a required field is missing.
        checkRequiredEntities();

        // TODO: OPTIMIZE: createdEntities is ignored for now because it's technically possible for it to affect
        //  entities not in the returned createdEntities for now...
        for(String entityAlias : getEntityAliasCreateOrder()) {
            GenericValue entity = getEntity(entityAlias);
            if (entity == null) {
                continue;
            }
            if (entity.containsPrimaryKey(true)) {
                entity = entity.createOrStore();
                setEntity(entityAlias, entity);
            }
        }
    }

    public void createOrStoreAll() throws GenericEntityException {
        createOrStoreAll(false);
    }

    public void createUpdateEntities(Map<String, Object> fields, boolean createPks) throws GenericEntityException {
        loadFromPkFields(fields);
        setAllFields(fields);
        createOrStoreAll(createPks);
    }
}
