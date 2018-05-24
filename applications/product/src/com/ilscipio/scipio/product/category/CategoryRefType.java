package com.ilscipio.scipio.product.category;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelReader;

/**
 * Each type represents a range of possible entities, due to view-entities.
 */
@SuppressWarnings("serial")
public enum CategoryRefType {
    CATEGORY("ProductCategory", new CategoryResolver()),
    CATALOG_ASSOC("ProdCatalogCategory", new CatalogAssocResolver()),
    CATEGORY_ASSOC("ProductCategoryRollup", new CategoryAssocResolver());
    
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    private static final Map<String, CategoryRefType> entityNameMap;
    static {
        Map<String, CategoryRefType> map = new HashMap<>();

        for(CategoryRefType type : CategoryRefType.values()) {
            map.put(type.getPrimaryEntity(), type);
        }

        ModelReader modelReader;
        try {
            modelReader = ModelReader.getModelReader("default");
            
            Map<String, ModelEntity> modelEntities = modelReader.getEntityCache();
            modelEntities = filterModelEntitiesHavingFields(modelEntities, "productCategoryId");
            
            // Should include both main entity and views
            // NOTE: we do all this in advance, because ModelEntity isField is weak at runtime
            for(String entityName : filterModelEntitiesHavingFields(modelEntities, "prodCatalogId").keySet()) {
                map.put(entityName, CATALOG_ASSOC);
            }
            
            for(String entityName : filterModelEntitiesHavingFields(modelEntities, "parentProductCategoryId").keySet()) {
                map.put(entityName, CATEGORY_ASSOC);
            }
            
        } catch (Exception e) {
            Debug.logError(e, module);
        }
        
        entityNameMap = map;
    }
    
    private final String primaryEntity;
    private final Resolver resolver;
    
    private CategoryRefType(String primaryEntity, Resolver resolver) {
        this.primaryEntity = primaryEntity;
        this.resolver = resolver;
    }

    public String getPrimaryEntity() {
        return primaryEntity;
    }

    public Resolver getResolver() {
        return resolver;
    }
    
    public boolean isAlwaysPhysicalDepthZero() {
        return (this == CategoryRefType.CATALOG_ASSOC);
    }

    public static CategoryRefType fromEntity(ModelEntity modelEntity) {
        return entityNameMap.get(modelEntity.getEntityName());
    }
    
    public static CategoryRefType fromEntity(GenericValue entityValue) {
        return entityNameMap.get(entityValue.getEntityName());
    }
    
    public static abstract class Resolver implements Serializable {
        public abstract CategoryRefType getCategoryRefType();
        
        public GenericValue getProductCategory(GenericValue refTypeValue, boolean useCache) throws GenericEntityException {
            // default impl
            return refTypeValue.getDelegator().findOne("ProductCategory", UtilMisc.toMap("productCategoryId", refTypeValue.get("productCategoryId")), useCache);
        }
        public GenericValue getProductCategoryStrict(GenericValue refTypeValue, boolean useCache) throws GenericEntityException {
            // default impl
            return refTypeValue.getDelegator().findOne("ProductCategory", UtilMisc.toMap("productCategoryId", refTypeValue.get("productCategoryId")), useCache);
        }
        
        protected GenericValue findProductCategoryFromProductCategoryId(GenericValue refTypeValue, boolean useCache) throws GenericEntityException {
            return refTypeValue.getDelegator().findOne("ProductCategory", UtilMisc.toMap("productCategoryId", refTypeValue.get("productCategoryId")), useCache);
        }
    }
    
    private static class CategoryResolver extends Resolver {
        @Override
        public CategoryRefType getCategoryRefType() {
            return CATEGORY;
        }
        @Override
        public GenericValue getProductCategory(GenericValue refTypeValue, boolean useCache) {
            // NOTE: for now here we just assume if it's a view, it contains the whole ProductCategory
            return refTypeValue;
        }
        @Override
        public GenericValue getProductCategoryStrict(GenericValue refTypeValue, boolean useCache) throws GenericEntityException {
            if ("ProductCategory".equals(refTypeValue.getEntityName())) {
                return refTypeValue;
            } else {
                return findProductCategoryFromProductCategoryId(refTypeValue, useCache);
            }
        }
    }
    
    private static class CatalogAssocResolver extends Resolver {
        @Override
        public CategoryRefType getCategoryRefType() {
            return CATALOG_ASSOC;
        }
        // default impl for getProductCategory
    }

    private static class CategoryAssocResolver extends Resolver {
        @Override
        public CategoryRefType getCategoryRefType() {
            return CATEGORY_ASSOC;
        }
        // default impl for getProductCategory
    }
    
    // TODO: move util to better place
    private static Map<String, ModelEntity> filterModelEntitiesHavingFields(Map<String, ModelEntity> inModelEntities, Map<String, ModelEntity> outModelEntities, Collection<String> fieldNames) {
        for(Map.Entry<String, ModelEntity> entry : inModelEntities.entrySet()) {
            ModelEntity modelEntity = entry.getValue();
            if (modelEntity != null && modelEntity.areFields(fieldNames)) {
                outModelEntities.put(entry.getKey(), modelEntity);
            }
        }
        return outModelEntities;
    }
    
    // TODO: move util to better place
    private static Map<String, ModelEntity> filterModelEntitiesHavingFields(Map<String, ModelEntity> inModelEntities, String... fieldNames) {
        return filterModelEntitiesHavingFields(inModelEntities, new HashMap<String, ModelEntity>(), Arrays.asList(fieldNames));
    }
}