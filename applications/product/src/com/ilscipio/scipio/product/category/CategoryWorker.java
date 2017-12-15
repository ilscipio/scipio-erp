package com.ilscipio.scipio.product.category;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.category.CategoryContentWrapper;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;

import com.ilscipio.scipio.treeMenu.TreeDataItem;
import com.ilscipio.scipio.treeMenu.jsTree.JsTreeDataItem;
import com.ilscipio.scipio.treeMenu.jsTree.JsTreeDataItem.JsTreeDataItemState;

/**
 * SCIPIO: Category worker for novel/extra functionality.
 */
public abstract class CategoryWorker {

    public static final String module = CategoryWorker.class.getName();
    
    protected CategoryWorker() {
    }

    @SuppressWarnings("serial")
    public static class TreeBuildOptions implements Serializable {
        public static final Set<String> ENTITY_DATA_NAMES = UtilMisc.unmodifiableHashSet(
                "prodCatalog", "productStoreCatalog", "productCategory", "productCategoryRollup", "prodCatalogCategory", "product", "productCategoryMember"
        );
        
        public String library = null;
        public Map<String, Map<String, Object>> categoryStates = null;
        public Map<String, EntityDataSpec> includeEntityData = null;
        public int maxProductsPerCat = -1;
        public boolean useCategoryCache = false;
        public boolean useProductCache = false;

        public TreeBuildOptions(Map<String, ?> context) {
            this.library = (String) context.get("library");
            this.categoryStates = UtilGenerics.checkMap(context.get("categoryStates"));
            Map<String, ?> includeEntityData = UtilGenerics.checkMap(context.get("includeEntityData"));
            this.includeEntityData = makeEntityDataMap(Boolean.TRUE.equals(context.get("includeAllEntityData")), includeEntityData);
            Integer maxProductsPerCat = (Integer) context.get("maxProductsPerCat");
            this.maxProductsPerCat = (maxProductsPerCat == null) ? -1 : maxProductsPerCat;
            this.useCategoryCache = Boolean.TRUE.equals(context.get("useCategoryCache"));
            this.useProductCache = Boolean.TRUE.equals(context.get("useProductCache"));
        }
        
        public TreeBuildOptions() {
            this.includeEntityData = makeEntityDataMap(false, null);
        }
        
        public String getEntityDataTreeFieldName(String name) {
            return name + "Entity";
        }
        
        Map<String, Object> checkPutEntityDataField(Map<String, Object> treeItem, String name, GenericValue value) {
            Map<String, Object> entityData = checkGetEntityData(name, value);
            if (entityData != null) {
                treeItem.put(getEntityDataTreeFieldName(name), entityData);
            }
            return entityData;
        }
        
        Map<String, Object> checkGetEntityData(String name, GenericValue value) {
            if (includeEntityData == null) return null;
            EntityDataSpec spec = includeEntityData.get(name);
            if (spec == null) return null;
            else return spec.extractFields(value);
        }
        
        public static Map<String, EntityDataSpec> makeEntityDataMap(boolean includeAllDefault, Map<String, ?> customSpecs) {
            Map<String, EntityDataSpec> map = new HashMap<>();
            if (customSpecs != null) {
                for(String name : ENTITY_DATA_NAMES) {
                    Object spec = customSpecs.get(name);
                    map.put(name, (spec != null) ? EntityDataSpec.fromAny(spec) : EntityDataSpec.fromBoolean(includeAllDefault));
                }
            } else {
                for(String name : ENTITY_DATA_NAMES) {
                    map.put(name, EntityDataSpec.fromBoolean(includeAllDefault));
                }
            }
            return map;
        }
        
        public static class EntityDataSpec implements Serializable {
            public static final EntityDataSpec ALL = new EntityDataSpec(true, null, null);
            public static final EntityDataSpec NONE = new EntityDataSpec(false, null, null);

            public final boolean use;
            public final Collection<String> inclFields;
            public final Collection<String> exclFields;
            
            protected EntityDataSpec(boolean use, Collection<String> inclFields, Collection<String> exclFields) {
                this.use = use;
                this.inclFields = inclFields;
                this.exclFields = exclFields;
            }
            
            protected EntityDataSpec(Collection<String> includeFields, Collection<String> exclFields) {
                this.inclFields = includeFields;
                this.exclFields = exclFields;
                this.use = (includeFields != null || exclFields != null);
            }
            
            @SuppressWarnings("unchecked")
            protected EntityDataSpec(Map<String, ?> map) {
                this.inclFields = (Collection<String>) map.get("inclFields");
                this.exclFields = (Collection<String>) map.get("exclFields");
                Boolean use = UtilMisc.booleanValueVersatile(map.get("use"));
                if (use == null) use = (inclFields != null || exclFields != null);
                this.use = use;
            }

            @SuppressWarnings("unchecked")
            public static EntityDataSpec fromAny(Object spec) {
                if (spec == null) return NONE;
                else if (spec instanceof EntityDataSpec) return (EntityDataSpec) spec;
                else if (spec instanceof Map) return new EntityDataSpec((Map<String, ?>) spec);
                else if (spec instanceof Boolean || spec instanceof String) return UtilMisc.booleanValueVersatile(spec, false) ? ALL : NONE;
                else if (spec instanceof Collection) return new EntityDataSpec(true, (Collection<String>) spec, null);
                else return NONE;
            }
            
            public static EntityDataSpec fromBoolean(Boolean spec) {
                return Boolean.TRUE.equals(spec) ? ALL : NONE;
            }
            
            /**
             * NOTE: result not always a copy.
             */
            public Map<String, Object> extractFields(GenericValue value) {
                if (value == null || !use) return null;
                if (inclFields != null) return value.getFields(inclFields);
                else if (exclFields != null) return value.getFieldsExclude(exclFields);
                else return value;
            }
        }
    }
    
    /**
     * SCIPIO: Retrieves categories based on either a list of
     * ProductCategoryRollup or ProdCatalogCategory and returns a list of
     * TreeDataItem representing categories
     * WARN: avoid this method - may change - use buildCatalogTree service instead
     */
    public static List<? extends TreeDataItem> getTreeCategories(Delegator delegator, LocalDispatcher dispatcher, Locale locale,
            List<GenericValue> productCategories, String parentId, TreeBuildOptions options, Map<String, ? super GenericValue> categoryOutMap) throws GenericEntityException, GenericServiceException {
        List<TreeDataItem> treeDataItemList = new ArrayList<>();
        for (GenericValue productCategory : productCategories) {
            GenericValue category = null;
            GenericValue productCategoryRollup = null;
            GenericValue prodCatalogCategory = null;
            if (productCategory.getModelEntity().getEntityName().equals("ProductCategoryRollup")) {
                category = productCategory.getRelatedOne("CurrentProductCategory", options.useCategoryCache);
                productCategoryRollup = productCategory;
            } else if (productCategory.getModelEntity().getEntityName().equals("ProdCatalogCategory")) {
                category = productCategory.getRelatedOne("ProductCategory", options.useCategoryCache);
                prodCatalogCategory = productCategory;
            }
            if (category != null) {
                String categoryId = category.getString("productCategoryId");
                String nodeId = "category_" + categoryId;
                
                if (categoryOutMap != null) {
                    categoryOutMap.put(categoryId, category);
                }
                
                Boolean isParent = null;
                List<GenericValue> childProductCategoryRollups = EntityQuery.use(delegator).from("ProductCategoryRollup")
                        .where("parentProductCategoryId", category.getString("productCategoryId")).filterByDate().orderBy("sequenceNum").cache(options.useCategoryCache).queryList();
                if (UtilValidate.isNotEmpty(childProductCategoryRollups)) {
                    treeDataItemList.addAll(
                            getTreeCategories(delegator, dispatcher, locale, childProductCategoryRollups, nodeId, options, categoryOutMap));
                    isParent = true;
                }
                
                // NOTE: we may need do the query even if maxProductsPerCat is zero to determine isParent flag
                if (options.maxProductsPerCat != 0 || isParent == null) { 
                    // SCIPIO: 2017-10-13: NOTE: now doing our own query here, service call was too limited
                    //Map<String, Object> productCategoryMembers = dispatcher.runSync("getProductCategoryMembers",
                    //        UtilMisc.toMap("categoryId", productCategory.getString("productCategoryId")));
                    EntityQuery query = EntityQuery.use(delegator).from("ProductCategoryMember")
                            .where("productCategoryId", category.getString("productCategoryId")).filterByDate()
                            .orderBy("sequenceNum").cache(options.useProductCache);
                    if (options.maxProductsPerCat > 0) {
                        query = query.maxRows(options.maxProductsPerCat);
                    } else if (options.maxProductsPerCat == 0 && isParent == null) {
                        query = query.select("productId");
                        query = query.maxRows(1);
                    }
                    List<GenericValue> productCategoryMembers = query.queryList();
                    if (UtilValidate.isNotEmpty(productCategoryMembers)) {
                        isParent = true;
                        if (options.maxProductsPerCat != 0) {
                            treeDataItemList.addAll(CategoryWorker.getTreeProducts(dispatcher, locale, productCategoryMembers,
                                    nodeId, options));
                        }
                    }
                }
    
                String categoryName = null;
                CategoryContentWrapper wrapper = new CategoryContentWrapper(dispatcher, category, locale, null, options.useCategoryCache);
                categoryName = wrapper.get("CATEGORY_NAME");
                if (UtilValidate.isEmpty(categoryName)) {
                    // 2016-03-22: Some categories don't have a name but have description
                    categoryName = wrapper.get("DESCRIPTION");
                    if (UtilValidate.isEmpty(categoryName)) {
                        categoryName = category.getString("productCategoryId");
                    }
                }
    
                if ("jsTree".equals(options.library)) {
                    JsTreeDataItem dataItem = null;
                    Map<String, Object> effState = UtilMisc.toMap("opened", false, "selected", false);
                    if (options.categoryStates != null && options.categoryStates.get(categoryId) != null) {
                        effState.putAll(options.categoryStates.get(categoryId));
                    }
                    dataItem = new JsTreeDataItem(nodeId, categoryId, categoryName + " [" + categoryId + "]", 
                            "jstree-folder", new JsTreeDataItemState(effState), parentId);
                    dataItem.setType("category");
                    if (UtilValidate.isNotEmpty(dataItem))
                        treeDataItemList.add(dataItem);
                    options.checkPutEntityDataField(dataItem, "productCategory", category);
                    options.checkPutEntityDataField(dataItem, "productCategoryRollup", productCategoryRollup);
                    options.checkPutEntityDataField(dataItem, "prodCatalogCategory", prodCatalogCategory);
                    dataItem.put("isParent", isParent != null ? isParent : false);
                }
            }
        }
        return treeDataItemList;
    }
    
    // TODO
//    public static Map<String, Map<String, Object>> getLocalizedCategoryContentTextFields(Delegator delegator, LocalDispatcher dispatcher, 
//            String productCategoryId, Collection<String> dataResourceTypeIdList, boolean useCache) {
//        
//    }

    /**
     * SCIPIO: Retrieves products members for a given category and returns a list
     * of JsTreeDataItem representing products
     * WARN: avoid this method - may change - use buildCatalogTree service instead
     */
    public static List<? extends TreeDataItem> getTreeProducts(LocalDispatcher dispatcher, Locale locale, List<GenericValue> productCategoryMembers,
            String parentId, TreeBuildOptions options) throws GenericEntityException {
        List<TreeDataItem> products = new ArrayList<>();
        if (UtilValidate.isNotEmpty(productCategoryMembers)) {
            for (GenericValue productCategoryMember : productCategoryMembers) {
                GenericValue product = productCategoryMember.getRelatedOne("Product", options.useProductCache);
    
                String productId = product.getString("productId");
                String productName = product.getString("productName");
                if (UtilValidate.isEmpty(productName)) {
                    productName = productId;
                    ProductContentWrapper wrapper = new ProductContentWrapper(dispatcher, product, locale, null, options.useProductCache);
                    if (UtilValidate.isNotEmpty(wrapper.get("PRODUCT_NAME")))
                        productName = wrapper.get("PRODUCT_NAME");
                }
    
                if ("jsTree".equals(options.library)) {
                    JsTreeDataItem dataItem = new JsTreeDataItem("product_" + productId, productId, productName + " [" + productId + "]", 
                            "jstree-file", new JsTreeDataItemState(false, false), parentId);
                    dataItem.setType("product");
                    products.add(dataItem);
                    options.checkPutEntityDataField(dataItem, "product", product);
                    options.checkPutEntityDataField(dataItem, "productCategoryMember", productCategoryMember);
                    // TODO: REVIEW: this flag doesn't consider more complex associations to Product
                    dataItem.put("isParent", false);
                }
            }
        }
        return products;
    }

    public static List<GenericValue> extractAllProductCategoryFromTreeItems(List<? extends Map<String, Object>> treeItems) {
        List<GenericValue> outList = new LinkedList<>();
        extractAllProductCategoryFromTreeItems(treeItems, outList);
        return outList;
    }
    
    public static void extractAllProductCategoryFromTreeItems(List<? extends Map<String, Object>> treeItems, List<GenericValue> outList) {
        if (treeItems == null) return;
        for(Map<String, Object> item : treeItems) {
            String type = (String) item.get("type");
            if ("category".equals(type)) {
                GenericValue productCategory = (GenericValue) item.get("productCategoryEntity");
                if (productCategory != null) {
                    outList.add(productCategory);
                }
            }
        }
    }
    
}
