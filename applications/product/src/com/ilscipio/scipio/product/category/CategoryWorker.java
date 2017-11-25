package com.ilscipio.scipio.product.category;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
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

    /**
     * SCIPIO: Retrieves categories based on either a list of
     * ProductCategoryRollup or ProdCatalogCategory and returns a list of
     * TreeDataItem representing categories
     * WARN: avoid this method - may change - use buildCatalogTree service instead
     */
    public static List<? extends TreeDataItem> getTreeCategories(Delegator delegator, LocalDispatcher dispatcher, Locale locale,
            List<GenericValue> productCategories, String library, String parentId, Map<String, Map<String, Object>> categoryStates, 
            boolean includeCategoryData, boolean includeProductData, int maxProductsPerCat, boolean useCategoryCache, boolean useProductCache) throws GenericEntityException, GenericServiceException {
        List<TreeDataItem> treeDataItemList = new ArrayList<>();
        for (GenericValue productCategory : productCategories) {
            GenericValue category = null;
            GenericValue productCategoryRollup = null;
            GenericValue prodCatalogCategory = null;
            if (productCategory.getModelEntity().getEntityName().equals("ProductCategoryRollup")) {
                category = productCategory.getRelatedOne("CurrentProductCategory", useCategoryCache);
                productCategoryRollup = productCategory;
            } else if (productCategory.getModelEntity().getEntityName().equals("ProdCatalogCategory")) {
                category = productCategory.getRelatedOne("ProductCategory", useCategoryCache);
                prodCatalogCategory = productCategory;
            }
            if (category != null) {
                String categoryId = category.getString("productCategoryId");
                String nodeId = "category_" + categoryId;
                
                Boolean isParent = null;
                List<GenericValue> childProductCategoryRollups = EntityQuery.use(delegator).from("ProductCategoryRollup")
                        .where("parentProductCategoryId", category.getString("productCategoryId")).filterByDate().orderBy("sequenceNum").cache(useCategoryCache).queryList();
                if (UtilValidate.isNotEmpty(childProductCategoryRollups)) {
                    treeDataItemList.addAll(
                            getTreeCategories(delegator, dispatcher, locale, childProductCategoryRollups, library, nodeId, 
                                        categoryStates, includeCategoryData, includeProductData, maxProductsPerCat, useCategoryCache, useProductCache));
                    isParent = true;
                }
                
                // NOTE: we may need do the query even if maxProductsPerCat is zero to determine isParent flag
                if (maxProductsPerCat != 0 || isParent == null) { 
                    // SCIPIO: 2017-10-13: NOTE: now doing our own query here, service call was too limited
                    //Map<String, Object> productCategoryMembers = dispatcher.runSync("getProductCategoryMembers",
                    //        UtilMisc.toMap("categoryId", productCategory.getString("productCategoryId")));
                    EntityQuery query = EntityQuery.use(delegator).from("ProductCategoryMember")
                            .where("productCategoryId", category.getString("productCategoryId")).filterByDate()
                            .orderBy("sequenceNum").cache(useProductCache);
                    if (maxProductsPerCat > 0) {
                        query = query.maxRows(maxProductsPerCat);
                    } else if (maxProductsPerCat == 0 && isParent == null) {
                        query = query.select("productId");
                        query = query.maxRows(1);
                    }
                    List<GenericValue> productCategoryMembers = query.queryList();
                    if (UtilValidate.isNotEmpty(productCategoryMembers)) {
                        isParent = true;
                        if (maxProductsPerCat != 0) {
                            treeDataItemList.addAll(CategoryWorker.getTreeProducts(dispatcher, locale, productCategoryMembers, library,
                                    nodeId, includeProductData, useProductCache));
                        }
                    }
                }
    
                String categoryName = null;
                CategoryContentWrapper wrapper = new CategoryContentWrapper(dispatcher, category, locale, null, useCategoryCache);
                categoryName = wrapper.get("CATEGORY_NAME");
                if (UtilValidate.isEmpty(categoryName)) {
                    // 2016-03-22: Some categories don't have a name but have description
                    categoryName = wrapper.get("DESCRIPTION");
                    if (UtilValidate.isEmpty(categoryName)) {
                        categoryName = category.getString("productCategoryId");
                    }
                }
    
                if (library.equals("jsTree")) {
                    JsTreeDataItem dataItem = null;
                    Map<String, Object> effState = UtilMisc.toMap("opened", false, "selected", false);
                    if (categoryStates != null && categoryStates.get(categoryId) != null) {
                        effState.putAll(categoryStates.get(categoryId));
                    }
                    dataItem = new JsTreeDataItem(nodeId, categoryId, categoryName + " [" + categoryId + "]", 
                            "jstree-folder", new JsTreeDataItemState(effState), parentId);
                    dataItem.setType("category");
                    if (UtilValidate.isNotEmpty(dataItem))
                        treeDataItemList.add(dataItem);
                    if (includeCategoryData) {
                        dataItem.put("productCategoryEntity", category);
                        dataItem.put("productCategoryRollupEntity", productCategoryRollup);
                        dataItem.put("prodCatalogCategoryEntity", prodCatalogCategory);
                    }
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
            String library, String parentId, boolean includeData, boolean useCache) throws GenericEntityException {
        List<TreeDataItem> products = new ArrayList<>();
        if (UtilValidate.isNotEmpty(productCategoryMembers)) {
            for (GenericValue productCategoryMember : productCategoryMembers) {
                GenericValue product = productCategoryMember.getRelatedOne("Product", useCache);
    
                String productId = product.getString("productId");
                String productName = product.getString("productName");
                if (UtilValidate.isEmpty(productName)) {
                    productName = productId;
                    ProductContentWrapper wrapper = new ProductContentWrapper(dispatcher, product, locale, null, useCache);
                    if (UtilValidate.isNotEmpty(wrapper.get("PRODUCT_NAME")))
                        productName = wrapper.get("PRODUCT_NAME");
                }
    
                if (library.equals("jsTree")) {
                    JsTreeDataItem dataItem = new JsTreeDataItem("product_" + productId, productId, productName + " [" + productId + "]", 
                            "jstree-file", new JsTreeDataItemState(false, false), parentId);
                    dataItem.setType("product");
                    products.add(dataItem);
                    if (includeData) {
                        dataItem.put("productEntity", product);
                        dataItem.put("productCategoryMemberEntity", productCategoryMember);
                    }
                    // TODO: REVIEW: this flag doesn't consider more complex associations to Product
                    dataItem.put("isParent", false);
                }
            }
        }
        return products;
    }

    // TODO
//  public static Map<String, Map<String, Object>> getLocalizedProductContentTextFields(Delegator delegator, LocalDispatcher dispatcher, 
//      String productId, Collection<String> dataResourceTypeIdList, boolean useCache) {
//
//  }
    
}
