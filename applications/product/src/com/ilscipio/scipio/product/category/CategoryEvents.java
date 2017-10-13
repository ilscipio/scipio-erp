package com.ilscipio.scipio.product.category;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.product.catalog.CatalogWorker;
import org.ofbiz.product.category.CategoryContentWrapper;
import org.ofbiz.product.category.CategoryWorker;

/**
 * SCIPIO: Category events for novel/extra functionality.
 */
public abstract class CategoryEvents {

    public static final String module = CategoryEvents.class.getName();
    
    protected CategoryEvents() {
    }

    /**
     * @deprecated SCIPIO: Use the new buildCatalogJsTree service instead which is compliant with jsTree latest version. 
     * TODO: Implement the required events so they can be used to populate a jsTree via ajax too.
     * <p>
     * Please note : the structure of map in this function is according to the
     * JSON data map of the jsTree
     */
    @SuppressWarnings("unchecked")
    @Deprecated
    public static String getChildCategoryTree(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        String productCategoryId = request.getParameter("productCategoryId");
        String isCatalog = request.getParameter("isCatalog");
        String isCategoryType = request.getParameter("isCategoryType");
        String onclickFunction = request.getParameter("onclickFunction");
        String additionParam = request.getParameter("additionParam");
        String hrefString = request.getParameter("hrefString");
        String hrefString2 = request.getParameter("hrefString2");
        String entityName = null;
        String primaryKeyName = null;
    
        if (isCatalog.equals("true")) {
            entityName = "ProdCatalog";
            primaryKeyName = "prodCatalogId";
        } else {
            entityName = "ProductCategory";
            primaryKeyName = "productCategoryId";
        }
    
        List categoryList = new ArrayList<>();
        List<GenericValue> childOfCats;
        List<String> sortList = org.ofbiz.base.util.UtilMisc.toList("sequenceNum", "title");
    
        try {
            GenericValue category = EntityQuery.use(delegator).from(entityName).where(primaryKeyName, productCategoryId).queryOne();
            if (UtilValidate.isNotEmpty(category)) {
                if (isCatalog.equals("true") && isCategoryType.equals("false")) {
                    CategoryWorker.getRelatedCategories(request, "ChildCatalogList", CatalogWorker.getCatalogTopCategoryId(request, productCategoryId), true);
                    childOfCats = EntityUtil.filterByDate((List<GenericValue>) request.getAttribute("ChildCatalogList"));
    
                } else if (isCatalog.equals("false") && isCategoryType.equals("false")) {
                    childOfCats = EntityQuery.use(delegator).from("ProductCategoryRollupAndChild").where("parentProductCategoryId", productCategoryId)
                            .filterByDate().queryList();
                } else {
                    childOfCats = EntityQuery.use(delegator).from("ProdCatalogCategory").where("prodCatalogId", productCategoryId).filterByDate().queryList();
                }
                if (UtilValidate.isNotEmpty(childOfCats)) {
    
                    for (GenericValue childOfCat : childOfCats) {
    
                        Object catId = null;
                        String catNameField = null;
    
                        catId = childOfCat.get("productCategoryId");
                        catNameField = "CATEGORY_NAME";
    
                        Map josonMap = new HashMap<>();
                        List<GenericValue> childList = null;
    
                        // Get the child list of chosen category
                        childList = EntityQuery.use(delegator).from("ProductCategoryRollup").where("parentProductCategoryId", catId).filterByDate().queryList();
    
                        // Get the chosen category information for the
                        // categoryContentWrapper
                        GenericValue cate = EntityQuery.use(delegator).from("ProductCategory").where("productCategoryId", catId).queryOne();
    
                        // If chosen category's child exists, then put the arrow
                        // before category icon
                        if (UtilValidate.isNotEmpty(childList)) {
                            josonMap.put("state", "closed");
                        }
                        Map dataMap = new HashMap<>();
                        Map dataAttrMap = new HashMap<>();
                        CategoryContentWrapper categoryContentWrapper = new CategoryContentWrapper(cate, request);
    
                        String title = null;
                        // SCIPIO: Do NOT HTML-escape this here
                        if (UtilValidate.isNotEmpty(categoryContentWrapper.get(catNameField))) {
                            title = new StringBuffer(categoryContentWrapper.get(catNameField)).append(" [").append(catId).append("]")
                                    .toString();
                            dataMap.put("title", title);
                        } else {
                            title = catId.toString();
                            dataMap.put("title", catId);
                        }
                        dataAttrMap.put("onClick", onclickFunction + "('" + catId + additionParam + "')");
    
                        String hrefStr = hrefString + catId;
                        if (UtilValidate.isNotEmpty(hrefString2)) {
                            hrefStr = hrefStr + hrefString2;
                        }
                        dataAttrMap.put("href", hrefStr);
    
                        dataMap.put("attr", dataAttrMap);
                        josonMap.put("data", dataMap);
                        Map attrMap = new HashMap<>();
                        attrMap.put("id", catId);
                        attrMap.put("isCatalog", false);
                        attrMap.put("rel", "CATEGORY");
                        josonMap.put("attr", attrMap);
                        josonMap.put("sequenceNum", childOfCat.get("sequenceNum"));
                        josonMap.put("title", title);
    
                        categoryList.add(josonMap);
                    }
                    List<Map<Object, Object>> sortedCategoryList = UtilMisc.sortMaps(categoryList, sortList);
                    request.setAttribute("treeData", sortedCategoryList);
                }
            }
        } catch (GenericEntityException e) {
            e.printStackTrace();
            return "error";
        }
        return "success";
    }

}
