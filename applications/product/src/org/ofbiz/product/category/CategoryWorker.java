/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.product.category;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilFormatOut;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import javolution.util.FastList;

/**
 * CategoryWorker - Worker class to reduce code in JSPs.
 */
public class CategoryWorker {

    public static final String module = CategoryWorker.class.getName();

    private CategoryWorker() {
    }

    /**
     * Gets catalog top category.
     * <p>
     * SCIPIO: NOTE (2017-08-15): This stock method relies entirely on the session attribute and request parameter
     * <code>CATALOG_TOP_CATEGORY</code>; in stock ofbiz, it was intended to be used in backend.
     * For store implementations, the method that you most likely want 
     * is {@link org.ofbiz.product.catalog.CatalogWorker#getCatalogTopCategoryId}.
     */
    public static String getCatalogTopCategory(ServletRequest request, String defaultTopCategory) {
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        Map<String, Object> requestParameters = UtilHttp.getParameterMap(httpRequest);
        String topCatName = null;
        boolean fromSession = false;

        // first see if a new category was specified as a parameter
        topCatName = (String) requestParameters.get("CATALOG_TOP_CATEGORY");
        // if no parameter, try from session
        if (topCatName == null) {
            topCatName = (String) httpRequest.getSession().getAttribute("CATALOG_TOP_CATEGORY");
            if (topCatName != null)
                fromSession = true;
        }
        // if nothing else, just use a default top category name
        if (topCatName == null)
            topCatName = defaultTopCategory;
        if (topCatName == null)
            topCatName = "CATALOG1";

        if (!fromSession) {
            if (Debug.infoOn())
                Debug.logInfo("[CategoryWorker.getCatalogTopCategory] Setting new top category: " + topCatName, module);
            httpRequest.getSession().setAttribute("CATALOG_TOP_CATEGORY", topCatName);
        }
        return topCatName;
    }

    public static void getCategoriesWithNoParent(ServletRequest request, String attributeName) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        Collection<GenericValue> results = FastList.newInstance();

        try {
            Collection<GenericValue> allCategories = EntityQuery.use(delegator).from("ProductCategory").queryList();

            for (GenericValue curCat : allCategories) {
                Collection<GenericValue> parentCats = curCat.getRelated("CurrentProductCategoryRollup", null, null, true);

                if (parentCats.isEmpty())
                    results.add(curCat);
            }
        } catch (GenericEntityException e) {
            Debug.logWarning(e, module);
        }
        request.setAttribute(attributeName, results);
    }

    public static void getRelatedCategories(ServletRequest request, String attributeName, boolean limitView) {
        Map<String, Object> requestParameters = UtilHttp.getParameterMap((HttpServletRequest) request);
        String requestId = null;

        requestId = UtilFormatOut.checkNull((String) requestParameters.get("catalog_id"), (String) requestParameters.get("CATALOG_ID"),
                (String) requestParameters.get("category_id"), (String) requestParameters.get("CATEGORY_ID"));

        if (requestId.equals(""))
            return;
        if (Debug.infoOn())
            Debug.logInfo("[CategoryWorker.getRelatedCategories] RequestID: " + requestId, module);
        getRelatedCategories(request, attributeName, requestId, limitView);
    }

    public static void getRelatedCategories(ServletRequest request, String attributeName, String parentId, boolean limitView) {
        getRelatedCategories(request, attributeName, parentId, limitView, false);
    }

    public static void getRelatedCategories(ServletRequest request, String attributeName, String parentId, boolean limitView, boolean excludeEmpty) {
        List<GenericValue> categories = getRelatedCategoriesRet(request, attributeName, parentId, limitView, excludeEmpty);

        if (!categories.isEmpty())
            request.setAttribute(attributeName, categories);
    }

    public static List<GenericValue> getRelatedCategoriesRet(ServletRequest request, String attributeName, String parentId, boolean limitView) {
        return getRelatedCategoriesRet(request, attributeName, parentId, limitView, false);
    }

    public static List<GenericValue> getRelatedCategoriesRet(ServletRequest request, String attributeName, String parentId, boolean limitView,
            boolean excludeEmpty) {
        return getRelatedCategoriesRet(request, attributeName, parentId, limitView, excludeEmpty, false);
    }

    public static List<GenericValue> getRelatedCategoriesRet(ServletRequest request, String attributeName, String parentId, boolean limitView,
            boolean excludeEmpty, boolean recursive) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");

        return getRelatedCategoriesRet(delegator, attributeName, parentId, limitView, excludeEmpty, recursive);
    }

    public static List<GenericValue> getRelatedCategoriesRet(Delegator delegator, String attributeName, String parentId, boolean limitView,
            boolean excludeEmpty, boolean recursive) {
        List<GenericValue> categories = FastList.newInstance();

        if (Debug.verboseOn())
            Debug.logVerbose("[CategoryWorker.getRelatedCategories] ParentID: " + parentId, module);

        List<GenericValue> rollups = null;

        try {
            rollups = EntityQuery.use(delegator).from("ProductCategoryRollup").where("parentProductCategoryId", parentId).orderBy("sequenceNum").cache(true)
                    .queryList();
            if (limitView) {
                rollups = EntityUtil.filterByDate(rollups, true);
            }
        } catch (GenericEntityException e) {
            Debug.logWarning(e.getMessage(), module);
        }
        if (rollups != null) {
            // Debug.logInfo("Rollup size: " + rollups.size(), module);
            for (GenericValue parent : rollups) {
                // Debug.logInfo("Adding child of: " +
                // parent.getString("parentProductCategoryId"), module);
                GenericValue cv = null;

                try {
                    cv = parent.getRelatedOne("CurrentProductCategory", true);
                } catch (GenericEntityException e) {
                    Debug.logWarning(e.getMessage(), module);
                }
                if (cv != null) {
                    if (excludeEmpty) {
                        if (!isCategoryEmpty(cv)) {
                            // Debug.logInfo("Child : " +
                            // cv.getString("productCategoryId") + " is not
                            // empty.", module);
                            categories.add(cv);
                            if (recursive) {
                                categories.addAll(getRelatedCategoriesRet(delegator, attributeName, cv.getString("productCategoryId"), limitView, excludeEmpty,
                                        recursive));
                            }
                        }
                    } else {
                        categories.add(cv);
                        if (recursive) {
                            categories.addAll(
                                    getRelatedCategoriesRet(delegator, attributeName, cv.getString("productCategoryId"), limitView, excludeEmpty, recursive));
                        }
                    }
                }
            }
        }
        return categories;
    }

    public static boolean isCategoryEmpty(GenericValue category) {
        boolean empty = true;
        long members = categoryMemberCount(category);
        // Debug.logInfo("Category : " + category.get("productCategoryId") + "
        // has " + members + " members", module);
        if (members > 0) {
            empty = false;
        }

        if (empty) {
            long rollups = categoryRollupCount(category);
            // Debug.logInfo("Category : " + category.get("productCategoryId") +
            // " has " + rollups + " rollups", module);
            if (rollups > 0) {
                empty = false;
            }
        }

        return empty;
    }

    public static long categoryMemberCount(GenericValue category) {
        if (category == null)
            return 0;
        Delegator delegator = category.getDelegator();
        long count = 0;
        try {
            count = EntityQuery.use(delegator).from("ProductCategoryMember").where("productCategoryId", category.getString("productCategoryId")).queryCount();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }
        return count;
    }

    public static long categoryRollupCount(GenericValue category) {
        if (category == null)
            return 0;
        Delegator delegator = category.getDelegator();
        long count = 0;
        try {
            count = EntityQuery.use(delegator).from("ProductCategoryRollup").where("parentProductCategoryId", category.getString("productCategoryId"))
                    .queryCount();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }
        return count;
    }

    private static EntityCondition buildCountCondition(String fieldName, String fieldValue) {
        List<EntityCondition> orCondList = FastList.newInstance();
        orCondList.add(EntityCondition.makeCondition("thruDate", EntityOperator.GREATER_THAN, UtilDateTime.nowTimestamp()));
        orCondList.add(EntityCondition.makeCondition("thruDate", EntityOperator.EQUALS, null));
        EntityCondition orCond = EntityCondition.makeCondition(orCondList, EntityOperator.OR);

        List<EntityCondition> andCondList = FastList.newInstance();
        andCondList.add(EntityCondition.makeCondition("fromDate", EntityOperator.LESS_THAN, UtilDateTime.nowTimestamp()));
        andCondList.add(EntityCondition.makeCondition(fieldName, EntityOperator.EQUALS, fieldValue));
        andCondList.add(orCond);
        EntityCondition andCond = EntityCondition.makeCondition(andCondList, EntityOperator.AND);

        return andCond;
    }

    public static void setTrail(ServletRequest request, String currentCategory) {
        Map<String, Object> requestParameters = UtilHttp.getParameterMap((HttpServletRequest) request);
        String previousCategory = (String) requestParameters.get("pcategory");
        setTrail(request, currentCategory, previousCategory);
    }

    public static void setTrail(ServletRequest request, String currentCategory, String previousCategory) {
        if (Debug.verboseOn())
            Debug.logVerbose("[CategoryWorker.setTrail] Start: previousCategory=" + previousCategory + " currentCategory=" + currentCategory, module);

        // if there is no current category, just return and do nothing to that
        // the last settings will stay
        if (UtilValidate.isEmpty(currentCategory)) {
            return;
        }

        // always get the last crumb list
        List<String> crumb = getTrail(request);
        crumb = adjustTrail(crumb, currentCategory, previousCategory);
        setTrail(request, crumb);
    }

    public static List<String> adjustTrail(List<String> origTrail, String currentCategoryId, String previousCategoryId) {
        List<String> trail = FastList.newInstance();
        if (origTrail != null) {
            trail.addAll(origTrail);
        }

        // if no previous category was specified, check to see if
        // currentCategory is in the list
        if (UtilValidate.isEmpty(previousCategoryId)) {
            if (trail.contains(currentCategoryId)) {
                // if cur category is in crumb, remove everything after it and
                // return
                int cindex = trail.lastIndexOf(currentCategoryId);

                if (cindex < (trail.size() - 1)) {
                    for (int i = trail.size() - 1; i > cindex; i--) {
                        trail.remove(i);
                        // FIXME can be removed ?
                        // String deadCat = trail.remove(i);
                        // if (Debug.infoOn())
                        // Debug.logInfo("[CategoryWorker.setTrail] Removed
                        // after current category index: " + i + " catname: " +
                        // deadCat, module);
                    }
                }
                return trail;
            } else {
                // current category is not in the list, and no previous category
                // was specified, go back to the beginning
                trail.clear();
                trail.add("TOP");
                if (UtilValidate.isNotEmpty(previousCategoryId)) {
                    trail.add(previousCategoryId);
                }
                // if (Debug.infoOn()) Debug.logInfo("[CategoryWorker.setTrail]
                // Starting new list, added TOP and previousCategory: " +
                // previousCategoryId, module);
            }
        }

        if (!trail.contains(previousCategoryId)) {
            // previous category was NOT in the list, ERROR, start over
            // if (Debug.infoOn()) Debug.logInfo("[CategoryWorker.setTrail]
            // previousCategory (" + previousCategoryId + ") was not in the
            // crumb list, position is lost, starting over with TOP", module);
            trail.clear();
            trail.add("TOP");
            if (UtilValidate.isNotEmpty(previousCategoryId)) {
                trail.add(previousCategoryId);
            }
        } else {
            // remove all categories after the previous category, preparing for
            // adding the current category
            int index = trail.indexOf(previousCategoryId);
            if (index < (trail.size() - 1)) {
                for (int i = trail.size() - 1; i > index; i--) {
                    trail.remove(i);
                    // FIXME can be removed ?
                    // String deadCat = trail.remove(i);
                    // if (Debug.infoOn())
                    // Debug.logInfo("[CategoryWorker.setTrail] Removed after
                    // current category index: " + i + " catname: " + deadCat,
                    // module);
                }
            }
        }

        // add the current category to the end of the list
        trail.add(currentCategoryId);
        if (Debug.verboseOn())
            Debug.logVerbose("[CategoryWorker.setTrail] Continuing list: Added currentCategory: " + currentCategoryId, module);

        return trail;
    }

    public static List<String> getTrail(ServletRequest request) {
        HttpSession session = ((HttpServletRequest) request).getSession();
        // SCIPIO: 2016-13-22: Trail must also be checked in request attributes to ensure the request is consistent
        //List<String> crumb = UtilGenerics.checkList(session.getAttribute("_BREAD_CRUMB_TRAIL_"));
        List<String> crumb = UtilGenerics.checkList(request.getAttribute("_BREAD_CRUMB_TRAIL_"));
        if (crumb == null) {
            crumb = UtilGenerics.checkList(session.getAttribute("_BREAD_CRUMB_TRAIL_"));
        }
        return crumb;
    }
    
    /**
     * SCIPIO: Version that returns a copy of the trail without the TOP category.
     */
    public static List<String> getTrailNoTop(ServletRequest request) {
        List<String> fullTrail = getTrail(request);
        List<String> res = null;
        if (fullTrail != null) {
            res = new ArrayList<String>(fullTrail.size());
            Iterator<String> it = fullTrail.iterator();
            while (it.hasNext()) {
                String next = it.next(); // check first
                if (!"TOP".equals(next)) {
                    res.add(next);
                }
            }
        }
        return res;
    }    

    /**
     * Sets breadcrumbs trail to the exact given value.
     * <p>
     * SCIPIO: This is modified to accept a onlyIfNewInRequest boolean that will check to see
     * if a breadcrumb was already set in the request. Default is false. 
     * This is needed in some places to prevent squashing breadcrumbs set in servlets and filters.
     */
    public static List<String> setTrail(ServletRequest request, List<String> crumb, boolean onlyIfNewInRequest) {
        HttpSession session = ((HttpServletRequest) request).getSession();
        if (onlyIfNewInRequest) {
            // SCIPIO: Check if was already set
            if (request.getAttribute("_BREAD_CRUMB_TRAIL_") == null) {
                session.setAttribute("_BREAD_CRUMB_TRAIL_", crumb);
                // SCIPIO: 2016-13-22: Trail must also be set in request attributes to ensure the request is consistent
                request.setAttribute("_BREAD_CRUMB_TRAIL_", crumb);
            }
        }
        else {
            // SCIPIO: stock case
            session.setAttribute("_BREAD_CRUMB_TRAIL_", crumb);
            // SCIPIO: 2016-13-22: Trail must also be set in request attributes to ensure the request is consistent
            request.setAttribute("_BREAD_CRUMB_TRAIL_", crumb);
        }
        return crumb;
    }
    
    /**
     * Sets breadcrumbs trail to the exact given value.
     */
    public static List<String> setTrail(ServletRequest request, List<String> crumb) {
        return setTrail(request, crumb, false);
    }
    
    /**
     * SCIPIO: Sets breadcrumbs trail to the exact given value but only if not yet set during request.
     */
    public static List<String> setTrailIfFirstInRequest(ServletRequest request, List<String> crumb) {
        return setTrail(request, crumb, true);
    } 

    public static boolean checkTrailItem(ServletRequest request, String category) {
        List<String> crumb = getTrail(request);

        if (crumb != null && crumb.contains(category)) {
            return true;
        } else {
            return false;
        }
    }

    public static String lastTrailItem(ServletRequest request) {
        List<String> crumb = getTrail(request);

        if (UtilValidate.isNotEmpty(crumb)) {
            return crumb.get(crumb.size() - 1);
        } else {
            return null;
        }
    }

    public static boolean isProductInCategory(Delegator delegator, String productId, String productCategoryId) throws GenericEntityException {
        if (productCategoryId == null)
            return false;
        if (UtilValidate.isEmpty(productId))
            return false;

        List<GenericValue> productCategoryMembers = EntityQuery.use(delegator).from("ProductCategoryMember")
                .where("productCategoryId", productCategoryId, "productId", productId).cache(true).filterByDate().queryList();
        if (UtilValidate.isEmpty(productCategoryMembers)) {
            // before giving up see if this is a variant product, and if so look
            // up the virtual product and check it...
            GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", productId).cache().queryOne();
            List<GenericValue> productAssocs = ProductWorker.getVariantVirtualAssocs(product);
            // this does take into account that a product could be a variant of
            // multiple products, but this shouldn't ever really happen...
            if (productAssocs != null) {
                for (GenericValue productAssoc : productAssocs) {
                    if (isProductInCategory(delegator, productAssoc.getString("productId"), productCategoryId)) {
                        return true;
                    }
                }
            }

            return false;
        } else {
            return true;
        }
    }

    public static List<GenericValue> filterProductsInCategory(Delegator delegator, List<GenericValue> valueObjects, String productCategoryId)
            throws GenericEntityException {
        return filterProductsInCategory(delegator, valueObjects, productCategoryId, "productId");
    }

    public static List<GenericValue> filterProductsInCategory(Delegator delegator, List<GenericValue> valueObjects, String productCategoryId,
            String productIdFieldName) throws GenericEntityException {
        List<GenericValue> newList = FastList.newInstance();

        if (productCategoryId == null)
            return newList;
        if (valueObjects == null)
            return null;

        for (GenericValue curValue : valueObjects) {
            String productId = curValue.getString(productIdFieldName);
            if (isProductInCategory(delegator, productId, productCategoryId)) {
                newList.add(curValue);
            }
        }
        return newList;
    }

    public static void getCategoryContentWrappers(Map<String, CategoryContentWrapper> catContentWrappers, List<GenericValue> categoryList,
            HttpServletRequest request) throws GenericEntityException {
        if (catContentWrappers == null || categoryList == null) {
            return;
        }
        for (GenericValue cat : categoryList) {
            String productCategoryId = (String) cat.get("productCategoryId");

            if (catContentWrappers.containsKey(productCategoryId)) {
                // if this ID is already in the Map, skip it (avoids
                // inefficiency, infinite recursion, etc.)
                continue;
            }

            CategoryContentWrapper catContentWrapper = new CategoryContentWrapper(cat, request);
            catContentWrappers.put(productCategoryId, catContentWrapper);
            List<GenericValue> subCat = getRelatedCategoriesRet(request, "subCatList", productCategoryId, true);
            if (subCat != null) {
                getCategoryContentWrappers(catContentWrappers, subCat, request);
            }
        }
    }

    /**
     * Returns a complete category trail - can be used for exporting proper
     * category trees. This is mostly useful when used in combination with
     * bread-crumbs, for building a faceted index tree, or to export a category
     * tree for migration to another system. Will create the tree from root
     * point to categoryId.
     * 
     * This method is not meant to be run on every request. Its best use is to
     * generate the trail every so often and store somewhere (a lucene/solr
     * tree, entities, cache or so).
     * 
     * @param dctx
     *            The DispatchContext that this service is operating in
     * @param context
     *            Map containing the input parameters
     * @return Map organized trail from root point to categoryId.
     */
    public static Map getCategoryTrail(DispatchContext dctx, Map context) {
        String productCategoryId = (String) context.get("productCategoryId");
        Map<String, Object> results = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        List<String> trailElements = FastList.newInstance();
        trailElements.add(productCategoryId);
        String parentProductCategoryId = productCategoryId;
        while (UtilValidate.isNotEmpty(parentProductCategoryId)) {
            // find product category rollup
            try {
                List<EntityCondition> rolllupConds = FastList.newInstance();
                rolllupConds.add(EntityCondition.makeCondition("productCategoryId", parentProductCategoryId));
                List<GenericValue> productCategoryRollups = EntityQuery.use(delegator).from("ProductCategoryRollup").where(rolllupConds).orderBy("sequenceNum")
                        .filterByDate().cache(true).queryList();
                if (UtilValidate.isNotEmpty(productCategoryRollups)) {
                    // add only categories that belong to the top category to
                    // trail
                    for (GenericValue productCategoryRollup : productCategoryRollups) {
                        String trailCategoryId = productCategoryRollup.getString("parentProductCategoryId");
                        parentProductCategoryId = trailCategoryId;
                        if (trailElements.contains(trailCategoryId)) {
                            break;
                        } else {
                            trailElements.add(trailCategoryId);
                        }
                    }
                } else {
                    parentProductCategoryId = null;
                }
            } catch (GenericEntityException e) {
                Map<String, String> messageMap = UtilMisc.toMap("errMessage", ". Cannot generate trail from product category. ");
                String errMsg = UtilProperties.getMessage("CommonUiLabels", "CommonDatabaseProblem", messageMap, (Locale) context.get("locale"));
                Debug.logError(e, errMsg, module);
                return ServiceUtil.returnError(errMsg);
            }
        }
        Collections.reverse(trailElements);
        results.put("trail", trailElements);
        return results;
    }

    /**
     * SCIPIO: Returns true only if the category ID is child of the given parent category ID.
     * <p>
     * NOTE: is caching
     */
    public static boolean isCategoryChildOf(Delegator delegator, LocalDispatcher dispatcher, String parentProductCategoryId, String productCategoryId) {
        try {
            List<EntityCondition> rolllupConds = FastList.newInstance();
            rolllupConds.add(EntityCondition.makeCondition("parentProductCategoryId", parentProductCategoryId));
            rolllupConds.add(EntityCondition.makeCondition("productCategoryId", productCategoryId));
            Collection<GenericValue> rollups = EntityQuery.use(delegator).from("ProductCategoryRollup").where(rolllupConds).filterByDate().cache().queryList();
            return !rollups.isEmpty();
        } catch (GenericEntityException e) {
            Debug.logWarning(e, module);
        }
        return false;
    }
    
    /**
     * SCIPIO: Returns true only if the category ID is child of the given parent category ID.
     * <p>
     * NOTE: is caching
     */
    public static boolean isCategoryChildOf(ServletRequest request, String parentProductCategoryId, String productCategoryId) {
        return isCategoryChildOf((Delegator) request.getAttribute("delegator"), (LocalDispatcher) request.getAttribute("dispatcher"), 
                parentProductCategoryId, productCategoryId);
    }    
    
    /**
     * SCIPIO: Returns true only if the category ID is a top category.
     * <p>
     * NOTE: is caching
     */
    public static boolean isCategoryTop(Delegator delegator, LocalDispatcher dispatcher, String productCategoryId) {
        if (UtilValidate.isEmpty(productCategoryId)) {
            return false;
        }
        try {
            List<EntityCondition> rolllupConds = FastList.newInstance();
            rolllupConds.add(EntityCondition.makeCondition("productCategoryId", productCategoryId));
            Collection<GenericValue> rollups = EntityQuery.use(delegator).from("ProductCategoryRollup").where(rolllupConds).filterByDate().cache().queryList();
            return rollups.isEmpty();
        } catch (GenericEntityException e) {
            Debug.logWarning(e, module);
        }
        return false; // can't tell, return false to play it safe
    }
    
    /**
     * SCIPIO: Returns true only if the category ID is a top category.
     * <p>
     * NOTE: is caching
     */
    public static boolean isCategoryTop(ServletRequest request, String productCategoryId) {
        return isCategoryTop((Delegator) request.getAttribute("delegator"), 
                (LocalDispatcher) request.getAttribute("dispatcher"), productCategoryId);
    }
    
    /**
     * SCIPIO: Returns true only if the category ID is a top category.
     * <p>
     * NOTE: is caching
     */
    public static boolean isCategoryContainsProduct(Delegator delegator, LocalDispatcher dispatcher, String productCategoryId, String productId) {
        if (UtilValidate.isEmpty(productCategoryId) || UtilValidate.isEmpty(productId)) {
            return false;
        }
        try {
            List<EntityCondition> conds = FastList.newInstance();
            conds.add(EntityCondition.makeCondition("productCategoryId", productCategoryId));
            conds.add(EntityCondition.makeCondition("productId", productId));
            List<GenericValue> productCategoryMembers = EntityQuery.use(delegator).select("productCategoryId").from("ProductCategoryMember")
                    .where(conds).filterByDate().cache(true).queryList();
            return !productCategoryMembers.isEmpty();
        } catch (GenericEntityException e) {
            Debug.logWarning(e, module);
        }
        return false; // can't tell, return false to play it safe
    }
    
    /**
     * SCIPIO: Returns true only if the category contains the product, NON-recursive.
     * <p>
     * NOTE: is caching
     */
    public static boolean isCategoryContainsProduct(ServletRequest request, String productCategoryId, String productId) {
        return isCategoryContainsProduct((Delegator) request.getAttribute("delegator"), 
                (LocalDispatcher) request.getAttribute("dispatcher"), productCategoryId, productId);
    }
    
    
    
    
    /**
     * SCIPIO: Returns a valid category path/trail (as parts) from the given trail,
     * starting with the top category (but without the fake "TOP" category). 
     * If none could be determined, returns null.
     * <p>
     * In some circumstances, getTrailNoTop alone could work, but we need a method that guarantees
     * a full path because the trail may not always be reliable or formal enough.
     * <p>
     * FIXME: This does NOT currently verify that the category path is actually valid!
     * We are relying on trailbuilding code elsewhere until can determine a reliable algorithm.
     * <p>
     * TODO?: This could try to produce better guesses in cases where top category is missing, instead
     * of returning null? I don't know if this is always safe, would want an extra flag and maybe
     * should not be the default behavior.
     */
    public static List<String> getCategoryPathFromTrailAsList(ServletRequest request, List<String> trail) {
        List<String> path = null;
        //List<String> trail = getTrailNoTop(request);
        // Get the last trail entry that is a top category. Usually this will be the first after "TOP",
        // but the trail is sometimes strange, so need to guarantee at least that.
        if (trail != null) {
            ListIterator<String> it = trail.listIterator(trail.size());
            while (it.hasPrevious() && path == null) {
                String part = it.previous();
                if (UtilValidate.isNotEmpty(part) && !"TOP".equals(part) && isCategoryTop(request, part)) {
                    path = new ArrayList<String>(trail.subList(it.nextIndex(), trail.size()));
                }
            }
        }
        /* TODO: should validate the path here
        if (path != null) {
            
        }*/
        return path;
    }
    
    /**
     * SCIPIO: Returns a valid category path/trail (as parts) from the current request trail in session,
     * starting with the top category (but without the fake "TOP" category). 
     * If none could be determined, returns null.
     */
    public static List<String> getCategoryPathFromTrailAsList(ServletRequest request) {
        return getCategoryPathFromTrailAsList(request, getTrail(request));
    }
    
    /**
     * SCIPIO: Checks the given trail for the last recorded top category ID, if any.
     * This can be the catalog top category or a different one.
     * <p>
     * NOTE: is caching
     */
    public static String getTopCategoryFromTrail(Delegator delegator, LocalDispatcher dispatcher, List<String> trail) {
        String catId = null;
        if (trail != null) {
            ListIterator<String> it = trail.listIterator(trail.size());
            while (it.hasPrevious()) {
                catId = it.previous();
                if (UtilValidate.isNotEmpty(catId) && !"TOP".equals(catId)) {
                    if (isCategoryTop(delegator, dispatcher, catId)) {
                        return catId;
                    }
                }
            }
        }
        return null;
    }
    
    
    /**
     * SCIPIO: Checks the given trail for the last recorded top category ID, if any.
     * This can be the catalog top category or a different one.
     */
    public static String getTopCategoryFromTrail(ServletRequest request, List<String> trail) {
        return getTopCategoryFromTrail((Delegator) request.getAttribute("delegator"), 
                (LocalDispatcher) request.getAttribute("dispatcher"), trail);
    }
    
    /**
     * SCIPIO: Checks the current trail for the last recorded top category ID, if any.
     * This can be the catalog top category or a different one.
     */
    public static String getTopCategoryFromTrail(ServletRequest request) {
        return getTopCategoryFromTrail(request, getTrail(request));
    }
    
    
    /**
     * SCIPIO: Attempts to determine a suitable category for the given product from given trail.
     */
    public static String getCategoryForProductFromTrail(ServletRequest request, String productId, List<String> trail) {
        if (UtilValidate.isNotEmpty(productId)) {
            if (trail != null && !trail.isEmpty()) {
                String catId = trail.get(trail.size() - 1);
                if (UtilValidate.isNotEmpty(catId) && !"TOP".equals(catId)) {
                    if (CategoryWorker.isCategoryContainsProduct(request, catId, productId)) {
                        return catId;
                    }
                }
            }
        }
        return null;
    }   
    
    /**
     * SCIPIO: Attempts to determine a suitable category for the given product from the trail in session.
     */
    public static String getCategoryForProductFromTrail(ServletRequest request, String productId) {
        return getCategoryForProductFromTrail(request, productId, CategoryWorker.getTrail(request));
    }
    
    /**
     * SCIPIO: For each simple-text-compatible prodCatContentTypeId, returns a list of complex record views,
     * where the first entry is ProductCategoryContentAndElectronicText and the following entries (if any)
     * are ContentAssocToElectronicText views.
     * <p>
     * NOTE: If there are multiple ProductCategoryContent for same cat/type, this fetches the lastest only (logs warning).
     * System or user is expected to prevent this.
     * <p>
     * filterByDate must be set to a value in order to filter by date.
     * Added 2017-10-27.
     */
    public static Map<String, List<GenericValue>> getProductCategoryContentLocalizedSimpleTextViews(Delegator delegator, LocalDispatcher dispatcher,
            String productCategoryId, Collection<String> prodCatContentTypeIdList, java.sql.Timestamp filterByDate, boolean useCache) throws GenericEntityException {
        Map<String, List<GenericValue>> fieldMap = new HashMap<>();
        
        List<EntityCondition> typeIdCondList = new ArrayList<>(prodCatContentTypeIdList.size());
        if (prodCatContentTypeIdList != null) {
            for(String prodCatContentTypeId : prodCatContentTypeIdList) {
                typeIdCondList.add(EntityCondition.makeCondition("prodCatContentTypeId", prodCatContentTypeId));
            }
        }
        List<EntityCondition> condList = new ArrayList<>();
        condList.add(EntityCondition.makeCondition("productCategoryId", productCategoryId));
        if (typeIdCondList.size() > 0) {
            condList.add(EntityCondition.makeCondition(typeIdCondList, EntityOperator.OR));
        }
        condList.add(EntityCondition.makeCondition("drDataResourceTypeId", "ELECTRONIC_TEXT"));
        
        EntityQuery query = EntityQuery.use(delegator).from("ProductCategoryContentAndElectronicText")
                .where(condList).orderBy("-fromDate").cache(useCache);
        if (filterByDate != null) {
            query = query.filterByDate(filterByDate);
        }
        List<GenericValue> prodCatContentList = query.queryList();
        for(GenericValue prodCatContent : prodCatContentList) {
            String prodCatContentTypeId = prodCatContent.getString("prodCatContentTypeId");
            if (fieldMap.containsKey(prodCatContentTypeId)) {
                Debug.logWarning("getProductCategoryContentLocalizedSimpleTextViews: multiple ProductCategoryContentAndElectronicText"
                        + " records found for prodCatContentTypeId '" + prodCatContentTypeId + "' for productCategoryId '" + productCategoryId + "'; "
                        + " returning first found only (this may cause unexpected texts to appear)", module);
                continue;
            }
            String contentIdStart = prodCatContent.getString("contentId");
            
            condList = new ArrayList<>();
            condList.add(EntityCondition.makeCondition("contentIdStart", contentIdStart));
            condList.add(EntityCondition.makeCondition("contentAssocTypeId", "ALTERNATE_LOCALE"));
            condList.add(EntityCondition.makeCondition("drDataResourceTypeId", "ELECTRONIC_TEXT"));
            query = EntityQuery.use(delegator).from("ContentAssocToElectronicText")
                    .where(condList).orderBy("-fromDate").cache(useCache);
            if (filterByDate != null) {
                query = query.filterByDate(filterByDate);
            }
            List<GenericValue> contentAssocList = query.queryList();
            List<GenericValue> valueList = new ArrayList<>(contentAssocList.size() + 1);
            valueList.add(prodCatContent);
            valueList.addAll(contentAssocList);
            fieldMap.put(prodCatContentTypeId, valueList);
        }
        
        return fieldMap;
    }

    
    /**
     * SCIPIO: Returns all rollups for a category.
     * Imported from SolrCategoryUtil, 2017-11-09.
     */
    public static List<List<String>> getCategoryRollupTrails(Delegator delegator, String productCategoryId, boolean useCache) {
        List<List<String>> trailElements = new ArrayList<>();
        try {
            // NOTE: Can't filter on sequenceNum because it only makes sense if querying by parentProductCategoryId
            List<GenericValue> productCategoryRollups = EntityQuery.use(delegator).from("ProductCategoryRollup")
                    .where("productCategoryId", productCategoryId).orderBy("-fromDate").filterByDate().cache(useCache).queryList();
            if (UtilValidate.isNotEmpty(productCategoryRollups)) {
                // For each parent cat, get its trails recursively and add our own
                for (GenericValue productCategoryRollup : productCategoryRollups) {
                    String parentProductCategoryId = productCategoryRollup.getString("parentProductCategoryId");
                    List<List<String>> parentTrails = getCategoryRollupTrails(delegator, parentProductCategoryId, useCache);
                    for (List<String> trail : parentTrails) {
                        // WARN: modifying the parent trail in-place for speed
                        trail.add(productCategoryId);
                        trailElements.add(trail);
                    }
                }
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Cannot generate trail from product category '" + productCategoryId + "'", module);
        }
        if (trailElements.isEmpty()) {
            List<String> trailElement = new ArrayList<>();
            trailElement.add(productCategoryId);
            trailElements.add(trailElement);
        }
        return trailElements;
    }
    
    /**
     * SCIPIO: Returns all rollups for a category that have the given top categories.
     * TODO: REVIEW: maybe this can be optimized with a smarter algorithm?
     * Added 2017-11-09.
     */
    public static List<List<String>> getCategoryRollupTrails(Delegator delegator, String productCategoryId, Set<String> topCategoryIds, boolean useCache) {
        List<List<String>> trails = getCategoryRollupTrails(delegator, productCategoryId, useCache);
        if (topCategoryIds == null) return trails;
        List<List<String>> filtered = new ArrayList<>(trails.size());
        for(List<String> trail : trails) {
            if (!trail.isEmpty() && topCategoryIds.contains(trail.get(0))) {
                filtered.add(trail);
            }
        }
        return filtered;
    }
}
