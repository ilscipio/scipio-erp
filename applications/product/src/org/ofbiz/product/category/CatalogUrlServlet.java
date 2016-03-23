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

import java.io.IOException;
import java.util.List;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import javolution.util.FastList;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;

/**
 * ControlServlet.java - Master servlet for the web application.
 */
@SuppressWarnings("serial")
public class CatalogUrlServlet extends HttpServlet {

    public static final String module = CatalogUrlServlet.class.getName();

    public static final String CATALOG_URL_MOUNT_POINT = "products";
    public static final String CONTROL_MOUNT_POINT = "control";
    public static final String PRODUCT_REQUEST = "product";
    public static final String CATEGORY_REQUEST = "category";

    public CatalogUrlServlet() {
        super();
    }

    /**
     * @see javax.servlet.http.HttpServlet#init(javax.servlet.ServletConfig)
     */
    @Override
    public void init(ServletConfig config) throws ServletException {
        super.init(config);
    }

    /**
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    @Override
    public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        doGet(request, response);
    }

    /**
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        Delegator delegator = (Delegator) getServletContext().getAttribute("delegator");

        String pathInfo = request.getPathInfo();
        List<String> pathElements = StringUtil.split(pathInfo, "/");

        String productId = null;
        String categoryId = null;

        if (pathElements == null) {
            RequestDispatcher rd = request.getRequestDispatcher("/" + CONTROL_MOUNT_POINT + "/main");
            rd.forward(request, response);
        } else {
        try {
            String lastPathElement = pathElements.get(pathElements.size() - 1);
            if (lastPathElement.startsWith("p_")) {
                productId = lastPathElement.substring(2);
                // Cato: remove for products only
                pathElements.remove(pathElements.size() - 1);
            } else {
                GenericValue productCategory =  EntityQuery.use(delegator).from("ProductCategory").where("productCategoryId", lastPathElement).cache(true).queryOne();
                if (UtilValidate.isNotEmpty(productCategory)) {
                    categoryId = lastPathElement;
                } else {
                    productId = lastPathElement;
                    // Cato: remove for products only
                    pathElements.remove(pathElements.size() - 1);
                }
            }
            // Cato: Don't remove this here; remove only for products
            //pathElements.remove(pathElements.size() - 1);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in looking up ProductUrl or CategoryUrl with path info [" + pathInfo + "]: " + e.toString(), module);
        }

        // Cato: 2016-03-22: NEW EXTRA BEHAVIOR FOR TOP-LESS BROWSING: 
        // We have a problem here that CatalogUrlFilter does not have:
        // CatalogUrlFilter should (now) always set a top category, but for CatalogUrlServlet,
        // it's often possible for us to receive links that don't indicate full path AND for which we
        // don't have a trail in session.
        // So in these cases, we will emulate CatalogUrlFilter and replace everything to a path
        // under the main top category.
        // NOTE: CatalogUrlFilter's solution is imperfect and restricts browsing, so it's not
        // a great model, but we should at least follow it.
        // WARN: this does not guarantee we have a "valid" category path; it's still possible
        // for other weirdness between the top category and the last part, but this should
        // help the worst cases.
        //
        // CASE 2: We will now also force a default trail if we get a product without path.
        // otherwise. This will fix some other cases.
        //
        // CASE 3: We will now also force a default trail if we get a category that's not top
        // and has no other path elements than itself.
        //
        // The combination of above cases will now make it so a link must be a full path if it wants
        // to override the default topCategory-based lookup. All other cases will use the default lookup like CatalogUrlFilter does.
        if ((UtilValidate.isNotEmpty(productId) && pathElements.size() == 0) ||
            (UtilValidate.isNotEmpty(categoryId) && pathElements.size() <= 1 && !CategoryWorker.isCategoryTop(request, categoryId)) ||
            ((UtilValidate.isNotEmpty(productId) || UtilValidate.isNotEmpty(categoryId)) && !hasTopCategory(request, categoryId, pathElements))
           ) {
            // We don't have a top category anywhere. So we'll emulate CatalogUrlFilter.
            List<String> trailElements = makeTrailElements(request, delegator, categoryId, productId);
            if (trailElements != null) {
                // Replace the pathElements with our trail
                pathElements = trailElements;
            }
        }
        
        // get category info going with the IDs that remain
        if (pathElements.size() == 1) {
            CategoryWorker.setTrail(request, pathElements.get(0), null);
            categoryId = pathElements.get(0);
        } else if (pathElements.size() == 2) {
            CategoryWorker.setTrail(request, pathElements.get(1), pathElements.get(0));
            categoryId = pathElements.get(1);
        } else if (pathElements.size() > 2) {
            List<String> trail = CategoryWorker.getTrail(request);
            if (trail == null) {
                trail = FastList.newInstance();
            }

            if (trail.contains(pathElements.get(0))) {
                // first category is in the trail, so remove it everything after that and fill it in with the list from the pathInfo
                int firstElementIndex = trail.indexOf(pathElements.get(0));
                while (trail.size() > firstElementIndex) {
                    trail.remove(firstElementIndex);
                }
                trail.addAll(pathElements);
            } else {
                // first category is NOT in the trail, so clear out the trail and use the pathElements list
                trail.clear();
                trail.addAll(pathElements);
            }
            CategoryWorker.setTrail(request, trail);
            categoryId = pathElements.get(pathElements.size() - 1);
        } else {
            /* Cato: NOTE: This was a new addition but has been moved to CategoryWorker.getCategoryForProductFromTrail
              which is called from data prep scripts instead of this (because other variables may need precedence over trail).
            if (UtilValidate.isNotEmpty(productId)) {
                List<String> trail = CategoryWorker.getTrail(request);
                if (trail != null && !trail.isEmpty()) {
                    String catId = trail.get(trail.size() - 1);
                    if (UtilValidate.isNotEmpty(catId) && !"TOP".equals(catId)) {
                        if (CategoryWorker.isCategoryContainsProduct(request, catId, productId)) {
                            
                        }
                    }
                }
            }
            */
        }
        if (categoryId != null) {
            request.setAttribute("productCategoryId", categoryId);
        }

        String rootCategoryId = null;
        if (pathElements.size() >= 1) {
            rootCategoryId = pathElements.get(0);
        }
        if (rootCategoryId != null) {
            request.setAttribute("rootCategoryId", rootCategoryId);
        }

        if (productId != null) {
            request.setAttribute("product_id", productId);
            request.setAttribute("productId", productId);
        }

        RequestDispatcher rd = request.getRequestDispatcher("/" + CONTROL_MOUNT_POINT + "/" + (productId != null ? PRODUCT_REQUEST : CATEGORY_REQUEST));
        rd.forward(request, response);
        }
    }

    public static boolean hasTopCategory(HttpServletRequest request, String categoryId, List<String> pathElements) {
        if (CategoryWorker.isCategoryTop(request, categoryId)) {
            return true;
        }
        String topCategoryId = CategoryWorker.getTopCategoryFromTrail(request, pathElements);
        if (topCategoryId != null) {
            return true;
        }
        List<String> trail = CategoryWorker.getTrail(request);
        topCategoryId = CategoryWorker.getTopCategoryFromTrail(request, trail);
        if (topCategoryId == null) {
            return true;
        }
        return false;   
    }
    
    
    /**
     * Cato: makeTrailElements, emulating CatalogUrlFilter.
     */
    public static List<String> makeTrailElements(HttpServletRequest request, Delegator delegator, String categoryId, String productId) {
        
        String productCategoryId = categoryId;
        
        if (UtilValidate.isNotEmpty(productId)) {
            String catId = CatalogUrlFilter.getProductDefaultCategoryId(delegator, productId);
            if (catId != null) {
                productCategoryId = catId;
            }
        }
        
        // generate trail belong to a top category
        String topCategoryId = CategoryWorker.getCatalogTopCategory(request, null);
        List<GenericValue> trailCategories = CategoryWorker.getRelatedCategoriesRet(request, "trailCategories", topCategoryId, false, false, true);
        List<String> trailCategoryIds = EntityUtil.getFieldListFromEntityList(trailCategories, "productCategoryId", true);
        
        // look for productCategoryId from productId
        if (UtilValidate.isNotEmpty(productId)) {
            String catId = CatalogUrlFilter.getProductMatchingCategoryId(delegator, productId, trailCategoryIds);
            if (catId != null) {
                productCategoryId = catId;
            }
        }
        
        if (UtilValidate.isNotEmpty(productCategoryId)) {
            List<String> trailElements = CatalogUrlFilter.getTrailElements(delegator, productCategoryId, trailCategoryIds);
            if (trailElements.size() > 0) {
                trailElements.add(0, topCategoryId);
                
                return trailElements;
            }
        }
        return null;
    }
    
    
    /**
     * @see javax.servlet.http.HttpServlet#destroy()
     */
    @Override
    public void destroy() {
        super.destroy();
    }

    public static String makeCatalogUrl(HttpServletRequest request, String productId, String currentCategoryId, String previousCategoryId) {
        StringBuilder urlBuilder = new StringBuilder();
        urlBuilder.append(request.getSession().getServletContext().getContextPath());
        if (urlBuilder.length() == 0 || urlBuilder.charAt(urlBuilder.length() - 1) != '/') {
            urlBuilder.append("/");
        }
        urlBuilder.append(CATALOG_URL_MOUNT_POINT);

        if (UtilValidate.isNotEmpty(currentCategoryId)) {
            List<String> trail = CategoryWorker.getTrail(request);
            trail = CategoryWorker.adjustTrail(trail, currentCategoryId, previousCategoryId);
            for (String trailCategoryId: trail) {
                if ("TOP".equals(trailCategoryId)) continue;
                urlBuilder.append("/");
                urlBuilder.append(trailCategoryId);
            }
        }

        if (UtilValidate.isNotEmpty(productId)) {
            urlBuilder.append("/p_");
            urlBuilder.append(productId);
        }

        return urlBuilder.toString();
    }

    public static String makeCatalogUrl(String contextPath, List<String> crumb, String productId, String currentCategoryId, String previousCategoryId) {
        StringBuilder urlBuilder = new StringBuilder();
        urlBuilder.append(contextPath);
        if (urlBuilder.length() == 0 || urlBuilder.charAt(urlBuilder.length() - 1) != '/') {
            urlBuilder.append("/");
        }
        urlBuilder.append(CATALOG_URL_MOUNT_POINT);

        if (UtilValidate.isNotEmpty(currentCategoryId)) {
            crumb = CategoryWorker.adjustTrail(crumb, currentCategoryId, previousCategoryId);
            for (String trailCategoryId: crumb) {
                if ("TOP".equals(trailCategoryId)) continue;
                urlBuilder.append("/");
                urlBuilder.append(trailCategoryId);
            }
        }

        if (UtilValidate.isNotEmpty(productId)) {
            urlBuilder.append("/p_");
            urlBuilder.append(productId);
        }

        return urlBuilder.toString();
    }
}
