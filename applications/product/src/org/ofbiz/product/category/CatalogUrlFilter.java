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
import java.util.Collections;
import java.util.List;

import javax.servlet.FilterChain;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import javolution.util.FastList;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.StringUtil.StringWrapper;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.UrlServletHelper;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.webapp.control.ContextFilter;

public class CatalogUrlFilter extends ContextFilter {

    public final static String module = CatalogUrlFilter.class.getName();
    
    public static final String CONTROL_MOUNT_POINT = "control";
    public static final String PRODUCT_REQUEST = "product";
    public static final String CATEGORY_REQUEST = "category";
    
    protected static String defaultLocaleString = null;
    protected static String redirectUrl = null;
    
    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        HttpServletResponse httpResponse = (HttpServletResponse) response;
        Delegator delegator = (Delegator) httpRequest.getSession().getServletContext().getAttribute("delegator");
        
        //Get ServletContext
        ServletContext servletContext = config.getServletContext();

        ContextFilter.setCharacterEncoding(request);

        //Set request attribute and session
        UrlServletHelper.setRequestAttributes(request, delegator, servletContext);
        
        // set initial parameters
        String initDefaultLocalesString = config.getInitParameter("defaultLocaleString");
        String initRedirectUrl = config.getInitParameter("redirectUrl");
        defaultLocaleString = UtilValidate.isNotEmpty(initDefaultLocalesString) ? initDefaultLocalesString : "";
        redirectUrl = UtilValidate.isNotEmpty(initRedirectUrl) ? initRedirectUrl : "";
        
        String pathInfo = httpRequest.getServletPath();
        if (UtilValidate.isNotEmpty(pathInfo)) {
            List<String> pathElements = StringUtil.split(pathInfo, "/");
            String alternativeUrl = pathElements.get(0);
            
            String productId = null;
            String productCategoryId = null;
            String urlContentId = null;
            try {
                // look for productId
                if (alternativeUrl.endsWith("-p")) {
                    List<EntityCondition> productContentConds = FastList.newInstance();
                    productContentConds.add(EntityCondition.makeCondition("productContentTypeId", "ALTERNATIVE_URL"));
                    productContentConds.add(EntityUtil.getFilterByDateExpr());
                    List<GenericValue> productContentInfos = EntityQuery.use(delegator).from("ProductContentAndInfo").where(productContentConds).orderBy("-fromDate").cache(true).queryList();
                    if (UtilValidate.isNotEmpty(productContentInfos)) {
                        for (GenericValue productContentInfo : productContentInfos) {
                            String contentId = (String) productContentInfo.get("contentId");
                            List<GenericValue> ContentAssocDataResourceViewTos = EntityQuery.use(delegator).from("ContentAssocDataResourceViewTo").where("contentIdStart", contentId, "caContentAssocTypeId", "ALTERNATE_LOCALE", "drDataResourceTypeId", "ELECTRONIC_TEXT").cache(true).queryList();
                            if (UtilValidate.isNotEmpty(ContentAssocDataResourceViewTos)) {
                                for (GenericValue ContentAssocDataResourceViewTo : ContentAssocDataResourceViewTos) {
                                    GenericValue ElectronicText = ContentAssocDataResourceViewTo.getRelatedOne("ElectronicText", true);
                                    if (UtilValidate.isNotEmpty(ElectronicText)) {
                                        String textData = (String) ElectronicText.get("textData");
                                        textData = UrlServletHelper.invalidCharacter(textData);
                                        if (alternativeUrl.matches(textData + ".+$")) {
                                            String productIdStr = null;
                                            productIdStr = alternativeUrl.replace(textData + "-", "");
                                            productIdStr = productIdStr.replace("-p", "");
                                            String checkProductId = (String) productContentInfo.get("productId");
                                            if (productIdStr.equalsIgnoreCase(checkProductId)) {
                                                productId = checkProductId;
                                                break;
                                            }
                                        }
                                    }
                                }
                            }
                            if (UtilValidate.isEmpty(productId)) {
                                List<GenericValue> contentDataResourceViews = EntityQuery.use(delegator).from("ContentDataResourceView").where("contentId", contentId, "drDataResourceTypeId", "ELECTRONIC_TEXT").cache(true).queryList();
                                for (GenericValue contentDataResourceView : contentDataResourceViews) {
                                    GenericValue ElectronicText = contentDataResourceView.getRelatedOne("ElectronicText", true);
                                    if (UtilValidate.isNotEmpty(ElectronicText)) {
                                        String textData = (String) ElectronicText.get("textData");
                                        if (UtilValidate.isNotEmpty(textData)) {
                                            textData = UrlServletHelper.invalidCharacter(textData);
                                            if (alternativeUrl.matches(textData + ".+$")) {
                                                String productIdStr = null;
                                                productIdStr = alternativeUrl.replace(textData + "-", "");
                                                productIdStr = productIdStr.replace("-p", "");
                                                String checkProductId = (String) productContentInfo.get("productId");
                                                if (productIdStr.equalsIgnoreCase(checkProductId)) {
                                                    productId = checkProductId;
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                
                // look for productCategoryId
                if (alternativeUrl.endsWith("-c")) {
                    List<EntityCondition> productCategoryContentConds = FastList.newInstance();
                    productCategoryContentConds.add(EntityCondition.makeCondition("prodCatContentTypeId", "ALTERNATIVE_URL"));
                    productCategoryContentConds.add(EntityUtil.getFilterByDateExpr());
                    List<GenericValue> productCategoryContentInfos = EntityQuery.use(delegator).from("ProductCategoryContentAndInfo").where(productCategoryContentConds).orderBy("-fromDate").cache(true).queryList();
                    if (UtilValidate.isNotEmpty(productCategoryContentInfos)) {
                        for (GenericValue productCategoryContentInfo : productCategoryContentInfos) {
                            String contentId = (String) productCategoryContentInfo.get("contentId");
                            List<GenericValue> ContentAssocDataResourceViewTos = EntityQuery.use(delegator).from("ContentAssocDataResourceViewTo").where("contentIdStart", contentId, "caContentAssocTypeId", "ALTERNATE_LOCALE", "drDataResourceTypeId", "ELECTRONIC_TEXT").cache(true).queryList();
                            if (UtilValidate.isNotEmpty(ContentAssocDataResourceViewTos)) {
                                for (GenericValue ContentAssocDataResourceViewTo : ContentAssocDataResourceViewTos) {
                                    GenericValue ElectronicText = ContentAssocDataResourceViewTo.getRelatedOne("ElectronicText", true);
                                    if (UtilValidate.isNotEmpty(ElectronicText)) {
                                        String textData = (String) ElectronicText.get("textData");
                                        if (UtilValidate.isNotEmpty(textData)) {
                                            textData = UrlServletHelper.invalidCharacter(textData);
                                            if (alternativeUrl.matches(textData + ".+$")) {
                                                String productCategoryStr = null;
                                                productCategoryStr = alternativeUrl.replace(textData + "-", "");
                                                productCategoryStr = productCategoryStr.replace("-c", "");
                                                String checkProductCategoryId = (String) productCategoryContentInfo.get("productCategoryId");
                                                if (productCategoryStr.equalsIgnoreCase(checkProductCategoryId)) {
                                                    productCategoryId = checkProductCategoryId;
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            if (UtilValidate.isEmpty(productCategoryId)) {
                                List<GenericValue> contentDataResourceViews = EntityQuery.use(delegator).from("ContentDataResourceView").where("contentId", contentId, "drDataResourceTypeId", "ELECTRONIC_TEXT").cache(true).queryList();
                                for (GenericValue contentDataResourceView : contentDataResourceViews) {
                                    GenericValue ElectronicText = contentDataResourceView.getRelatedOne("ElectronicText", true);
                                    if (UtilValidate.isNotEmpty(ElectronicText)) {
                                        String textData = (String) ElectronicText.get("textData");
                                        if (UtilValidate.isNotEmpty(textData)) {
                                            textData = UrlServletHelper.invalidCharacter(textData);
                                            if (alternativeUrl.matches(textData + ".+$")) {
                                                String productCategoryStr = null;
                                                productCategoryStr = alternativeUrl.replace(textData + "-", "");
                                                productCategoryStr = productCategoryStr.replace("-c", "");
                                                String checkProductCategoryId = (String) productCategoryContentInfo.get("productCategoryId");
                                                if (productCategoryStr.equalsIgnoreCase(checkProductCategoryId)) {
                                                    productCategoryId = checkProductCategoryId;
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

            } catch (GenericEntityException e) {
                Debug.logWarning("Cannot look for product and product category", module);
            }
            
            // generate forward URL
            StringBuilder urlBuilder = new StringBuilder();
            urlBuilder.append("/" + CONTROL_MOUNT_POINT);
            
            // Cato: TODO: The code below should somehow be changed to delegate to makeTrailElements
            // call; currently duplicated due to heavy var reuse.
            
            if (UtilValidate.isNotEmpty(productId)) {
                // Cato: factored out
                String catId = getProductDefaultCategoryId(delegator, productId);
                if (catId != null) {
                    productCategoryId = catId;
                }
                urlBuilder.append("/" + PRODUCT_REQUEST);
            } else {
                urlBuilder.append("/" + CATEGORY_REQUEST);
            }

            // Cato: 2016-03-22: FIXME?: this getCatalogTopCategory call below 
            // is currently left unchanged, but note that because of it,
            // currently CatalogUrlFilter/ofbizCatalogAltUrl force browsing toward only the main top catalog category.
            // It does not allow browsing any other top categories (best-selling, promotions, etc.).
            // In some cases this is desirable, in others not.
            
            // generate trail belong to a top category
            String topCategoryId = CategoryWorker.getCatalogTopCategory(httpRequest, null);
            List<GenericValue> trailCategories = CategoryWorker.getRelatedCategoriesRet(httpRequest, "trailCategories", topCategoryId, false, false, true);
            List<String> trailCategoryIds = EntityUtil.getFieldListFromEntityList(trailCategories, "productCategoryId", true);
            
            // look for productCategoryId from productId
            if (UtilValidate.isNotEmpty(productId)) {
                // Cato: factored out
                String catId = getProductMatchingCategoryId(delegator, productId, trailCategoryIds);
                if (catId != null) {
                    productCategoryId = catId;
                }
            }

            // Cato: 2016-03-22: FIXME?: The loop below was found to cause invalid category paths in SOLR addToSolr
            // (was very similar code) and had to be fixed there. I think there is a chance there may be bugs here as well,
            // but I'm not certain.
            
            // generate trail elements from productCategoryId
            if (UtilValidate.isNotEmpty(productCategoryId)) {
                List<String> trailElements = getTrailElements(delegator, productCategoryId, trailCategoryIds);
                
                // Cato: NOTE: Parts of this could reuse updateRequestAndTrail but there are minor difference,
                // not risking it for now.
                
                List<String> trail = CategoryWorker.getTrail(httpRequest);
                if (trail == null) {
                    trail = FastList.newInstance();
                }

                // adjust trail
                String previousCategoryId = null;
                if (trail.size() > 0) {
                    previousCategoryId = trail.get(trail.size() - 1);
                }
                trail = CategoryWorker.adjustTrail(trail, productCategoryId, previousCategoryId);
                
                // Cato: 2016-03-23: There is a high risk here that the trail does not contain the top
                // category.
                // If top category is not in trail, we'll prepend it to trailElements. 
                // This will cause the trailElements to replace the whole trail in the code that follows
                // because of the way setTrail with ID works.
                // I'm not sure what the intention of stock code was in these cases, but I think
                // this will simply prevent a lot of confusion and makes the trail more likely to be
                // a valid category path.
                // EDIT: This behavior is now changed, see next comment
                if (trailElements.size() > 0) {
                    // Cato: REVISION 2: we will ALWAYS add the top category to the trail
                    // elements. It's needed because sometimes the code above will produce
                    // entries incompatible with the current trail and it results in an
                    // incomplete trail. Adding the top category should cause the code below
                    // to reset much of the trail.
                    //if (!trail.contains(topCategoryId)) {
                    //    trailElements.add(0, topCategoryId);
                    //}
                    trailElements.add(0, topCategoryId);
                }

                // Cato: this is now delegated
                updateRequestAndTrail(httpRequest, productCategoryId, productId, trailElements, trail);
            }
            
            //Set view query parameters
            UrlServletHelper.setViewQueryParameters(request, urlBuilder);
            if (UtilValidate.isNotEmpty(productId) || UtilValidate.isNotEmpty(productCategoryId) || UtilValidate.isNotEmpty(urlContentId)) {
                Debug.logInfo("[Filtered request]: " + pathInfo + " (" + urlBuilder + ")", module);
                ContextFilter.setAttributesFromRequestBody(request);
                RequestDispatcher dispatch = request.getRequestDispatcher(urlBuilder.toString());
                dispatch.forward(request, response);
                return;
            }
            
            //Check path alias
            UrlServletHelper.checkPathAlias(request, httpResponse, delegator, pathInfo);
        }
        
        // we're done checking; continue on
        chain.doFilter(request, response);
    }

    /**
     * Cato: Returns appropriate trail elements for a category or ID (abstraction method).
     * <p>
     * TODO: Modify doGet above to invoke this (too much variable reuse)
     * <p>
     * FIXME?: Currently this forces the trail to be a path under the top category, which is desirable in
     * some cases but not necessarily all. It does not take into account the existing trail.
     * This is generally not desirable, but okay for simple shops.
     */
    public static List<String> makeTrailElements(HttpServletRequest request, Delegator delegator, String categoryId, String productId) {
        // FIXME?: Completely ignores current trail...
        return makeDefaultCategoryTrailElements(request, delegator, categoryId, productId);
    }
    
    /**
     * Cato: Makes a fresh trail based on the default category for a product or category under the top catalog category.
     * Ignores the current trail. 
     * Based on original {@link #doFilter} code.
     */
    public static List<String> makeDefaultCategoryTrailElements(HttpServletRequest request, Delegator delegator, String categoryId, String productId) {
        
        String productCategoryId = categoryId;
        
        if (UtilValidate.isNotEmpty(productId)) {
            String catId = getProductDefaultCategoryId(delegator, productId);
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
            String catId = getProductMatchingCategoryId(delegator, productId, trailCategoryIds);
            if (catId != null) {
                productCategoryId = catId;
            }
        }
        
        if (UtilValidate.isNotEmpty(productCategoryId)) {
            List<String> trailElements = getTrailElements(delegator, productCategoryId, trailCategoryIds);
    
            // Cato: NOTE: CatalogUrlFilter#doGet does another adjustment to trail
            // here before we add topCategoryId, but I don't know why, and I think it will 
            // make no difference because we add topCategoryId now.
            
            if (trailElements.size() > 0) {
                trailElements.add(0, topCategoryId);
                return trailElements;
            }
        }
        return null;
    }
    

    /**
     * Cato: Stock code factored out from {@link #doFilter}.
     */
    public static String getProductDefaultCategoryId(Delegator delegator, String productId) {
        String productCategoryId = null;
        try {
            List<EntityCondition> conds = FastList.newInstance();
            conds.add(EntityCondition.makeCondition("productId", productId));
            conds.add(EntityUtil.getFilterByDateExpr());
            List<GenericValue> productCategoryMembers = EntityQuery.use(delegator).select("productCategoryId").from("ProductCategoryMember").where(conds).orderBy("-fromDate").cache(true).queryList();
            if (UtilValidate.isNotEmpty(productCategoryMembers)) {
                GenericValue productCategoryMember = EntityUtil.getFirst(productCategoryMembers);
                productCategoryId = productCategoryMember.getString("productCategoryId");
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Cannot find product category for product: " + productId, module);
        }
        return productCategoryId;
    }
    
    /**
     * Cato: Stock code factored out from doGet.
     */
    public static String getProductMatchingCategoryId(Delegator delegator, String productId, List<String> categoryIds) {
        String productCategoryId = null;
        try {
            List<EntityCondition> rolllupConds = FastList.newInstance();
            rolllupConds.add(EntityCondition.makeCondition("productId", productId));
            rolllupConds.add(EntityUtil.getFilterByDateExpr());
            List<GenericValue> productCategoryMembers = EntityQuery.use(delegator).from("ProductCategoryMember").where(rolllupConds).orderBy("-fromDate").cache(true).queryList();
            for (GenericValue productCategoryMember : productCategoryMembers) {
                String trailCategoryId = productCategoryMember.getString("productCategoryId");
                if (categoryIds.contains(trailCategoryId)) {
                    productCategoryId = trailCategoryId;
                    break;
                }
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Cannot generate trail from product category", module);
        }
        return productCategoryId;
    }    
    
    /**
     * Cato: Stock code factored out from doGet.
     */
    public static List<String> getTrailElements(Delegator delegator, String productCategoryId, List<String> trailCategoryIds) {
        List<String> trailElements = FastList.newInstance();
        trailElements.add(productCategoryId);
        String parentProductCategoryId = productCategoryId;
        while (UtilValidate.isNotEmpty(parentProductCategoryId)) {
            // find product category rollup
            try {
                List<EntityCondition> rolllupConds = FastList.newInstance();
                rolllupConds.add(EntityCondition.makeCondition("productCategoryId", parentProductCategoryId));
                rolllupConds.add(EntityUtil.getFilterByDateExpr());
                List<GenericValue> productCategoryRollups = EntityQuery.use(delegator).from("ProductCategoryRollup").where(rolllupConds).orderBy("-fromDate").cache(true).queryList();
                if (UtilValidate.isNotEmpty(productCategoryRollups)) {
                    // add only categories that belong to the top category to trail
                    for (GenericValue productCategoryRollup : productCategoryRollups) {
                        String trailCategoryId = productCategoryRollup.getString("parentProductCategoryId");
                        parentProductCategoryId = trailCategoryId;
                        if (trailCategoryIds.contains(trailCategoryId)) {
                            trailElements.add(trailCategoryId);
                            break;
                        }
                    }
                } else {
                    parentProductCategoryId = null;
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, "Cannot generate trail from product category", module);
            }
        }
        Collections.reverse(trailElements);
        return trailElements;
    }    
    
    /**
     * Cato: Returns true if the given category or any of the path elements is a top-level category.
     */
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
     * Cato: Updates the trail elements using logic originally found in {@link CatalogUrlServlet#doGet}.
     * <p>
     * The caller should ensure the last path element is the same as the passed category ID.
     * <p>
     * trail is optional, will be fetched automatically.
     */
    public static void updateRequestAndTrail(HttpServletRequest request, String categoryId, String productId, List<String> pathElements, List<String> trail) {
        if (UtilValidate.isEmpty(categoryId)) {
            categoryId = null;
        }
        if (UtilValidate.isEmpty(productId)) {
            productId = null;
        }
        
        if (pathElements != null) {
        
            // get category info going with the IDs that remain
            if (pathElements.size() == 1) {
                CategoryWorker.setTrail(request, pathElements.get(0), null);
                //categoryId = pathElements.get(0); // Cato: Assume caller did this
            } else if (pathElements.size() == 2) {
                CategoryWorker.setTrail(request, pathElements.get(1), pathElements.get(0));
                //categoryId = pathElements.get(1); // Cato: Assume caller did this
            } else if (pathElements.size() > 2) {
                if (trail == null) {
                    trail = CategoryWorker.getTrail(request);
                    if (trail == null) {
                        trail = FastList.newInstance();
                    }
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
                //categoryId = pathElements.get(pathElements.size() - 1);  // Cato: Assume caller did this
            }
        } else {
            // Cato: nothing here for now
        }
        
        if (pathElements == null || pathElements.size() <= 0) {
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
        
        // Cato: Make sure we always reset this
        //if (categoryId != null) {
        request.setAttribute("productCategoryId", categoryId);
        //}

        String rootCategoryId = null;
        if (pathElements.size() >= 1) {
            rootCategoryId = pathElements.get(0);
        }
        // Cato: Make sure we always reset this
        //if (rootCategoryId != null) {
        request.setAttribute("rootCategoryId", rootCategoryId);
        //}

        if (productId != null) {
            request.setAttribute("product_id", productId);
            request.setAttribute("productId", productId);
        }
        
        request.setAttribute("categoryTrailUpdated", Boolean.TRUE); // Cato: This is new
    }
    
    /**
     * Cato: Checks if the current category and product was already processed for this request,
     * and if not, adjusts them in request and session (including trail).
     * Returns the categoryId.
     * 
     * @see #getAdjustCurrentCategoryAndProduct(HttpServletRequest, String, String)
     */
    public static String getAdjustCurrentCategoryAndProduct(HttpServletRequest request, String productId) {
        return getAdjustCurrentCategoryAndProduct(request, productId, null);
    }
    
    /**
     * Cato: Checks if the current category was already processed for this request,
     * and if not, adjusts them in request and session (including trail).
     * Returns the categoryId.
     * 
     * @see #getAdjustCurrentCategoryAndProduct(HttpServletRequest, String, String)
     */
    public static String getAdjustCurrentCategory(HttpServletRequest request, String categoryId) {
        return getAdjustCurrentCategoryAndProduct(request, null, categoryId);
    }
    
    /**
     * Cato: Checks if the current category and product was already processed for this request,
     * and if not, adjusts them in request and session (including trail).
     * Returns the categoryId.
     * <p>
     * This may be called from other events or screen actions where modifying request and session is safe.
     * <p>
     * If productId is empty, assumes dealing with categories only.
     * <p>
     * NOTE: This is an amalgamation of logic in {@link CatalogUrlFilter#doFilter} and {@link CatalogUrlServlet#doGet}.
     * <p>
     * FIXME?: Currently, like original CatalogUrlFilter, when trail was not already set, this will always produce 
     * a trail based on product or category's default category under the catalog top category. 
     * This is generally not desirable, but okay for simple shops.
     */
    public static String getAdjustCurrentCategoryAndProduct(HttpServletRequest request, String productId, String categoryId) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        
        String currentProductId = (String) request.getAttribute("productId");
        String currentCategoryId = (String) request.getAttribute("productCategoryId");
        
        if (UtilValidate.isEmpty(productId)) {
            productId = null;
        }
        if (UtilValidate.isEmpty(categoryId)) {
            categoryId = null;
        }
        if (UtilValidate.isEmpty(currentProductId)) {
            currentProductId = null;
        }
        if (UtilValidate.isEmpty(currentCategoryId)) {
            currentCategoryId = null;
        }
        
        // Just use a dedicated flag instead of this. Much less likely to conflict with other code.
        //// Generally, we want to enter this if the current category is not set. If it's set it means we already did this call (or an equivalent one) somewhere.
        //if (currentCategoryId == null || 
        //    (!currentCategoryId.equals(categoryId)) || // This shouldn't really happen, but can deal with it for free
        //    (productId != null && !productId.equals(currentProductId))) {
        if (!Boolean.TRUE.equals(request.getAttribute("categoryTrailUpdated"))) {
        
            // NOTE: We only reuse the current category ID, not product ID, because if caller passed productId null it means
            // we're only doing categories.
            if (categoryId == null) {
                categoryId = currentCategoryId;
            }
            
            List<String> trailElements = CatalogUrlFilter.makeTrailElements(request, delegator, categoryId, productId);

            updateRequestAndTrail(request, categoryId, productId, trailElements, null);
        }
        return currentCategoryId;
    }
    
    
    public static String makeCategoryUrl(HttpServletRequest request, String previousCategoryId, String productCategoryId, String productId, String viewSize, String viewIndex, String viewSort, String searchString) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        try {
            GenericValue productCategory = EntityQuery.use(delegator).from("ProductCategory").where("productCategoryId", productCategoryId).cache().queryOne();
            CategoryContentWrapper wrapper = new CategoryContentWrapper(productCategory, request);
            List<String> trail = CategoryWorker.getTrail(request);
            return makeCategoryUrl(delegator, wrapper, trail, request.getContextPath(), previousCategoryId, productCategoryId, productId, viewSize, viewIndex, viewSort, searchString);
        } catch (GenericEntityException e) {
            Debug.logWarning(e, "Cannot create category's URL for: " + productCategoryId, module);
            return redirectUrl;
        }
    }

    public static String makeCategoryUrl(Delegator delegator, CategoryContentWrapper wrapper, List<String> trail, String contextPath, String previousCategoryId, String productCategoryId, String productId, String viewSize, String viewIndex, String viewSort, String searchString) {
        String url = "";
        StringWrapper alternativeUrl = wrapper.get("ALTERNATIVE_URL", "url");
        
        if (UtilValidate.isNotEmpty(alternativeUrl) && UtilValidate.isNotEmpty(alternativeUrl.toString())) {
            StringBuilder urlBuilder = new StringBuilder();
            urlBuilder.append(contextPath);
            if (urlBuilder.length() == 0 || urlBuilder.charAt(urlBuilder.length() - 1) != '/') {
                urlBuilder.append("/");
            }
            // append alternative URL
            url = UrlServletHelper.invalidCharacter(alternativeUrl.toString());
            urlBuilder.append(url);
            if (UtilValidate.isNotEmpty(productCategoryId)) {
                urlBuilder.append("-");
                urlBuilder.append(productCategoryId);
                urlBuilder.append("-c");
            }
            // append view index
            if (UtilValidate.isNotEmpty(viewIndex)) {
                if (!urlBuilder.toString().endsWith("?") && !urlBuilder.toString().endsWith("&")) {
                    urlBuilder.append("?");
                }
                urlBuilder.append("viewIndex=" + viewIndex + "&");
            }
            // append view size
            if (UtilValidate.isNotEmpty(viewSize)) {
                if (!urlBuilder.toString().endsWith("?") && !urlBuilder.toString().endsWith("&")) {
                    urlBuilder.append("?");
                }
                urlBuilder.append("viewSize=" + viewSize + "&");
            }
            // append view sort
            if (UtilValidate.isNotEmpty(viewSort)) {
                if (!urlBuilder.toString().endsWith("?") && !urlBuilder.toString().endsWith("&")) {
                    urlBuilder.append("?");
                }
                urlBuilder.append("viewSort=" + viewSort + "&");
            }
            // append search string
            if (UtilValidate.isNotEmpty(searchString)) {
                if (!urlBuilder.toString().endsWith("?") && !urlBuilder.toString().endsWith("&")) {
                    urlBuilder.append("?");
                }
                urlBuilder.append("searchString=" + searchString + "&");
            }
            if (urlBuilder.toString().endsWith("&")) {
                return urlBuilder.toString().substring(0, urlBuilder.toString().length()-1);
            }
            
            url = urlBuilder.toString();
        } else {
            if (UtilValidate.isEmpty(trail)) {
                trail = FastList.newInstance();
            }
            url = CatalogUrlServlet.makeCatalogUrl(contextPath, trail, productId, productCategoryId, previousCategoryId);
        }
        
        return url;
    }
    
    public static String makeProductUrl(HttpServletRequest request, String previousCategoryId, String productCategoryId, String productId) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        String url = null;
        try {
            GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", productId).cache().queryOne();
            ProductContentWrapper wrapper = new ProductContentWrapper(product, request);
            List<String> trail = CategoryWorker.getTrail(request);
            url = makeProductUrl(delegator, wrapper, trail, request.getContextPath(), previousCategoryId, productCategoryId, productId);
        } catch (GenericEntityException e) {
            Debug.logWarning(e, "Cannot create product's URL for: " + productId, module);
            return redirectUrl;
        }
        return url;
    }

    public static String makeProductUrl(Delegator delegator, ProductContentWrapper wrapper, List<String> trail, String contextPath, String previousCategoryId, String productCategoryId, String productId) {
        String url = "";
        StringWrapper alternativeUrl = wrapper.get("ALTERNATIVE_URL", "url");
        if (UtilValidate.isNotEmpty(alternativeUrl) && UtilValidate.isNotEmpty(alternativeUrl.toString())) {
            StringBuilder urlBuilder = new StringBuilder();
            urlBuilder.append(contextPath);
            if (urlBuilder.length() == 0 || urlBuilder.charAt(urlBuilder.length() - 1) != '/') {
                urlBuilder.append("/");
            }
            // append alternative URL
            url = UrlServletHelper.invalidCharacter(alternativeUrl.toString());
            urlBuilder.append(url);
            if (UtilValidate.isNotEmpty(productId)) {
                urlBuilder.append("-");
                urlBuilder.append(productId);
                urlBuilder.append("-p");
            }
            url = urlBuilder.toString();
        } else {
            if (UtilValidate.isEmpty(trail)) {
                trail = FastList.newInstance();
            }
            url = CatalogUrlServlet.makeCatalogUrl(contextPath, trail, productId, productCategoryId, previousCategoryId);
        }
        return url;
    }
}
