/*
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
 */
/*
 * NOTE: This script is also referenced by the shop's screens and
 * should not contain order component's specific code.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.service.*;
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.category.CategoryContentWrapper;
import org.ofbiz.product.category.CategoryWorker;
import org.ofbiz.product.store.ProductStoreWorker;
import javolution.util.FastMap;
import javolution.util.FastList;
try{
	if(!context.productCategoryId){
		productCategoryId = request.getAttribute("productCategoryId");
		context.productCategoryId = productCategoryId;
	}
	currentCatalogId = CatalogWorker.getCurrentCatalogId(request);
	
	// get the product category & members
	if (context.viewSize)
	    parameters.VIEW_SIZE = context.viewSize;
	if (context.viewIndex)
	    parameters.VIEW_INDEX = context.viewIndex;
	result = dispatcher.runSync("solrProductsSearch",[productCategoryId:productCategoryId,viewSize:parameters.VIEW_SIZE, viewIndex:parameters.VIEW_INDEX]);
	
	productCategory = delegator.findByPrimaryKeyCache("ProductCategory", UtilMisc.toMap("productCategoryId", productCategoryId));
	solrProducts = result.results;
	
	// Prevents out of stock product to be displayed on site
	productStore = ProductStoreWorker.getProductStore(request);
	if(productStore) {
	    if("N".equals(productStore.showOutOfStockProducts)) {
	        productsInStock = [];
	        solrProducts.each { productCategoryMember ->
	            productFacility = delegator.findOne("ProductFacility", [productId : productCategoryMember.productId, facilityId : productStore.inventoryFacilityId], true);
	            if(productFacility) {
	                if(productFacility.lastInventoryCount >= 1) {
	                    productsInStock.add(productCategoryMember);
	                }
	            }
	        }
	        context.solrProducts = productsInStock;
	    } else {
	        context.solrProducts = solrProducts;
	    }
	}
	
	/*
	subCatList = FastList.newInstance();
	if (CategoryWorker.checkTrailItem(request, productCategory.getString("productCategoryId")) || (!UtilValidate.isEmpty(productCategoryId) && productCategoryId == productCategory.productCategoryId))
	    subCatList = CategoryWorker.getRelatedCategoriesRet(request, "subCatList", productCategory.getString("productCategoryId"), true);
	
	context.productSubCategoryList = subCatList;
	*/
	context.productCategory = productCategory;
	context.viewIndex = result.viewIndex;
	context.viewSize = result.viewSize;
	context.listSize = result.listSize;
	
	
	if (result.viewSize > 0)
	    context.listIndex = Math.ceil(result.listSize/result.viewSize);
	if (!parameters.VIEW_SIZE.equals(String.valueOf(context.viewSize))) {
	    pageViewSize = Integer.parseInt(parameters.VIEW_SIZE).intValue();
	    context.listIndex = Math.ceil(result.listSize/pageViewSize);
	    context.pageViewSize = pageViewSize;
	}
	
	if (!parameters.CURR_INDEX)
	    context.currIndex = 1;
	else
	    context.currIndex = Integer.parseInt(parameters.CURR_INDEX).intValue();
	
	// set the content path prefix
	contentPathPrefix = CatalogWorker.getContentPathPrefix(request);
	context.put("contentPathPrefix", contentPathPrefix);
} catch(Exception e){
	Debug.logInfo(""+e,"CategoryDetail.groovy")
}