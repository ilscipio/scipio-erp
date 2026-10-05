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


import org.ofbiz.base.util.*;
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.category.*;
import com.ilscipio.scipio.solr.*;
import org.ofbiz.product.product.ProductContentWrapper;

// SCIPIO: NOTE: This script is responsible for checking whether solr is applicable (if no check, implies the shop assumes solr is always enabled).

final module = "Breadcrumbs.groovy";
breadcrumbsList = [];

try {
    currentTrail = org.ofbiz.product.category.CategoryWorker.getCategoryPathFromTrailAsList(request);
    
    currentCatalogId = CatalogWorker.getCurrentCatalogId(request);
    // SCIPIO: IMPORTANT: Check request attribs before parameters map
    curCategoryId = parameters.category_id ?: parameters.CATEGORY_ID ?: request.getAttribute("productCategoryId") ?: parameters.productCategoryId ?: "";
    curProductId = parameters.product_id ?: "" ?: parameters.PRODUCT_ID ?: "";
    if (UtilValidate.isEmpty(curCategoryId)) {
        if (context.product) {
            curCategoryId = product.primaryProductCategoryId;
        }
    }
    
    topCategoryId = CatalogWorker.getCatalogTopCategoryId(request, currentCatalogId);
    productCategoryId = curCategoryId;
    
    validBreadcrumb = topCategoryId + "/";
    
    dctx = dispatcher.getDispatchContext();
    categoryPath = SolrCategoryUtil.getCategoryNameWithTrail(productCategoryId, currentCatalogId, dctx, currentTrail);
    breadcrumbs = categoryPath.split("/");
    for (breadcrumb in breadcrumbs) {
        if (!breadcrumb.equals(topCategoryId) && !breadcrumbsList.contains(breadcrumb))
            breadcrumbsList.add(breadcrumb);
        if (breadcrumb.equals(curCategoryId))
            break;
    }
    
    if (context.product) {
        if (context.productContentWrapper == null) {
            productContentWrapper = new ProductContentWrapper(product, request);
            context.productContentWrapper = productContentWrapper;
        }
    }
    
} catch(Exception e) {
    // We are not in a store, so we continue with regular page based breadcrumbs
    Debug.logError(e, "Error getting breadcrumbs: " + e.getMessage(), module);
}
context.breadcrumbsList = breadcrumbsList;

/*
I think there is a conceptual mistake here. The breadcrumbs don't really care if another category exists or not, nor do they list EVERY category they have.
They are rather to be seen as a way of leading up to a certain directory

if (curCategoryId) {
    availableBreadcrumbsList = dispatcher.runSync("solrAvailableCategories",
        [productCategoryId:curCategoryId,productId:null,displayProducts:false,
         catalogId:currentCatalogId,currentTrail:currentTrail, locale:context.locale, 
         userLogin:context.userLogin, timeZone:context.timeZone],
         -1, true); // SEPARATE TRANSACTION so error doesn't crash screen
    validBreadcrumb = curCategoryId;
} else if (curProductId) {
    availableBreadcrumbsList = dispatcher.runSync("solrAvailableCategories",
        [productCategoryId:null,productId:curProductId,displayProducts:false,
         catalogId:currentCatalogId,currentTrail:currentTrail, locale:context.locale, 
         userLogin:context.userLogin, timeZone:context.timeZone],
         -1, true); // SEPARATE TRANSACTION so error doesn't crash screen
}


if (availableBreadcrumbsList) {
    breadcrumbsList = [];
    for (availableBreadcrumbs in availableBreadcrumbsList.get("categories").keySet()) {
        breadcrumbs = availableBreadcrumbs.split("/");
        if (availableBreadcrumbs.contains(validBreadcrumb)) {
            for (breadcrumb in breadcrumbs) {
                if (!breadcrumb.equals(topCategoryId) && !breadcrumbsList.contains(breadcrumb))
                    breadcrumbsList.add(breadcrumb);
                if (breadcrumb.equals(curCategoryId))
                    break;
            }
        }
    }
    context.breadcrumbsList = breadcrumbsList;
}
*/
