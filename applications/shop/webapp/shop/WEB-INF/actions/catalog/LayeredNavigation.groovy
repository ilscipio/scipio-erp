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

import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.product.catalog.CatalogWorker;
import org.ofbiz.product.category.CategoryContentWrapper;
import org.ofbiz.product.category.CategoryWorker;
import org.ofbiz.product.product.ProductSearch;
import org.ofbiz.product.product.ProductSearchSession;

searchCategoryId = parameters.searchCategoryId;
if (!searchCategoryId) {
    searchCategoryId = context.productCategoryId;
}
if (searchCategoryId) {
    currentSearchCategory = from("ProductCategory").where("productCategoryId", searchCategoryId).queryOne();
    CategoryWorker.getRelatedCategories(request, "subCategoryList", searchCategoryId, false);
    subCategoryList = request.getAttribute("subCategoryList");
    CategoryContentWrapper categoryContentWrapper = new CategoryContentWrapper(currentSearchCategory, request);
    context.currentSearchCategory = currentSearchCategory;
    context.categoryContentWrapper = categoryContentWrapper;
}
productCategoryId = context.productCategoryId;
if (productCategoryId)  {
   context.productCategory = from("ProductCategory").where("productCategoryId", productCategoryId).queryOne();
   parameters.SEARCH_CATEGORY_ID = productCategoryId;
}

if (!parameters.clearSearch || !"N".equals(parameters.clearSearch)) {
    ProductSearchSession.searchClear(session);
}

ProductSearchSession.processSearchParameters(parameters, request);
prodCatalogId = CatalogWorker.getCurrentCatalogId(request);
result = ProductSearchSession.getProductSearchResult(request, delegator, prodCatalogId);

context.index = ProductSearchSession.getCategoryCostraintIndex(session);

searchConstraintList = ProductSearchSession.getProductSearchOptions(session).getConstraintList();

if (searchCategoryId) {
    productCategoryRollups = from("ProductCategoryRollup").where("productCategoryId", searchCategoryId).filterByDate().queryList();
    previousCategoryId = null;
    if (productCategoryRollups) {
        for (categoryRollup in productCategoryRollups) {
            categoryConstraint = new ProductSearch.CategoryConstraint(categoryRollup.parentProductCategoryId, true, false);
            if (searchConstraintList.contains(categoryConstraint)) {
                previousCategoryId = categoryRollup.parentProductCategoryId;
                context.previousCategoryId = previousCategoryId;
            }
        }
    }
}

context.showSubCats = true;
if (subCategoryList) {
    thisSubCategoryList = [];
    subCategoryList.each { subCategory ->
        categoryCount = ProductSearchSession.getCountForProductCategory(subCategory.productCategoryId, session, delegator);
        if (categoryCount > 0) {
            subCategoryContentWrapper = new CategoryContentWrapper(subCategory, request);
            thisSubCategoryList.add([productCategoryId: subCategory.productCategoryId, categoryName: subCategory.categoryName, count: categoryCount, categoryContentWrapper: subCategoryContentWrapper]);
        }
    }
    if (thisSubCategoryList) {
        context.subCategoryList = thisSubCategoryList;
    } else {
        context.showSubCats = false;
    }
} else {
    context.showSubCats = false;
}

context.showColors = true;
colors = ProductSearchSession.listCountByFeatureForType("COLOR", session, delegator);
colorFeatureType = from("ProductFeatureType").where("productFeatureTypeId", "COLOR").queryOne();
if (colors) {
    colors.each { color ->
        featureConstraint = new ProductSearch.FeatureConstraint(color.productFeatureId, false);
        if (searchConstraintList.contains(featureConstraint)) {
            context.showColors=false;
        }
    }    
} else {
    context.showColors = false;
}
if (context.showColors) {
    context.colors = colors;
    context.colorFeatureType = colorFeatureType;
}

availablePriceRangeList = [[low: "0", high: "10"], [low: "10", high: "20"], [low: "20", high: "30"], [low: "30", high: "40"], [low: "40", high: "50"], [low: "50", high: "60"], [low: "60", high: "70"], [low: "70", high: "80"], [low: "80", high: "90"], [low: "90", high: "100"]];
priceRangeList = [];
context.showPriceRange = true;
availablePriceRangeList.each { priceRange ->
    priceRangeConstraint = new ProductSearch.ListPriceRangeConstraint(new BigDecimal(priceRange.low), new BigDecimal(priceRange.high), UtilHttp.getCurrencyUom(request));
    if (searchConstraintList.contains(priceRangeConstraint)) {
        context.showPriceRange = false;
    } else {
        priceRangeCount = ProductSearchSession.getCountForListPriceRange(new BigDecimal(priceRange.low), new BigDecimal(priceRange.high), session, delegator);
        if (priceRangeCount != 0) {
            priceRangeList.add([low: priceRange.low, high: priceRange.high, count: priceRangeCount]);
        }
    }
}
if (!priceRangeList) {
    context.showPriceRange = false;
}
if (context.showPriceRange) {
    context.priceRangeList = priceRangeList;
}

context.productIds = result.productIds;
context.viewIndex = result.viewIndex;
context.viewSize = result.viewSize;
context.listSize = result.listSize;
context.lowIndex = result.lowIndex;
context.highIndex = result.highIndex;
context.paging = result.paging;
context.previousViewSize = result.previousViewSize;
context.searchConstraintStrings = result.searchConstraintStrings;
