/**
 * SCIPIO: Supplements the AdvancedSearchOptions.groovy and KeywordSearch.groovy scripts,
 * because the ofbiz originals are missing fields and things.
 * This also sets defaults so are consistent across templates (the defaults are actually
 * decided by ProductSearchSession, replicated here)
 * Added 2017-08-24.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator
import org.ofbiz.entity.util.EntityUtilProperties
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.feature.*;
import org.ofbiz.product.product.*;
import org.ofbiz.product.product.ProductSearchSession.ProductSearchOptions;
import org.ofbiz.product.product.ProductSearch.CategoryConstraint;
import org.ofbiz.product.product.ProductSearch.FeatureConstraint;
import org.ofbiz.product.product.ProductSearch.KeywordConstraint;
import org.ofbiz.product.product.ProductSearch.ProductSearchConstraint;
import org.ofbiz.product.product.ProductSearch.ProductSearchContext;
import org.ofbiz.product.product.ProductSearch.ResultSortOrder;
import org.ofbiz.product.product.ProductSearch.SortKeywordRelevancy;
import org.ofbiz.product.product.ProductSearch.SortProductField;
import org.ofbiz.product.product.ProductSearch.SortProductPrice;
import org.ofbiz.product.product.ProductSearch.SortProductFeature;
import org.ofbiz.product.category.CategoryWorker;
import com.ilscipio.scipio.solr.*;
 
localVarsOnly = context.localVarsOnly;
if (localVarsOnly == null) localVarsOnly = false;
context.remove("localVarsOnly");

currentCatalogId = context.currentCatalogId;
if (!currentCatalogId) currentCatalogId = CatalogWorker.getCurrentCatalogId(request);
context.currentCatalogId = currentCatalogId;

resetSearch = context.resetSearch != null ? context.resetSearch : (parameters.resetSearch != null ? parameters.resetSearch.toString() : null);
resetSearch = UtilMisc.booleanValueVersatile(resetSearch, false);
context.resetSearch = resetSearch;

searchApplyDefaults = context.searchApplyDefaults != null ? context.searchApplyDefaults : false;

// preferDefaultSearchCat: if set true, will tend to favor the "default search category", otherwise
// the "Any" gets selected by default.
// FIXME: preferDefaultSearchCat=true currently interferes with preserving user's last search (only works if showSearchAnyCat=false) 
preferDefaultSearchCat = context.preferDefaultSearchCat;
if (preferDefaultSearchCat == null) preferDefaultSearchCat = false;
context.preferDefaultSearchCat = preferDefaultSearchCat;

// showSearchAnyCat: if false, empty search cat is not display (WARN: this is not enforced server side)
showSearchAnyCat = context.showSearchAnyCat;
if (showSearchAnyCat == null) showSearchAnyCat = true;
context.showSearchAnyCat = showSearchAnyCat;

kwsParams = context.kwsParams;
if (!localVarsOnly && !kwsParams) kwsParams = ProductSearchSession.getProductSearchOptions(session);
context.kwsParams = kwsParams;

sortOrder = context.sortOrder;
if (!sortOrder) {
    sortOrder = kwsParams?.getResultSortOrder()?.getOrderName();
    if (sortOrder && !sortOrder.startsWith("Sort")) sortOrder = "Sort" + sortOrder;
}
sortOrderDef = context.sortOrderDef != null? context.sortOrderDef : "SortKeywordRelevancy";
context.sortOrderDef = sortOrderDef;
if (resetSearch) sortOrder = null;
if (searchApplyDefaults && !sortOrder) sortOrder = sortOrderDef;
context.sortOrder = sortOrder;

sortAscending = context.sortAscending;
if (sortAscending == null) {
    sortAscending = kwsParams?.getResultSortOrder()?.isAscending();
}
sortAscendingDef = context.sortAscendingDef != null ? context.sortAscendingDef : true;
context.sortAscendingDef = sortAscendingDef;
if (resetSearch) sortAscending = null;
if (searchApplyDefaults && sortAscending == null) sortAscending = sortAscendingDef;
context.sortAscending = sortAscending;

searchString = context.searchString;
searchOperator = context.searchOperator;
if (searchString == null) {
    // NOTE: ofbiz supported multiple of these, but our form currently only sends one
    keywordConstraints = kwsParams?.getKeywordConstraints();
    if (keywordConstraints) {
        keywordConstraint = keywordConstraints[0];
        searchString = keywordConstraint.getKeywordsString();
        // NOTE: for advancedsearch, we intentionally override whatever's in context,
        // because it is just gotten from parameters map in groovy and won't make sense otherwise
        searchOperator = keywordConstraint.isAnd() ? "AND" : "OR";
    }
}
if (resetSearch) searchString = null;
context.searchString = searchString;
if (resetSearch) searchOperator = null;
searchOperatorDef = context.searchOperatorDef ?: "OR";
context.searchOperatorDef = searchOperatorDef;
if (searchApplyDefaults && !searchOperator) searchOperator = searchOperatorDef;
context.searchOperator = searchOperator;

categoryConstraints = kwsParams?.getConstraintsByType(CategoryConstraint.class);

// NOTE: the extra searchCategoryId* are because searchCategoryId is reserved in and used ambiguously by AdvancedSearchOptions.groovy
// searchCategoryIdEff is the last category ID _actually_ searched
// searchCategoryIdSel is for selection, workaround for advancedsearch weird groovy
searchIncludeSubCat = context.searchIncludeSubCat;

searchCategoryIdEff = context.searchCategoryIdEff;
if (searchCategoryIdEff == null) {
    if (categoryConstraints) {
        searchCategoryIdEff = [];
        for(categoryConstraint in categoryConstraints) {
            searchCategoryIdEff.add(categoryConstraint.getProductCategoryId());
        }
        // WARN: 2017-08-28: currently assume all are using same searchIncludeSubCat flag
        searchIncludeSubCat = categoryConstraints[0].isIncludeSubCategories();
    }
}
if (resetSearch) searchCategoryIdEff = null;
context.searchCategoryIdEff = searchCategoryIdEff;
searchIncludeSubCatDef = context.searchIncludeSubCatDef != null ? context.searchIncludeSubCatDef : true;
context.searchIncludeSubCatDef = searchIncludeSubCatDef;
if (resetSearch) searchIncludeSubCat = null;
if (searchApplyDefaults && searchIncludeSubCat == null) searchIncludeSubCat = searchIncludeSubCatDef;
context.searchIncludeSubCat = searchIncludeSubCat;

searchCategoryIdDef = context.searchCategoryIdDef;
if (searchCategoryIdDef == null) {
    if (preferDefaultSearchCat || !showSearchAnyCat) {
        searchCategoryIdDef = CatalogWorker.getCatalogSearchCategoryId(request, currentCatalogId);
    }
}
context.searchCategoryIdDef = searchCategoryIdDef;

searchCategoryIdSelParam = (!localVarsOnly) ? (parameters.SEARCH_CATEGORY_ID != null ? parameters.SEARCH_CATEGORY_ID.toString() : null) : null;
searchCategoryIdSel = context.searchCategoryIdSel != null ? context.searchCategoryIdSel : searchCategoryIdSelParam;
if (searchCategoryIdSel == null) {
    searchCategoryIdSel = searchCategoryIdEff != null ? searchCategoryIdEff : searchCategoryIdDef;
}
if (resetSearch) searchCategoryIdSel = searchCategoryIdSelParam;
if (searchApplyDefaults && searchCategoryIdSel == null) searchCategoryIdSel = searchCategoryIdDef;
context.searchCategoryIdSel = searchCategoryIdSel;


