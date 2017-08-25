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
import com.ilscipio.solr.SolrUtil;
 
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
if (!kwsParams) kwsParams = ProductSearchSession.getProductSearchOptions(session);
context.kwsParams = kwsParams;

sortOrder = context.sortOrder;
if (!sortOrder) {
    sortOrder = kwsParams?.getResultSortOrder()?.getOrderName();
    if (sortOrder && !sortOrder.startsWith("Sort")) sortOrder = "Sort" + sortOrder;
    else sortOrder = "SortKeywordRelevancy";
}
context.sortOrder = sortOrder;

sortAscending = context.sortAscending;
if (sortAscending == null) {
    sortAscending = kwsParams?.getResultSortOrder()?.isAscending();
    if (sortAscending == null) sortAscending = true;
}
context.sortAscending = sortAscending;

searchString = context.searchString;
if (searchString == null) {
    // NOTE: ofbiz supported multiple of these, but our form currently only sends one
    keywordConstraints = kwsParams?.getKeywordConstraints();
    if (keywordConstraints) {
        keywordConstraint = keywordConstraints[0];
        searchString = keywordConstraint.getKeywordsString();
        // NOTE: for advancedsearch, we intentionally override whatever's in context,
        // because it is just gotten from parameters map in groovy and won't make sense otherwise
        searchOperator = keywordConstraint.isAnd() ? "AND" : "OR";
        context.searchOperator = searchOperator;
    }
}
context.searchString = searchString;

// searchCategoryIdSel is workaround for advancedsearch weird groovy
searchCategoryIdSel = parameters.SEARCH_CATEGORY_ID ? parameters.SEARCH_CATEGORY_ID.toString() : null;
searchIncludeSubCat = context.searchIncludeSubCat;
if (searchCategoryIdSel == null) {
    categoryConstraints = kwsParams?.getConstraintsByType(CategoryConstraint.class);
    categoryConstraint = null;
    if (categoryConstraints) {
        categoryConstraint = categoryConstraints[0];
        searchCategoryIdSel = categoryConstraint.getProductCategoryId();
        searchIncludeSubCat = categoryConstraint.isIncludeSubCategories();
    } else {
        // LEGACY FALLBACK: depends on preferDefaultSearchCat
        if (preferDefaultSearchCat || !showSearchAnyCat) {
            searchCategoryIdSel = context.searchCategoryId;
        }
    }
}
if (searchIncludeSubCat == null) searchIncludeSubCat = true;
context.searchCategoryIdSel = searchCategoryIdSel;
context.searchIncludeSubCat = searchIncludeSubCat;

