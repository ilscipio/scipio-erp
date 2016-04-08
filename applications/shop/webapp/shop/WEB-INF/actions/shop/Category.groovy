/*
 * ILSCIPIO
 */

/*
 * This script is also referenced by the shop's screens and
 * should not contain order component's specific code.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.category.CategoryWorker;
import org.ofbiz.product.category.CategoryContentWrapper;

// Cato: NOTE: This script is responsible for checking whether solr is applicable.

// Cato: this allows to use the script for local scopes without affecting request
localVarsOnly = context.localVarsOnly;
if (localVarsOnly == null) {
    localVarsOnly = false;
}
context.remove("localVarsOnly");
// Cato: In some screens may need to read request vars/params, but not update request...
updateRequestVars = context.updateRequestVars;
if (updateRequestVars == null) {
    if (localVarsOnly) {
        updateRequestVars = false;
    } else {
        updateRequestVars = true; // default true for now (legacy)
    }
}
context.remove("updateRequestVars");

detailScreen = "categorydetail";
catalogName = CatalogWorker.getCatalogName(request);

productCategoryId = context.productCategoryId;
if (!localVarsOnly) {
    if (!productCategoryId) {
        productCategoryId = request.getAttribute("productCategoryId") ?: parameters.category_id;
    }
}

context.productCategoryId = productCategoryId;


/* NOTE DEJ20070220: this is a weird way to do this and caused unacceptable side effects as described in the related
 * comment in the Main.groovy file
 *
 * NOTE JLR 20070221 this should be done using the same method than in add to cart. I will do it like that and remove all this after.
 *
if (productCategoryId) {
    session.setAttribute("productCategoryId", productCategoryId);// for language change
    previousParams = session.getAttribute("_PREVIOUS_PARAMS_");
    if (previousParams) {
        previousParams = UtilHttp.stripNamedParamsFromQueryString(previousParams, ["category_id"]);
        previousParams += "&category_id=" + productCategoryId;
    } else {
        previousParams = "category_id=" + productCategoryId;
    }
    session.setAttribute("_PREVIOUS_PARAMS_", previousParams);    // for login
    context.previousParams = previousParams;
}
 */

category = delegator.findOne("ProductCategory", [productCategoryId : productCategoryId], true);
if (category) {
    if (category.detailScreen) {
        detailScreen = category.detailScreen;
    }
    categoryContentWrapper = new CategoryContentWrapper(category, request);
    // Cato: don't want page title overridden/forced by groovy
    //context.title = categoryContentWrapper.get("CATEGORY_NAME","html");
    context.categoryTitle = categoryContentWrapper.get("CATEGORY_NAME","html");
    categoryDescription = categoryContentWrapper.get("DESCRIPTION","html");
    if (categoryDescription) {
        context.metaDescription = categoryDescription;
        context.metaKeywords = categoryDescription + ", " + catalogName;
    } else {
        context.metaKeywords = catalogName;
    }
    context.productCategory = category;
}

// check the catalogs template path and update
templatePathPrefix = CatalogWorker.getTemplatePathPrefix(request);
if (templatePathPrefix) {
    detailScreen = templatePathPrefix + detailScreen;
}
context.detailScreen = detailScreen;

if (updateRequestVars) {
    // Cato: NOTE: If this happens more than once in a request, you need to set updateRequestVars Boolean false in some of the screens
    // Ideally this should only be done from the actions of full screen definitions (not screen parts)
    Debug.logInfo("Cato: Setting request-wide productCategoryId (should be once per request only!): " + productCategoryId, "Category.groovy");
    request.setAttribute("productCategoryId", productCategoryId);
    request.setAttribute("defaultViewSize", "9");
    request.setAttribute("limitView", true);
}
