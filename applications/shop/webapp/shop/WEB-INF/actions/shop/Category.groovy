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

detailScreen = "categorydetail";
catalogName = CatalogWorker.getCatalogName(request);

productCategoryId = request.getAttribute("productCategoryId") ?: parameters.category_id;
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

category = delegator.findByPrimaryKeyCache("ProductCategory", [productCategoryId : productCategoryId]);
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

request.setAttribute("productCategoryId", productCategoryId);
request.setAttribute("defaultViewSize", "9");
request.setAttribute("limitView", true);
