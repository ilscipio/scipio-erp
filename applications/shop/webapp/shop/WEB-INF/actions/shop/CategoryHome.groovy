/*
 * Ilscipio
 */

import org.ofbiz.base.util.*;
import org.ofbiz.service.*;
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.category.CategoryContentWrapper;
import org.ofbiz.product.category.CategoryWorker;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.content.content.ContentWorker;

// SCIPIO: NOTE: This script is responsible for checking whether solr is applicable (if no check, implies the shop assumes solr is always enabled).

final module = "CategoryHome.groovy";

private void getCatalogCategoriesByType(productCategoryId, title, id) {
    if (productCategoryId != null) {
        productCategoryMap = [:];
        // get the product category & members
        andMap = [productCategoryId : productCategoryId,
                viewIndexString : viewIndex,
                viewSizeString : viewSize,
                defaultViewSize : defaultViewSize,
                limitView : limitView];
        andMap.put("prodCatalogId", catalogId);
        andMap.put("checkViewAllow", true);
        if (context.orderByFields) {
                andMap.put("orderByFields", context.orderByFields);
        } else {
                andMap.put("orderByFields", ["sequenceNum", "productId"]);
        }
        catResult = dispatcher.runSync("getProductCategoryAndLimitedMembers", andMap,
            -1, true); // SEPARATE TRANSACTION so error doesn't crash screen
        if (!ServiceUtil.isSuccess(catResult)) {
            Debug.logError("Error in getProductCategoryAndLimitedMembers: " + ServiceUtil.getErrorMessage(catResult), "CategoryHome.groovy");
            return;
        }

        productCategory = catResult.productCategory;
        productCategoryMembers = catResult.productCategoryMembers;

        // Prevents out of stock product to be displayed on site
        productStore = ProductStoreWorker.getProductStore(request);
        if(productStore) {
            productCategoryMembersList = [];
            if("N".equals(productStore.showOutOfStockProducts)) {
                productsInStock = [];
                productCategoryMembers.each { productCategoryMember ->
                    productFacility = delegator.findOne("ProductFacility", [productId : productCategoryMember.productId, facilityId : productStore.inventoryFacilityId], true);
                    if(productFacility) {
                        if(productFacility.lastInventoryCount >= 1) {
                            productCategoryMembersList.add(productCategoryMember);
                            //productsInStock.add(productCategoryMember);
                        } 
                    }
                }
                //context.productCategoryMembers = productsInStock;
            } else {
                productCategoryMembersList.addAll(productCategoryMembers);
                //context.productCategoryMembers = productCategoryMembers;
            }
            productCategoryMap.put("productCategoryMembers", productCategoryMembersList);
        }
                
        productCategoryMap.put("viewIndex", catResult.viewIndex);
        productCategoryMap.put("viewSize", catResult.viewSize);
        productCategoryMap.put("lowIndex", catResult.lowIndex);
        productCategoryMap.put("highIndex", catResult.highIndex);
        productCategoryMap.put("listSize", catResult.listSize);
        productCategoryMap.put("categoryContentWrapper", new CategoryContentWrapper(productCategory, request));
        productCategoryMap.put("productCategory", productCategory);
        productCategoryMap.put("title", uiLabelMap.get(title));
        productCategoryMap.put("id", id);
        
        productCategoriesMap.put(productCategoryId, productCategoryMap);
    }

}

/*
 * -------------------------------------------------------------------------------------------------------- 
 */

catalogId = CatalogWorker.getCurrentCatalogId(request);
whatsNewCategory = CatalogWorker.getCatalogWhatsNewCategoryId(request, catalogId);
mostPopularCategory = CatalogWorker.getCatalogMostPopularCategoryId(request, catalogId);
mostDownloadedCategory = CatalogWorker.getCatalogBestSellingCategoryId(request, catalogId);

viewSize = parameters.VIEW_SIZE;
viewIndex = parameters.VIEW_INDEX;

// set the default view size
//defaultViewSize = request.getAttribute("defaultViewSize") ?: 9;
defaultViewSize = 3;
context.defaultViewSize = defaultViewSize;

// set the limit view
limitView = request.getAttribute("limitView") ?: true;
context.limitView = limitView;

// get the products form the best-selling and promotions categories
productCategoriesMap = [:];
try {
    getCatalogCategoriesByType(mostDownloadedCategory, "SyracusCategoryHomeMostDownloadedTitle", "most-downloaded");
    getCatalogCategoriesByType(whatsNewCategory, "SyracusCategoryHomeLastestBooksTitle", "whats-new");
} catch(Exception e) {
    Debug.logError(e, "Error getting catalog categories by type: " + e.getMessage(), module);
}

// get the most-popular related categories
mostPopularCates = [];
childCategoryList = CategoryWorker.getRelatedCategoriesRet(request, "childCategoryList", mostPopularCategory, true);
if (childCategoryList.size() > 0) {
    mostPopularCates.add(childCategoryList);
}

// get the additional content to show on the home page
//additionalContent=ContentWorker.findContentForRendering(delegator,"SYRACUS_HOME_PAGE", locale, null, null, false);
//additionalContent=ContentWorker.getContentAssocsWithId(delegator, "SYRACUS_HOME_PAGE", new Date(), null, "from", UtilMisc.toList("SUB_CONTENT"));


//context.additionalContent=additionalContent;
context.productCategoriesMap = productCategoriesMap; 
context.mostPopularCategoryList = mostPopularCates;