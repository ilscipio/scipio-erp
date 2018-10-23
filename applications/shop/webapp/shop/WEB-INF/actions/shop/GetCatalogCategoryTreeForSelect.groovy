/**
 * SCIPIO: a custom script for getting all catalog categories (useCache=true by default),
 * originally written for advancedsearch to get simple category list.
 * TODO?: move to java? cache results?
 */

import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.product.catalog.CatalogWorker;
import org.ofbiz.product.category.CategoryWorker;

final module = "GetCatalogCategoryTreeForSelect.groovy";

currentCatalogId = context.currentCatalogId ?: CatalogWorker.getCurrentCatalogId(request);
// DEV NOTE: for specific store implementations, don't change the default here - set it in screen widget
selectCatCategoryTypeId = context.selectCatCategoryTypeId ?: ["PCCT_BROWSE_ROOT", "PCCT_SEARCH", "PCCT_PROMOTIONS", "PCCT_BEST_SELL"];
if (selectCatCategoryTypeId instanceof String) selectCatCategoryTypeId = [selectCatCategoryTypeId];

makeCatEntry = { cat ->
    def children = null;
    def childrenValues = CategoryWorker.getRelatedCategoriesRet(delegator, "", cat.productCategoryId, true, false, false);
    if (childrenValues) {
        children = []
        for(childValue in childrenValues) {
            children.add(makeCatEntry(childValue));
        }
    }
    return [value:cat, children:children];
}

makeCatTree = { cats ->
    def entryList = null;
    def searchCatList = null;
    def regCatList = null;
    if (cats) {
        entryList = [];
        searchCatList = [];
        regCatList = [];
        for(cat in cats) {
            def prodCat = cat;
            def extraFields = [:];
            if ("ProdCatalogCategory" == cat.getEntityName()) {
                prodCat = cat.getRelatedOne("ProductCategory", true);
                extraFields.prodCatalogCategoryTypeId = cat.prodCatalogCategoryTypeId;
            } 
            def entry = makeCatEntry(prodCat);
            entry.isTop = true;
            entry.putAll(extraFields);
            entryList.add(entry);
            if ("ProdCatalogCategory" == cat.getEntityName()) {
                if (cat.prodCatalogCategoryTypeId == "PCCT_SEARCH") {
                    searchCatList.add(entry);
                } else {
                    regCatList.add(entry);
                }
            }
        }
    }
    return [isRoot:true, children:entryList, searchChildren:searchCatList, regChildren:regCatList];
};

condList = [];
for(typeId in selectCatCategoryTypeId) {
    condList.add(EntityCondition.makeCondition("prodCatalogCategoryTypeId", typeId));
}
topCats = EntityUtil.filterByOr(CatalogWorker.getProdCatalogCategories(delegator, currentCatalogId, null), condList);

categoryTree = makeCatTree(topCats);
context.categoryTree = categoryTree;
