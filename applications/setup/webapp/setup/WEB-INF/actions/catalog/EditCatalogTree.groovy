import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;

import com.ilscipio.scipio.treeMenu.jsTree.JsTreeCore;
import com.ilscipio.scipio.treeMenu.jsTree.JsTreeHelper;
import com.ilscipio.scipio.treeMenu.jsTree.JsTreePlugin.JsTreeTypesPlugin;
import com.ilscipio.scipio.treeMenu.jsTree.JsTreePlugin.JsTreeTypesPlugin.JsTreeType;

final module = "EditCatalogTree.groovy";
final displayProductsDefault = true; // FIXME: should be false default + option to show, otherwise risk very slow

productStoreId = context.productStoreId;
if (!productStoreId) {
    Debug.logError("Setup: No product store Id found for catalog tree.", module);
    return;
}

displayProducts = context.displayProducts;
if (displayProducts == null) {
    displayProducts = UtilMisc.booleanValueVersatile(parameters.displayProducts);
    if (displayProducts == null) {
        displayProducts = displayProductsDefault;
    }
}
context.displayProducts = displayProducts;

currentProdCatalogId = context.prodCatalogId;
currentProductCategoryId = context.productCategoryId;

// only either catalog or category should be "selected"
currentCatalogSelected = false;
currentCategorySelected = false;
selectedProdCatalogId = null;
selectedProductCategoryId = null;
if (currentProductCategoryId) {
    currentCategorySelected = true;
    selectedProductCategoryId = currentProductCategoryId;
} else if (currentProdCatalogId) {
    currentCatalogSelected = true;
    selectedProdCatalogId = currentProdCatalogId;
}
context.currentCatalogSelected = currentCatalogSelected;
context.currentCategorySelected = currentCategorySelected;
context.selectedProdCatalogId = selectedProdCatalogId;
context.selectedProductCategoryId = selectedProductCategoryId;

pluginList = new JsTreeHelper.JsTreePluginList();
pluginList.add("sort, state, unique");
pluginList.add(new JsTreeTypesPlugin(["catalog", "category", "product"], new JsTreeType(3, 5, ["category"], null), new JsTreeType(6, 2, ["category", "product"], null), new JsTreeType(0, 0, null, null)));
context.treeMenuPlugins = pluginList;
context.treeMenuSettings = new JsTreeCore(false, null, null);

treeMenuHelper = new JsTreeHelper();
treeMenuData = [];

productStoreCatalogs = context.productStoreCatalogList ?: [];
for (productStoreCatalog in productStoreCatalogs) {    
    prodCatalog = productStoreCatalog.getRelatedOne("ProdCatalog", false);
    if (prodCatalog) {
        def state = null;
        def categoryStates = null;
        if (currentProdCatalogId == prodCatalog.prodCatalogId) {
            state = ["opened":true, "selected":currentCatalogSelected];
            if (currentProductCategoryId) {
                // FIXME: issue with categories shared between parents - full path needed
                categoryStates = [
                    (currentProductCategoryId): ["opened":true, "selected":currentCategorySelected]
                ];
            }
        }
        
        result = dispatcher.runSync("buildCatalogTree", [
            "useCategoryCache": false,
            "prodCatalogId" : prodCatalog.prodCatalogId,
            "state": state,
            "categoryStates": categoryStates,
            "includeCategoryData": true,
            "includeProducts": displayProducts,
            "includeEmptyTop": true,
            "productStoreCatalog": productStoreCatalog
        ]);
        if (result?.treeList) {
            treeMenuData = treeMenuData + result.treeList;
        }
    }
}
//JsTreeHelper.preventDataItemsSameId(treeMenuData);
treeMenuHelper.addAll(treeMenuData)
context.treeMenuData = treeMenuHelper;

