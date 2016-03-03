import org.ofbiz.base.util.Debug
import org.ofbiz.entity.condition.EntityComparisonOperator
import org.ofbiz.entity.condition.EntityExpr
import org.ofbiz.product.category.CategoryServices
import org.ofbiz.product.category.CategoryWorker
import org.ofbiz.service.LocalDispatcher

import com.ilscipio.cato.helper.JsTreeHelper
import com.ilscipio.cato.helper.JsTreeHelper.JsTreeDataItem
import com.ilscipio.cato.helper.JsTreeHelper.JsTreePluginList
import com.ilscipio.cato.helper.JsTreeHelper.JsTreeDataItem.JsTreeDataItemState
import com.ilscipio.cato.helper.JsTreeHelper.JsTreePlugin.JsTreeTypesPlugin
import com.ilscipio.cato.helper.JsTreeHelper.JsTreePlugin.JsTreeTypesPlugin.JsTreeType

productStoreId = (context.productStoreId) ? context.productStoreId : parameters.productStoreId;
if (!productStoreId) {
    Debug.logError("No product store Id found.", "");
    return;
}

LocalDispatcher dispatcher = context.dispatcher;
pluginList = new JsTreeHelper.JsTreePluginList();
pluginList.add("sort, state");
pluginList.add(new JsTreeTypesPlugin(["catalog", "category", "product"], new JsTreeType(3, 5, ["category"], null), new JsTreeType(6, 2, ["category", "product"], null), new JsTreeType(0, 0, null, null)));
context.treeMenuPlugins = pluginList;
context.treeMenuSettings = new JsTreeHelper.JsTreeCore(false, null, null);

treeMenuData =  [];
//Get the Catalogs
productStoreCatalogs = from("ProductStoreCatalog").where(new EntityExpr("productStoreId", EntityComparisonOperator.EQUALS, productStoreId)).filterByDate().queryList();
for (productStoreCatalog in productStoreCatalogs) {
    
    prodCatalog = productStoreCatalog.getRelatedOne("ProdCatalog", true);
    if (prodCatalog) {
        result = dispatcher.runSync("buildCatalogTree", ["prodCatalogId" : prodCatalog.getString("prodCatalogId")]);
        if (result && result.get("treeList"))
            treeMenuData = treeMenuData + result.get("treeList");
    }
}

JsTreeHelper.preventDataItemsSameId(treeMenuData);
Debug.logInfo("treeMenuData "+treeMenuData,"");
context.treeMenuData = treeMenuData;
