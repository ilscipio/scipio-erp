/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

/*
 * This script is also referenced by the ecommerce's screens and
 * should not contain order component's specific code.
 */
import org.ofbiz.base.util.Debug
import org.ofbiz.entity.condition.EntityComparisonOperator
import org.ofbiz.entity.condition.EntityExpr

import com.ilscipio.cato.helper.JsTreeHelper
import com.ilscipio.cato.helper.JsTreeHelper.JsTreeDataItem
import com.ilscipio.cato.helper.JsTreeHelper.JsTreeEvent
import com.ilscipio.cato.helper.JsTreeHelper.JsTreePluginList
import com.ilscipio.cato.helper.JsTreeHelper.JsTreeTheme
import com.ilscipio.cato.helper.JsTreeHelper.JsTreeDataItem.JsTreeDataItemState
import com.ilscipio.cato.helper.JsTreeHelper.JsTreePlugin.JsTreeTypesPlugin
import com.ilscipio.cato.helper.JsTreeHelper.JsTreePlugin.JsTreeTypesPlugin.JsTreeType

// Put the result of CategoryWorker.getRelatedCategories into the separateRootType function as attribute.
// The separateRootType function will return the list of category of given catalog.
// PLEASE NOTE : The structure of the list of separateRootType function is according to the JSON_DATA plugin of the jsTree.

//Debug.log("context ========> " + context);

productStoreId = (context.productStoreId) ? context.productStoreId : parameters.productStoreId;

//JsTreeHelper.JsTreeDataItem.JsTreeDataItemState.class.getDeclaredConstructors().each { constructor ->
//  Debug.log("constructor (from groovy) ========> " + constructor);
//}
//
//Debug.log(JsTreeHelper.displayConstructor());


if (!productStoreId) {
    Debug.log("No product store Id found.");
    return;
}
Debug.log("productStoreId =======> " + productStoreId);
List separateRootType(roots) {
    if(roots) {
        prodRootTypeTree = [];
        roots.each { root ->
            productCategory = root.getRelatedOne("ProductCategory", false);            
            itemState = new JsTreeDataItemState(true, false);
            dataItem = new JsTreeDataItem(productCategory.getString("productCategoryId"), productCategory.getString("categoryName"), "jstree-file", itemState, null);
            prodRootTypeTree.add(dataItem);
        }
        return prodRootTypeTree;
    }
}

treeMenuSettings = [:];
pluginList = new JsTreeHelper.JsTreePluginList();
pluginList.add("sort, state");
pluginList.add(new JsTreeTypesPlugin(["catalog", "category", "product"], new JsTreeType(3, 5, null, null), new JsTreeType(6, 2, null, null), new JsTreeType(0, 0, null, null)));
treeMenuSettings["plugins"] = pluginList;

treeMenuSettings["themes"] = new JsTreeTheme();
StringBuffer function = new StringBuffer();
function.append("jQuery.ajax({");
function.append("url: URL,");
function.append("type: 'POST',");
function.append("data: dataSet,");
function.append("error: function(msg) {");
function.append("alert('An error occurred loading content! : ' + msg);");
function.append("},");
function.append("success: function(msg) {");
function.append("jQuery('#centerdiv').html(msg);");
function.append("}");
function.append("});");

selectNodeEvents = [new JsTreeEvent("select_node", function.toString())];
treeMenuSettings["events"] = selectNodeEvents;
context.treeMenuSettings = treeMenuSettings;



treeMenuData =  [];
//Get the Catalogs
productStoreCatalogs = from("ProductStoreCatalog").where(new EntityExpr("productStoreId", EntityComparisonOperator.EQUALS, productStoreId)).filterByDate().queryList();
for (productStoreCatalog in productStoreCatalogs) {
//    Debug.log("prodCatalogId ==========> " + productStoreCatalog.prodCatalogId);
    prodCatalog = productStoreCatalog.getRelatedOne("ProdCatalog", true);
    if (prodCatalog) {
        children = [];
        prodCatalogCategories = from("ProdCatalogCategory").where("prodCatalogId", prodCatalog.prodCatalogId).filterByDate().queryList();        
        if (prodCatalogCategories) {
            children = separateRootType(prodCatalogCategories);
//            Debug.log("productCatalogCategories ======> " + prodCatalogMap.child);
        }
        itemState = new JsTreeDataItemState(true, false);
        dataItem = new JsTreeDataItem(prodCatalog.getString("prodCatalogId"), prodCatalog.getString("catalogName"), "jstree-folder", itemState, children);
        treeMenuData.add(dataItem);        
    }
}
context.treeMenuData = treeMenuData;

//stillInCatalogManager = true;
//productCategoryId = null;
//prodCatalogId = null;
//showProductCategoryId = null;
//
//// Reset tree condition check. Are we still in the Catalog Manager ?. If not , then reset the tree.
//if ((parameters.productCategoryId != null) || (parameters.showProductCategoryId != null)) {
//    stillInCatalogManager = false;
//    productCategoryId = parameters.productCategoryId;
//    showProductCategoryId = parameters.showProductCategoryId;
//} else if (parameters.prodCatalogId != null) {
//    stillInCatalogManager = false;
//    prodCatalogId = parameters.prodCatalogId;
//}
//context.stillInCatalogManager = stillInCatalogManager;
//context.productCategoryId = productCategoryId;
//context.prodCatalogId = prodCatalogId;
//context.showProductCategoryId = showProductCategoryId;
