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
import org.ofbiz.base.util.Debug
import org.ofbiz.entity.condition.EntityComparisonOperator
import org.ofbiz.entity.condition.EntityExpr
import org.ofbiz.service.LocalDispatcher

import com.ilscipio.scipio.treeMenu.jsTree.JsTreeCore
import com.ilscipio.scipio.treeMenu.jsTree.JsTreeHelper
import com.ilscipio.scipio.treeMenu.jsTree.JsTreePlugin.JsTreeTypesPlugin
import com.ilscipio.scipio.treeMenu.jsTree.JsTreePlugin.JsTreeTypesPlugin.JsTreeType

final module = "ViewCatalogTree.groovy";

productStoreId = (context.productStoreId) ? context.productStoreId : parameters.productStoreId;
if (!productStoreId) {
    Debug.logError("No product store Id found.", "");
    return;
}

LocalDispatcher dispatcher = context.dispatcher;
pluginList = new JsTreeHelper.JsTreePluginList();
pluginList.add("sort, state, unique");
pluginList.add(new JsTreeTypesPlugin(["catalog", "category", "product"], new JsTreeType(3, 5, ["category"], null), new JsTreeType(6, 2, ["category", "product"], null), new JsTreeType(0, 0, null, null)));
context.treeMenuPlugins = pluginList;
context.treeMenuSettings = new JsTreeCore(false, null, null);

treeMenuHelper = new JsTreeHelper();
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
//JsTreeHelper.preventDataItemsSameId(treeMenuData);
treeMenuHelper.addAll(treeMenuData)
context.treeMenuData = treeMenuHelper;
