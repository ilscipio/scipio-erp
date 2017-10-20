/**
 * SCIPIO: CORE interactive catalog tree include.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;

import com.ilscipio.scipio.treeMenu.jsTree.JsTreeCore;
import com.ilscipio.scipio.treeMenu.jsTree.JsTreeHelper;
import com.ilscipio.scipio.treeMenu.jsTree.JsTreePlugin.JsTreeTypesPlugin;
import com.ilscipio.scipio.treeMenu.jsTree.JsTreePlugin.JsTreeTypesPlugin.JsTreeType;

final module = "EditCatalogTreeCore.groovy";

etcAdvanced = context.etcAdvanced != null ? context.etcAdvanced : false;
context.etcAdvanced = etcAdvanced;

getSetCtxBool = { name, defVal ->
    def res = context[name];
    if (res == null) res = defVal;
    context[name] = res;
    return res;
};

maxProductsPerCat = context.ectMaxProductsPerCat;
useProductCache = context.ectUseProductCache != null ? context.ectUseProductCache : true;

productStoreId = context.productStoreId;
if (!productStoreId) {
    Debug.logError("Setup: No product store Id found for catalog tree.", module);
    return;
}

isDeleteSuccess = getSetCtxBool("isDeleteSuccess", false);
isDeleteCatalogSuccess = getSetCtxBool("isDeleteCatalogSuccess", false);
isDeleteCategorySuccess = getSetCtxBool("isDeleteCategorySuccess", false);
isDeleteProductSuccess = getSetCtxBool("isDeleteProductSuccess", false);

isCreateSuccess = getSetCtxBool("isCreateSuccess", false);
isCreateCatalogSuccess = getSetCtxBool("isCreateCatalogSuccess", false);
isCreateCategorySuccess = getSetCtxBool("isCreateCategorySuccess", false);
isCreateProductSuccess = getSetCtxBool("isCreateProductSuccess", false);

curProdCatalogId = context.ectProdCatalogId != null ? context.ectProdCatalogId : context.prodCatalogId;
curProductCategoryId = context.ectProductCategoryId != null ? context.ectProductCategoryId : context.productCategoryId;
curProductId = context.ectProductId != null ? context.ectProductId : context.productId;

// ectTargetNodePath is the preferred pre-selection mechanism. 
// if not set, falls back on prodCatalogId or productCategoryId (below)
targetNodePath = context.ectTargetNodePath;
if (targetNodePath == null) {
    targetNodePath = parameters.ectTargetNodePath as String;
}
context.ectTargetNodePath = targetNodePath;

submittedFormId = context.ectSubmittedFormId;
if (submittedFormId == null) {
    submittedFormId = parameters.ectSubmittedFormId as String;
}
context.ectSubmittedFormId = submittedFormId;

parseTargetNodeInfo = { targetNodePath ->
    def objectIdList = [];
    def targetObjectType = null;
    def defined = targetNodePath ? true : false;
    def splitIndex = targetNodePath ? targetNodePath.lastIndexOf('#') : -1;
    if (splitIndex >= 0) {
        targetObjectType = targetNodePath.substring(splitIndex + 1);
        pathStr = targetNodePath.substring(0, splitIndex);
    }
    if (targetNodePath) {
        objectIdList = new ArrayList(pathStr.split("/") as List);
    }
    return [objectIdList:objectIdList, targetObjectType:targetObjectType, defined:defined];
};

targetNodeInfo = parseTargetNodeInfo(targetNodePath);
if (isDeleteSuccess) {
    // Remove last entry (deleted)
    if (targetNodeInfo.objectIdList) {
        targetNodeInfo.objectIdList.remove(targetNodeInfo.objectIdList.size() - 1);
        if (targetNodeInfo.objectIdList.size() <= 1) {
            targetNodeInfo.targetObjectType = "catalog";
        } else {
            targetNodeInfo.targetObjectType = "category";
        }
    }
} else if (isCreateSuccess) {
    // Append new entry
    if (isCreateCatalogSuccess) {
        if (!targetNodeInfo.objectIdList && curProdCatalogId) {
            targetNodeInfo.objectIdList.add(curProdCatalogId);
            targetNodeInfo.targetObjectType = "catalog";
        }
    } else if (isCreateCategorySuccess) {
        if (targetNodeInfo.objectIdList && curProductCategoryId) {
            targetNodeInfo.objectIdList.add(curProductCategoryId);
            targetNodeInfo.targetObjectType = "category";
        }
    } else if (isCreateProductSuccess) {
        if (targetNodeInfo.objectIdList && curProductId) {
            targetNodeInfo.objectIdList.add(curProductId);
            targetNodeInfo.targetObjectType = "product";
        }
    }
}
context.ectTargetNodeInfo = targetNodeInfo;

// only either catalog or category should be "selected"
currentCatalogSelected = false;
currentCategorySelected = false;
if (!targetNodeInfo.defined && !isDeleteSuccess) { // fallback auto-select (best-effort) for when targetNodePath is not set
    if (curProductCategoryId) {
        currentCategorySelected = true;
    } else if (curProdCatalogId) {
        currentCatalogSelected = true;
    }
}

treeMenuHelper = new JsTreeHelper();
treeMenuData = [];

productStoreCatalogs = context.productStoreCatalogList ?: [];
for (productStoreCatalog in productStoreCatalogs) {    
    prodCatalog = productStoreCatalog.getRelatedOne("ProdCatalog", false);
    if (prodCatalog) {
        def state = null;
        def categoryStates = null;
        if (!targetNodeInfo.defined) {
            if (curProdCatalogId == prodCatalog.prodCatalogId) {
                state = ["opened":true, "selected":currentCatalogSelected];
                if (curProductCategoryId) {
                    // WARN: BEST-EFFORT-ONLY: this will not preserve the full category path;
                    // this is ONLY good as a fallback when targetNodePath is not set (e.g. first visit)
                    categoryStates = [
                        (curProductCategoryId): ["opened":true, "selected":currentCategorySelected]
                    ];
                }
            }
        }
        
        result = dispatcher.runSync("buildCatalogTree", [
            "useCategoryCache": false,
            "useProductCache": useProductCache,
            "prodCatalogId" : prodCatalog.prodCatalogId,
            "state": state,
            "categoryStates": categoryStates,
            "includeCategoryData": true,
            "includeProductData": true,
            "maxProductsPerCat": maxProductsPerCat,
            "includeEmptyTop": true,
            "productStoreCatalog": productStoreCatalog
        ]);
        if (result?.treeList) {
            treeMenuData = treeMenuData + result.treeList;
        }
    }
}

treeMenuHelper.addAll(treeMenuData)
context.treeMenuData = treeMenuHelper;

