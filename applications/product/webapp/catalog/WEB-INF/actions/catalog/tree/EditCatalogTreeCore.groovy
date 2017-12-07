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
final DEBUG = true;

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

isEventError = context.ectIsEventError;
if (isEventError == null) isEventError = context.isError;
if (isEventError == null) isEventError = false;
context.ectIsEventError = isEventError;

objectLocalizedFields = context.ectObjectLocalizedFields;
if (!objectLocalizedFields) {
    // TODO: redo this whole map
    objectLocalizedFields = [
        category: [
            fieldNames: ["categoryName", "description", "longDescription"],
            typeNames: ["CATEGORY_NAME", "DESCRIPTION", "LONG_DESCRIPTION"],
            typeNameListStr: '[CATEGORY_NAME, DESCRIPTION, LONG_DESCRIPTION]',
            typeInfo: ["LONG_DESCRIPTION":["isLong":true]]
        ],
        product: [
            fieldNames: ["productName", "description", "longDescription"],
            typeNames: ["PRODUCT_NAME", "DESCRIPTION", "LONG_DESCRIPTION"],
            typeNameListStr: '[PRODUCT_NAME, DESCRIPTION, LONG_DESCRIPTION]',
            typeInfo: ["LONG_DESCRIPTION":["isLong":true]]
        ] 
    ];
}
context.ectObjectLocalizedFields = objectLocalizedFields;

getSetStringParam = { paramName ->
    def value = context[paramName];
    if (value == null) value = parameters[paramName] as String;
    context[paramName] = value;
    return value;
};

eventStates = context.ectEventStates ?: [:];
if (eventStates == null) {
    // FIXME: need a non-setup function, should refactor setup worker
    //eventStates = setupWorker?.getRecordRequestStatesMap(["New", "Create", "Update", "Delete", "Copy", "Move", "Add"], true, ["Catalog", "Category", "Product"]);
}
context.ectEventStates = eventStates;

curProdCatalogId = context.ectProdCatalogId != null ? context.ectProdCatalogId : context.prodCatalogId;
curProductCategoryId = context.ectProductCategoryId != null ? context.ectProductCategoryId : context.productCategoryId;
curProductId = context.ectProductId != null ? context.ectProductId : context.productId;

// special handling for the initial form - if event error, need to treat the preselect differently.
// NOTE: these flags only apply to visible "show" forms. noShowFormChange implies noShowFormPopulate.
// FIXME?: CURRENTLY RELYING ON SCREENS TO ENSURE CORRECT INITIAL FORM IS SHOWN AND POPULATED CORRECTLY
// - ideally tree could handle to simplify screens...
initialSettings = context.ectInitialSettings ?: [:];
if (initialSettings.noShowFormChange == null && initialSettings.noShowFormPopulate == null) { // caller can override if need
    initialSettings.noShowFormPopulate = false;
    initialSettings.noShowFormChange = false;
    if (isEventError) {
        initialSettings.noShowFormPopulate = true;
        initialSettings.noShowFormChange = true;
    }
}
context.ectInitialSettings = initialSettings;

// ectTargetNodePath/ectNewTargetNodePath is the preferred pre-selection mechanism.
// if not set, falls back on prodCatalogId or productCategoryId (below)
targetNodePath = getSetStringParam("ectTargetNodePath");
newTargetNodePath = getSetStringParam("ectNewTargetNodePath");
submittedFormId = getSetStringParam("ectSubmittedFormId");

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

// NOTE: the content of targetNodePath and newTargetNodePath is actionType-specific,
// because the JS cannot know in advance some things like sequenced PK of newly-created records
// TODO: make this more consistent in future, maybe can simplify things

def targetNodeInfo;
if (!isEventError && newTargetNodePath) {
    targetNodeInfo = parseTargetNodeInfo(newTargetNodePath);
} else {
    targetNodeInfo = parseTargetNodeInfo(targetNodePath);
}
if (!isEventError) {
    if (eventStates.isDeleteRecord) {
        // Remove last entry (deleted)
        if (targetNodeInfo.objectIdList) {
            targetNodeInfo.objectIdList.remove(targetNodeInfo.objectIdList.size() - 1);
            if (targetNodeInfo.objectIdList.size() <= 1) {
                targetNodeInfo.targetObjectType = "catalog";
            } else {
                targetNodeInfo.targetObjectType = "category";
            }
        }
    } else if (eventStates.isCreateRecord) {
        // Append new entry
        if (eventStates.isCreateCatalog) {
            if (!targetNodeInfo.objectIdList && curProdCatalogId) {
                targetNodeInfo.objectIdList.add(curProdCatalogId);
                targetNodeInfo.targetObjectType = "catalog";
            }
        } else if (eventStates.isCreateCategory) {
            if (targetNodeInfo.objectIdList && curProductCategoryId) {
                targetNodeInfo.objectIdList.add(curProductCategoryId);
                targetNodeInfo.targetObjectType = "category";
            }
        } else if (eventStates.isCreateProduct) {
            if (targetNodeInfo.objectIdList && curProductId) {
                targetNodeInfo.objectIdList.add(curProductId);
                targetNodeInfo.targetObjectType = "product";
            }
        }
    } else if (eventStates.isAddRecord) {
        // Append new entry
        if (eventStates.isAddCatalog) {
            if (!targetNodeInfo.objectIdList && curProdCatalogId) {
                targetNodeInfo.objectIdList.add(curProdCatalogId);
                targetNodeInfo.targetObjectType = "catalog";
            }
        } else if (eventStates.isAddCategory) {
            if (targetNodeInfo.objectIdList && curProductCategoryId) {
                targetNodeInfo.objectIdList.add(curProductCategoryId);
                targetNodeInfo.targetObjectType = "category";
            }
        } else if (eventStates.isAddProduct) {
            if (targetNodeInfo.objectIdList && curProductId) {
                targetNodeInfo.objectIdList.add(curProductId);
                targetNodeInfo.targetObjectType = "product";
            }
        }
    } else if (eventStates.isCopyRecord) {
        if (eventStates.isCopyCategory) {
            if (targetNodeInfo.objectIdList && curProductCategoryId) {
                targetNodeInfo.objectIdList.add(curProductCategoryId);
                targetNodeInfo.targetObjectType = "category";
            }
        } else if (eventStates.isCopyProduct) {
            if (targetNodeInfo.objectIdList && curProductId) {
                targetNodeInfo.objectIdList.add(curProductId);
                targetNodeInfo.targetObjectType = "product";
            }
        }
    } else if (eventStates.isMoveRecord) {
        if (eventStates.isMoveCategory) {
            if (targetNodeInfo.objectIdList && curProductCategoryId) {
                targetNodeInfo.objectIdList.add(curProductCategoryId);
                targetNodeInfo.targetObjectType = "category";
            }
        } else if (eventStates.isMoveProduct) {
            if (targetNodeInfo.objectIdList && curProductId) {
                targetNodeInfo.objectIdList.add(curProductId);
                targetNodeInfo.targetObjectType = "product";
            }
        }
    }
}
if (DEBUG) Debug.logInfo("Catalog tree: targetNodeInfo: " + targetNodeInfo, module);
context.ectTargetNodeInfo = targetNodeInfo;

// only either catalog or category should be "selected"
currentCatalogSelected = false;
currentCategorySelected = false;
if (!targetNodeInfo.defined && !(!isEventError && eventStates.isDeleteRecord)) { // fallback auto-select (best-effort) for when targetNodePath is not set
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
