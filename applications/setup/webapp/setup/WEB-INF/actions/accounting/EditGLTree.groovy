/**
 * SCIPIO: SETUP interactive catalog tree data prep.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
 
final module = "EditGlTree.groovy";

//// FIXME?: setupEctMaxProductsPerCat is a session-based control for the time being, breaking convention with rest of setup
//ectMaxProductsPerCat = context.ectMaxProductsPerCat;
//if (ectMaxProductsPerCat == null) {
//    try {
//        ectMaxProductsPerCat = (request.getAttribute("setupEctMaxProductsPerCat") ?: request.getParameter("setupEctMaxProductsPerCat")) as Integer;
//    } catch(Exception e) {
//    }
//    if (ectMaxProductsPerCat != null) {
//        session.setAttribute("setupEctMaxProductsPerCat", ectMaxProductsPerCat);
//    }
//}
//if (ectMaxProductsPerCat == null) ectMaxProductsPerCat = 0; // DEFAULT ZERO: fastest and least confusing
//context.ectMaxProductsPerCat = ectMaxProductsPerCat;
//
//context.ectEventStates = context.eventStates;
//
//// CORE DATA PREP
//GroovyUtil.runScriptAtLocation("component://product/webapp/catalog/WEB-INF/actions/catalog/tree/EditCatalogTreeCore.groovy", null, context);

//treeMenuHelper = new JsTreeHelper();
//treeMenuData = [];
//
//glAccountList = context.glAccountList ?: [];
//for (glAccount in glAccountList) {
//	
//		def state = null;
//		def categoryStates = null;
//		if (!targetNodeInfo.defined) {
//			if (curProdCatalogId == prodCatalog.prodCatalogId) {
//				state = ["opened":true, "selected":currentCatalogSelected];
//				if (curProductCategoryId) {
//					// WARN: BEST-EFFORT-ONLY: this will not preserve the full category path;
//					// this is ONLY good as a fallback when targetNodePath is not set (e.g. first visit)
//					categoryStates = [
//						(curProductCategoryId): ["opened":true, "selected":currentCategorySelected]
//					];
//				}
//			}
//		}
//		
//		result = dispatcher.runSync("buildGLAccountTree", [
//			"useCategoryCache": false,
//			"useProductCache": useProductCache,
//			"prodCatalogId" : prodCatalog.prodCatalogId,
//			"state": state,
//			"categoryStates": categoryStates,
//			"includeCategoryData": true,
//			"includeProductData": true,
//			"maxProductsPerCat": maxProductsPerCat,
//			"includeEmptyTop": true,
//			"productStoreCatalog": productStoreCatalog
//		]);
//		if (result?.treeList) {
//			treeMenuData = treeMenuData + result.treeList;
//		}
//	
//}
//
//treeMenuHelper.addAll(treeMenuData)
//context.treeMenuData = treeMenuHelper;


Debug.log("glAccountList   ==================> " + UtilValidate.isNotEmpty(context.glAccountList));
Debug.log("topGlAccountId  ==================> " + UtilValidate.isNotEmpty(context.topGlAccountId));