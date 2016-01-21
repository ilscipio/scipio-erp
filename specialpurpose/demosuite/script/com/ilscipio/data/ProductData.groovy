import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilProperties
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*
import org.ofbiz.product.category.CategoryWorker
import org.ofbiz.service.ServiceUtil



public Map createDemoProduct(DispatcherContext ) {
	final String resource_error = "DemoSuiteUiLabels";
	
	List<String> prodCatalogCategoryTypes = [
		"CCT_ADMIN_ALLW",
		"PCCT_BROWSE_ROOT",
		"PCCT_MOST_POPULAR",
		"PCCT_OTHER_SEARCH",
		"PCCT_PROMOTIONS",
		"PCCT_PURCH_ALLW",
		"PCCT_QUICK_ADD",
		"PCCT_SEARCH",
		"PCCT_VIEW_ALLW",
		"PCCT_WHATS_NEW"
	]
	
	List<String> productStoreIds = [
		"CATO_FOUNDATION_1",
		"CATO_FOUNDATION_2"
	]
	
	Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - PRODUCT DATA-=-=-=-", "");
	Map result = ServiceUtil.returnSuccess();
	
	List<GenericValue> toBeStored = new ArrayList<GenericValue>();
	List<GenericValue> productItems = new ArrayList<GenericValue>();
	
	boolean createNewCategories = (context.createNewCategories) ? context.createNewCategories: false;	
	GenericValue productStore = checkProductStore(productStoreIds);

	if (productStore) {
		Debug.log("ProductStoreId ======> " + productStore.productStoreId);
		
		// Find Catalogs
		productStoreCatalogs = productStore.getRelated("ProductStoreCatalog", null, null, true);
//		productCatalogs = productStoreCatalog.getRelated("ProductCatalog", true);
		for (productStoreCatalog in productStoreCatalogs) {
			prodCatalog = productStoreCatalog.getRelatedOne("ProdCatalog", true);
			Debug.log("prodCatalogId ===============> " + prodCatalog.prodCatalogId);
			prodCatalogCategories = prodCatalog.getRelated("ProdCatalogCategory", null, null, true);
			
			
			for (prodCatalogCategory in prodCatalogCategories) {
				Debug.log("prodCatalogCategoryId ===============> " + prodCatalogCategory.productCategoryId);
				
				List productCategories = CategoryWorker.getRelatedCategoriesRet(delegator, "categoryList", prodCatalogCategory.productCategoryId, false, false, true);
				for (productCategory in productCategories) {
					Debug.log("productCategoryId ===============> " + productCategory.productCategoryId);
				}
			}
		}
	
		
//		String productCategoryTypeId = (context.productCategoryTypeId) ? context.productCategoryTypeId : "CATALOG_CATEGORY";
//		String prodCatalogCategoryTypeId = (context.prodCatalogCategoryTypeId) ? context.prodCatalogCategoryTypeId : null;
//		int num = context.num;
//		
//		for (int i = 0; i < num; i++) {
//			// Create Product
//			String productId = "GEN_" + delegator.getNextSeqId("demo-product");
//			
//			
//		}
//		
//		// store the changes
//		if (toBeStored.size() > 0) {
//			try {
//				delegator.storeAll(toBeStored);
//			} catch (GenericEntityException e) {
//				return ServiceUtil.returnError(UtilProperties.getMessage(resource_error,
//				"OrderErrorCannotStoreStatusChanges", locale) + e.getMessage());
//			}
//		}
	} else {
		return ServiceUtil.returnError(UtilProperties.getMessage(resource_error, "ProductErrorProductStoreNotFound", locale) + ". Please load the specific Cato demo data.");
	}
	
	return result;
}

private GenericValue checkProductStore(productStoreIds) {
	String productStoreId = (context.productStoreId) ? context.productStoreId : null;
	if (!productStoreId)
		productStoreId = productStoreIds.get(UtilRandom.random(productStoreIds));
	GenericValue productStore;
	try {
		productStore = delegator.findOne("ProductStore", ["productStoreId" :  productStoreId],true);
	} catch (Exception e) {
		return null;
	}
	return productStore;
}