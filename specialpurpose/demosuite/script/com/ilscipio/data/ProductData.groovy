import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*
import org.ofbiz.product.category.CategoryWorker

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.DemoSuiteDataGeneratorUtil
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.MockarooDataGenerator
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataProduct
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript;



public class ProductData extends DataGeneratorGroovyBaseScript {

    ProductData() {
        Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - PRODUCT DATA-=-=-=-", "");
    }

    public void init() {
        // FIXME: I'm not sure about this, maybe a different service data generator for categories and even catalogs may be better
        //        boolean createNewCategories = (context.createNewCategories) ? context.createNewCategories: false;
        GenericValue productStore = checkProductStore(productStoreIds);
        GenericValue prodCatalog = checkProdCatalog();
        GenericValue productCategory = checkProductCategory();

        List productCategoryIds = [];
        if (productCategory) {
            // An explicit productCategory has preference, skipping the rest
            productCategoryIds += [
                productCategory.productCategoryId
            ];
        } else if (prodCatalog) {
            //  If productCategory is empty, try with an explicit prodCatalog
            productCategoryIds = getCatalogRelatedCategoryIds(prodCatalog);
        } else if (productStore) {
            // Ultimately check for productStore, if nothing has been passed for that too, a random one taken from productStoreIds will be used
            // Find Catalogs
            productStoreCatalogs = EntityUtil.filterByDate(productStore.getRelated("ProductStoreCatalog", null, null, true));
            //      productCatalogs = productStoreCatalog.getRelated("ProductCatalog", true);
            for (productStoreCatalog in productStoreCatalogs) {
                prodCatalog = productStoreCatalog.getRelatedOne("ProdCatalog", true);
                productCategoryIds += getCatalogRelatedCategoryIds(prodCatalog);
            }
        }
        int numRecords = getNumRecordsToBeGenerated();
        List<DemoDataProduct> generatedProducts = [];
        if (productCategoryIds) {
            String productCategoryTypeId = (context.productCategoryTypeId) ? context.productCategoryTypeId : "CATALOG_CATEGORY";
            String prodCatalogCategoryTypeId = (context.prodCatalogCategoryTypeId) ? context.prodCatalogCategoryTypeId : null;
			// FIXME: This is no longer valid. Use a custom helper for each provider.
//            generatedProducts = DemoSuiteDataGeneratorUtil.generateProduct(numRecords, MockarooDataGenerator.class);
        }
        context.generatedProducts = generatedProducts;
        context.productCategoryIds = productCategoryIds;
    }

    final List<String> prodCatalogCategoryTypes = [
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

    final List<String> productTypes = [
        //TODO: Gotta figure how to handle these two types
        //        "AGGREGATED",
        //        "AGGREGATED_CONF",
        "ASSET_USAGE",
        "DIGITAL_GOOD",
        "FINDIG_GOOD",
        "FINISHED_GOOD",
        "GOOD",
        "MARKETING_PKG_PICK",
        "MARKETING_PKG_AUTO",
        "RAW_MATERIAL",
        "SERVICE",
        "SUBASSEMBLY",
        "WIP"
    ]

    final List<String> productStoreIds = [
        "ScipioShop",
        "RentalStore"
    ]

    List prepareData(int index) throws Exception {
        Debug.log("prepareData");
        List<GenericValue> toBeStored = new ArrayList<GenericValue>();
        List<GenericValue> productItems = new ArrayList<GenericValue>();
        if (context.generatedProducts) {
            DemoDataProduct demoDataProduct = context.generatedProducts.get(index);
            // Create Product
            String productId = "GEN_" + delegator.getNextSeqId("demo-product");
            productCategoryId = context.productCategoryIds.get(UtilRandom.random(context.productCategoryIds));
            productTypeId = productTypes.get(UtilRandom.random(productTypes));
            introductionDate = UtilRandom.generateRandomTimestamp(context);

            fields = UtilMisc.toMap("productId", productId, "productTypeId", productTypeId, "productName", demoDataProduct.getName(), "description", demoDataProduct.getDescription(),
                    "longDescription", demoDataProduct.getLongDescription(), "introductionDate", introductionDate);
            GenericValue product = delegator.makeValue("Product", fields);
            toBeStored.add(product);

            fields = UtilMisc.toMap("productId", productId, "productCategoryId", productCategoryId, "fromDate", introductionDate);
            GenericValue productCategoryMember = delegator.makeValue("ProductCategoryMember", fields);
            toBeStored.add(productCategoryMember);
        }
        return toBeStored;
    }

    private GenericValue checkProdCatalog() {
        String prodCatalogId = (context.prodCatalogId) ? context.prodCatalogId : null;
        if (!prodCatalogId)
            return null;
        GenericValue prodCatalog;
        try {
            prodCatalog = delegator.findOne("ProdCatalog", ["prodCatalogId" :  prodCatalogId],true);
        } catch (Exception e) {
            return null;
        }
        return prodCatalog;
    }

    private GenericValue checkProductCategory(productStoreIds) {
        String productCategoryId = (context.productCategoryId) ? context.productCategoryId : null;
        if (!productCategoryId)
            return productCategoryId;
        GenericValue productCategory;
        try {
            productCategory = delegator.findOne("ProductCategory", ["productCategoryId" :  productCategoryId],true);
        } catch (Exception e) {
            return null;
        }
        return productCategory;

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

    private List<String> getCatalogRelatedCategoryIds(GenericValue prodCatalog) {
        productCategoryIds = [];
        prodCatalogCategories = prodCatalog.getRelated("ProdCatalogCategory", null, null, true);
        for (prodCatalogCategory in prodCatalogCategories) {
            List productCategories = CategoryWorker.getRelatedCategoriesRet(delegator, "categoryList", prodCatalogCategory.productCategoryId, false, false, true);
            for (productCategory in productCategories) {
                productCategoryIds += [
                    productCategory.productCategoryId
                ];
            }
        }
        return productCategoryIds;
    }

}