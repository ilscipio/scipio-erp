import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*
import org.ofbiz.product.category.CategoryWorker
import org.ofbiz.product.store.ProductStoreWorker

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DemoDataHelper.dataTypeEnum
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript;



public class ProductData extends DataGeneratorGroovyBaseScript {

    public String getDataType() {
        return dataTypeEnum.PRODUCT;
    }

    ProductData() {
        Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - PRODUCT DATA-=-=-=-", "");
    }

    public void init() {
        productStoreCount = from("ProductStore").queryCount();
        if (productStoreCount == 0) {
            throw new Exception("This service depends on product store data to be present. Please load or create product store data.");
        }
        totalProductStoreCount = (productStoreCount  < Integer.MAX_VALUE) ? (int) productStoreCount : Integer.MAX_VALUE - 1;

        context.totalProductStoreCount = totalProductStoreCount;

        String prodCatalogId = context.prodCatalogId ?: null;
        String productStoreId = context.productStoreId ?: null;

        EntityFindOptions efo = new EntityFindOptions();
        efo.setMaxRows(1);

        // Check if we got a valid ProductStore
        if (!productStoreId) {
            efo.setOffset(UtilRandom.getRandomInt(0, context.totalProductStoreCount - 1));
            //            Debug.log("productStoreId offset ======> " + efo.getOffset());
            productStores = from("ProductStore").query(efo);
            if (productStores) {
                productStoreId = productStores[0].productStoreId;
            }
            if (!prodCatalogId && productStoreId) {
                productStoreCatalog = EntityUtil.getFirst(EntityUtil.filterByDate(from("ProductStoreCatalog").where("productStoreId", productStoreId).queryList()));
                prodCatalogId = productStoreCatalog.getString("prodCatalogId");
            }
        } else {
            if (!ProductStoreWorker.getProductStore(productStoreId))
                productStoreId = null;
        }
        if (!productStoreId) {
            throw new Exception("Product store not found or invalid.");
        }

        // Check if we got a valid WebSite
        GenericValue webSite = from("WebSite").where("productStoreId", productStoreId).queryFirst();
        if (!webSite) {
            throw new Exception("Website not found or invalid.");
        }


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
        } else if (productStoreId) {
            // Ultimately check for productStore, if nothing has been passed for that too, a random one taken from productStoreIds will be used
            // Find Catalogs
            productStoreCatalogs = from("ProductStoreCatalog").where("productStoreId", productStoreId).queryList();
            //      productCatalogs = productStoreCatalog.getRelated("ProductCatalog", true);
            for (productStoreCatalog in productStoreCatalogs) {
                prodCatalog = productStoreCatalog.getRelatedOne("ProdCatalog", true);
                productCategoryIds += getCatalogRelatedCategoryIds(prodCatalog);
            }
        }
        if (productCategoryIds) {
            String productCategoryTypeId = (context.productCategoryTypeId) ? context.productCategoryTypeId : "CATALOG_CATEGORY";
            String prodCatalogCategoryTypeId = (context.prodCatalogCategoryTypeId) ? context.prodCatalogCategoryTypeId : null;
        }
        context.productCategoryIds = productCategoryIds;
        context.productCategoryTypeId = productCategoryTypeId;
        context.prodCatalogCategoryTypeId = prodCatalogCategoryTypeId;
    }



    List prepareData(int index, DemoDataObject productData) throws Exception {
        List<GenericValue> toBeStored = new ArrayList<GenericValue>();
        List<GenericValue> productItems = new ArrayList<GenericValue>();

        DataGenerator generator = context.generator;

        productFields = UtilMisc.toMap("productId", productData.getProductId(), "productTypeId", productData.getProductTypeId(), "productName", productData.getName(), "description", productData.getDescription(),
                "longDescription", productData.getLongDescription(), "introductionDate", productData.getIntroductionDate());
        toBeStored.add(delegator.makeValue("Product", productFields));

        productCategoryMemberFields = UtilMisc.toMap("productId", productData.getProductId(), "productCategoryId", context.productCategoryId, "fromDate", productData.getIntroductionDate());
        GenericValue productCategoryMember = delegator.makeValue("ProductCategoryMember", fields);

        toBeStored.add(productCategoryMember);
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

    private GenericValue checkProductCategory() {
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