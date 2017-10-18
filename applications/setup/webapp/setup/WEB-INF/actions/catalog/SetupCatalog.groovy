import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import com.ilscipio.scipio.setup.*;

final module = "SetupCatalog.groovy";

catalogData = context.catalogData ?: [:];

prodCatalog = catalogData.prodCatalog;
context.prodCatalog = prodCatalog;
context.prodCatalogId = prodCatalog?.prodCatalogId;

productStoreCatalog = catalogData.productStoreCatalog;
context.productStoreCatalog = productStoreCatalog;

productStoreCatalogList = catalogData.productStoreCatalogList;
context.productStoreCatalogList = productStoreCatalogList;

highestSequenceNum = null;
if (productStoreCatalogList) {
    for(cat in productStoreCatalogList) {
        if (cat.sequenceNum != null) {
            if (highestSequenceNum == null || cat.sequenceNum > highestSequenceNum) {
                highestSequenceNum = cat.sequenceNum;
            }
        }
    }
}
context.highestSequenceNum = highestSequenceNum;

defaultSequenceNum = (highestSequenceNum != null) ? (highestSequenceNum + 1) : 1;
context.defaultSequenceNum = defaultSequenceNum;

prodCatalogAndStoreAssoc = null
if (prodCatalog != null && productStoreCatalog != null) {
    prodCatalogAndStoreAssoc = new HashMap(prodCatalog);
    prodCatalogAndStoreAssoc.putAll(productStoreCatalog);
}
context.prodCatalogAndStoreAssoc = prodCatalogAndStoreAssoc;

// CATEGORY
productCategoryAndAssoc = null;
productCategoryId = context.productCategoryId ?: parameters.productCategoryId;
productCategory = null;
if (prodCatalog && productCategoryId) {
    // VERIFY category is actually part of this store (any catalog)
    productCategory = delegator.findOne("ProductCategory", [productCategoryId:productCategoryId], false);
    
    def prodCatalogCategory = null;
    def productCategoryRollup = null;
    parentProductCategoryId = parameters.parentProductCategoryId;
    if (parentProductCategoryId) {
        productCategoryRollup = EntityQuery.use(delegator).from("ProductCategoryRollup").where("parentProductCategoryId", parentProductCategoryId,
            "productCategoryId", productCategoryId).filterByDate().queryFirst();
        if (productCategoryRollup && productCategory) {
            productCategoryAndAssoc = new HashMap(productCategory);
            productCategoryAndAssoc.putAll(productCategoryRollup);
        }
    } else if (prodCatalogId) {
        prodCatalogCategory = EntityQuery.use(delegator).from("ProdCatalogCategory").where("prodCatalogId", prodCatalogId,
            "productCategoryId", productCategoryId).filterByDate().queryFirst();
        if (prodCatalogCategory && productCategory) {
            productCategoryAndAssoc = new HashMap(productCategory);
            productCategoryAndAssoc.putAll(prodCatalogCategory);
        }
    }
}
context.productCategoryId = productCategoryId;
context.productCategory = productCategory;
context.productCategoryAndAssoc = productCategoryAndAssoc;


