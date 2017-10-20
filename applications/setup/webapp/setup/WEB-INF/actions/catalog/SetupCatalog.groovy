import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import com.ilscipio.scipio.setup.*;

final module = "SetupCatalog.groovy";

SetupWorker setupWorker = context.setupWorker;
catalogData = context.catalogData ?: [:];

/*
 * Request states
 * NOTE: this is complicated for this screen due to multiple forms + half-JS + submit behavior
 */

eventFlags = [:];

// TODO: SetupWorker method to generate this map instead
isNewCatalog = setupWorker?.isNewRecordRequest("catalog");
isNewCategory = setupWorker?.isNewRecordRequest("category");
isNewProduct = setupWorker?.isNewRecordRequest("product");
isNewRequest = (isNewCatalog || isNewCategory || isNewProduct);
eventFlags.isNewCatalog = isNewCatalog;
eventFlags.isNewCategory = isNewCategory;
eventFlags.isNewProduct = isNewProduct;
eventFlags.isNewRequest = isNewRequest;

isDeleteCatalog = setupWorker?.isDeleteRecordRequest("catalog");
isDeleteCategory = setupWorker?.isDeleteRecordRequest("category");
isDeleteProduct = setupWorker?.isDeleteRecordRequest("product");
isDeleteRequest = (isDeleteCatalog || isDeleteCategory || isDeleteProduct);
eventFlags.isDeleteCatalog = isDeleteCatalog;
eventFlags.isDeleteCategory = isDeleteCategory;
eventFlags.isDeleteProduct = isDeleteProduct;
eventFlags.isDeleteRequest = isDeleteRequest;

isCreateCatalog = setupWorker?.isCreateRecordRequest("catalog");
isCreateCategory = setupWorker?.isCreateRecordRequest("category");
isCreateProduct = setupWorker?.isCreateRecordRequest("product");
isCreateRequest = (isCreateCatalog || isCreateCategory || isCreateProduct);
eventFlags.isCreateCatalog = isCreateCatalog;
eventFlags.isCreateCategory = isCreateCategory;
eventFlags.isCreateProduct = isCreateProduct;
eventFlags.isCreateRequest = isCreateRequest;

// WARN: some of these are not valid setup step names to pass to worker, but works anyway
isDeleteCatalogSuccess = setupWorker?.isSuccessDeleteRecordRequest("catalog");
isDeleteCategorySuccess = setupWorker?.isSuccessDeleteRecordRequest("category");
isDeleteProductSuccess = setupWorker?.isSuccessDeleteRecordRequest("product");
isDeleteSuccess = (isDeleteCatalogSuccess || isDeleteCategorySuccess || isDeleteProductSuccess);
eventFlags.isDeleteCatalogSuccess = isDeleteCatalogSuccess;
eventFlags.isDeleteCategorySuccess = isDeleteCategorySuccess;
eventFlags.isDeleteProductSuccess = isDeleteProductSuccess;
eventFlags.isDeleteSuccess = isDeleteSuccess;

isCreateCatalogSuccess = setupWorker?.isSuccessCreateRecordRequest("catalog");
isCreateCategorySuccess = setupWorker?.isSuccessCreateRecordRequest("category");
isCreateProductSuccess = setupWorker?.isSuccessCreateRecordRequest("product");
isCreateSuccess = (isCreateCatalogSuccess || isCreateCategorySuccess || isCreateProductSuccess);
eventFlags.isCreateCatalogSuccess = isCreateCatalogSuccess;
eventFlags.isCreateCategorySuccess = isCreateCategorySuccess;
eventFlags.isCreateProductSuccess = isCreateProductSuccess;
eventFlags.isCreateSuccess = isCreateSuccess;

isEffNewCatalog = setupWorker?.isEffectiveNewRecordRequest("catalog");
isEffNewCategory = setupWorker?.isEffectiveNewRecordRequest("category");
isEffNewProduct = setupWorker?.isEffectiveNewRecordRequest("product");
isEffNewRequest = (isEffNewCatalog || isEffNewCategory || isEffNewProduct);
eventFlags.isEffNewCatalog = isEffNewCatalog;
eventFlags.isEffNewCategory = isEffNewCategory;
eventFlags.isEffNewProduct = isEffNewProduct;
eventFlags.isEffNewRequest = isEffNewRequest;

/*
 * Catalog
 */
prodCatalog = catalogData.prodCatalog;
context.prodCatalog = prodCatalog;
prodCatalogId = prodCatalog?.prodCatalogId;
context.prodCatalogId = prodCatalogId;

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

/*
 * Category
 * WARN: no verify category is strictly part of store
 */
productCategoryAndAssoc = null;
productCategoryId = context.productCategoryId ?: parameters.productCategoryId;
productCategory = null;
if (prodCatalog && productCategoryId) {
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

prodCatalogCategoryTypes = EntityQuery.use(delegator).from("ProdCatalogCategoryType").orderBy("description").queryList();
context.prodCatalogCategoryTypes = prodCatalogCategoryTypes;

productCategoryTypes = EntityQuery.use(delegator).from("ProductCategoryType").orderBy("description").queryList();
context.productCategoryTypes = productCategoryTypes;

context.productCategoryId = productCategoryId;
context.productCategory = productCategory;
context.productCategoryAndAssoc = productCategoryAndAssoc;

/*
 * Product
 * WARN: no verify product is strictly part of store
 */
productAndAssoc = null;
productId = context.productId ?: parameters.productId;
product = null;
productCategoryMember = null;
if (productCategory && productId) {
    product = delegator.findOne("Product", [productId:productId], false);
    
    productCategoryMember = EntityQuery.use(delegator).from("ProductCategoryMember").where("productId", productId,
            "productCategoryId", productCategoryId).filterByDate().queryFirst();
    
    if (product && productCategoryMember) {
        productAndAssoc = new HashMap(product);
        productAndAssoc.putAll(productCategoryMember);
    }
}
context.product = product;
context.productCategoryMember = productCategoryMember;
context.productId = productId;
context.productAndAssoc = productAndAssoc;

/*
 * Extra prep
 */

if (!eventFlags.isDeleteProductSuccess && (productAndAssoc != null || eventFlags.isEffNewProduct)) {
    eventFlags.targetRecord = "product"
    eventFlags.isCreate = (context.productAndAssoc == null);
} else if (!eventFlags.isDeleteCategorySuccess && (context.productCategoryAndAssoc != null || eventFlags.isEffNewCategory)) {
    eventFlags.targetRecord = "category"
    eventFlags.isCreate = (context.productCategoryAndAssoc == null);
} else {
    eventFlags.targetRecord = "catalog"
    eventFlags.isCreate = (context.prodCatalogAndStoreAssoc == null);
}

// dump flags in context (FIXME?: remove this later)
context.putAll(eventFlags);

eventStates = [:];
eventStates.putAll(eventFlags);

// add some more (not in context)
eventStates.isError = context.isSetupEventError;

context.eventStates = eventStates;


