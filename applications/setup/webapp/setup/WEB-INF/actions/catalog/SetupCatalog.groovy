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

eventFlags = setupWorker?.getRecordRequestStatesMap(["New", "Create", "Delete"], true, ["Catalog", "Category", "Product"]);


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

if (!eventFlags.isDeleteProductSuccess && (productAndAssoc != null || eventFlags.isEffnewProduct)) {
    eventFlags.targetRecord = "product"
    eventFlags.isCreateForm = (context.productAndAssoc == null);
} else if (!eventFlags.isDeleteCategorySuccess && (context.productCategoryAndAssoc != null || eventFlags.isEffnewCategory)) {
    eventFlags.targetRecord = "category"
    eventFlags.isCreateForm = (context.productCategoryAndAssoc == null);
} else {
    eventFlags.targetRecord = "catalog"
    eventFlags.isCreateForm = (context.prodCatalogAndStoreAssoc == null);
}

// dump flags in context (FIXME?: remove this later)
context.putAll(eventFlags);

eventStates = [:];
eventStates.putAll(eventFlags);

// add some more (not in context)
eventStates.isError = context.isSetupEventError;

context.eventStates = eventStates;


