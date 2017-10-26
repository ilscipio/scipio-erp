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

eventFlags = setupWorker?.getRecordRequestStatesMap(["New", "Create", "Update", "Delete", "Copy", "Move", "Add"], true, ["Catalog", "Category", "Product"]);


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

prodCatalogIdSet = new HashSet();
if (productStoreCatalogList) {
    for(psc in productStoreCatalogList) {
        prodCatalogIdSet.add(psc.prodCatalogId);
    }
}
context.prodCatalogIdSet = prodCatalogIdSet;

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

productTypes = EntityQuery.use(delegator).from("ProductType").orderBy("description").queryList();
context.productTypes = productTypes;

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

// TODO: REVIEW: this may not be covering all cases properly... some bad cases may be being hidden by jstree logic
// this is ignoring the delete/expire/copy/move actions because they are hidden forms
if (!eventFlags.isDeleteProductSuccess && (productAndAssoc != null || eventFlags.isEffnewProduct || eventFlags.isAddProductFailed)) {
    eventFlags.targetRecord = "product";
    if (eventFlags.isAddProductFailed) eventFlags.formActionType = "add";
    else eventFlags.formActionType = (context.productAndAssoc == null) ? "new" : "edit";
} else if (!eventFlags.isDeleteCategorySuccess && (context.productCategoryAndAssoc != null || eventFlags.isEffnewCategory || eventFlags.isAddCategoryFailed)) {
    eventFlags.targetRecord = "category";
    if (eventFlags.isAddCategoryFailed) eventFlags.formActionType = "add";
    else eventFlags.formActionType = (context.productCategoryAndAssoc == null) ? "new" : "edit";
} else {
    eventFlags.targetRecord = "catalog";
    if (eventFlags.isAddCatalogFailed) eventFlags.formActionType = "add";
    else eventFlags.formActionType = (context.prodCatalogAndStoreAssoc == null) ? "new" : "edit";
}
eventFlags.targetRecordAction = eventFlags.targetRecord + "-" + eventFlags.formActionType; // easier to check in ftl

// dump flags in context (FIXME?: remove this later)
context.putAll(eventFlags);

eventStates = [:];
eventStates.putAll(eventFlags);

// add some more (not in context)
eventStates.isError = context.isSetupEventError;

context.eventStates = eventStates;

allProdCatalogList = EntityQuery.use(delegator).from("ProdCatalog").orderBy("catalogName", "prodCatalogId").cache(true).queryList();
// remove the ones already linked to tree
availProdCatalogList = [];
for(prodCatalog in allProdCatalogList) {
    if (!prodCatalogIdSet.contains(prodCatalog.prodCatalogId)) {
        availProdCatalogList.add(prodCatalog);
    }
}
context.availProdCatalogList = availProdCatalogList;
context.allProdCatalogList = allProdCatalogList;



