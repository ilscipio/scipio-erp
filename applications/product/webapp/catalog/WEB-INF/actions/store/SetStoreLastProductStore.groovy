/**
 * SCIPIO: save the current product store into session as last viewed store product store.
 */

storeLastProductStoreId = context.productStoreId ?: (globalContext.productStoreId ?: parameters.productStoreId);

if (storeLastProductStoreId) {
    session.setAttribute("storeLastProductStoreId", storeLastProductStoreId);
    context.storeLastProductStoreId = storeLastProductStoreId;
}
