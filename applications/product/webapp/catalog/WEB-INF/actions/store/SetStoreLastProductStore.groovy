/**
 * SCIPIO: save the current product store into session as last viewed store product store.
 */

if (context.productStoreId) {
    session.setAttribute("storeLastProductStoreId", context.productStoreId);
} else if (globalContext.productStoreId) {
    session.setAttribute("storeLastProductStoreId", globalContext.productStoreId);
} else if (parameters.productStoreId) {
    session.setAttribute("storeLastProductStoreId", parameters.productStoreId);
}
