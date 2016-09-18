/**
 * SCIPIO: gets the last viewed product store and puts it into context/parameters.
 * WARN: this might not be appropriate for all store screens. this version does NOT
 * set globalContext, only context, for safety.
 */

if (!parameters.productStoreId && !context.productStoreId && !globalContext.productStoreId) {
    productStoreId = session.getAttribute("storeLastProductStoreId"); 
    parameters.productStoreId = productStoreId;
    context.productStoreId = productStoreId;
}
