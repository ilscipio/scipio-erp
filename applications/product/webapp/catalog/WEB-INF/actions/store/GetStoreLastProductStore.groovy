/**
 * SCIPIO: gets the last viewed product store and puts it into context/parameters.
 * WARN: this might not be appropriate for all store screens. this version does NOT
 * set globalContext, only context, for safety.
 */

args = context.getStoreLastProductStore ?: [:];
 
useGlobal = args.global;
if (useGlobal == null) {
    useGlobal = false;
}
  
storeLastProductStoreId = session.getAttribute("storeLastProductStoreId");
context.storeLastProductStoreId = storeLastProductStoreId;
 
if (!parameters.productStoreId && !context.productStoreId && !globalContext.productStoreId) {
    productStoreId = args.overrideProductStoreId ?: session.getAttribute("storeLastProductStoreId") ?: args.defaultProductStoreId;

    parameters.productStoreId = productStoreId;
    if (useGlobal) {
        globalContext.productStoreId = productStoreId;
    } else {
        context.productStoreId = productStoreId;
    }
}
