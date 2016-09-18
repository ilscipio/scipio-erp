/**
 * SCIPIO: gets the last viewed product and puts it into context/parameters.
 * WARN: this might not be appropriate for all store screens. this version does NOT
 * set globalContext, only context, for safety.
 * specific sub-decorators can copy this script and make their own versions.
 */

if (!parameters.productId && !context.productId && !globalContext.productId) {
    productId = session.getAttribute("productLastProductId"); 
    parameters.productId = productId;
    context.productId = productId;
}
