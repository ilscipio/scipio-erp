/**
 * SCIPIO: gets the last viewed product and puts it into context/parameters.
 * By default this does NOT set in globalContext and MUST NOT unless flag
 * asking for it is passed.
 */

useGlobal = context.getProductLastProduct?.global;
if (useGlobal == null) {
    useGlobal = false;
}
 
productLastProductId = session.getAttribute("productLastProductId"); 
context.productLastProductId = productLastProductId;

if (!parameters.productId && !context.productId && !globalContext.productId) {
    productId = productLastProductId;
    parameters.productId = productId;
    if (useGlobal) {
        globalContext.productId = productId;
    } else {
        context.productId = productId;
    }
}
