/**
 * SCIPIO: save the current product into session as last viewed product.
 */

if (context.productId) {
    session.setAttribute("productLastProductId", context.productId);
} else if (globalContext.productId) {
    session.setAttribute("productLastProductId", globalContext.productId);
} else if (parameters.productId) {
    session.setAttribute("productLastProductId", parameters.productId);
}
