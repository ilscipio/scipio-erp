/**
 * SCIPIO: save the current product into session as last viewed product.
 */

productLastProductId = context.productId ?: (globalContext.productId ?: parameters.productId);

if (productLastProductId) {
    session.setAttribute("productLastProductId", productLastProductId);
    context.productLastProductId = productLastProductId;
}
