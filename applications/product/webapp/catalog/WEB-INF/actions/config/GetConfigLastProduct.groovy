/**
 * SCIPIO: save the current product into session as last viewed config product.
 */

if (!parameters.productId && !context.productId) {
    productId = session.getAttribute("configLastProductId");
    parameters.productId = productId;
    context.productId = productId;
}
