/**
 * SCIPIO: save the current product into session as last viewed features product.
 */

if (!parameters.productId && !context.productId) {
    productId = session.getAttribute("featureLastProductId"); 
    parameters.productId = productId;
    context.productId = productId;
}
