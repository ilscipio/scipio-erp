/**
 * SCIPIO: gets the last viewed product and puts it into context/parameters.
 */

if (!parameters.productId && !context.productId) {
    productId = session.getAttribute("productLastProductId");
    parameters.productId = productId;
    context.productId = productId;
}
