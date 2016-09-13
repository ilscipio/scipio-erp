/**
 * SCIPIO: gets the last viewed feature product and puts it into parameters.productId.
 */

if (context.productId) {
    session.setAttribute("featureLastProductId", context.productId);
} else if (parameters.productId) {
    session.setAttribute("featureLastProductId", parameters.productId);
}
