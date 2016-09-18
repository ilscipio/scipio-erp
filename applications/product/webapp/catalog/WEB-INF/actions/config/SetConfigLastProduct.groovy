/**
 * SCIPIO: gets the last viewed config product and puts it into parameters.productId.
 */

if (context.productId) {
    session.setAttribute("configLastProductId", context.productId);
} else if (parameters.productId) {
    session.setAttribute("configLastProductId", parameters.productId);
}
