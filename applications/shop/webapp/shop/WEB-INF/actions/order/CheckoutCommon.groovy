

final module = "CheckoutCommon.groovy";

// SCIPIO: TODO? Determine these in smarter way, to unify anon and registered checkouts

if (!context.checkoutType) {
    context.checkoutType = "full"
}

if (!context.checkoutMode) {
    context.checkoutMode = "registered"
}

cart = org.ofbiz.order.shoppingcart.ShoppingCartEvents.getCartObject(request); // SCIPIO: Must use accessor, not this: session.getAttribute("shoppingCart");
cartSize = 0;
if (cart != null) {
    cartSize = cart.size();
}

context.cart = cart;
context.cartSize = cartSize;

