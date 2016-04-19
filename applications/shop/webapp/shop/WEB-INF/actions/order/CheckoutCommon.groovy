

final module = "CheckoutCommon.groovy";

// Cato: TODO? Determine these in smarter way, to unify anon and registered checkouts

if (!context.checkoutType) {
    context.checkoutType = "full"
}

if (!context.checkoutMode) {
    context.checkoutMode = "registered"
}

