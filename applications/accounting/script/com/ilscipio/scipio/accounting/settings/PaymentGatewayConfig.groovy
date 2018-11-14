// SCIPIO: Use this script to check first if custom PaymentGatewayConfig (mostly from addons)

// Check if PaymentGatewayStripeRest entity exists
paymentGatewayStripeRestModelEntity = delegator.getModelReader().getModelEntity("PaymentGatewayStripeRest");
if (paymentGatewayStripeRestModelEntity) {
    paymentGatewayStripeRest = delegator.findOne("PaymentGatewayStripeRest", ["paymentGatewayConfigId" : parameters.paymentGatewayConfigId], false);
    context.paymentGatewayStripeRest = paymentGatewayStripeRest;
}

// Check if PaymentGatewayPayPalRest entity exists
paymentGatewayPayPalRestModelEntity = delegator.getModelReader().getModelEntity("PaymentGatewayPayPalRest");
if (paymentGatewayPayPalRestModelEntity) {
    paymentGatewayPayPalRest = delegator.findOne("PaymentGatewayPayPalRest", ["paymentGatewayConfigId" : parameters.paymentGatewayConfigId], false);
    context.paymentGatewayPayPalRest = paymentGatewayPayPalRest;
}