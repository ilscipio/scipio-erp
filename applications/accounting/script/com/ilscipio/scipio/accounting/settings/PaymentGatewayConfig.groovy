import org.ofbiz.base.util.Debug

// SCIPIO: Use this script to check first if custom PaymentGatewayConfig exist (mostly from addons)

// Check if PaymentGatewayStripeRest entity exists
paymentGatewayStripeRestModelEntity = delegator.getModelReader().getModelEntityNoCheck("PaymentGatewayStripeRest");
if (paymentGatewayStripeRestModelEntity) {
    Debug.log("paymentGatewayStripeRestModelEntity exists");
    paymentGatewayStripeRest = delegator.findOne("PaymentGatewayStripeRest", ["paymentGatewayConfigId" : parameters.paymentGatewayConfigId], false);
    context.paymentGatewayStripeRest = paymentGatewayStripeRest;
    context.paymentGatewayStripeRestModelEntity = paymentGatewayStripeRestModelEntity;    
}

// Check if PaymentGatewayPayPalRest entity exists
paymentGatewayPayPalRestModelEntity = delegator.getModelReader().getModelEntityNoCheck("PaymentGatewayPayPalRest");
if (paymentGatewayPayPalRestModelEntity) {
    paymentGatewayPayPalRest = delegator.findOne("PaymentGatewayPayPalRest", ["paymentGatewayConfigId" : parameters.paymentGatewayConfigId], false);
    context.paymentGatewayPayPalRest = paymentGatewayPayPalRest;
    context.paymentGatewayPayPalRestModelEntity = paymentGatewayPayPalRestModelEntity;
}
