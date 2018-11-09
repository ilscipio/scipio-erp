package com.ilscipio.scipio.shop.shoppingcart;

import java.math.BigDecimal;

import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.order.shoppingcart.shipping.ShippingEstimateWrapper;

/**
 * SCIPIO
 */
public final class CheckOutHelper {

    private static final boolean isShowAlways = "always".equals(UtilProperties.getPropertyValue("shop", "shop.shipping.estimate.showMethodIfNoEst"));

    private CheckOutHelper() {
    }

    public static boolean isDisplayShipEstimate(BigDecimal shippingEst, GenericValue carrierShipmentMethod, ShippingEstimateWrapper shippingEstWpr) {
        if (shippingEstWpr.isValidEstimate(shippingEst, carrierShipmentMethod)) {
            return true;
        }
        return isShowAlways;
    }
}
