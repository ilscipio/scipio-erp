package org.ofbiz.order.shoppingcart;

import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.MapValidator;

/**
 * Cato: Shipping address validator class for checkout.
 * <p>
 * TODO: factor out into party app?
 * <p>
 * TODO: This should have a per-store factory pattern.
 */
public class PostalAddressValidator implements MapValidator {

    private final HttpServletRequest request;
    
    protected PostalAddressValidator(HttpServletRequest request) {
        super();
        this.request = request;
    }
    
    public static PostalAddressValidator getInstance(HttpServletRequest request) {
        // TODO: per-shop support
        return new PostalAddressValidator(request);
    }

    @Override
    public void validate(Map<String, Object> map, Map<String, String> errorMessages) {
        // TODO: This is definitely insufficient... for now just make sure the essential fields are not empty
        // TODO: localize
        // toName
        // attnName
        // address1
        // address2
        // city
        // stateProvinceGeoId
        // postalCode
        // countryGeoId
        // allowSolicitation
        if (!stringNotEmpty(map.get("address1"))) {
            errorMessages.put("address1", "Missing address line");
        }
        if (!stringNotEmpty(map.get("city"))) {
            errorMessages.put("city", "Missing city");
        }
        if (!stringNotEmpty(map.get("postalCode"))) {
            errorMessages.put("postalCode", "Missing postal code");
        }
        if (!stringNotEmpty(map.get("countryGeoId"))) {
            errorMessages.put("countryGeoId", "Missing country");
        }
    }
    
    private static boolean stringNotEmpty(Object str) {
        return (str instanceof String) && ((String) str).trim().length() > 0;
    }
}