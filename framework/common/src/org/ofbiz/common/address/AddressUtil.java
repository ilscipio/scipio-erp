package org.ofbiz.common.address;

import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.ModelService;

import com.google.i18n.phonenumbers.NumberParseException;
import com.google.i18n.phonenumbers.PhoneNumberUtil;
import com.google.i18n.phonenumbers.Phonenumber.PhoneNumber;

/**
 * SCIPIO: Common address/telecom/phone utilities.
 * <p>
 * Some validation utilities require Delegator, which is not available from
 * {@link org.ofbiz.base.util#UtilValidate} at this time, so some stock
 * utilities originally from that class may be moved here instead.
 * <p>
 * NOTE: This util is meant to be ContactMech-agnostic or unaware of the business schema.
 * <p>
 * Added 2018-08-30.
 */
public final class AddressUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private AddressUtil() {
    }

    /**
     * Validates phone numbers (using external library).
     * <p>
     * SCIPIO: 2018-08-30: Stock method moved here from {@link org.ofbiz.base.util#UtilValidate}
     * due to dependency issues.
     */
    public static boolean isValidPhoneNumber(String phoneNumber, Delegator delegator) {
        String geoId = EntityUtilProperties.getPropertyValue("general", "country.geo.id.default", delegator);
        return isValidPhoneNumber(phoneNumber, geoId, delegator);
    }

    /**
     * Validates phone numbers (using external library).
     * <p>
     * SCIPIO: 2018-08-30: Stock method moved here from {@link org.ofbiz.base.util#UtilValidate}
     * due to dependency issues.
     */
    public static boolean isValidPhoneNumber(String phoneNumber, String geoId, Delegator delegator) {
        boolean isValid = false;
        try {
            GenericValue geo = EntityQuery.use(delegator).from("Geo").where("geoId", geoId).cache().queryOne();
            PhoneNumberUtil phoneUtil = PhoneNumberUtil.getInstance();
            String geoCode = geo != null ? geo.getString("geoCode") : "US";
            PhoneNumber phNumber = phoneUtil.parse(phoneNumber, geoCode);
            if (phoneUtil.isValidNumber(phNumber) || phoneUtil.isPossibleNumber(phNumber)) {
                isValid = true;
            }
        } catch (GenericEntityException | NumberParseException ex) {
            Debug.logError(ex, module);
        }
        return isValid;
    }

    /**
     * Splits phone number (using external library).
     * <p>
     * SCIPIO: 2018-08-30: Stock method moved here from {@link org.ofbiz.base.util#UtilMisc}
     * due to dependency issues (and really did not belong there).
     */
    public static Map<String, String> splitPhoneNumber(String phoneNumber, Delegator delegator) {
        Map<String, String> result = new HashMap<>();
        try {
            PhoneNumberUtil phoneUtil = PhoneNumberUtil.getInstance();
            String defaultCountry = EntityUtilProperties.getPropertyValue("general", "country.geo.id.default", delegator);
            GenericValue defaultGeo = EntityQuery.use(delegator).from("Geo").where("geoId", defaultCountry).cache().queryOne();
            String defaultGeoCode = defaultGeo != null ? defaultGeo.getString("geoCode") : "US";
            PhoneNumber phNumber = phoneUtil.parse(phoneNumber, defaultGeoCode);
            if (phoneUtil.isValidNumber(phNumber) || phoneUtil.isPossibleNumber(phNumber)) {
                String nationalSignificantNumber = phoneUtil.getNationalSignificantNumber(phNumber);
                int areaCodeLength = phoneUtil.getLengthOfGeographicalAreaCode(phNumber);
                result.put("countryCode", Integer.toString(phNumber.getCountryCode()));
                if (areaCodeLength > 0) {
                    result.put("areaCode", nationalSignificantNumber.substring(0, areaCodeLength));
                    result.put("contactNumber", nationalSignificantNumber.substring(areaCodeLength));
                } else {
                    result.put("areaCode", "");
                    result.put("contactNumber", nationalSignificantNumber);
                }
            } else {
                Debug.logError("Invalid phone number " + phoneNumber, module);
                result.put(ModelService.ERROR_MESSAGE, "Invalid phone number");
            }
        } catch (GenericEntityException | NumberParseException ex) {
            Debug.logError(ex, module);
            result.put(ModelService.ERROR_MESSAGE, ex.getMessage());
        }
        return result;
    }
}
