package com.ilscipio.scipio.setup;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;

/**
 * Raw setup step data check logic. USE {@link SetupWorker} TO INVOKE THESE
 * DURING REAL SETUP. This is for general reuse and to keep the core logic
 * clear/separate.
 */
public abstract class SetupDataUtil {

    public static final String module = SetupDataUtil.class.getName();

    protected SetupDataUtil() {
    }

    // WARN: params map may contain unvalidated user input - others in the map may be already validated.
    // The SetupWorker.StepState subclasses handle the implicit deps and decides which params must be pre-validated.
    // DO NOT call these methods from screen - all must go through SetupWorker.

    public static Map<String, Object> getOrganizationStepData(Delegator delegator, LocalDispatcher dispatcher, 
            Map<String, Object> params, boolean useCache) throws GeneralException {
        Map<String, Object> result = new HashMap<>();

        String orgPartyId = (String) params.get("orgPartyId");

        if (UtilValidate.isNotEmpty(orgPartyId)) {
            Map<String, Object> fields = new HashMap<>();
            fields.put("roleTypeId", "INTERNAL_ORGANIZATIO");
            fields.put("partyId", orgPartyId);
            List<GenericValue> partyRoles = delegator.findByAnd("PartyRole", fields, null, useCache);
            if (UtilValidate.isNotEmpty(partyRoles)) {
                result.put("partyValid", true);
                result.put("orgPartyId", orgPartyId);
                result.put("completed", true);
                return result;
            }
            result.put("partyValid", false);
        }
        result.put("completed", false);
        return result;
    }

    public static Map<String, Object> getUserStepData(Delegator delegator, LocalDispatcher dispatcher, 
            Map<String, Object> params, boolean useCache) throws GeneralException {
        Map<String, Object> result = new HashMap<>();

        String userPartyId = (String) params.get("userPartyId");
        if (UtilValidate.isNotEmpty(userPartyId)) {
            Map<String, Object> fields = new HashMap<>();            
            fields.put("partyId", userPartyId);
            List<GenericValue> party = delegator.findByAnd("Party", fields, null, useCache);
            if (UtilValidate.isNotEmpty(party)) {
                result.put("userValid", true);
                result.put("userPartyId", userPartyId);
                result.put("completed", true);
                return result;
            }
            result.put("userValid", false);
        }

        result.put("completed", false);
        return result;
    }

    public static Map<String, Object> getAccountingStepData(Delegator delegator, LocalDispatcher dispatcher, 
            Map<String, Object> params, boolean useCache) throws GeneralException {
        Map<String, Object> result = new HashMap<>();
        // TODO
        result.put("completed", false);
        return result;
    }

    public static Map<String, Object> getFacilityStepData(Delegator delegator, LocalDispatcher dispatcher, 
            Map<String, Object> params, boolean useCache) throws GeneralException {
        Map<String, Object> result = new HashMap<>();
        // TODO
        result.put("completed", false);
        return result;
    }

    public static Map<String, Object> getCatalogStepStateData(Delegator delegator, LocalDispatcher dispatcher, 
            Map<String, Object> params, boolean useCache) throws GeneralException {
        Map<String, Object> result = new HashMap<>();
        // TODO
        result.put("completed", false);
        return result;
    }

    public static Map<String, Object> getStoreStepStateData(Delegator delegator, LocalDispatcher dispatcher, 
            Map<String, Object> params, boolean useCache) throws GeneralException {
        Map<String, Object> result = new HashMap<>();

        String productStoreId = (String) params.get("productStoreId");
        String orgPartyId = (String) params.get("orgPartyId");

        if (UtilValidate.isNotEmpty(productStoreId)) {
            if (UtilValidate.isNotEmpty(orgPartyId)) {
                Map<String, Object> fields = new HashMap<>();
                fields.put("productStoreId", productStoreId);
                fields.put("payToPartyId", orgPartyId);
                List<GenericValue> productStores = delegator.findByAnd("ProductStore", fields, null, useCache);
                if (UtilValidate.isNotEmpty(productStores)) {
                    result.put("storeValid", true);
                    result.put("productStoreId", productStores.get(0).getString("productStoreId"));
                    result.put("productStore", productStores.get(0));
                    result.put("completed", true);
                    return result;
                } else {
                    result.put("storeValid", false);
                }
            } else {
                // we'll require a non-null orgPartyId here to simplify, so both
                // parameters should be passed around
                result.put("storeValid", false);
            }
        } else if (!UtilMisc.booleanValueVersatile(params.get("newProductStore"), false)) {
            // Unless asked to create a new store, read the first store by default;
            // in majority cases clients will create one store per company, so this saves some reloading.
            if (UtilValidate.isNotEmpty(orgPartyId)) {
                Map<String, Object> fields = new HashMap<>();
                fields.put("payToPartyId", orgPartyId);
                List<GenericValue> productStores = delegator.findByAnd("ProductStore", fields, null, useCache);
                if (UtilValidate.isNotEmpty(productStores)) {
                    result.put("storeValid", true);
                    result.put("productStoreId", productStores.get(0).getString("productStoreId"));
                    result.put("productStore", productStores.get(0));
                    result.put("completed", true);
                    return result;
                }
            }
        }

        result.put("completed", false);
        return result;
    }

    public static Map<String, Object> getWebsiteStepStateData(Delegator delegator, LocalDispatcher dispatcher, 
            Map<String, Object> params, boolean useCache) throws GeneralException {
        Map<String, Object> result = new HashMap<>();
        // TODO
        result.put("completed", false);
        return result;
    }
}
