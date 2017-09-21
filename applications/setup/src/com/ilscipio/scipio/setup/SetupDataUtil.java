package com.ilscipio.scipio.setup;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;

/**
 * Raw setup step data check logic.
 * USE {@link SetupWorker} TO INVOKE THESE DURING REAL SETUP.
 * This is for general reuse and to keep the core logic clear/separate.
 */
public abstract class SetupDataUtil {

    public static final String module = SetupDataUtil.class.getName();
    
    protected SetupDataUtil() {
    }

    public static Map<String, Object> getOrganizationStepData(Delegator delegator, LocalDispatcher dispatcher, String orgPartyId, Map<String, Object> params, boolean useCache) throws GeneralException {
        Map<String, Object> result = new HashMap<>();
        
        if (UtilValidate.isNotEmpty(orgPartyId)) {
            Map<String, Object> fields = new HashMap<>();
            fields.put("roleTypeId", "INTERNAL_ORGANIZATIO");
            fields.put("partyId", orgPartyId);
            List<GenericValue> partyRoles = delegator.findByAnd("PartyRole", fields, null, useCache);
            if (UtilValidate.isNotEmpty(partyRoles)) {
                result.put("partyValid", true);
                // this isn't needed - see SetupWizardCommonActions.groovy
                //result.put("orgPartyId", orgPartyId);
                result.put("completed", true);
                return result;
            }
            result.put("partyValid", false);
        }
        result.put("completed", false);
        return result;
    }
    
    public static Map<String, Object> getUserStepData(Delegator delegator, LocalDispatcher dispatcher, String orgPartyId, Map<String, Object> params, boolean useCache) throws GeneralException {
        Map<String, Object> result = new HashMap<>();
        // TODO
        result.put("completed", false);
        return result;
    }
    
    public static Map<String, Object> getAccountingStepData(Delegator delegator, LocalDispatcher dispatcher, String orgPartyId, Map<String, Object> params, boolean useCache) throws GeneralException {
        Map<String, Object> result = new HashMap<>();
        // TODO
        result.put("completed", false);
        return result;
    }
    
    public static Map<String, Object> getFacilityStepData(Delegator delegator, LocalDispatcher dispatcher, String orgPartyId, Map<String, Object> params, boolean useCache) throws GeneralException {
        Map<String, Object> result = new HashMap<>();
        // TODO
        result.put("completed", false);
        return result;
    }
    
    public static Map<String, Object> getCatalogStepStateData(Delegator delegator, LocalDispatcher dispatcher, String orgPartyId, Map<String, Object> params, boolean useCache) throws GeneralException {
        Map<String, Object> result = new HashMap<>();
        // TODO
        result.put("completed", false);
        return result;
    }
    
    public static Map<String, Object> getStoreStepStateData(Delegator delegator, LocalDispatcher dispatcher, String orgPartyId, Map<String, Object> params, boolean useCache) throws GeneralException {
        Map<String, Object> result = new HashMap<>();
        // TODO
        result.put("completed", false);
        return result;
    }
}
