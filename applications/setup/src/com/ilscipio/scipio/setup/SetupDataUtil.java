package com.ilscipio.scipio.setup;

import java.util.ArrayList;
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
 * Raw setup step data check logic.
 * USE {@link SetupWorker} TO INVOKE THESE DURING REAL SETUP.
 * This is for general reuse and to keep the core logic clear/separate.
 */
public abstract class SetupDataUtil {

    public static final String module = SetupDataUtil.class.getName();
    
    protected SetupDataUtil() {
    }

    public static Map<String, Object> getOrganizationStepData(Delegator delegator, LocalDispatcher dispatcher) throws GeneralException {
        Map<String, Object> result = new HashMap<>();
        
        List<GenericValue> partyRoles = delegator.findByAnd("PartyRole", UtilMisc.toMap("roleTypeId", "INTERNAL_ORGANIZATIO"), null, false);
        if (UtilValidate.isNotEmpty(partyRoles)) {
            List<String> scpOrgPartyIdList = new ArrayList<>(partyRoles.size());
            for(GenericValue partyRole : partyRoles) {
                scpOrgPartyIdList.add(partyRole.getString("partyId"));
            }
            result.put("orgPartyIdList", scpOrgPartyIdList);
            result.put("orgPartyRoleList", partyRoles);
            result.put("completed", true);
            return result;
        }
        result.put("completed", false);
        return result;
    }
    
    public static Map<String, Object> getUserStepData(Delegator delegator, LocalDispatcher dispatcher) throws GeneralException {
        Map<String, Object> result = new HashMap<>();
        // TODO
        result.put("completed", false);
        return result;
    }
    
    public static Map<String, Object> getAccountingStepData(Delegator delegator, LocalDispatcher dispatcher) throws GeneralException {
        Map<String, Object> result = new HashMap<>();
        // TODO
        result.put("completed", false);
        return result;
    }
    
    public static Map<String, Object> getFacilityStepData(Delegator delegator, LocalDispatcher dispatcher) throws GeneralException {
        Map<String, Object> result = new HashMap<>();
        // TODO
        result.put("completed", false);
        return result;
    }
    
    public static Map<String, Object> getCatalogStepStateData(Delegator delegator, LocalDispatcher dispatcher) throws GeneralException {
        Map<String, Object> result = new HashMap<>();
        // TODO
        result.put("completed", false);
        return result;
    }
    
    public static Map<String, Object> getWebsiteStepStateData(Delegator delegator, LocalDispatcher dispatcher) throws GeneralException {
        Map<String, Object> result = new HashMap<>();
        // TODO
        result.put("completed", false);
        return result;
    }
}
