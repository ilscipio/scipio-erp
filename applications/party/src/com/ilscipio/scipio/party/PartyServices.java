package com.ilscipio.scipio.party;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityListIterator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;

import java.util.List;
import java.util.Map;

public class PartyServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String resource = "PartyUiLabels";
    public static final String resource_error = "PartyErrorUiLabels";

    /**
     * Checks if an a UserLogin exists.
     *
     * @param dctx
     * @param context
     * @return
     */
    public static Map<String, Object> findUserLogin(DispatchContext dctx, Map<String, ? extends Object> context) {

        Delegator delegator = dctx.getDelegator();

        GenericValue userLogin = null;
        try {
            userLogin = delegator.findOne("UserLogin",
                    UtilMisc.toMap("userLoginId", context.get("userLoginId")), false);
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError("Error trying to get UserLogin: " + e.getMessage());
        }

        // to avoid that error messages are always printed out we return success, if no technical error occured
        Map<String, Object> result = ServiceUtil.returnSuccess();
        if (userLogin != null) {
            if (Debug.infoOn()) Debug.logInfo("userLogin exists", module);
            result.put("exists", Boolean.TRUE);
            result.put("userLogin", userLogin);
        } else {
            if (Debug.infoOn()) Debug.logInfo("userLogin does not exist", module);
            result.put("exists", Boolean.FALSE);
        }
        return result;
    }

    /**
     * Creates a Person and UserLogin and adds the new user to provided security groups.
     *
     * @param dctx
     * @param context
     * @return
     */
    public static Map<String, Object> createPartyLoginAndSecurityGroups(DispatchContext dctx, Map<String, Object> context) {
        Map<String, Object> result = null;
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Delegator delegator = dctx.getDelegator();

        try {
            Map<String, Object> subContext = dctx.getModelService("createPersonAndUserLogin").makeValid(context,
                    ModelService.IN_PARAM);
            result = dispatcher.runSync("createPersonAndUserLogin", subContext);
        } catch (GenericServiceException e) {
            Debug.logError(e, module);
        }

        if (!ServiceUtil.isError(result)) {
            GenericValue system;
            try {
                system = delegator.findOne("UserLogin", UtilMisc.toMap("userLoginId", "system"), false);

                Map<String, Object> subContext = dctx.getModelService("addUserLoginToSecurityGroup").makeValid(context,
                        ModelService.IN_PARAM);
                subContext.put("userLogin", system);

                Map<String, Object> resultGroups = dispatcher.runSync("addUserLoginToSecurityGroup", subContext);

                result.putAll(resultGroups);

            } catch (GenericEntityException | GenericServiceException e) {
                Debug.logError(e, module);
            }
        }

        return result;
    }

    public static Map<String, Object> countOldUnexpiredContactMechPurposes(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();

        EntityCondition cond = EntityCondition.makeCondition(EntityCondition.makeCondition("purposeThruDate", EntityOperator.EQUALS, null),
                EntityOperator.AND, EntityCondition.makeCondition("contactThruDate", EntityOperator.NOT_EQUAL, null));
        long partyPurposeCount;
        try {
            partyPurposeCount = EntityQuery.use(delegator).from("PartyContactMechAndPurpose").where(cond).queryCount();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.toString());
        }

        long facilityPurposeCount;
        try {
            facilityPurposeCount = EntityQuery.use(delegator).from("FacilityContactMechAndPurpose").where(cond).queryCount();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.toString());
        }
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("partyPurposeCount", partyPurposeCount);
        result.put("facilityPurposeCount", facilityPurposeCount);
        result.put("totalPurposeCount", partyPurposeCount + facilityPurposeCount);
        return result;
    }

    public static Map<String, Object> expireOldUnexpiredContactMechPurposes(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();

        // TODO?: this could be optimized to update in batches
        boolean previewOnly = Boolean.TRUE.equals(context.get("previewOnly"));

        EntityCondition cond = EntityCondition.makeCondition(EntityCondition.makeCondition("purposeThruDate", EntityOperator.EQUALS, null),
                EntityOperator.AND, EntityCondition.makeCondition("contactThruDate", EntityOperator.NOT_EQUAL, null));
        long partyPurposeCount = 0;
        try {
            try (EntityListIterator listIt = EntityQuery.use(delegator).from("PartyContactMechAndPurpose").where(cond).queryIterator()) {
                GenericValue mechAndPurpose;
                while ((mechAndPurpose = listIt.next()) != null) {
                    if (partyPurposeCount == 0) {
                        Debug.logInfo("expireOldUnexpiredContactMechPurposes: Found unexpired PartyContactMechPurpose records"
                                + " associated to expired PartyContactMech records", module);
                    }
                    GenericValue purpose = mechAndPurpose.extractViewMember("PartyContactMechPurpose");
                    purpose.set("thruDate", mechAndPurpose.get("contactThruDate"));
                    if (previewOnly) {
                        Debug.logInfo("expireOldUnexpiredContactMechPurposes: preview: would expire: " + mechAndPurpose.toString(), module);
                    } else {
                        purpose.store();
                    }
                    partyPurposeCount++;
                }
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.toString());
        }

        long facilityPurposeCount = 0;
        try {
            try (EntityListIterator listIt = EntityQuery.use(delegator).from("FacilityContactMechAndPurpose").where(cond).queryIterator()) {
                GenericValue mechAndPurpose;
                while ((mechAndPurpose = listIt.next()) != null) {
                    if (partyPurposeCount == 0) {
                        Debug.logInfo("expireOldUnexpiredContactMechPurposes: Found unexpired FacilityContactMechPurpose records"
                                + " associated to expired FacilityContactMech records", module);
                    }
                    GenericValue purpose = mechAndPurpose.extractViewMember("FacilityContactMechPurpose");
                    purpose.set("thruDate", mechAndPurpose.get("contactThruDate"));
                    if (previewOnly) {
                        Debug.logInfo("expireOldUnexpiredContactMechPurposes: preview: would expire: " + mechAndPurpose.toString(), module);
                    } else {
                        purpose.store();
                    }
                    facilityPurposeCount++;
                }
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.toString());
        }
        if (previewOnly) {
            Debug.logInfo("expireOldUnexpiredContactMechPurposes: preview: would expire " + partyPurposeCount
                    + " PartyContactMechPurpose and " + facilityPurposeCount + " FacilityContactMechPurpose records", module);
        } else {
            Debug.logInfo("expireOldUnexpiredContactMechPurposes: expired " + partyPurposeCount
                    + " PartyContactMechPurpose and " + facilityPurposeCount + " FacilityContactMechPurpose records", module);
        }

        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("partyPurposeCount", partyPurposeCount);
        result.put("facilityPurposeCount", facilityPurposeCount);
        result.put("totalPurposeCount", partyPurposeCount + facilityPurposeCount);
        return result;
    }

    /**
     * Checks if a PartyContactMech for a given contactMechTypeId exists.
     *
     * @param dctx
     * @param context
     * @return
     */
    public static Map<String, Object> checkEmailPartyContactMech(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();

        Map<String, Object> serviceContext = (Map<String, Object>) context.get("serviceContext");

        String partyId = (String) serviceContext.get("partyId");
        Map<String, Object> result = ServiceUtil.returnSuccess();

        try {
            List<GenericValue> partyContactMechs = EntityQuery.use(delegator).from("PartyContactMech").where("partyId", partyId).queryList();
            result.put("conditionReply", Boolean.FALSE);
            for (GenericValue partyContactMech : partyContactMechs) {
                GenericValue contactMech = partyContactMech.getRelatedOne("ContactMech", false);
                if ("EMAIL_ADDRESS".equals(contactMech.getString("contactMechTypeId")) && "Y".equals(partyContactMech.getString("allowSolicitation"))) {
                    result.put("conditionReply", Boolean.TRUE);
                    break;
                }
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }

        return result;
    }

}
