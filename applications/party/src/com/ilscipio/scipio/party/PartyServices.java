package com.ilscipio.scipio.party;

import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;

public class PartyServices {
    
    public final static String module = PartyServices.class.getName();
    
    /**
     * Checks if an a UserLogin exists.
     * 
     * @param dctx
     * @param context
     * @return
     */
    public static Map<String,Object> findUserLogin(DispatchContext dctx, Map<String, ? extends Object> context) {        

        Delegator delegator = dctx.getDelegator();
        
        GenericValue userLogin = null;
        try {
            userLogin = delegator.findOne("UserLogin", 
                    UtilMisc.toMap("userLoginId", context.get("userLoginId")),false);
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError("Error trying to get UserLogin: " + e.getMessage());
        }        
        
        // to avoid that error messages are always printed out we return success, if no technical error occured
        Map<String,Object> result = ServiceUtil.returnSuccess();
        if (userLogin != null) {
            if (Debug.infoOn()) Debug.logInfo("userLogin exists", module);
            result.put("exists", new Boolean(true));
            result.put("userLogin", userLogin);
        } else {
            if (Debug.infoOn()) Debug.logInfo("userLogin does not exist", module);
            result.put("exists", new Boolean(false));
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
    public static Map<String,Object> createPartyLoginAndSecurityGroups(DispatchContext dctx, Map<String,Object> context) {
        Map<String,Object> result = null;
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Delegator delegator = dctx.getDelegator();
        
        try {
            Map<String, Object> subContext = dctx.getModelService("createPersonAndUserLogin").makeValid(context,
                    ModelService.IN_PARAM);
            result = dispatcher.runSync("createPersonAndUserLogin", subContext);
        } catch (GenericServiceException e) {

        }
        
        if (!ServiceUtil.isError(result)) {
            GenericValue system;
            try {
                system = delegator.findOne("UserLogin", UtilMisc.toMap("userLoginId", "system"),false);
                
                Map<String, Object> subContext = dctx.getModelService("addUserLoginToSecurityGroup").makeValid(context,
                        ModelService.IN_PARAM);
                subContext.put("userLogin", system);

                Map<String,Object> resultGroups = dispatcher.runSync("addUserLoginToSecurityGroup", subContext);
                
                result.putAll(resultGroups);
                
            } catch (GenericEntityException e) {
               
            } catch (GenericServiceException e) {

            }
        }
        
        return result;
    }

}
