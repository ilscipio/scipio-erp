package com.ilscipio.scipio.cms.menu;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityFindOptions;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.cms.CmsServiceUtil;
import com.ilscipio.scipio.cms.ServiceErrorFormatter;
import com.ilscipio.scipio.cms.ServiceErrorFormatter.FormattedError;
import com.ilscipio.scipio.cms.template.CmsAssetTemplate;

public abstract class CmsMenuServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    static final String logPrefix = "Cms: Menu: ";
    private static final ServiceErrorFormatter errorFmt = 
            CmsServiceUtil.getErrorFormatter().specialize().setDefaultLogMsgGeneral("Menu Error").build();
    
    protected CmsMenuServices() {
    }
    
    /**
     * Returns a single Menu as a JSON object
     */
    public static Map<String, Object> getMenu(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        //LocalDispatcher dispatcher = dctx.getDispatcher();
        Map<String, Object> result = ServiceUtil.returnSuccess();
        String menuId = (String) context.get("menuId");
        //String websiteId = (String) context.get("websiteId");
        
        try {
            GenericValue value = delegator.findOne("CmsMenu", false, UtilMisc.toMap("menuId", menuId));
            result.put("menuJson", value.getString("menuJson"));

        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, "Error getting menus", null, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnFailure();
        }
        return result;
    }
    
    /**
     * Generates a list of all available menus. Can be filtered by
     * WebsiteId (TODO).
     */
    public static Map<String, Object> getMenus(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        //LocalDispatcher dispatcher = dctx.getDispatcher();
        Map<String, Object> result = ServiceUtil.returnSuccess();
        String websiteId = (String) context.get("websiteId");
        
        try {
            EntityFindOptions efo = new EntityFindOptions();
            List<GenericValue> values;
            efo.setFetchSize(1);
            if (UtilValidate.isNotEmpty(websiteId)) {
                EntityCondition ec = EntityCondition.makeCondition(
                        "websiteId", EntityOperator.EQUALS, websiteId);

                values = (List<GenericValue>) delegator
                        .findList("CmsMenu", ec, null,
                                UtilMisc.toList("createdStamp DESC"), efo,
                                true);
            }else {
                values = (List<GenericValue>) delegator
                        .findList("CmsMenu", null, null,
                                UtilMisc.toList("createdStamp DESC"), efo,
                                true);
            }
            
            /*
            List<String> resultValues = new ArrayList<String>();
            for(GenericValue menu : values){
                resultValues.add(menu.getString("menuJson"));
            }*/
            
            String resultJson = JSON.from(values).toString();
            result.put("menuJson", resultJson);

        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, "Error getting menus", null, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnFailure();
        }

        return result;
    }

    /**
     * Creates or updates a menu
     * 
     * @param dctx
     * @param context
     * @return
     */
    public static Map<String, Object> createUpdateMenu(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Map<String, Object> result = ServiceUtil.returnSuccess();
        String menuId = (String) context.get("menuId");
        String websiteId = (String) context.get("websiteId");
        
        try {
            GenericValue userLogin = CmsServiceUtil.getUserLoginOrSystem(dctx, context);
            // Create empty template
            Map<String, Object> fields = ServiceUtil.setServiceFields(dispatcher, "cmsCreateUpdateMenu", 
                    UtilGenerics.<String, Object> checkMap(context), userLogin, null, null);
            
            CmsMenu menuTmp = null;
            if (UtilValidate.isNotEmpty(menuId)) {
                fields.put("createdBy", userLogin.getString("userLoginId"));
                menuTmp = new CmsMenu(delegator, fields);
                menuTmp.update(fields);
            } else {
                fields.put("lastUpdatedBy", (String) userLogin.get("userLoginId"));
                menuTmp = new CmsMenu(delegator, fields);
            }
            
            menuTmp.store();
            result.put("menuId", menuTmp.getId());
            result.put("menuJson", menuTmp.getMenuJson());
            
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, "Error creating or updating menu", context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }
    
    /**
     * Deletes a menu
     */
    public static Map<String, Object> deleteMenu(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> result = ServiceUtil.returnSuccess();

        String menuId = (String) context.get("menuId");
           
        try {
            
            CmsMenu menuTmp = null;
            if (UtilValidate.isNotEmpty(menuId)) {
                GenericValue value = delegator.findOne("CmsMenu", false, UtilMisc.toMap("menuId", menuId));
                menuTmp = new CmsMenu(value);
                menuTmp.remove();
            }
          
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, "Error removing menu", context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }

        return result;
    }
}
