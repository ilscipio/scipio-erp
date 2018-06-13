package com.ilscipio.scipio.cms.menu;

import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.cms.CmsServiceUtil;
import com.ilscipio.scipio.cms.ServiceErrorFormatter;
import com.ilscipio.scipio.cms.ServiceErrorFormatter.FormattedError;

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
        
        try {

            result.put("menuJson", "");

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
        try {

            result.put("menuJson", "");

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
        //GenericValue userLogin = (GenericValue) context.get("userLogin");
        
        String menuId = (String) context.get("menuId");

        try {
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

        String contentId = (String) context.get("menuId");
           
        try {
          
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, "Error removing menu", context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }

        return result;
    }
}
