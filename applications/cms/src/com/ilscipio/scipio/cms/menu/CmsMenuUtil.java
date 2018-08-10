package com.ilscipio.scipio.cms.menu;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;

import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

/**
 * SCIPIO: CmsMenuUtil
 * <p>
 */
public abstract class CmsMenuUtil {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    public static CmsMenu getMenuById(Delegator delegator, String menuId) {
        GenericValue value;
        CmsMenu cmsMenu = null;
        try {
            value = delegator.findOne("CmsMenu", true, UtilMisc.toMap("menuId", menuId));
            if(value!=null){
                cmsMenu = new CmsMenu(value);
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }
        return cmsMenu;
    }
    
    public static Object getMenuJsonById(Delegator delegator, String menuId) {
        GenericValue value;
        Object returnObj = new ArrayList<Object>();
        try {
            value = delegator.findOne("CmsMenu", true, UtilMisc.toMap("menuId", menuId));
            if(value!=null){
                CmsMenu cmsMenu = new CmsMenu(value);
                JSON json = JSON.from(cmsMenu.getEntityMenuJson());
                returnObj = json.toObject(LinkedList.class);
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        } catch (IOException e) {
            Debug.logError(e, module);
        }
        return returnObj;
    }    
}
