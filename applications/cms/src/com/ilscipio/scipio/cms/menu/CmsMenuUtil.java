package com.ilscipio.scipio.cms.menu;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

/**
 * SCIPIO: CmsMenuUtil
 * <p>
 */
public abstract class CmsMenuUtil {
    
    
    public static CmsMenu getMenuById(Delegator delegator, String menuId) {
        GenericValue value;
        CmsMenu cmsMenu = null;
        try {
            value = delegator.findOne("CmsMenu", true, UtilMisc.toMap("menuId", menuId));
            if(value!=null){
                cmsMenu = new CmsMenu(value);
            }
        } catch (GenericEntityException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return cmsMenu;
    }
    
    public static Object getMenuJsonById(Delegator delegator, String menuId) {
        GenericValue value;
        List<HashMap> returnObj = new ArrayList();
        try {
            value = delegator.findOne("CmsMenu", true, UtilMisc.toMap("menuId", menuId));
            if(value!=null){
                CmsMenu cmsMenu = new CmsMenu(value);
                JSON json = JSON.from(cmsMenu.getEntityMenuJson());
                returnObj = json.toObject(LinkedList.class);
            }
        } catch (GenericEntityException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return returnObj;
    }    
}
