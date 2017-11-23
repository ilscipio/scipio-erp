package com.ilscipio.scipio.cms;

import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.PropertyMessage;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

public abstract class CmsServiceUtil {
    
    public static final String module = CmsServiceUtil.class.getName();
    
    private CmsServiceUtil() {
    }
    
    /**
     * NOTE: better to use cmsGenericPermission on service def because self-documenting.
     */
    public static void checkCmsPermission(DispatchContext dctx, Map<String, ?> context, 
            String permAction) throws CmsPermissionException {
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (!dctx.getSecurity().hasEntityPermission("CMS", permAction, userLogin)) {
            PropertyMessage propMsg = PropertyMessage.make(ServiceUtil.resource, "serviceUtil.no_permission_to_run", null, " (CMS" + permAction + ")");
            throw new CmsPermissionException(propMsg);
        }
    }
    
    public static String getUserId(Map<String, ?> context) {
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        String userId = userLogin.getString("userLoginId");
        return userId;
    }

    public static String getUserPartyId(Map<String, ?> context) {
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        GenericValue userLogin;
        if (request != null) {
            userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        } else {
            userLogin = (GenericValue) context.get("userLogin");
        }
        String partyId = userLogin.getString("partyId");
        return partyId;
    }
    
    public static GenericValue getUserLoginOrSystem(DispatchContext dctx, Map<String, ?> context) throws GenericEntityException {
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (userLogin == null) {
            userLogin = dctx.getDelegator().findOne("UserLogin", true, UtilMisc.toMap("userLoginId", "system"));
        }
        return userLogin;
    }

}
