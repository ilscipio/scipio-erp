package com.ilscipio.scipio.ce.service.common;


import org.ofbiz.base.util.*;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Service Engine Services
 */
public class ServiceEngineServices {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String err_resource = "WebtoolsErrorUiLabels";

    /**
     * SCIPIO: clearCacheFull - clears all system cache (equivalent to UtilCacheEvents.clearAllEvent)
     * Added 2020-03-10.
     */
    public static Map<String, Object> clearCacheFull(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        try {
            String errMsg = "";
            Security security = dctx.getSecurity();

            if (!security.hasPermission("UTIL_CACHE_EDIT", userLogin)) { // SCIPIO: Now using request; was: request.getSession()
                errMsg = UtilProperties.getMessage(err_resource, "utilCacheEvents.permissionEdit", locale) + ".";
                return ServiceUtil.returnError("Error - cache could not be cleared: " + errMsg);

            }

            UtilCache.clearAllCaches();
            return ServiceUtil.returnSuccess();
        } catch(Exception e) {
            return ServiceUtil.returnError("Error - cache could not be cleared: " + e.getMessage());
        }
    }

}
