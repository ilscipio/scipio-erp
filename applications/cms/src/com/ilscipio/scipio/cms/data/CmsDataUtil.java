package com.ilscipio.scipio.cms.data;

import java.util.Collections;
import java.util.List;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;

public abstract class CmsDataUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    private CmsDataUtil() {}
    
    /**
     * SCIPIO: findUserLoginSecurityGroupByUserLoginId.
     * <p>
     * From org.ofbiz.security.SecurityFactory.OFBizSecurity.findUserLoginSecurityGroupByUserLoginId(String)
     * which is now deprecated in stock.
     */
    public static List<GenericValue> findUserLoginSecurityGroupByUserLoginId(Delegator delegator, String userLoginId) {
        try {
            return EntityUtil.filterByDate(EntityQuery.use(delegator).from("UserLoginSecurityGroup").where("userLoginId", userLoginId).cache(true).queryList());
        } catch (GenericEntityException e) {
            Debug.logWarning(e, module);
            return Collections.<GenericValue>emptyList();
        }
    }
    
    public static void getContentTypes(Delegator delegator, String parentTypeId, boolean deep, List<GenericValue> outTypes) throws GenericEntityException {
        List<GenericValue> types = delegator.findByAnd("ContentType", 
                UtilMisc.toMap("parentTypeId", parentTypeId), UtilMisc.toList("sequenceId"), true);
        for(GenericValue type : types) {
            outTypes.add(type);
            if (deep) {
                getContentTypes(delegator, type.getString("contentTypeId"), deep, outTypes);
            }
        }
    }
}
