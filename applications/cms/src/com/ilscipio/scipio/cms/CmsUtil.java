package com.ilscipio.scipio.cms;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;

import com.ilscipio.scipio.party.PartyUtil;

/**
 * Scipio CmsUtil - various cms helper methods
 *
 */
public abstract class CmsUtil {

    public static final String module = CmsUtil.class.getName();
    
    private static final boolean DEBUG = UtilProperties.getPropertyAsBoolean("cms.properties", "debug", false);
    
    static {
        if (DEBUG) {
            Debug.logInfo("Cms: Debug mode enabled", module);
        }
    }
    
    private CmsUtil() {
        
    }
    
    /**
     * Returns a person's full name in a manner appropriate for display within CMS.
     * 
     * @param delegator
     * @param partyId
     * @return
     * @throws GenericEntityException
     */
    public static String getPersonDisplayName(Delegator delegator, 
            String partyId) throws GenericEntityException {
        // Currently delegates to a standard method.
        return PartyUtil.getPersonFullName(delegator, partyId);
    }
    
    /**
     * Substitute (factored out control) for Debug.verboseOn(). Callers should check this first
     * and then log with Debug.logInfo, NOT Debug.logVerbose, for now (to be reviewed)...
     * NOTE: This method is more log-message oriented compared to {@link #debugOn()}.
     */
    public static boolean verboseOn() {
        //return Debug.verboseOn() || DEBUG;
        return DEBUG;
    }
    
    /**
     * Substitute (factored out control) for Debug.verboseOn(). Callers should check this first
     * and then log with Debug.logInfo, NOT Debug.logVerbose, for now (to be reviewed)...
     * NOTE: This method is more functionality-oriented compared to {@link #verboseOn()}.
     */
    public static boolean debugOn() {
        return DEBUG;
    }

}
