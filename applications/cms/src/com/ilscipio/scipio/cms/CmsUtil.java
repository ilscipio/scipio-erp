package com.ilscipio.scipio.cms;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;

import com.ilscipio.scipio.party.PartyUtil;

/**
 * Scipio CmsUtil - various cms helper methods
 *
 */
public final class CmsUtil {

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
    
    
}
