package com.ilscipio.scipio.party;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

/**
 * SCIPIO: General purpose Party utility functions
 */
public final class PartyUtil {

    private PartyUtil() {
    }
    
    
    /**
     * SCIPIO: Return a person's full name. Defaults to partyId if empty.
     * 
     * @param delegator
     * @param partyId
     * @return
     * @throws GenericEntityException
     */
    public static String getPersonFullName(Delegator delegator, 
            String partyId) throws GenericEntityException {
        
        GenericValue user = delegator.findOne("Person", true,UtilMisc.toMap("partyId", partyId));
        
        if (UtilValidate.isNotEmpty(user)) {
            return user.getString("firstName")+" "+user.getString("lastName");
        }
        else {
            return partyId;
        }
    }
    
    
}
