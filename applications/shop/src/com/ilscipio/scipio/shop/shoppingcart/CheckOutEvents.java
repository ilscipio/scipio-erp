package com.ilscipio.scipio.shop.shoppingcart;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * SCIPIO: Shop-specific checkout events.
 */
public abstract class CheckOutEvents extends org.ofbiz.order.shoppingcart.CheckOutEvents {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    protected CheckOutEvents() {
    }
    
    /**
     * NOT IMPLEMENTED - CURRENTLY HANDLED BY CustomerEvents.xml#checkCreateUpdateAnonUser.
     * <p>
     * Creates/updates an anon user for checkoutoptions if a form was submitted containing a request
     * to create one (createNewUser "Y").
     * <p>
     * Currently it will always create a new anon user if the screen requested it.
     * This at least may prevent (or cause!) some issues with session.
     * <p>
     * It will use a minimal set of info (first and last name) to create the Person; 
     * the rest should be handled by other events.
     * <p>
     * It stores the fake userLogin (userLoginId "anonymous", partyId to a new party) in session.
     * <p>
     * Based on CustomerEvents.xml#processCustomerSettings.
     */
    public static String checkCreateUpdateAnonUser(HttpServletRequest request, HttpServletResponse response) {
        /*
        String newUser = request.getParameter("createUpdateAnonUser");
        if ("Y".equals(newUser)) {
            String firstName = request.getParameter("firstName");
            String lastName = request.getParameter("lastName");
        }
        return "success";
        */
        throw new UnsupportedOperationException("Not implemented");
    }
    
    
}
