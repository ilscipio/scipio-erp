package org.ofbiz.order.shoppinglist;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilRandom;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.service.DispatchContext;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

/**
 * SCIPIO: ShoppingList utils.
 */
public abstract class ShoppingListWorker {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * SCIPIO: The length of the generated ShoppingList.shoppingListAuthToken string.
     */
    public static final int AUTH_TOKEN_LENGTH = 32;

    protected ShoppingListWorker() {}

    /**
     * SCIPIO: Generates random token for ShoppingList.shoppingListAuthToken.
     */
    public static String generateShoppingListAuthToken(Delegator delegator) {
        return UtilRandom.generateAlphaNumericString(AUTH_TOKEN_LENGTH);
    }

    /**
     * SCIPIO: Based on: component://order/script/org/ofbiz/order/shoppinglist/ShoppingListServices.xml#checkShoppingListSecurity
     */
    public static boolean checkShoppingListSecurity(DispatchContext dctx, GenericValue userLogin, String permissionAction, GenericValue shoppingList, String userAuthToken) {
        if (shoppingList == null) {
            return false;
        }
        String shoppingListPartyId = shoppingList.getString("partyId");
        String userPartyId = (userLogin != null) ? userLogin.getString("partyId") : null;

        if (shoppingListPartyId != null && shoppingListPartyId.equals(userPartyId)) {
            return true; // users can modify own lists
        } else if (dctx.getSecurity().hasEntityPermission("PARTYMGR", "_" + permissionAction, userLogin)) { // admins
            return true; // admins can modify anything
        } else if (userPartyId == null) { // NOTE: For the time being, only allow shoppingListAuthToken is partyId is not set, so there is no way for this to escalate, but could change this later...
            String shoppingListAuthToken = shoppingList.getString("shoppingListAuthToken");
            if (shoppingListAuthToken != null && shoppingListAuthToken.equals(userAuthToken)) {
                return true;
            }
        }
        return false;
    }

    /**
     * SCIPIO: Based on: component://order/script/org/ofbiz/order/shoppinglist/ShoppingListServices.xml#checkShoppingListSecurity
     */
    public static boolean checkShoppingListSecurity(DispatchContext dctx, GenericValue userLogin, String permissionAction, String shoppingListId, String shoppingListAuthToken) {
        GenericValue shoppingList = null;
        try {
            shoppingList = dctx.getDelegator().from("ShoppingList").where("shoppingListId", shoppingListId).queryOne();
        } catch (GenericEntityException e) {
            Debug.logError("Could not get ShoppingList '" + shoppingListId + "'", module);
        }
        return checkShoppingListSecurity(dctx, userLogin, permissionAction, shoppingList, shoppingListAuthToken);
    }

    public static GenericValue getUserDefaultShoppingList(HttpServletRequest request, boolean useCache) {
        HttpSession session = request.getSession(false);
        if (session != null) {
            String currentShoppingListId = (String) session.getAttribute("currentShoppingListId");
            if (UtilValidate.isNotEmpty(currentShoppingListId)) {
                Delegator delegator = (Delegator) request.getAttribute("delegator");
                try {
                    return delegator.from("ShoppingList").where("shoppingListId", currentShoppingListId).cache(useCache).queryOne();
                } catch (GenericEntityException e) {
                    Debug.logError(e, module);
                    return null;
                }
            }
        }
        GenericValue userLogin = (session != null) ? (GenericValue) session.getAttribute("userLogin") : null;
        if (userLogin == null || userLogin.getString("partyId") == null) {
            return null;
        }
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        try {
            return delegator.from("ShoppingList")
                    .where(UtilMisc.toList(EntityCondition.makeCondition("partyId", EntityOperator.EQUALS, userLogin.getString("partyId")),
                            EntityCondition.makeCondition("listName", EntityOperator.NOT_EQUAL, "auto-save"))).orderBy("listName").cache(useCache).queryFirst();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return null;
        }
    }

    public static String getUserDefaultShoppingListId(HttpServletRequest request, boolean useCache) {
        HttpSession session = request.getSession(false);
        if (session != null) {
            String currentShoppingListId = (String) session.getAttribute("currentShoppingListId");
            if (UtilValidate.isNotEmpty(currentShoppingListId)) {
                return currentShoppingListId;
            }
        }
        GenericValue userLogin = (session != null) ? (GenericValue) session.getAttribute("userLogin") : null;
        if (userLogin == null || userLogin.getString("partyId") == null) {
            return null;
        }
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        try {
            GenericValue shoppingList = delegator.from("ShoppingList")
                    .where(UtilMisc.toList(EntityCondition.makeCondition("partyId", EntityOperator.EQUALS, userLogin.getString("partyId")),
                            EntityCondition.makeCondition("listName", EntityOperator.NOT_EQUAL, "auto-save"))).orderBy("listName").cache(useCache).queryFirst();
            return  (shoppingList != null) ? shoppingList.getString("shoppingListId") : null;
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return null;
        }
    }
}
