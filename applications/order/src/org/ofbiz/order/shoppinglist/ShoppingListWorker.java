package org.ofbiz.order.shoppinglist;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilRandom;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.util.List;
import java.util.Map;

/**
 * SCIPIO: ShoppingList utils.
 */
public abstract class ShoppingListWorker {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected ShoppingListWorker() {}

    /**
     * SCIPIO: Returns the length of the generated ShoppingList.shoppingListAuthToken string.
     */
    public static int getAuthTokenLength(Delegator delegator) {
        return EntityUtilProperties.getPropertyAsInteger("order", "shoppinglist.guest.authToken.length", 32, delegator);
    }

    /**
     * SCIPIO: Generates random token for ShoppingList.shoppingListAuthToken.
     */
    public static String generateShoppingListAuthToken(Delegator delegator) {
        return UtilRandom.generateAlphaNumericString(getAuthTokenLength(delegator));
    }

    /**
     * Checks shopping list security, with anon shoppingListAuthToken support.
     * Based on: component://order/script/org/ofbiz/order/shoppinglist/ShoppingListServices.xml#checkShoppingListSecurity
     */
    public static boolean checkShoppingListSecurity(DispatchContext dctx, GenericValue shoppingList, String permissionAction, GenericValue userLogin, String userAuthToken, boolean useCache) {
        if (shoppingList == null) {
            return false;
        }
        String shoppingListPartyId = shoppingList.getString("partyId");
        String userPartyId = (userLogin != null) ? userLogin.getString("partyId") : null;
        if (shoppingListPartyId != null && shoppingListPartyId.equals(userPartyId)) {
            return true; // users can access/modify own lists
        } else if (dctx.getSecurity().hasEntityPermission("PARTYMGR", "_" + permissionAction, userLogin)) { // admins
            return true; // admins can access/modify anything
        } else if (shoppingListPartyId == null) {
            // NOTE: As precaution (for now), do not allow shoppingListAuthToken modification attempts on lists having a partyId,
            // because we currently have no cases (except admin) where this is a good idea to allow
            String shoppingListAuthToken = shoppingList.getString("shoppingListAuthToken");
            if (shoppingListAuthToken != null && shoppingListAuthToken.equals(userAuthToken)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks shopping list security, with anon shoppingListAuthToken support.
     * Based on: component://order/script/org/ofbiz/order/shoppinglist/ShoppingListServices.xml#checkShoppingListSecurity
     */
    public static boolean checkShoppingListSecurity(DispatchContext dctx, String shoppingListId, String permissionAction, GenericValue userLogin, String shoppingListAuthToken, boolean useCache) {
        GenericValue shoppingList = dctx.getDelegator().from("ShoppingList").where("shoppingListId", shoppingListId).cache(useCache).queryOneSafe();
        return checkShoppingListSecurity(dctx, shoppingList, permissionAction, userLogin, shoppingListAuthToken, useCache);
    }

    /**
     * Checks shopping list security, automatically taking into account the authToken stored in cookies if the user is anonymous.
     */
    public static boolean checkShoppingListSecurityAutoAuth(HttpServletRequest request, GenericValue shoppingList, String permissionAction, GenericValue userLogin, boolean useCache) {
        return checkShoppingListSecurity(((LocalDispatcher) request.getAttribute("dispatcher")).getDispatchContext(), shoppingList, permissionAction, userLogin,
                getShoppingListAuthTokenFromSession(request, (shoppingList != null) ? shoppingList.getString("shoppingListId") : null), useCache);
    }

    /**
     * Checks shopping list security, automatically taking into account the authToken stored in cookies if the user is anonymous.
     */
    public static boolean checkShoppingListSecurityAutoAuth(HttpServletRequest request, String shoppingListId, String permissionAction, GenericValue userLogin, boolean useCache) {
        return checkShoppingListSecurity(((LocalDispatcher) request.getAttribute("dispatcher")).getDispatchContext(), shoppingListId, permissionAction, userLogin,
                getShoppingListAuthTokenFromSession(request, shoppingListId), useCache);
    }

    /**
     * Checks shopping list security, automatically taking into account the authToken stored in cookies if the user is anonymous.
     */
    public static boolean checkShoppingListSecurityAutoAuth(HttpServletRequest request, GenericValue shoppingList, String permissionAction, boolean useCache) {
        return checkShoppingListSecurityAutoAuth(request, shoppingList, permissionAction, UtilHttp.getSessionAttribute(request, "userLogin"), useCache);
    }

    /**
     * Checks shopping list security, automatically taking into account the authToken stored in cookies if the user is anonymous.
     */
    public static boolean checkShoppingListSecurityAutoAuth(HttpServletRequest request, String shoppingListId, String permissionAction, boolean useCache) {
        return checkShoppingListSecurityAutoAuth(request, shoppingListId, permissionAction, UtilHttp.getSessionAttribute(request, "userLogin"), useCache);
    }

    public static GenericValue getUserDefaultWishList(HttpServletRequest request, boolean useCache) {
        // NO: This was a bad idea; default != current
        //String shoppingListId = getUserCurrentShoppingListId(request, useCache);
        //if (shoppingListId != null) {
        //    return ((Delegator) request.getAttribute("delegator")).from("ShoppingList").where("shoppingListId", shoppingListId).cache(useCache).queryOneSafe();
        //}
        GenericValue userLogin = UtilHttp.getSessionAttribute(request, "userLogin");
        if (userLogin != null && userLogin.getString("partyId") != null) {
            return getRegisteredUserDefaultWishList(userLogin, useCache);
        }
        if (useAnonShoppingList(request) && isAnonUser(userLogin)) {
            return getAnonUserDefaultWishList(request, userLogin, useCache);
        }
        return null;
    }

    public static String getUserDefaultWishListId(HttpServletRequest request, boolean useCache) {
        // NO: This was a bad idea; default != current
        //String shoppingListId = getUserCurrentShoppingListId(request, useCache);
        //if (shoppingListId != null) {
        //    return shoppingListId;
        //}
        GenericValue userLogin = UtilHttp.getSessionAttribute(request, "userLogin");
        if (userLogin != null && userLogin.getString("partyId") != null) {
            GenericValue shoppingList = getRegisteredUserDefaultWishList(userLogin, useCache);
            return (shoppingList != null) ? shoppingList.getString("shoppingListId") : null;
        }
        if (useAnonShoppingList(request) && isAnonUser(userLogin)) {
            GenericValue shoppingList = getAnonUserDefaultWishList(request, userLogin, useCache);
            return (shoppingList != null) ? shoppingList.getString("shoppingListId") : null;
        }
        return null;
    }

    /**
     * Gets the currentShoppingListId IF valid.
     * To get the ID alone, use {@link #getRawUserCurrentShoppingListId}.
     * DEV NOTE: Currently this might not be used, I left it here since it was already written and should work.
     */
    public static String getUserCurrentShoppingListId(HttpServletRequest request, boolean useCache) {
        String shoppingListId = getRawUserCurrentShoppingListId(request, useCache);
        if (shoppingListId != null && checkShoppingListSecurityAutoAuth(request, shoppingListId, "VIEW", useCache)) {
            return shoppingListId;
        }
        return null;
    }

    /**
     * Gets the currentShoppingListId as-is, no validation.
     * DEV NOTE: Currently this might not be used, I left it here since it was already written and should work.
     */
    public static String getRawUserCurrentShoppingListId(HttpServletRequest request, boolean useCache) {
        HttpSession session = request.getSession(false);
        if (session != null) {
            String currentShoppingListId = (String) session.getAttribute("currentShoppingListId");
            if (UtilValidate.isNotEmpty(currentShoppingListId)) {
                return currentShoppingListId;
            }
        }
        return null;
    }

    /**
     * Returns the default wishlist for the given user (partyId, required). Prioritizes isUserDefault, must not be a child list, must be active.
     */
    public static GenericValue getRegisteredUserDefaultWishList(GenericValue userLogin, boolean useCache) {
        return getRegisteredUserDefaultShoppingList(userLogin, "SLT_WISH_LIST", useCache);
    }

    /**
     * Returns the default list for the given user (partyId, required) and shoppingListTypeId. Prioritizes isUserDefault, must not be a child list, must be active.
     */
    public static GenericValue getRegisteredUserDefaultShoppingList(GenericValue userLogin, String shoppingListTypeId, boolean useCache) {
        if (userLogin == null || userLogin.getString("partyId") == null) {
            return null;
        }
        String partyId = userLogin.getString("partyId");
        if (partyId == null) {
            return null;
        }
        List<GenericValue> shoppingLists = userLogin.getDelegator().from("ShoppingList")
                .where(UtilMisc.toList(EntityCondition.makeCondition("partyId", EntityOperator.EQUALS, partyId),
                        EntityCondition.makeCondition("parentShoppingListId", EntityOperator.EQUALS, null),
                        EntityCondition.makeCondition("shoppingListTypeId", EntityOperator.EQUALS, shoppingListTypeId),
                        EntityCondition.makeCondition("isActive", EntityOperator.EQUALS, "Y"))).cache(useCache).queryListSafe();
        if (UtilValidate.isEmpty(shoppingLists)) {
            return null;
        }
        if (shoppingLists.size() == 1) {
            return shoppingLists.get(0);
        }
        // Try to find one with isUserDefault Y
        for(GenericValue shoppingList : shoppingLists) {
            if (Boolean.TRUE.equals(shoppingList.getBoolean("isUserDefault"))) {
                return shoppingList;
            }
        }
        // Get the first one, when ordered by listName, as this is what EditShoppingList.groovy does to show the initial list
        shoppingLists = EntityUtil.orderBy(shoppingLists, UtilMisc.toList("listName"));
        // Return the first active list (ordered by name... not ideal)
        GenericValue shoppingList = shoppingLists.get(0);
        // TODO: REVIEW: I'm not sure this info is productive, but do it for now to help spot problems (warning?)
        Debug.logInfo("getRegisteredUserDefaultWishList: Party '" + partyId
                + "' has multiple candidate default shopping lists; returning first (" + shoppingList.get("shoppingListId") + ")", module);
        return shoppingList;
    }

    /**
     * Gets the anon user default wish list, from session/cookies, if one created.
     */
    public static GenericValue getAnonUserDefaultWishList(HttpServletRequest request, GenericValue userLogin, boolean useCache) {
        ShoppingListCookieInfo anonListCookieInfo = ShoppingListCookieInfo.fromCookie(request, ShoppingListCookieInfo.getAnonShoppingListCookieName(request));
        if (anonListCookieInfo != null && anonListCookieInfo.getShoppingListId() != null) {
            return getValidUserShoppingList(request, anonListCookieInfo, userLogin, useCache);
        }
        return null;
    }

    /**
     * Gets ShoppingList for the given shoppingListId if passes security.
     */
    public static GenericValue getValidUserShoppingList(HttpServletRequest request, String shoppingListId, GenericValue userLogin, String authToken, boolean useCache) { // SCIPIO: refactored from createGuestShoppingListCookies
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        GenericValue shoppingList = delegator.from("ShoppingList").where("shoppingListId", shoppingListId).cache(useCache).queryOneSafe();
        if (shoppingList != null) {
            if (ShoppingListWorker.checkShoppingListSecurity(dispatcher.getDispatchContext(), shoppingList, "UPDATE", userLogin, authToken, useCache)) {
                return shoppingList;
            } else {
                Debug.logWarning("getShoppingListIfValid: Could not authenticate user '"
                        + (userLogin != null ? userLogin.getString("partyId") : "(anonymous)")
                        + "' to use ShoppingList '" + shoppingListId + "'", module);
            }
        }
        return null;
    }

    /**
     * Gets ShoppingList for the given shoppingListId if passes security.
     */
    public static GenericValue getValidUserShoppingList(HttpServletRequest request, String shoppingListId, String authToken, boolean useCache) { // SCIPIO: refactored from createGuestShoppingListCookies
        return getValidUserShoppingList(request, shoppingListId, UtilHttp.getSessionAttribute(request, "userLogin"), authToken, useCache);
    }

    /**
     * Gets ShoppingList for the given shoppingListId if passes security.
     */
    public static GenericValue getValidUserShoppingList(HttpServletRequest request, ShoppingListCookieInfo shoppingListCookieInfo, GenericValue userLogin, boolean useCache) { // SCIPIO: refactored from createGuestShoppingListCookies
        return getValidUserShoppingList(request, shoppingListCookieInfo.getShoppingListId(), userLogin, shoppingListCookieInfo.getAuthToken(), useCache);
    }

    /**
     * Gets ShoppingList for the given shoppingListId if passes security.
     */
    public static GenericValue getValidUserShoppingList(HttpServletRequest request, ShoppingListCookieInfo shoppingListCookieInfo, boolean useCache) { // SCIPIO: refactored from createGuestShoppingListCookies
        return getValidUserShoppingList(request, shoppingListCookieInfo.getShoppingListId(), UtilHttp.getSessionAttribute(request, "userLogin"), shoppingListCookieInfo.getAuthToken(), useCache);
    }

    /**
     * Gets the anon authToken from request attribute, parameter, or from session/cookies if the shoppingListId matches any of the cookies.
     */
    public static String getShoppingListAuthTokenForRequest(HttpServletRequest request, String shoppingListId) { // SCIPIO
        if (UtilValidate.isEmpty(shoppingListId)) {
            return null;
        }
        String authToken = (String) request.getAttribute("shoppingListAuthToken");
        if (authToken == null) {
            authToken = request.getParameter("shoppingListAuthToken");
        }
        if (UtilValidate.isNotEmpty(authToken)) {
            return authToken;
        }
        return getShoppingListAuthTokenFromSession(request, shoppingListId);
    }

    /**
     * Gets the anon authToken from session/cookies if the shoppingListId matches any of the cookies.
     */
    public static String getShoppingListAuthTokenFromSession(HttpServletRequest request, String shoppingListId) { // SCIPIO
        if (UtilValidate.isEmpty(shoppingListId)) {
            return null;
        }
        ShoppingListCookieInfo cookieInfo = ShoppingListCookieInfo.getAutoSaveShoppingListCookieInfo(request);
        if (cookieInfo != null && shoppingListId.equals(cookieInfo.getShoppingListId()) && UtilValidate.isNotEmpty(cookieInfo.getAuthToken())) {
            return cookieInfo.getAuthToken();
        }
        cookieInfo = ShoppingListCookieInfo.getAnonShoppingListCookieInfo(request);
        if (cookieInfo != null && shoppingListId.equals(cookieInfo.getShoppingListId()) && UtilValidate.isNotEmpty(cookieInfo.getAuthToken())) {
            return cookieInfo.getAuthToken();
        }
        return null;
    }

    /**
     * Checks if the passed shoppingListId matches a guest list cookie and requires shoppingListAuthToken and if so, adds it to the service context.
     * @return the auth token, if applicable
     */
    public static String checkSetShoppingListAuthTokenForService(HttpServletRequest request, Map<String, Object> servCtx) { // SCIPIO
        if (!servCtx.containsKey("shoppingListAuthToken")) {
            String authToken = getShoppingListAuthTokenForRequest(request, (String) servCtx.get("shoppingListId"));
            if (authToken != null) {
                servCtx.put("shoppingListAuthToken", authToken);
                return authToken;
            }
        }
        return null;
    }

    public static boolean isAnonUser(HttpServletRequest request) {
        return isAnonUser((GenericValue) UtilHttp.getSessionAttribute(request, "userLogin"));
    }

    public static boolean isAnonUser(GenericValue userLogin) {
        return (userLogin == null || "anonymous".equals(userLogin.getString("userLoginId")));
    }

    public static boolean useAnonShoppingList(HttpServletRequest request) {
        return useAnonShoppingList(ProductStoreWorker.getProductStore(request));
    }

    public static boolean useAnonShoppingList(Delegator delegator, String productStoreId) {
        return useAnonShoppingList(ProductStoreWorker.getProductStore(productStoreId, delegator));
    }

    public static boolean useAnonShoppingList(GenericValue productStore) {
        return (productStore != null && Boolean.TRUE.equals(productStore.getBoolean("useAnonShoppingList")));
    }
}
