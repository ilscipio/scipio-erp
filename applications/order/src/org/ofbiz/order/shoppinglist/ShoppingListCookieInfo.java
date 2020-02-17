package org.ofbiz.order.shoppinglist;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.webapp.control.LoginWorker;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Map;

public class ShoppingListCookieInfo { // SCIPIO
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String AUTO_SAVE_LIST = "autoSave"; // "guest"
    public static final String ANON_WISH_LIST = "anonWish";
    public static final int COOKIE_MAX_AGE_DEFAULT = 60 * 60 * 24 * 30; // 1 month

    private final String shoppingListId;
    private final String authToken;
    private final Cookie cookie; // may be null if not exist yet

    private ShoppingListCookieInfo(String shoppingListId, String authToken, Cookie cookie) {
        this.shoppingListId = shoppingListId;
        this.authToken = authToken;
        this.cookie = cookie;
    }

    public static ShoppingListCookieInfo fromFields(String shoppingListId, String authToken, Cookie cookie) {
        if (UtilValidate.isEmpty(shoppingListId)) {
            return null;
        }
        return new ShoppingListCookieInfo(shoppingListId, UtilValidate.isNotEmpty(authToken) ? authToken : null, cookie);
    }

    public static ShoppingListCookieInfo fromFields(String shoppingListId, String authToken) {
        return fromFields(shoppingListId, authToken, null);
    }

    public static ShoppingListCookieInfo fromFields(Map<String, ?> fields) {
        if (fields == null) {
            return null;
        }
        return fromFields((String) fields.get("shoppingListId"), (String) fields.get("shoppingListAuthToken"), null);
    }

    private static ShoppingListCookieInfo fromStringRepr(String value, Cookie cookie) {
        if (value == null) {
            return null;
        }
        String[] parts = value.split("::", 2);
        if (parts.length >= 2 && UtilValidate.isNotEmpty(parts[0])) {
            return new ShoppingListCookieInfo(parts[0], UtilValidate.isNotEmpty(parts[1]) ? parts[1] : null, cookie);
        }
        return null;
    }

    public static ShoppingListCookieInfo fromStringRepr(String value) {
        return fromStringRepr(value, null);
    }

    public static ShoppingListCookieInfo fromCookie(Cookie cookie) {
        return fromStringRepr(cookie != null ? cookie.getValue() : null, cookie);
    }

    public static ShoppingListCookieInfo fromCookie(HttpServletRequest request, String cookieName) {
        Cookie[] cookies = request.getCookies();
        if (cookies != null) {
            for (Cookie cookie: cookies) {
                if (cookie.getName().equals(cookieName)) {
                    return ShoppingListCookieInfo.fromCookie(cookie);
                }
            }
        }
        return null;
    }

    public String getShoppingListId() { return shoppingListId; }
    public String getAuthToken() { return authToken; }
    public Cookie getCookie() { return cookie; }

    // NOTE: DON'T use toString - potential security issue
    public String toValueString() {
        return toValueString(getShoppingListId(), getAuthToken());
    }

    @Override
    public String toString() {
        return toValueString(getShoppingListId(), getAuthToken() != null ? "[auth-token]" : null);
    }

    public Map<String, Object> toFields(Map<String, Object> fields) {
        fields.put("shoppingListId", getShoppingListId());
        fields.put("shoppingListAuthToken", getAuthToken());
        return fields;
    }

    public static String toValueString(String autoSaveListId, String shoppingListAuthToken) { // shoppingListId::shoppingListAuthToken
        if (UtilValidate.isEmpty(autoSaveListId)) {
            return null;
        }
        if (UtilValidate.isEmpty(shoppingListAuthToken)) {
            return autoSaveListId;
        }
        return autoSaveListId + "::" + shoppingListAuthToken;
    }

    public static String getShoppingListCookieName(HttpServletRequest request, String listType) {
        String defaultNamePat = AUTO_SAVE_LIST.equals(listType) ? "GuestShoppingListId_${sysName}" : (ANON_WISH_LIST.equals(listType) ? "AnonShoppingListId_${sysName}" : null);
        String namePat = EntityUtilProperties.getPropertyValue("order", "shoppinglist." + listType + ".cookie.name", defaultNamePat, (Delegator) request.getAttribute("delegator"));
        if (UtilValidate.isEmpty(namePat)) {
            Debug.logError("Cannot get shopping list cookie name for list type (order.properties): " + listType, module);
            return "";
        }
        return LoginWorker.expandCookieName(request, namePat);
    }

    public static int getShoppingListCookieMaxAge(HttpServletRequest request, String listType) {
        return EntityUtilProperties.getPropertyAsInteger("order", "shoppinglist." + listType + ".cookie.maxAge", COOKIE_MAX_AGE_DEFAULT, (Delegator) request.getAttribute("delegator"));
    }

    public static String getAutoSaveShoppingListCookieName(HttpServletRequest request) {
        return getShoppingListCookieName(request, AUTO_SAVE_LIST);
    }

    public static ShoppingListCookieInfo getAutoSaveShoppingListCookieInfo(HttpServletRequest request) {
        return fromCookie(request, getShoppingListCookieName(request, AUTO_SAVE_LIST));
    }

    public static String getAnonShoppingListCookieName(HttpServletRequest request) {
        return getShoppingListCookieName(request, ANON_WISH_LIST);
    }

    public static ShoppingListCookieInfo getAnonShoppingListCookieInfo(HttpServletRequest request) { // SCIPIO
        return fromCookie(request, getShoppingListCookieName(request, ANON_WISH_LIST));
    }

    public static void createShoppingListCookie(HttpServletRequest request, HttpServletResponse response, String listType, String guestShoppingUserName, ShoppingListCookieInfo cookieInfo) { // SCIPIO: refactored from createGuestShoppingListCookies
        createShoppingListCookie(request, response, listType, new Cookie(guestShoppingUserName, cookieInfo.toValueString()));
    }

    public static void createShoppingListCookie(HttpServletRequest request, HttpServletResponse response, String listType, String guestShoppingUserName, String shoppingListId, String authToken) { // SCIPIO: refactored from createGuestShoppingListCookies
        createShoppingListCookie(request, response, listType, new Cookie(guestShoppingUserName, toValueString(shoppingListId, authToken)));
    }

    public static void createShoppingListCookie(HttpServletRequest request, HttpServletResponse response, String listType, Cookie guestShoppingListCookie) { // SCIPIO: refactored from createGuestShoppingListCookies
        guestShoppingListCookie.setMaxAge(getShoppingListCookieMaxAge(request, listType));
        guestShoppingListCookie.setPath("/");
        guestShoppingListCookie.setSecure(true);
        guestShoppingListCookie.setHttpOnly(true);
        response.addCookie(guestShoppingListCookie);
    }

    public static void clearShoppingListCookie(HttpServletRequest request, HttpServletResponse response, String guestShoppingUserName) { // SCIPIO: refactored from clearGuestShoppingListCookies
        Cookie guestShoppingListCookie = new Cookie(guestShoppingUserName, null);
        guestShoppingListCookie.setMaxAge(0);
        guestShoppingListCookie.setPath("/");
        response.addCookie(guestShoppingListCookie);
    }

}
