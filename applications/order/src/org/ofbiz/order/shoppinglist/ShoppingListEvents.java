/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.order.shoppinglist;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.RecordNotFoundException;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.order.shoppingcart.CartItemModifyException;
import org.ofbiz.order.shoppingcart.CartUpdate;
import org.ofbiz.order.shoppingcart.ItemNotFoundException;
import org.ofbiz.order.shoppingcart.ShoppingCart;
import org.ofbiz.order.shoppingcart.ShoppingCartEvents;
import org.ofbiz.order.shoppingcart.ShoppingCartItem;
import org.ofbiz.product.catalog.CatalogWorker;
import org.ofbiz.product.config.ProductConfigWorker;
import org.ofbiz.product.config.ProductConfigWrapper;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webapp.event.EventUtil;
import org.ofbiz.webapp.website.WebSiteWorker;

/**
 * Shopping cart events.
 */
public class ShoppingListEvents {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String resource = "OrderUiLabels";
    public static final String resource_error = "OrderErrorUiLabels";
    public static final String PERSISTANT_LIST_NAME = "auto-save";
    public static final String DEFAULT_ANON_LIST_NAME = "default-wishlist"; // SCIPIO

    public static String addBulkFromCart(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");

        String shoppingListId = request.getParameter("shoppingListId");
        String shoppingListTypeId = request.getParameter("shoppingListTypeId");
        String selectedCartItems[] = request.getParameterValues("selectedItem");
        String shoppingListAuthToken = ShoppingListWorker.getShoppingListAuthTokenForRequest(request, shoppingListId); // SCIPIO

        try (CartUpdate cartUpdate = CartUpdate.updateSection(request)) { // SCIPIO
        ShoppingCart cart = cartUpdate.getCartForUpdate();

        if (UtilValidate.isEmpty(selectedCartItems)) {
            selectedCartItems = makeCartItemsArray(cart);
        }

        try {
            shoppingListId = addBulkFromCart(delegator, dispatcher, cart, userLogin, shoppingListId, shoppingListTypeId, selectedCartItems, true, true, shoppingListAuthToken);
        } catch (IllegalArgumentException e) {
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        cartUpdate.commit(cart); // SCIPIO
        }
        request.setAttribute("shoppingListId", shoppingListId);
        return "success";
    }

    // SCIPIO: Added shoppingListAuthToken, needed for anon operations
    public static String addBulkFromCart(Delegator delegator, LocalDispatcher dispatcher, ShoppingCart cart, GenericValue userLogin, String shoppingListId, String shoppingListTypeId, String[] items, boolean allowPromo, boolean append, String shoppingListAuthToken) throws IllegalArgumentException {
        String errMsg = null;
        
        // SCIPIO (2019-03-07): Ensuring shoppingListId really exists, otherwise force creation. 
        // This fixes the bug where shoppingListId passed wasn't null but didn't exist in DB. Later on, failed to create ShoppingListItems.
        GenericValue shoppingList = null;
        try {
            shoppingList = delegator.findOne("ShoppingList", false, UtilMisc.toMap("shoppingListId", shoppingListId));
        } catch (GenericEntityException e) {
            Debug.logError(e, "Problems creating getting ShoppingList [" + shoppingListId + "]", module);
            errMsg = UtilProperties.getMessage(resource_error,"shoppinglistevents.cannot_create_retrieve_shopping_list", cart.getLocale());
            throw new IllegalArgumentException(errMsg);
        }

        if (items == null || items.length == 0) {
            errMsg = UtilProperties.getMessage(resource_error, "shoppinglistevents.select_items_to_add_to_list", cart.getLocale());
            throw new IllegalArgumentException(errMsg);
        }

        // SCIPIO: fixed
        //if (UtilValidate.isEmpty(shoppingList)) {
        if (shoppingList == null) {
            // create a new shopping list
            Map<String, Object> newListResult = null;
            try {
                newListResult = dispatcher.runSync("createShoppingList", UtilMisc.<String, Object>toMap("userLogin", userLogin, "productStoreId", cart.getProductStoreId(), "partyId", cart.getOrderPartyId(), "shoppingListTypeId", shoppingListTypeId, "currencyUom", cart.getCurrency()));
            } catch (GenericServiceException e) {
                Debug.logError(e, "Problems creating new ShoppingList", module);
                errMsg = UtilProperties.getMessage(resource_error,"shoppinglistevents.cannot_create_new_shopping_list", cart.getLocale());
                throw new IllegalArgumentException(errMsg);
            }

            // check for errors
            if (ServiceUtil.isError(newListResult)) {
                throw new IllegalArgumentException(ServiceUtil.getErrorMessage(newListResult));
            }

            // get the new list id
            if (newListResult != null) {
                shoppingListId = (String) newListResult.get("shoppingListId");
                shoppingListAuthToken = (String) newListResult.get("shoppingListAuthToken"); // SCIPIO
            }

            // if no list was created throw an error
            if (UtilValidate.isEmpty(shoppingListId)) {
                errMsg = UtilProperties.getMessage(resource_error,"shoppinglistevents.shoppingListId_is_required_parameter", cart.getLocale());
                throw new IllegalArgumentException(errMsg);
            }
        } else if (!append) {
            // SCIPIO: Verify before making any modifications
            if (!ShoppingListWorker.checkShoppingListSecurity(dispatcher.getDispatchContext(), shoppingList, "UPDATE", userLogin, shoppingListAuthToken, false)) {
                throw new IllegalArgumentException(UtilProperties.getMessage("CommonErrorUiLabels", "CommonPermissionErrorTryAccountSupport", cart.getLocale()));
            }
            try {
                clearListInfo(delegator, shoppingListId);
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                throw new IllegalArgumentException("Could not clear current shopping list: " + e.toString());
            }
        }

        for (String item2 : items) {
            Integer cartIdInt = null;
            try {
                cartIdInt = Integer.valueOf(item2);
            } catch (Exception e) {
                Debug.logWarning(e, UtilProperties.getMessage(resource_error,"OrderIllegalCharacterInSelectedItemField", Debug.getLogLocale()), module); // SCIPIO: log locale
            }
            if (cartIdInt != null) {
                ShoppingCartItem item = cart.findCartItem(cartIdInt);
                if (allowPromo || !item.getIsPromo()) {
                    Debug.logInfo("Adding cart item to shopping list [" + shoppingListId + "], allowPromo=" + allowPromo + ", item.getIsPromo()=" + item.getIsPromo() + ", item.getProductId()=" + item.getProductId() + ", item.getQuantity()=" + item.getQuantity(), module);
                    Map<String, Object> serviceResult = null;
                    try {
                        Map<String, Object> ctx = UtilMisc.<String, Object>toMap("userLogin", userLogin, "shoppingListId", shoppingListId, "productId", item.getProductId(), "quantity", item.getQuantity());
                        ctx.put("reservStart", item.getReservStart());
                        ctx.put("reservLength", item.getReservLength());
                        ctx.put("reservPersons", item.getReservPersons());
                        if (item.getConfigWrapper() != null) {
                            ctx.put("configId", item.getConfigWrapper().getConfigId());
                        }
                        ctx.put("shoppingListAuthToken", shoppingListAuthToken); // SCIPIO
                        serviceResult = dispatcher.runSync("createShoppingListItem", ctx);
                    } catch (GenericServiceException e) {
                        Debug.logError(e, "Problems creating ShoppingList item entity", module);
                        errMsg = UtilProperties.getMessage(resource_error,"shoppinglistevents.error_adding_item_to_shopping_list", cart.getLocale());
                        throw new IllegalArgumentException(errMsg);
                    }

                    // check for errors
                    if (ServiceUtil.isError(serviceResult)) {
                        throw new IllegalArgumentException(ServiceUtil.getErrorMessage(serviceResult));
                    }
                }
            }
        }

        // return the shoppinglist id
        return shoppingListId;
    }

    // SCIPIO: Original overload
    public static String addBulkFromCart(Delegator delegator, LocalDispatcher dispatcher, ShoppingCart cart, GenericValue userLogin, String shoppingListId, String shoppingListTypeId, String[] items, boolean allowPromo, boolean append) throws IllegalArgumentException {
        return addBulkFromCart(delegator, dispatcher, cart, userLogin, shoppingListId, shoppingListTypeId, items, allowPromo, append, null);
    }

    public static String addListToCart(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");

        String shoppingListId = request.getParameter("shoppingListId");
        String includeChild = request.getParameter("includeChild");
        String prodCatalogId =  CatalogWorker.getCurrentCatalogId(request);

        // SCIPIO: Security check to make sure we have access to the list
        String shoppingListAuthToken = ShoppingListWorker.getShoppingListAuthTokenForRequest(request, shoppingListId); // SCIPIO
        GenericValue userLogin = (GenericValue) request.getAttribute("userLogin");
        if (!ShoppingListWorker.checkShoppingListSecurity(dispatcher.getDispatchContext(), shoppingListId, "UPDATE", userLogin, shoppingListAuthToken, false)) {
            request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("CommonErrorUiLabels", "CommonPermissionErrorTryAccountSupport", UtilHttp.getLocale(request)));
            return "error";
        }

        try (CartUpdate cartUpdate = CartUpdate.updateSection(request)) { // SCIPIO
        ShoppingCart cart = cartUpdate.getCartForUpdate();

        try {
            addListToCart(delegator, dispatcher, cart, prodCatalogId, shoppingListId, (includeChild != null), true, true);
        } catch (IllegalArgumentException e) {
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        cartUpdate.commit(cart); // SCIPIO
        }
        return "success";
    }

    public static String addListToCart(Delegator delegator, LocalDispatcher dispatcher, ShoppingCart cart, String prodCatalogId, String shoppingListId, boolean includeChild, boolean setAsListItem, boolean append) throws java.lang.IllegalArgumentException {
        String errMsg = null;

        // no list; no add
        if (shoppingListId == null) {
            errMsg = UtilProperties.getMessage(resource_error,"shoppinglistevents.choose_shopping_list", cart.getLocale());
            throw new IllegalArgumentException(errMsg);
        }

        // get the shopping list
        GenericValue shoppingList = null;
        List<GenericValue> shoppingListItems = null;
        String shoppingListAuthToken = null; // SCIPIO: 2.1.0: Now handled
        try {
            shoppingList = EntityQuery.use(delegator).from("ShoppingList").where("shoppingListId", shoppingListId).queryOne();
            if (shoppingList == null) {
                errMsg = UtilProperties.getMessage(resource_error,"shoppinglistevents.error_getting_shopping_list_and_items", cart.getLocale());
                throw new RecordNotFoundException(errMsg); // SCIPIO: switched IllegalArgumentException to RecordNotFoundException
            }
            shoppingListAuthToken = shoppingList.getString("shoppingListAuthToken");

            shoppingListItems = shoppingList.getRelated("ShoppingListItem", null, null, false);
            if (shoppingListItems == null) {
                shoppingListItems = new LinkedList<>();
            }

            // include all items of child lists if flagged to do so
            if (includeChild) {
                List<GenericValue> childShoppingLists = shoppingList.getRelated("ChildShoppingList", null, null, false);
                for (GenericValue v : childShoppingLists) {
                    List<GenericValue> items = v.getRelated("ShoppingListItem", null, null, false);
                    shoppingListItems.addAll(items);
                }
            }

        } catch (GenericEntityException e) {
            Debug.logError(e, "Problems getting ShoppingList and ShoppingListItem records", module);
            errMsg = UtilProperties.getMessage(resource_error,"shoppinglistevents.error_getting_shopping_list_and_items", cart.getLocale());
            throw new IllegalArgumentException(errMsg);
        }

        // no items; not an error; just mention that nothing was added
        if (UtilValidate.isEmpty(shoppingListItems)) {
            errMsg = UtilProperties.getMessage(resource_error,"shoppinglistevents.no_items_added", cart.getLocale());
            return errMsg;
        }

        // check if we are to clear the cart first
        if (!append) {
            cart.clear();

            // Prevent the system from creating a new shopping list every time the cart is restored for anonymous user.
            cart.setAutoSaveListId(shoppingListId);
            cart.setAutoSaveListAuthToken(shoppingListAuthToken); // SCIPIO
        }

        // get the survey info for all the items
        Map<String, List<String>> shoppingListSurveyInfo = getItemSurveyInfos(shoppingListItems);

        // add the items
        StringBuilder eventMessage = new StringBuilder();
        for (GenericValue shoppingListItem : shoppingListItems) {
            String productId = shoppingListItem.getString("productId");
            BigDecimal quantity = shoppingListItem.getBigDecimal("quantity");
            Timestamp reservStart = shoppingListItem.getTimestamp("reservStart");
            BigDecimal reservLength = shoppingListItem.getBigDecimal("reservLength");
            BigDecimal reservPersons = shoppingListItem.getBigDecimal("reservPersons");
            String configId = shoppingListItem.getString("configId");
            try {
                String listId = shoppingListItem.getString("shoppingListId");
                String itemId = shoppingListItem.getString("shoppingListItemSeqId");

                Map<String, Object> attributes = new HashMap<>();
                // list items are noted in the shopping cart
                if (setAsListItem) {
                    attributes.put("shoppingListId", listId);
                    attributes.put("shoppingListItemSeqId", itemId);
                }

                // check if we have existing survey responses to append
                if (shoppingListSurveyInfo.containsKey(listId + "." + itemId) && UtilValidate.isNotEmpty(shoppingListSurveyInfo.get(listId + "." + itemId))) {
                    attributes.put("surveyResponses", shoppingListSurveyInfo.get(listId + "." + itemId));
                }

                ProductConfigWrapper configWrapper = null;
                if (UtilValidate.isNotEmpty(configId)) {
                    configWrapper = ProductConfigWorker.loadProductConfigWrapper(delegator, dispatcher, configId, productId, cart.getProductStoreId(), prodCatalogId, cart.getWebSiteId(), cart.getCurrency(), cart.getLocale(), cart.getAutoUserLogin());
                }
                // TODO: add code to check for survey response requirement

                // i cannot get the addOrDecrease function to accept a null reservStart field: i get a null pointer exception a null constant works....
                if (reservStart == null) {
                       cart.addOrIncreaseItem(productId, null, quantity, null, null, null, null, null, null, attributes, prodCatalogId, configWrapper, null, null, null, dispatcher);
                } else {
                    cart.addOrIncreaseItem(productId, null, quantity, reservStart, reservLength, reservPersons, null, null, null, null, null, attributes, prodCatalogId, configWrapper, null, null, null, dispatcher);
                }
                Map<String, Object> messageMap = UtilMisc.<String, Object>toMap("productId", productId);
                errMsg = UtilProperties.getMessage(resource_error,"shoppinglistevents.added_product_to_cart", messageMap, cart.getLocale());
                eventMessage.append(errMsg).append("\n");
            } catch (CartItemModifyException e) {
                Debug.logWarning(e, UtilProperties.getMessage(resource_error,"OrderProblemsAddingItemFromListToCart", Debug.getLogLocale())); // SCIPIO: log locale
                Map<String, Object> messageMap = UtilMisc.<String, Object>toMap("productId", productId);
                errMsg = UtilProperties.getMessage(resource_error,"shoppinglistevents.problem_adding_product_to_cart", messageMap, cart.getLocale());
                eventMessage.append(errMsg).append("\n");
            } catch (ItemNotFoundException e) {
                Debug.logWarning(e, UtilProperties.getMessage(resource_error,"OrderProductNotFound", Debug.getLogLocale())); // SCIPIO: log locale
                Map<String, Object> messageMap = UtilMisc.<String, Object>toMap("productId", productId);
                errMsg = UtilProperties.getMessage(resource_error,"shoppinglistevents.problem_adding_product_to_cart", messageMap, cart.getLocale());
                eventMessage.append(errMsg).append("\n");
            }
        }

        if (eventMessage.length() > 0) {
            return eventMessage.toString();
        }

        // all done
        return ""; // no message to return; will simply reply as success
    }

    public static String replaceShoppingListItem(HttpServletRequest request, HttpServletResponse response) {
        String quantityStr = request.getParameter("quantity");

        // just call the updateShoppingListItem service
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Locale locale = UtilHttp.getLocale(request);

        BigDecimal quantity = null;
        try {
            quantity = new BigDecimal(quantityStr);
        } catch (Exception e) {
            // do nothing, just won't pass to service if it is null
            //Debug.logError(e, module); // SCIPIO: 2018-10-09: Don't log this as exception
        }

        Map<String, Object> serviceInMap = new HashMap<String, Object>();
        serviceInMap.put("shoppingListId", request.getParameter("shoppingListId"));
        serviceInMap.put("shoppingListItemSeqId", request.getParameter("shoppingListItemSeqId"));
        serviceInMap.put("productId", request.getParameter("add_product_id"));
        serviceInMap.put("userLogin", userLogin);
        ShoppingListWorker.checkSetShoppingListAuthTokenForService(request, serviceInMap); // SCIPIO
        if (quantity != null) serviceInMap.put("quantity", quantity);
        Map<String, Object> result = null;
        try {
            result = dispatcher.runSync("updateShoppingListItem", serviceInMap);
        } catch (GenericServiceException e) {
            String errMsg = UtilProperties.getMessage(resource_error,"shoppingListEvents.error_calling_update", locale) + ": "  + e.toString();
            request.setAttribute("_ERROR_MESSAGE_", errMsg);
            String errorMsg = "Error calling the updateShoppingListItem in handleShoppingListItemVariant: " + e.toString();
            Debug.logError(e, errorMsg, module);
            return "error";
        }

        ServiceUtil.getMessages(request, result, "", "", "", "", "", "", "");
        if ("error".equals(result.get(ModelService.RESPONSE_MESSAGE))) {
            return "error";
        } else {
            return "success";
        }
    }

    /**
     * Finds or creates a specialized (auto-save) shopping list used to record shopping bag contents between user visits.
     * <p>SCIPIO: WARNING: Do not pass partyId from unverified input without checking {@link ShoppingListWorker#checkShoppingListSecurity}.</p>
     * <p>SCIPIO: 2.1.0: Replaced getAutoSaveListId implementation.</p>
     */
    public static Map<String, Object> getAutoSaveListInfo(Delegator delegator, LocalDispatcher dispatcher, String partyId, GenericValue userLogin,
                                                          String productStoreId, boolean createIfMissing, Map<String, ?> createParams) throws GenericEntityException, GenericServiceException {
        if (partyId == null && userLogin != null) {
            partyId = userLogin.getString("partyId");
        }

        Map<String, Object> autoSaveListInfo = null;
        GenericValue list = null;
        // TODO: add sorting, just in case there are multiple...
        if (partyId != null) {
            Map<String, Object> findMap = UtilMisc.toMap("partyId", partyId, "productStoreId", productStoreId, "shoppingListTypeId", "SLT_SPEC_PURP", "listName", PERSISTANT_LIST_NAME);
            List<GenericValue> existingLists = EntityQuery.use(delegator).from("ShoppingList").where(findMap).queryList();
            Debug.logInfo("Finding existing auto-save shopping list with:\nfindMap: " + findMap + "\nlists: " + existingLists, module);

            if (UtilValidate.isNotEmpty(existingLists)) {
                list = EntityUtil.getFirst(existingLists);
                autoSaveListInfo = list;
            }
        }
        if (createIfMissing && list == null && dispatcher != null) {
            Map<String, Object> listFields = UtilMisc.toMap("userLogin", userLogin, "productStoreId", productStoreId, "shoppingListTypeId", "SLT_SPEC_PURP", "listName", PERSISTANT_LIST_NAME);
            if (listFields != null) {
                listFields.putAll(createParams);
            }
            listFields = dispatcher.makeValidContext("createShoppingList", ModelService.IN_PARAM, listFields); // SCIPIO
            Map<String, Object> newListResult = dispatcher.runSync("createShoppingList", listFields);
            if (ServiceUtil.isError(newListResult)) {
                String errorMessage = ServiceUtil.getErrorMessage(newListResult);
                Debug.logError(errorMessage, module);
                return null;
            }
            if (newListResult != null) {
                autoSaveListInfo = newListResult;
            }
        }

        return autoSaveListInfo;
    }

    /**
     * Finds or creates a specialized (auto-save) shopping list used to record shopping bag contents between user visits.
     * <p>SCIPIO: WARNING: Do not pass partyId from unverified input without checking {@link ShoppingListWorker#checkShoppingListSecurity}.</p>
     */
    @Deprecated
    public static String getAutoSaveListId(Delegator delegator, LocalDispatcher dispatcher, String partyId, GenericValue userLogin, String productStoreId) throws GenericEntityException, GenericServiceException {
        Map<String, Object> autoSaveListInfo = getAutoSaveListInfo(delegator, dispatcher, partyId, userLogin, productStoreId, true, null);
        return (autoSaveListInfo != null) ? (String) autoSaveListInfo.get("shoppingListId") : null;
    }

    /**
     * Fills the specialized shopping list with the current shopping cart if one exists (if not leaves it alone)
     */
    public static void fillAutoSaveList(ShoppingCart cart, LocalDispatcher dispatcher) throws GeneralException {
        if (cart != null && dispatcher != null) {
            GenericValue userLogin = ShoppingListEvents.getCartUserLogin(cart);
            Delegator delegator = cart.getDelegator();
            String autoSaveListId = cart.getAutoSaveListId();
            String autoSaveListAuthToken = cart.getAutoSaveListAuthToken();
            if (autoSaveListId == null) {
                Map<String, Object> autoSaveListInfo = getAutoSaveListInfo(delegator, dispatcher, null, userLogin,
                        cart.getProductStoreId(), true, cart.getAutoSaveListParams());
                if (autoSaveListInfo != null) {
                    autoSaveListId = (String) autoSaveListInfo.get("shoppingListId");
                    autoSaveListAuthToken = (String) autoSaveListInfo.get("shoppingListAuthToken");
                    cart.setAutoSaveListId(autoSaveListId);
                    cart.setAutoSaveListAuthToken(autoSaveListAuthToken);
                }
            }
            /* SCIPIO: added this, but is now redundant with getAutoSaveListInfo
            if (autoSaveListId == null) {
                // SCIPIO: 2.1.0: We now create the list here instead of after-login event
                Map<String, Object> newListResult = createGuestShoppingList(delegator, dispatcher, userLogin,
                        cart.getProductStoreId(), cart.getAutoSaveListParams());
                if (!ServiceUtil.isSuccess(newListResult)) {
                    throw new GeneralException(ServiceUtil.getErrorMessage(newListResult));
                }
                autoSaveListId = (String) newListResult.get("shoppingListId");
                // Now done in post-processor event
                //ShoppingListCookieInfo.createShoppingListCookie(request, response,
                //        ShoppingListCookieInfo.AUTO_SAVE_LIST, guestShoppingUserName, autoSaveListId,
                //        shoppingListAuthToken);
                cart.setAutoSaveListId(autoSaveListId);
                cart.setAutoSaveListAuthToken((String) newListResult.get("shoppingListAuthToken"));
            }
             */

            GenericValue shoppingList = EntityQuery.use(delegator).from("ShoppingList").where("shoppingListId", autoSaveListId).queryOne();
            Integer currentListSize = 0;
            if (shoppingList != null) {
                List<GenericValue> shoppingListItems = shoppingList.getRelated("ShoppingListItem", null, null, false);
                if (UtilValidate.isNotEmpty(shoppingListItems)) {
                    currentListSize = shoppingListItems.size();
                }

                // SCIPIO: NOTE: It is usually WRONG to get the shoppingListAuthToken from ShoppingList, but in this case the shoppingListId only comes from internal or pre-verified sources
                // (because callers are expected to validate autoSaveListId before calling cart.setAutoSaveListId(autoSaveListId))
                String shoppingListAuthToken = shoppingList.getString("shoppingListAuthToken");

                try {
                    String[] itemsArray = makeCartItemsArray(cart);
                    if (itemsArray.length != 0) {
                        addBulkFromCart(delegator, dispatcher, cart, userLogin, autoSaveListId, null, itemsArray, false, false, shoppingListAuthToken);
                    } else if (currentListSize != 0) {
                        clearListInfo(delegator, autoSaveListId);
                    }
                } catch (IllegalArgumentException e) {
                    throw new GeneralException(e.getMessage(), e);
                }
            } else {
                String partyId = (userLogin != null) ? userLogin.getString("partyId") : null;
                Debug.logWarning("fillAutoSaveList: could not find auto-save list '" + autoSaveListId + "' (" + (autoSaveListId != null ? "ID from cart" : "ID looked up for party '" + partyId + "'") + ")", module);
            }
        }
    }

    /**
     * Saves the shopping cart to the specialized (auto-save) shopping list
     */
    public static String saveCartToAutoSaveList(HttpServletRequest request, HttpServletResponse response) {
        // SCIPIO: Introduced request attribute/parameter to skip restore if requested (non-sensitive functionality, so parameter permissible)
        String skipSaveCartToAutoSaveList = (String) request.getAttribute("skipSaveCartToAutoSaveList");
        if ("Y".equals(skipSaveCartToAutoSaveList) || (skipSaveCartToAutoSaveList == null && "Y".equals(request.getParameter("skipSaveCartToAutoSaveList")))) {
            return "success";
        }
        if (!ProductStoreWorker.autoSaveCart(ProductStoreWorker.getProductStore(request))) {
            // SCIPIO: 2.1.0: Added missing check
            return "success";
        }

        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        
        try (CartUpdate cartUpdate = CartUpdate.updateSection(request)) { // SCIPIO
        ShoppingCart cart = cartUpdate.getCartForUpdate();

        try {
            fillAutoSaveList(cart, dispatcher);
        } catch (GeneralException e) {
            Debug.logError(e, "Error saving the cart to the auto-save list: " + e.toString(), module);
        }

        cartUpdate.commit(cart); // SCIPIO
        }
        return "success";
    }

    /**
     * Restores the specialized (auto-save) shopping list back into the shopping cart
     */
    public static String restoreAutoSaveList(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        GenericValue productStore = ProductStoreWorker.getProductStore(request);

        if (!ProductStoreWorker.autoSaveCart(productStore)) {
            // if auto-save is disabled just return here
            return "success";
        }

        // SCIPIO: Introduced request attribute/parameter to skip restore if requested (non-sensitive functionality, so parameter permissible)
        String skipRestoreAutoSaveList = (String) request.getAttribute("skipRestoreAutoSaveList");
        if ("Y".equals(skipRestoreAutoSaveList) || (skipRestoreAutoSaveList == null && "Y".equals(request.getParameter("skipRestoreAutoSaveList")))) {
            return "success";
        }

        HttpSession session = request.getSession();
        
        try (CartUpdate cartUpdate = CartUpdate.updateSection(request)) { // SCIPIO
        ShoppingCart cart = cartUpdate.getCartForUpdate();

        // safety check for missing required parameter.
        if (cart.getWebSiteId() == null) {
            cart.setWebSiteId(WebSiteWorker.getWebSiteId(request));
        }

        // locate the user's identity
        GenericValue userLogin = (GenericValue) session.getAttribute("userLogin");
        if (userLogin == null) {
            userLogin = (GenericValue) session.getAttribute("autoUserLogin");
        }

        // find the list ID
        String autoSaveListId = cart.getAutoSaveListId();
        String autoSaveListAuthToken = cart.getAutoSaveListAuthToken();
        if (autoSaveListId == null) {
            Map<String, Object> autoSaveListInfo = null;
            try {
                autoSaveListInfo = getAutoSaveListInfo(delegator, dispatcher, null, userLogin, cart.getProductStoreId(), false, cart.getAutoSaveListParams());
            } catch (GeneralException e) {
                Debug.logError(e, module);
            }
            if (autoSaveListInfo != null) {
                autoSaveListId = (String) autoSaveListInfo.get("shoppingListId");
                autoSaveListAuthToken = (String) autoSaveListInfo.get("shoppingListAuthToken");
                cart.setAutoSaveListId(autoSaveListId);
                cart.setAutoSaveListAuthToken(autoSaveListAuthToken);
            }
        } else if (userLogin != null) {
            Map<String, Object> existingAutoSaveListInfo = null;
            String existingAutoSaveListId = null;
            String existingAutoSaveListAuthToken = null;
            try {
                existingAutoSaveListInfo = getAutoSaveListInfo(delegator, dispatcher, null, userLogin, cart.getProductStoreId(), false, cart.getAutoSaveListParams());
            } catch (GeneralException e) {
                Debug.logError(e, module);
            }
            if (existingAutoSaveListInfo != null) {
                existingAutoSaveListId = (String) existingAutoSaveListInfo.get("shoppingListId");
                existingAutoSaveListAuthToken = (String) existingAutoSaveListInfo.get("shoppingListAuthToken");
            }
            if (existingAutoSaveListId != null) {
                if (!existingAutoSaveListId.equals(autoSaveListId)) {
                    // Replace with existing shopping list
                    cart.setAutoSaveListId(existingAutoSaveListId);
                    cart.setAutoSaveListAuthToken(existingAutoSaveListAuthToken);
                    autoSaveListId = existingAutoSaveListId;
                    autoSaveListAuthToken = existingAutoSaveListAuthToken;
                    cart.setLastListRestore(null);
                } else {
                    // CASE: User first login and logout and then re-login again. This condition does not require a restore at all
                    // because at this point items in the cart and the items in the shopping list are same so just return.
                    return "success";
                }
            }
        }

        // check to see if we are okay to load this list
        java.sql.Timestamp lastLoad = cart.getLastListRestore();
        boolean okayToLoad = autoSaveListId == null ? false : (lastLoad == null ? true : false);
        if (!okayToLoad && lastLoad != null) {
            GenericValue shoppingList = null;
            try {
                shoppingList = EntityQuery.use(delegator).from("ShoppingList").where("shoppingListId", autoSaveListId).queryOne();
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
            }
            if (shoppingList != null) {
                java.sql.Timestamp lastModified = shoppingList.getTimestamp("lastAdminModified");
                if (lastModified != null) {
                    if (lastModified.after(lastLoad)) {
                        okayToLoad = true;
                    }
                    if (cart.size() == 0 && lastModified.after(cart.getCartCreatedTime())) {
                        okayToLoad = true;
                    }
                }
            }
        }

        // load (restore) the list of we have determined it is okay to load
        if (okayToLoad) {
            String prodCatalogId = CatalogWorker.getCurrentCatalogId(request);
            try {
                addListToCart(delegator, dispatcher, cart, prodCatalogId, autoSaveListId, false, false, userLogin != null ? true : false);
                cart.setLastListRestore(UtilDateTime.nowTimestamp());
                cartUpdate.commit(cart); // SCIPIO
            } catch(RecordNotFoundException e) { // SCIPIO: log this as warning because it is a "normal" case when we receive old cookies
                Debug.logWarning("Auto-save shopping list not found for shoppingListId [" + autoSaveListId + "]; abandoning cart changes", module);
            } catch (IllegalArgumentException e) {
                Debug.logError(e, "Could not load auto-save shopping list to cart; abandoning cart changes", module); // SCIPIO: Added mesasge
            }
        } else {
            cartUpdate.commit(cart); // SCIPIO
        }

        }
        return "success";
    }

    /**
     * Remove all items from the given list.
     */
    public static int clearListInfo(Delegator delegator, String shoppingListId) throws GenericEntityException {
        // remove the survey responses first
        delegator.removeByAnd("ShoppingListItemSurvey", UtilMisc.toMap("shoppingListId", shoppingListId));

        // next remove the items
        return delegator.removeByAnd("ShoppingListItem", UtilMisc.toMap("shoppingListId", shoppingListId));
    }

    /**
     * Creates records for survey responses on survey items
     */
    public static int makeListItemSurveyResp(Delegator delegator, GenericValue item, List<String> surveyResps) throws GenericEntityException {
        if (UtilValidate.isNotEmpty(surveyResps)) {
            int count = 0;
            for (String responseId : surveyResps) {
                GenericValue listResp = delegator.makeValue("ShoppingListItemSurvey");
                listResp.set("shoppingListId", item.getString("shoppingListId"));
                listResp.set("shoppingListItemSeqId", item.getString("shoppingListItemSeqId"));
                listResp.set("surveyResponseId", responseId);
                delegator.create(listResp);
                count++;
            }
            return count;
        }
        return -1;
    }

    /**
     * Returns Map keyed on item sequence ID containing a list of survey response IDs
     */
    public static Map<String, List<String>> getItemSurveyInfos(List<GenericValue> items) {
        Map<String, List<String>> surveyInfos = new HashMap<>();
        if (UtilValidate.isNotEmpty(items)) {
            for (GenericValue item : items) {
                String listId = item.getString("shoppingListId");
                String itemId = item.getString("shoppingListItemSeqId");
                surveyInfos.put(listId + "." + itemId, getItemSurveyInfo(item));
            }
        }

        return surveyInfos;
    }

    /**
     * Returns a list of survey response IDs for a shopping list item
     */
    public static List<String> getItemSurveyInfo(GenericValue item) {
        List<String> responseIds = new LinkedList<>();
        List<GenericValue> surveyResp = null;
        try {
            surveyResp = item.getRelated("ShoppingListItemSurvey", null, null, false);
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }

        if (UtilValidate.isNotEmpty(surveyResp)) {
            for (GenericValue resp : surveyResp) {
                responseIds.add(resp.getString("surveyResponseId"));
            }
        }

        return responseIds;
    }

    private static GenericValue getCartUserLogin(ShoppingCart cart) {
        GenericValue ul = cart.getUserLogin();
        if (ul == null) {
            ul = cart.getAutoUserLogin();
        }
        return ul;
    }

    private static String[] makeCartItemsArray(ShoppingCart cart) {
        int len = cart.size();
        String[] arr = new String[len];
        for (int i = 0; i < len; i++) {
            arr[i] = Integer.toString(i);
        }
        return arr;
    }

    /**
     * Checks the auto-save list cookies and applies to ShoppingCart.
     * <p>
     * SCIPIO: NOTE: This does NOT create the new Scipio anon shopping list cookies; those are created on-demand
     * by {@link #addItemToShoppingList(HttpServletRequest, HttpServletResponse)} and similar calls, otherwise
     * too many needless lists will be created.
     */
    public static String applyAutoSaveListCookies(HttpServletRequest request, HttpServletResponse response) {
        if (!ProductStoreWorker.autoSaveCart(request)) {
            return "success";
        }
        String autoSaveListCookieName = ShoppingListCookieInfo.getAutoSaveShoppingListCookieName(request); // SCIPIO
        ShoppingListCookieInfo autoSaveCookieInfo = ShoppingListCookieInfo.fromCookie(request, autoSaveListCookieName);
        if (autoSaveCookieInfo == null) {
            // SCIPIO: Return because the cookie is now set later and we only read it here
            return "success";
        }
        // SCIPIO: For performance, check if the cart already contains the cookie and quit if so
        ShoppingCart cart = ShoppingCartEvents.getCartObjectIfExists(request);
        if (cart != null && autoSaveCookieInfo.valueEquals(cart.getAutoSaveListId(), cart.getAutoSaveListAuthToken())) {
            return "success";
        }

        HttpSession session = request.getSession(false);
        GenericValue userLogin = (session != null) ? (GenericValue) session.getAttribute("userLogin") : null;
        if (userLogin != null) {
            if (UtilValidate.isEmpty(userLogin.getString("partyId"))) {
                return "success";
            }
        }

        // SCIPIO: We must do a security check here, because the autoSaveListId is just read out of the cart by other code
        if (ShoppingListWorker.getValidUserShoppingList(request, autoSaveCookieInfo, userLogin, false) != null) {
            try (CartUpdate cartUpdate = CartUpdate.updateSection(request)) {
                cart = cartUpdate.getCartForUpdate();

                String userAddr = request.getRemoteAddr();
                if (UtilValidate.isNotEmpty(request.getHeader("X-Forwarded-For"))) {
                    userAddr = request.getHeader("X-Forwarded-For");
                }
                cart.addAutoSaveListParams("userAddr", userAddr);
                cart.setAutoSaveList(autoSaveCookieInfo);

                cartUpdate.commit(cart); // SCIPIO
            }
        }
        return "success";
    }

    /**
     * Creates guest shopping list cookies.
     * @deprecated SCIPIO: 2.1.0: Cookies are now created in post-processor event {@link #refreshAutoSaveListCookies}.
     */
    @Deprecated
    public static String createGuestShoppingListCookies(HttpServletRequest request, HttpServletResponse response) {
        return applyAutoSaveListCookies(request, response);
    }

    /**
     * Checks if cart auto-save list should be transferred, meant as post-processor event after cart add and fillAutoSaveList calls.
     * <p>SCIPIO: 2.1.0: Added for post-processor.</p>
     */
    public static String refreshAutoSaveListCookies(HttpServletRequest request, HttpServletResponse response) {
        if (!ProductStoreWorker.autoSaveCart(request)) {
            return "success";
        }
        ShoppingCart cart = ShoppingCartEvents.getCartObjectIfExists(request);
        if (cart == null) {
            return "success";
        }
        String autoSaveListId = cart.getAutoSaveListId();
        String autoSaveListAuthToken = cart.getAutoSaveListAuthToken();
        // TODO: REVIEW: should we clear if empty? For now may have side effects...
        if (UtilValidate.isNotEmpty(autoSaveListId)) {
            ShoppingListCookieInfo.refreshShoppingListCookie(request, response, ShoppingListCookieInfo.AUTO_SAVE_LIST,
                    ShoppingListCookieInfo.getAutoSaveShoppingListCookieName(request), autoSaveListId, autoSaveListAuthToken);
        }
        return "success";
    }

    /**
     * Clear the guest cookies for a shopping list
     */
    public static String clearAutoSaveListCookies(HttpServletRequest request, HttpServletResponse response) {
        ShoppingListCookieInfo.clearShoppingListCookie(request, response, ShoppingListCookieInfo.getAutoSaveShoppingListCookieName(request));
        if (ShoppingListWorker.useAnonShoppingList(request)) { // SCIPIO
            ShoppingListCookieInfo.clearShoppingListCookie(request, response, ShoppingListCookieInfo.getAnonShoppingListCookieName(request));
        }
        // SCIPIO: After login (where this event should be hooked), make sure to clear the session list to get rid of the anon list
        clearCurrentShoppingList(request, response);
        return "success";
    }

    /**
     * Clear the guest cookies for a shopping list
     */
    public static String clearGuestShoppingListCookies(HttpServletRequest request, HttpServletResponse response) {
        return clearAutoSaveListCookies(request, response);
    }

    protected static Map<String, Object> createGuestShoppingList(Delegator delegator, LocalDispatcher dispatcher,
                                                                 GenericValue userLogin, String productStoreId,
                                                                 Map<String, ?> createParams) throws GeneralException { // SCIPIO
        Map<String, Object> listFields = UtilMisc.toMap("userLogin", userLogin, "productStoreId", productStoreId,
                "shoppingListTypeId", "SLT_SPEC_PURP", "listName", PERSISTANT_LIST_NAME); // SCIPIO: userAddr
        if (createParams != null) {
            listFields.putAll(createParams);
        }
        return dispatcher.runSync("createShoppingList",
                dispatcher.makeValidContext("createShoppingList", ModelService.IN_PARAM, listFields));
    }

    public static String clearCurrentShoppingList(HttpServletRequest request, HttpServletResponse response) { // SCIPIO
        // SCIPIO: After login (where this event should be hooked), make sure to clear the session list to get rid of the anon list
        UtilHttp.removeSessionAttribute(request, "currentShoppingListId");
        return "success";
    }

    // SCIPIO: TODO: EventUtil.runServiceAsEvent calls are intended to be replaced with a small helper class based on EventUtil because EventUtil is too limited

    public static String createEmptyShoppingList(HttpServletRequest request, HttpServletResponse response) throws GenericServiceException { // SCIPIO
        // SCIPIO: TODO: REVIEW: For now, disallow this request for anonymous users even if anon users are enabled - this request is not likely useful for anon requests
        //          as they should be automated through addItemToShoppingList to create the single default list on first use... maybe support in future
        boolean anonUser = ShoppingListWorker.isAnonUser(request);
        if (anonUser) {
            //request.setAttribute("_ERROR_MESSAGE_"); // TODO? or better not?
            return "error";
        }
        Map<String, Object> servCtx = EventUtil.getServiceEventParamMap(request, "createShoppingList");
        ShoppingListWorker.checkSetShoppingListAuthTokenForService(request, servCtx);
        if (anonUser) {
            servCtx.put("userAddr", request.getRemoteAddr());
        } else {
            servCtx.remove("userAddr"); // Not necessary for registered users
        }
        return EventUtil.runServiceAsEvent(request, response, "createShoppingList", servCtx);
    }

    public static String updateShoppingList(HttpServletRequest request, HttpServletResponse response) throws GenericServiceException { // SCIPIO
        boolean anonUser = ShoppingListWorker.isAnonUser(request);
        Map<String, Object> servCtx = EventUtil.getServiceEventParamMap(request, "updateShoppingList");
        ShoppingListWorker.checkSetShoppingListAuthTokenForService(request, servCtx);
        if (anonUser) {
            servCtx.put("userAddr", request.getRemoteAddr());
        } else {
            servCtx.remove("userAddr"); // Not necessary for registered users
        }
        return EventUtil.runServiceAsEvent(request, response, "updateShoppingList", servCtx);
    }

    public static String createShoppingListFromOrder(HttpServletRequest request, HttpServletResponse response) throws GenericServiceException { // SCIPIO
        Map<String, Object> servCtx = EventUtil.getServiceEventParamMap(request, "makeShoppingListFromOrder");
        ShoppingListWorker.checkSetShoppingListAuthTokenForService(request, servCtx);
        return EventUtil.runServiceAsEvent(request, response, "makeShoppingListFromOrder", servCtx);
    }

    public static String addItemToShoppingList(HttpServletRequest request, HttpServletResponse response) throws GenericServiceException { // SCIPIO
        boolean anonUser = ShoppingListWorker.isAnonUser(request);
        // SCIPIO: SPECIAL: Automatically create the new anon shopping list, but for anon users we can only allow at most one for now, so it needs special handling
        // 2019-01-16: TODO: REVIEW: Do not do this through this call for now, because it's a legacy call and in current cases will probably do funny things
        //              Stores that need this can explicitly send to addItemToDefaultShoppingList instead, which will behave more consistent for anon + registered users.
        //if (anonUser && ShoppingListWorker.useAnonShoppingList(request)) {
        //    return addItemToDefaultAnonShoppingList(request, response, true);
        //}
        Map<String, Object> servCtx = EventUtil.getServiceEventParamMap(request, "createShoppingListItem");
        boolean hasShoppingListId = UtilValidate.isNotEmpty((String) servCtx.get("shoppingListId"));
        if (anonUser && !hasShoppingListId) {
            // SCIPIO: If anon shopping list is not enabled and no shoppingListId was passed, disallow this call to prevent list creations
            Debug.logWarning("addItemToShoppingList: anonymous user tried to create a ShoppingList by adding an item, but not allowed by store", module);
            request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("OrderErrorUiLabels", "shoppinglistevents.error_must_be_logged_in_modify_list", UtilHttp.getLocale(request)));
            return "error";
        }
        ShoppingListWorker.checkSetShoppingListAuthTokenForService(request, servCtx);
        String result = EventUtil.runServiceAsEvent(request, response, "createShoppingListItem", servCtx);
        if ("success".equals(result) && hasShoppingListId && UtilValidate.isNotEmpty((String) request.getAttribute("shoppingListId"))) {
            request.setAttribute("shoppingListCreated", true);
        }
        return result;
    }

    public static String addItemToDefaultShoppingList(HttpServletRequest request, HttpServletResponse response) throws GenericServiceException { // SCIPIO
        if (request.getParameter("shoppingListId") != null) { // prevent from interfering with service or screens, because we can't remove/override this parameter when request attribute is null (do not pass - you can get out of request attributes)
            Debug.logError("addItemToDefaultShoppingList: shoppingListId parameter passed, not allowed here", module);
            request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", UtilHttp.getLocale(request)));
            return "error";
        }
        boolean anonUser = ShoppingListWorker.isAnonUser(request);
        if (anonUser && ShoppingListWorker.useAnonShoppingList(request)) {
            return addItemToDefaultAnonShoppingList(request, response, true);
        }
        String shoppingListId = ShoppingListWorker.getUserDefaultWishListId(request, false);
        // NOTE: If shoppingListId is null, createShoppingListItem will automatically create one, so this is fine
        request.setAttribute("shoppingListId", shoppingListId);
        boolean hasDefaultList = UtilValidate.isNotEmpty(shoppingListId);
        String result = ShoppingListEvents.addItemToShoppingList(request, response);
        if ("success".equals(result) && !hasDefaultList) {
            // FIXME: This is a kludge due to the way the ECA on createShoppingListItem works... fine for now
            shoppingListId = (String) request.getAttribute("shoppingListId");
            if (UtilValidate.isEmpty(shoppingListId)) {
                Debug.logError("addItemToDefaultShoppingList: no shoppingListId found after addItemToShoppingList", module);
                request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", UtilHttp.getLocale(request)));
                return "error";
            }
            GenericValue shoppingList = ((Delegator) request.getAttribute("delegator")).from("ShoppingList").where("shoppingListId", shoppingListId).queryOneSafe();
            if (shoppingList == null) {
                Debug.logError("addItemToDefaultShoppingList: Could not find ShoppingList '" + shoppingListId + "'", module);
                request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", UtilHttp.getLocale(request)));
                return "error";
            }
            shoppingList.put("isUserDefault", "Y");
            try {
                shoppingList.store();
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", UtilHttp.getLocale(request)));
                return "error";
            }
        }
        return result;
    }

    /**
     * SCIPIO: Adds the item to the anonymous user single shopping list (distinct from the auto-save list), creating and storing in cookie if necessary.
     * Do not call directly as event.
     * NOTE: Currently there is only support for one list per anonymous user, and must be enabled with ProductStore.
     */
    public static String addItemToDefaultAnonShoppingList(HttpServletRequest request, HttpServletResponse response, boolean setErrorMessage) throws GenericServiceException { // SCIPIO
        boolean shoppingListCreated = false;
        Map<String, Object> servCtx = EventUtil.getServiceEventParamMap(request, "createShoppingListItem");
        String shoppingListId = (String) servCtx.get("shoppingListId");
        if (UtilValidate.isEmpty(shoppingListId)) { // NOTE: 2019-01-16: This should always be null currently, unless addItem above is changed...
            String anonListCookieName = ShoppingListCookieInfo.getAnonShoppingListCookieName(request);
            ShoppingListCookieInfo anonListCookieInfo = ShoppingListCookieInfo.fromCookie(request, anonListCookieName);
            if (anonListCookieInfo != null && anonListCookieInfo.getShoppingListId() != null && ShoppingListWorker.getValidUserShoppingList(request, anonListCookieInfo, false) != null) {
                anonListCookieInfo.toFields(servCtx);
            } else {
                try {
                    // SCIPIO: NOTE: Create this list as SLT_WISH_LIST, not SLT_SPEC_PURP, because it's meant as manual user wishlist, and can still detect using partyId null and listName DEFAULT_ANON_LIST_NAME
                    Map<String, Object> listFields = UtilMisc.<String, Object>toMap("userLogin", servCtx.get("userLogin"), "productStoreId", servCtx.get("productStoreId"), "shoppingListTypeId", "SLT_WISH_LIST", "listName", DEFAULT_ANON_LIST_NAME,
                            "userAddr", request.getRemoteAddr(), "isUserDefault", "Y");
                    Map<String, Object> newListResult = ((LocalDispatcher) request.getAttribute("dispatcher")).runSync("createShoppingList", listFields);
                    if (ServiceUtil.isError(newListResult)) {
                        String errorMessage = ServiceUtil.getErrorMessage(newListResult);
                        Debug.logError(errorMessage, module);
                        if (setErrorMessage) {
                            request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", UtilHttp.getLocale(request)));
                        }
                        return "error";
                    }
                    anonListCookieInfo = ShoppingListCookieInfo.fromFields(newListResult);
                    if (anonListCookieInfo == null) {
                        if (setErrorMessage) {
                            request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", UtilHttp.getLocale(request)));
                        }
                        return "error";
                    }
                    ShoppingListCookieInfo.createShoppingListCookie(request, response, ShoppingListCookieInfo.ANON_WISH_LIST, anonListCookieName, anonListCookieInfo);
                    anonListCookieInfo.toFields(servCtx);
                } catch (GeneralException e) {
                    Debug.logError(e, module);
                    if (setErrorMessage) {
                        request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", UtilHttp.getLocale(request)));
                    }
                    return "error";
                }
                shoppingListCreated = true;
            }
        }
        ShoppingListWorker.checkSetShoppingListAuthTokenForService(request, servCtx);
        String result = EventUtil.runServiceAsEvent(request, response, "createShoppingListItem", servCtx);
        if (!"error".equals(result) && shoppingListCreated) {
            request.setAttribute("shoppingListCreated", true);
            request.setAttribute("anonShoppingListCreated", true);
        }
        return result;
    }

    public static String updateShoppingListItem(HttpServletRequest request, HttpServletResponse response) throws GenericServiceException { // SCIPIO
        Map<String, Object> servCtx = EventUtil.getServiceEventParamMap(request, "updateShoppingListItem");
        ShoppingListWorker.checkSetShoppingListAuthTokenForService(request, servCtx);
        return EventUtil.runServiceAsEvent(request, response, "updateShoppingListItem", servCtx);
    }

    public static String removeFromShoppingList(HttpServletRequest request, HttpServletResponse response) throws GenericServiceException { // SCIPIO
        Map<String, Object> servCtx = EventUtil.getServiceEventParamMap(request, "removeShoppingListItem");
        ShoppingListWorker.checkSetShoppingListAuthTokenForService(request, servCtx);
        return EventUtil.runServiceAsEvent(request, response, "removeShoppingListItem", servCtx);
    }

    public static String removeFromDefaultShoppingList(HttpServletRequest request, HttpServletResponse response) throws GenericServiceException { // SCIPIO
        if (request.getParameter("shoppingListId") != null) { // prevent from interfering with service or screens, because we can't remove/override this parameter when request attribute is null (do not pass - you can get out of request attributes)
            Debug.logError("removeFromDefaultShoppingList: shoppingListId parameter passed, not allowed here", module);
            request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", UtilHttp.getLocale(request)));
            return "error";
        }
        boolean anonUser = ShoppingListWorker.isAnonUser(request);
        if (anonUser && ShoppingListWorker.useAnonShoppingList(request)) {
            return removeFromDefaultAnonShoppingList(request, response, true); // There is only one, default anon list
        }
        String shoppingListId = ShoppingListWorker.getUserDefaultWishListId(request, false);
        if (shoppingListId == null) {
            Debug.logInfo("removeFromDefaultShoppingList: no default ShoppingList found for user; not removing anything", module);
            return "success"; // be permissive, no need for error here because stale requests are likely
        }
        request.setAttribute("shoppingListId", shoppingListId);
        return ShoppingListEvents.removeFromShoppingList(request, response);
    }

    public static String removeFromDefaultAnonShoppingList(HttpServletRequest request, HttpServletResponse response, boolean setErrorMessage) throws GenericServiceException { // SCIPIO
        Map<String, Object> servCtx = EventUtil.getServiceEventParamMap(request, "removeShoppingListItem");
        String shoppingListId = (String) servCtx.get("shoppingListId");
        if (UtilValidate.isEmpty(shoppingListId)) { // NOTE: 2019-01-16: This should always be null currently, unless calls above are changed...
            String anonListCookieName = ShoppingListCookieInfo.getAnonShoppingListCookieName(request);
            ShoppingListCookieInfo anonListCookieInfo = ShoppingListCookieInfo.fromCookie(request, anonListCookieName);
            if (anonListCookieInfo != null && anonListCookieInfo.getShoppingListId() != null && ShoppingListWorker.getValidUserShoppingList(request, anonListCookieInfo, false) != null) {
                anonListCookieInfo.toFields(servCtx);
            } else {
                Debug.logInfo("removeFromDefaultAnonShoppingList: no default ShoppingList found for anonymous user; not removing anything", module);
                return "success"; // be permissive, no need for error here because the list is created on-demand and stale requests are likely
            }
        }
        ShoppingListWorker.checkSetShoppingListAuthTokenForService(request, servCtx);
        return EventUtil.runServiceAsEvent(request, response, "removeShoppingListItem", servCtx);
    }

    /**
     * SCIPIO: See applications/shop/script/com/ilscipio/scipio/shop/customer/CustomerEvents.xml#createCustomer for usage.
     */
    public static String convertAnonShoppingListsToRegisteredForStore(HttpServletRequest request, HttpServletResponse response, GenericValue userLogin) { // SCIPIO
        if (!ShoppingListWorker.useAnonShoppingList(request)) {
            return "success";
        }
        GenericValue shoppingList = ShoppingListWorker.getAnonUserDefaultWishList(request, userLogin, false);
        if (shoppingList == null) {
            return "success";
        }
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);

        // Cannot be anon to do this
        Map<String, Object> servCtx = new HashMap<>();
        servCtx.put("userLogin", userLogin);
        servCtx.put("locale", locale);
        servCtx.put("shoppingListId", shoppingList.get("shoppingListId"));
        servCtx.put("shoppingListAuthToken", shoppingList.get("shoppingListAuthToken")); // NOTE: In most cases you should not do this!! But here the returned list was already validated.

        Map<String, Object> servResult = null;
        try {
            servResult = dispatcher.runSync("convertAnonShoppingListToRegistered", servCtx);
        } catch (GenericServiceException e) {
            Debug.logError(e, module);
            return "error";
        }
        if (!ServiceUtil.isSuccess(servResult)) {
            return "error";
        }
        return "success";
    }
}