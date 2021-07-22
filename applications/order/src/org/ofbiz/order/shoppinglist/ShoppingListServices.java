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
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.transaction.GenericTransactionException;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityListIterator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityTypeUtil;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.order.order.OrderReadHelper;
import org.ofbiz.order.shoppingcart.CartItemModifyException;
import org.ofbiz.order.shoppingcart.CheckOutHelper;
import org.ofbiz.order.shoppingcart.ItemNotFoundException;
import org.ofbiz.order.shoppingcart.ShoppingCart;
import org.ofbiz.order.shoppingcart.ShoppingCartFactory;
import org.ofbiz.product.config.ProductConfigWorker;
import org.ofbiz.product.config.ProductConfigWrapper;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.calendar.RecurrenceInfo;
import org.ofbiz.service.calendar.RecurrenceInfoException;

import com.ibm.icu.util.Calendar;

import javax.transaction.Transaction;

/**
 * Shopping List Services
 */
public class ShoppingListServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String resource_error = "OrderErrorUiLabels";

    public static Map<String, Object> setShoppingListRecurrence(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Timestamp startDate = (Timestamp) context.get("startDateTime");
        Timestamp endDate = (Timestamp) context.get("endDateTime");
        Integer frequency = (Integer) context.get("frequency");
        Integer interval = (Integer) context.get("intervalNumber");
        Locale locale = (Locale) context.get("locale");

        if (frequency == null || interval == null) {
            Debug.logWarning(UtilProperties.getMessage(resource_error,"OrderFrequencyOrIntervalWasNotSpecified", Debug.getLogLocale()), module); // SCIPIO: log locale
            return ServiceUtil.returnSuccess();
        }

        if (startDate == null) {
            switch (frequency) {
                case 5:
                    startDate = UtilDateTime.getWeekStart(UtilDateTime.nowTimestamp(), 0, interval);
                    break;
                case 6:
                    startDate = UtilDateTime.getMonthStart(UtilDateTime.nowTimestamp(), 0, interval);
                    break;
                case 7:
                    startDate = UtilDateTime.getYearStart(UtilDateTime.nowTimestamp(), 0, interval);
                    break;
                default:
                    return ServiceUtil.returnError(UtilProperties.getMessage(resource_error,"OrderInvalidFrequencyForShoppingListRecurrence",locale));
            }
        }

        long startTime = startDate.getTime();
        long endTime = 0;
        if (endDate != null) {
            endTime = endDate.getTime();
        }

        RecurrenceInfo recInfo = null;
        try {
            recInfo = RecurrenceInfo.makeInfo(delegator, startTime, frequency, interval, -1, endTime);
        } catch (RecurrenceInfoException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(UtilProperties.getMessage(resource_error,"OrderUnableToCreateShoppingListRecurrenceInformation",locale));
        }

        Debug.logInfo("Next Recurrence - " + UtilDateTime.getTimestamp(recInfo.next()), module);
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("recurrenceInfoId", recInfo.getID());

        return result;
    }

    public static Map<String, Object> createListReorders(DispatchContext dctx, Map<String, ? extends Object> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Delegator delegator = dctx.getDelegator();

        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Locale locale = (Locale) context.get("locale");

        boolean beganTransaction = false;
        EntityQuery eq = EntityQuery.use(delegator)
                .from("ShoppingList")
                .where("shoppingListTypeId", "SLT_AUTO_REODR", "isActive", "Y")
                .orderBy("-lastOrderedDate");
        try {
            beganTransaction = TransactionUtil.begin();
        } catch (GenericTransactionException e1) {
            Debug.logError(e1, "[Delegator] Could not begin transaction: " + e1.toString(), module);
        }

        try (EntityListIterator eli = eq.queryIterator()) {
            if (eli != null) {
                GenericValue shoppingList;
                while (((shoppingList = eli.next()) != null)) {
                    Timestamp lastOrder = shoppingList.getTimestamp("lastOrderedDate");
                    RecurrenceInfo recurrence = null;

                    GenericValue recurrenceInfo = shoppingList.getRelatedOne("RecurrenceInfo", false);
                    Timestamp startDateTime = recurrenceInfo.getTimestamp("startDateTime");

                    if (recurrenceInfo != null) {
                        try {
                            recurrence = new RecurrenceInfo(recurrenceInfo);
                        } catch (RecurrenceInfoException e) {
                            Debug.logError(e, module);
                        }
                    }


                    // check the next recurrence
                    if (recurrence != null) {
                        long next = lastOrder == null ? recurrence.next(startDateTime.getTime()) : recurrence.next(lastOrder.getTime());
                        Timestamp now = UtilDateTime.nowTimestamp();
                        Timestamp nextOrder = UtilDateTime.getDayStart(UtilDateTime.getTimestamp(next));

                        if (nextOrder.after(now)) {
                            continue;
                        }
                    } else {
                        continue;
                    }

                    ShoppingCart listCart = makeShoppingListCart(dispatcher, shoppingList, locale);
                    CheckOutHelper helper = new CheckOutHelper(dispatcher, delegator, listCart);

                    // store the order
                    Map<String, Object> createResp = helper.createOrder(userLogin);
                    if (createResp == null || (createResp != null && ServiceUtil.isError(createResp))) {
                        Debug.logError("Cannot create order for shopping list - " + shoppingList, module);
                    } else {

                        String orderId = (String) createResp.get("orderId");

                        // authorize the payments
                        Map<String, Object> payRes = null;
                        try {
                            payRes = helper.processPayment(ProductStoreWorker.getProductStore(listCart.getProductStoreId(), delegator), userLogin);
                        } catch (GeneralException e) {
                            Debug.logError(e, module);
                        }

                        if (payRes != null && ServiceUtil.isError(payRes)) {
                            Debug.logError("Payment processing problems with shopping list - " + shoppingList, module);
                        }

                        shoppingList.set("lastOrderedDate", UtilDateTime.nowTimestamp());
                        shoppingList.store();

                        // send notification
                        try {
                            dispatcher.runAsync("sendOrderPayRetryNotification", UtilMisc.toMap("orderId", orderId));
                        } catch (GenericServiceException e) {
                            Debug.logError(e, module);
                        }

                        // increment the recurrence
                        recurrence.incrementCurrentCount();
                    }
                }
            }

            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException e) {
            try {
                // only rollback the transaction if we started one...
                TransactionUtil.rollback(beganTransaction, "Error creating shopping list auto-reorders", e);
            } catch (GenericEntityException e2) {
                Debug.logError(e2, "[Delegator] Could not rollback transaction: " + e2.toString(), module);
            }

            String errMsg = UtilProperties.getMessage(resource_error, "OrderErrorWhileCreatingNewShoppingListBasedAutomaticReorder", UtilMisc.toMap("errorString", e.toString()), locale);
            Debug.logError(e, errMsg, module);
            return ServiceUtil.returnError(errMsg);
        } finally {
            try {
                // only commit the transaction if we started one... this will throw an exception if it fails
                TransactionUtil.commit(beganTransaction);
            } catch (GenericEntityException e) {
                Debug.logError(e, "Could not commit transaction for creating new shopping list based automatic reorder", module);
            }
        }
    }

    public static Map<String, Object> splitShipmentMethodString(DispatchContext dctx, Map<String, ? extends Object> context) {
        String shipmentMethodString = (String) context.get("shippingMethodString");
        Map<String, Object> result = ServiceUtil.returnSuccess();

        if (UtilValidate.isNotEmpty(shipmentMethodString)) {
            int delimiterPos = shipmentMethodString.indexOf('@');
            String shipmentMethodTypeId = null;
            String carrierPartyId = null;

            if (delimiterPos > 0) {
                shipmentMethodTypeId = shipmentMethodString.substring(0, delimiterPos);
                carrierPartyId = shipmentMethodString.substring(delimiterPos + 1);
                result.put("shipmentMethodTypeId", shipmentMethodTypeId);
                result.put("carrierPartyId", carrierPartyId);
            }
        }
        return result;
    }

    public static Map<String, Object> makeListFromOrder(DispatchContext dctx, Map<String, ? extends Object> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Delegator delegator = dctx.getDelegator();

        String shoppingListTypeId = (String) context.get("shoppingListTypeId");
        String shoppingListId = (String) context.get("shoppingListId");
        String orderId = (String) context.get("orderId");
        String partyId = (String) context.get("partyId");

        Timestamp startDate = (Timestamp) context.get("startDateTime");
        Timestamp endDate = (Timestamp) context.get("endDateTime");
        Integer frequency = (Integer) context.get("frequency");
        Integer interval = (Integer) context.get("intervalNumber");

        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Locale locale = (Locale) context.get("locale");
        String shoppingListAuthToken = (String) context.get("shoppingListAuthToken"); // SCIPIO

        boolean beganTransaction = false;
        try {
            beganTransaction = TransactionUtil.begin();

            GenericValue orderHeader = null;
            orderHeader = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();

            if (orderHeader == null) {
                return ServiceUtil.returnError(UtilProperties.getMessage(resource_error,"OrderUnableToLocateOrder", UtilMisc.toMap("orderId",orderId), locale));
            }
            String productStoreId = orderHeader.getString("productStoreId");

            if (UtilValidate.isEmpty(shoppingListId)) {
                // create a new shopping list
                if (partyId == null) {
                    partyId = userLogin.getString("partyId");
                }

                Map<String, Object> serviceCtx = UtilMisc.<String, Object>toMap("userLogin", userLogin, "partyId", partyId,
                        "productStoreId", productStoreId, "listName", "List Created From Order #" + orderId);

                if (UtilValidate.isNotEmpty(shoppingListTypeId)) {
                    serviceCtx.put("shoppingListTypeId", shoppingListTypeId);
                }

                Map<String, Object> newListResult = null;
                try {
                    newListResult = dispatcher.runSync("createShoppingList", serviceCtx);
                } catch (GenericServiceException e) {
                    Debug.logError(e, "Problems creating new ShoppingList", module);
                    return ServiceUtil.returnError(UtilProperties.getMessage(resource_error,"OrderUnableToCreateNewShoppingList",locale));
                }

                // check for errors
                if (ServiceUtil.isError(newListResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(newListResult));
                }

                // get the new list id
                if (newListResult != null) {
                    shoppingListId = (String) newListResult.get("shoppingListId");
                    shoppingListAuthToken = (String) newListResult.get("shoppingListAuthToken"); // SCIPIO
                }
            }

            GenericValue shoppingList = null;
            shoppingList = EntityQuery.use(delegator).from("ShoppingList").where("shoppingListId", shoppingListId).queryOne();

            if (shoppingList == null) {
                return ServiceUtil.returnError(UtilProperties.getMessage(resource_error,"OrderNoShoppingListAvailable",locale));
            }
            shoppingListTypeId = shoppingList.getString("shoppingListTypeId");

            OrderReadHelper orh;
            try {
                orh = new OrderReadHelper(orderHeader);
            } catch (IllegalArgumentException e) {
                Debug.logError(e, module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resource_error,"OrderUnableToLoadOrderReadHelper", UtilMisc.toMap("orderId",orderId), locale));
            }

            List<GenericValue> orderItems = orh.getOrderItems();
            for (GenericValue orderItem : orderItems) {
                String productId = orderItem.getString("productId");
                if (UtilValidate.isNotEmpty(productId)) {
                    Map<String, Object> ctx = UtilMisc.<String, Object>toMap("userLogin", userLogin, "shoppingListId", shoppingListId, "productId",
                            orderItem.get("productId"), "quantity", orderItem.get("quantity"));
                    if (EntityTypeUtil.hasParentType(delegator, "ProductType", "productTypeId", ProductWorker.getProductTypeId(delegator, productId), "parentTypeId", "AGGREGATED")) {
                        try {
                            GenericValue instanceProduct = EntityQuery.use(delegator).from("Product").where("productId", productId).queryOne();
                            String configId = instanceProduct.getString("configId");
                            ctx.put("configId", configId);
                            String aggregatedProductId = ProductWorker.getInstanceAggregatedId(delegator, productId);
                            //override the instance productId with aggregated productId
                            ctx.put("productId", aggregatedProductId);
                        } catch (GenericEntityException e) {
                            Debug.logError(e, module);
                        }
                    }
                    ctx.put("shoppingListAuthToken", shoppingListAuthToken); // SCIPIO
                    Map<String, Object> serviceResult = null;
                    try {
                        serviceResult = dispatcher.runSync("createShoppingListItem", ctx);
                    } catch (GenericServiceException e) {
                        Debug.logError(e, module);
                    }
                    if (serviceResult == null || ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(UtilProperties.getMessage(resource_error,"OrderUnableToAddItemToShoppingList",UtilMisc.toMap("shoppingListId",shoppingListId), locale));
                    }
                }
            }

            if ("SLT_AUTO_REODR".equals(shoppingListTypeId)) {
                GenericValue paymentPref = EntityUtil.getFirst(orh.getPaymentPreferences());
                GenericValue shipGroup = EntityUtil.getFirst(orh.getOrderItemShipGroups());

                Map<String, Object> slCtx = new HashMap<>();
                slCtx.put("shipmentMethodTypeId", shipGroup.get("shipmentMethodTypeId"));
                slCtx.put("carrierRoleTypeId", shipGroup.get("carrierRoleTypeId"));
                slCtx.put("carrierPartyId", shipGroup.get("carrierPartyId"));
                slCtx.put("contactMechId", shipGroup.get("contactMechId"));
                slCtx.put("paymentMethodId", paymentPref.get("paymentMethodId"));
                slCtx.put("currencyUom", orh.getCurrency());
                slCtx.put("startDateTime", startDate);
                slCtx.put("endDateTime", endDate);
                slCtx.put("frequency", frequency);
                slCtx.put("intervalNumber", interval);
                slCtx.put("isActive", "Y");
                slCtx.put("shoppingListId", shoppingListId);
                slCtx.put("userLogin", userLogin);
                slCtx.put("shoppingListAuthToken", shoppingListAuthToken); // SCIPIO

                Map<String, Object> slUpResp = null;
                try {
                    slUpResp = dispatcher.runSync("updateShoppingList", slCtx);
                } catch (GenericServiceException e) {
                    Debug.logError(e, module);
                }

                if (slUpResp == null || ServiceUtil.isError(slUpResp)) {
                    return ServiceUtil.returnError(UtilProperties.getMessage(resource_error,"OrderUnableToUpdateShoppingListInformation",UtilMisc.toMap("shoppingListId",shoppingListId), locale));
                }
            }

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("shoppingListId", shoppingListId);
            return result;

        } catch (GenericEntityException e) {
            try {
                // only rollback the transaction if we started one...
                TransactionUtil.rollback(beganTransaction, "Error making shopping list from order", e);
            } catch (GenericEntityException e2) {
                Debug.logError(e2, "[Delegator] Could not rollback transaction: " + e2.toString(), module);
            }

            String errMsg = UtilProperties.getMessage(resource_error, "OrderErrorWhileCreatingNewShoppingListBasedOnOrder", UtilMisc.toMap("errorString", e.toString()), locale);
            Debug.logError(e, errMsg, module);
            return ServiceUtil.returnError(errMsg);
        } finally {
            try {
                // only commit the transaction if we started one... this will throw an exception if it fails
                TransactionUtil.commit(beganTransaction);
            } catch (GenericEntityException e) {
                Debug.logError(e, "Could not commit transaction for creating new shopping list based on order", module);
            }
        }
    }
    /**
     * Create a new shoppingCart form a shoppingList
     * @param dispatcher the local dispatcher
     * @param shoppingList a GenericValue object of the shopping list
     * @param locale the locale in use
     * @return returns a new shopping cart form a shopping list
     */
    public static ShoppingCart makeShoppingListCart(LocalDispatcher dispatcher, GenericValue shoppingList, Locale locale) {
        return makeShoppingListCart(null, dispatcher, shoppingList, locale); }

    /**
     * Add a shoppinglist to an existing shoppingcart
     *
     * @param listCart the shopping cart list
     * @param dispatcher the local dispatcher
     * @param shoppingList a GenericValue object of the shopping list
     * @param locale the locale in use
     * @return the modified shopping cart adding the shopping list elements
     */
    public static ShoppingCart makeShoppingListCart(ShoppingCart listCart, LocalDispatcher dispatcher, GenericValue shoppingList, Locale locale) {
        Delegator delegator = dispatcher.getDelegator();
        if (shoppingList != null && shoppingList.get("productStoreId") != null) {
            String productStoreId = shoppingList.getString("productStoreId");
            String currencyUom = shoppingList.getString("currencyUom");
            if (currencyUom == null) {
                GenericValue productStore = ProductStoreWorker.getProductStore(productStoreId, delegator);
                if (productStore == null) {
                    return null;
                }
                currencyUom = productStore.getString("defaultCurrencyUomId");
            }
            if (locale == null) {
                locale = Locale.getDefault();
            }

            List<GenericValue> items = null;
            try {
                items = shoppingList.getRelated("ShoppingListItem", null, UtilMisc.toList("shoppingListItemSeqId"), false);
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
            }

            if (UtilValidate.isNotEmpty(items)) {
                if (listCart == null) {
                    listCart = ShoppingCartFactory.createShoppingCart(delegator, productStoreId, locale, currencyUom); // SCIPIO: use factory
                    listCart.setOrderPartyId(shoppingList.getString("partyId"));
                    listCart.setAutoOrderShoppingListId(shoppingList.getString("shoppingListId"));
                } else {
                    if (!listCart.getPartyId().equals(shoppingList.getString("partyId"))) {
                        Debug.logError("CANNOT add shoppingList: " + shoppingList.getString("shoppingListId")
                                + " of partyId: " + shoppingList.getString("partyId")
                                + " to a shoppingcart with a different orderPartyId: "
                                + listCart.getPartyId(), module);
                        return listCart;
                    }
                }


                ProductConfigWrapper configWrapper = null;
                for (GenericValue shoppingListItem : items) {
                    String productId = shoppingListItem.getString("productId");
                    BigDecimal quantity = shoppingListItem.getBigDecimal("quantity");
                    Timestamp reservStart = shoppingListItem.getTimestamp("reservStart");
                    BigDecimal reservLength = null;
                    String configId = shoppingListItem.getString("configId");

                    if (shoppingListItem.get("reservLength") != null) {
                        reservLength = shoppingListItem.getBigDecimal("reservLength");
                    }
                    BigDecimal reservPersons = null;
                    if (shoppingListItem.get("reservPersons") != null) {
                        reservPersons = shoppingListItem.getBigDecimal("reservPersons");
                    }
                    if (UtilValidate.isNotEmpty(productId) && quantity != null) {
                        if (UtilValidate.isNotEmpty(configId)) {
                            configWrapper = ProductConfigWorker.loadProductConfigWrapper(delegator, dispatcher, configId, productId, listCart.getProductStoreId(), null, listCart.getWebSiteId(), listCart.getCurrency(), listCart.getLocale(), listCart.getAutoUserLogin());
                        }
                        // list items are noted in the shopping cart
                        String listId = shoppingListItem.getString("shoppingListId");
                        String itemId = shoppingListItem.getString("shoppingListItemSeqId");
                        Map<String, Object> attributes = UtilMisc.<String, Object>toMap("shoppingListId", listId, "shoppingListItemSeqId", itemId);

                        try {
                            listCart.addOrIncreaseItem(productId, null, quantity, reservStart, reservLength, reservPersons, null, null, null, null, null, attributes, null, configWrapper, null, null, null, dispatcher);
                        } catch (CartItemModifyException e) {
                            Debug.logError(e, "Unable to add product to List Cart - " + productId, module);
                        } catch (ItemNotFoundException e) {
                            Debug.logError(e, "Product not found - " + productId, module);
                        }
                    }
                }

                if (listCart.size() > 0) {
                    if (UtilValidate.isNotEmpty(shoppingList.get("paymentMethodId"))) {
                        listCart.addPayment(shoppingList.getString("paymentMethodId"));
                    }
                    if (UtilValidate.isNotEmpty(shoppingList.get("contactMechId"))) {
                        listCart.setAllShippingContactMechId(shoppingList.getString("contactMechId"));
                    }
                    if (UtilValidate.isNotEmpty(shoppingList.get("shipmentMethodTypeId"))) {
                        listCart.setAllShipmentMethodTypeId(shoppingList.getString("shipmentMethodTypeId"));
                    }
                    if (UtilValidate.isNotEmpty(shoppingList.get("carrierPartyId"))) {
                        listCart.setAllCarrierPartyId(shoppingList.getString("carrierPartyId"));
                    }
                    if (UtilValidate.isNotEmpty(shoppingList.getString("productPromoCodeId"))) {
                        listCart.addProductPromoCode(shoppingList.getString("productPromoCodeId"), dispatcher);
                    }
                }
            }
        }
        return listCart;
    }

    public static ShoppingCart makeShoppingListCart(LocalDispatcher dispatcher, String shoppingListId, Locale locale) {
        Delegator delegator = dispatcher.getDelegator();
        GenericValue shoppingList = null;
        try {
            shoppingList = EntityQuery.use(delegator).from("ShoppingList").where("shoppingListId", shoppingListId).queryOne();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }
        return makeShoppingListCart(dispatcher, shoppingList, locale);
    }

    /**
     *
     * Given an orderId, this service will look through all its OrderItems and for each shoppingListItemId
     * and shoppingListItemSeqId, update the quantity purchased in the ShoppingListItem entity.  Used for
     * tracking how many of shopping list items are purchased.  This service is mounted as a seca on storeOrder.
     *
     * @param ctx - The DispatchContext that this service is operating in
     * @param context - Map containing the input parameters
     * @return Map with the result of the service, the output parameters
     */
    public static Map<String, Object> updateShoppingListQuantitiesFromOrder(DispatchContext ctx, Map<String, ? extends Object> context) {
        Map<String, Object> result = new HashMap<>();
        Delegator delegator = ctx.getDelegator();
        String orderId = (String) context.get("orderId");
        try {
            List<GenericValue> orderItems = EntityQuery.use(delegator).from("OrderItem").where("orderId", orderId).queryList();
            for (GenericValue orderItem : orderItems) {
                String shoppingListId = orderItem.getString("shoppingListId");
                String shoppingListItemSeqId = orderItem.getString("shoppingListItemSeqId");
                if (UtilValidate.isNotEmpty(shoppingListId)) {
                    GenericValue shoppingListItem = EntityQuery.use(delegator).from("ShoppingListItem").where("shoppingListId", shoppingListId, "shoppingListItemSeqId", shoppingListItemSeqId).queryOne();
                    if (shoppingListItem != null) {
                        BigDecimal quantityPurchased = shoppingListItem.getBigDecimal("quantityPurchased");
                        BigDecimal orderQuantity = orderItem.getBigDecimal("quantity");
                        if (quantityPurchased != null) {
                            shoppingListItem.set("quantityPurchased", orderQuantity.add(quantityPurchased));
                        } else {
                            shoppingListItem.set("quantityPurchased", orderQuantity);
                        }
                        shoppingListItem.store();
                    }
                }
            }
        } catch (GenericEntityException gee) {
            Debug.logInfo("updateShoppingListQuantitiesFromOrder error:"+gee.getMessage(), module);
        } catch (Exception e) {
            Debug.logInfo("updateShoppingListQuantitiesFromOrder error:"+e.getMessage(), module);
        }
        return result;
    }

    public static Map<String,Object> autoDeleteAutoSaveShoppingList(DispatchContext dctx, Map<String, ? extends Object> context) {
        List<String> errorMessageList = new ArrayList<>();
        Delegator delegator = dctx.getDelegator();
        List<GenericValue> shoppingList = null;
        // SCIPIO: Also clear anon ShoppingLists, if applicable
        boolean deleteAutoSaveList = !Boolean.FALSE.equals(context.get("deleteAutoSaveList"));
        boolean deleteAnonWishList = !Boolean.FALSE.equals(context.get("deleteAnonWishList"));
        Integer sepTransBatch = (Integer) context.get("sepTransBatch");
        Map<String, Object> stats = new LinkedHashMap<>();
        if (deleteAutoSaveList) {
            try {
                shoppingList = EntityQuery.use(delegator).select("shoppingListId", "lastAdminModified", "lastUpdatedStamp")
                        .from("ShoppingList").where("partyId", null, "shoppingListTypeId", "SLT_SPEC_PURP").queryList();
            } catch (GenericEntityException e) {
                Debug.logError("autoDeleteAutoSaveShoppingList: " + e, module);
                return ServiceUtil.returnError(e.toString());
            }
            int maxDays = EntityUtilProperties.getPropertyAsInteger("order", "autosave.max.age", 30, delegator);
            Map<String, Object> servResult = deleteExpiredShoppingLists(dctx, context, shoppingList, maxDays, sepTransBatch);
            if (ServiceUtil.isError(servResult)) {
                return servResult;
            } else if (ServiceUtil.isFailure(servResult)) {
                errorMessageList.add(ServiceUtil.getErrorMessage(servResult));
            }
            stats.put("auto-save lists deleted", servResult.get("deleted"));
            stats.put("auto-save lists skipped still referenced", servResult.get("skippedReferenced"));
        }
        if (deleteAnonWishList) {
            try {
                shoppingList = EntityQuery.use(delegator).select("shoppingListId", "lastAdminModified", "lastUpdatedStamp")
                        .from("ShoppingList").where("partyId", null, "shoppingListTypeId", "SLT_WISH_LIST").queryList();
            } catch (GenericEntityException e) {
                Debug.logError("autoDeleteAutoSaveShoppingList: " + e, module);
                return ServiceUtil.returnError(e.toString());
            }
            int maxDays = EntityUtilProperties.getPropertyAsInteger("order", "anonwishlist.max.age", 30, delegator);
            Map<String, Object> servResult = deleteExpiredShoppingLists(dctx, context, shoppingList, maxDays, sepTransBatch);
            if (ServiceUtil.isError(servResult)) {
                return servResult;
            } else if (ServiceUtil.isFailure(servResult)) {
                errorMessageList.add(ServiceUtil.getErrorMessage(servResult));
            }
            stats.put("anon wishlists deleted", servResult.get("deleted"));
            stats.put("anon wishlists skipped still referenced", servResult.get("skippedReferenced"));
        }
        if (errorMessageList.size() > 0) {
            String msg = "Auto-delete shopping lists finished with skippable errors " + stats;
            errorMessageList.add(0, msg);
            Debug.logWarning("autoDeleteAutoSaveShoppingList: " + errorMessageList, module);
            return ServiceUtil.returnFailure(errorMessageList);
        } else {
            String msg = "Auto-delete shopping lists finished with " + stats;
            Debug.logInfo("autoDeleteAutoSaveShoppingList: " + msg, module);
            return ServiceUtil.returnSuccess(msg);
        }
    }

    // SCIPIO: Refactored from autoDeleteAutoSaveShoppingList
    protected static Map<String, Object> deleteExpiredShoppingLists(DispatchContext dctx, Map<String, ? extends Object> context,
                                                                    List<GenericValue> shoppingList, int maxDays, Integer sepTransBatch) {
        Map<String, Object> result = null;
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (shoppingList == null) {
            return ServiceUtil.returnSuccess();
        }
        int skippedReferenced = 0;
        int deleted = 0;
        if (!shoppingList.isEmpty() && maxDays > 0) {
            int total = 0;
            if (sepTransBatch == null) {
                sepTransBatch = 0;
            }
            int transEntryCount = -1;
            Transaction parentTransaction = null; // NOTE: There should probably not be a parent transaction, but handle in case
            boolean beganTransaction = false;
            try {
                if (TransactionUtil.isTransactionInPlace()) {
                    parentTransaction = TransactionUtil.suspend();
                }
                for (GenericValue sl : shoppingList) {
                    if (transEntryCount < 0 || (sepTransBatch > 0 && transEntryCount >= sepTransBatch)) {
                        try {
                            if (transEntryCount >= 0) {
                                Debug.logInfo("deleteExpiredShoppingLists: committing " + transEntryCount +
                                        " ShoppingList removals (visited: " + total + "/" + shoppingList.size() + ")", module);
                                TransactionUtil.commit(beganTransaction);
                            }
                        } finally {
                            beganTransaction = false;
                            transEntryCount = 0;
                        }
                        beganTransaction = TransactionUtil.begin();
                    }

                    // SCIPIO: NOTE: ShoppingList (sl) GenericValue has limited field select, see callers
                    // SCIPIO: do not try to delete old lists that are (for whatever reason) associated to OrderHeader, otherwise this ruins the whole execution
                    if (delegator.from("OrderHeader").where("autoOrderShoppingListId", sl.get("shoppingListId")).queryCountSafe() > 0) {
                        skippedReferenced++;
                        continue;
                    }
                    Timestamp lastModified = sl.getTimestamp("lastAdminModified");
                    if (lastModified == null) {
                        lastModified = sl.getTimestamp("lastUpdatedStamp");
                    }
                    Calendar cal = Calendar.getInstance();
                    cal.setTimeInMillis(lastModified.getTime());
                    cal.add(Calendar.DAY_OF_YEAR, maxDays);
                    Date expireDate = cal.getTime();
                    Date nowDate = new Date();
                    if (expireDate.equals(nowDate) || nowDate.after(expireDate)) {
                        List<GenericValue> shoppingListItems = sl.getRelated("ShoppingListItem",
                                null, null, false);
                        if (shoppingListItems != null) {
                            // SCIPIO: Also make sure the dates on the ShoppingListItem are also past the expired date, because
                            // not all ShoppingList-modifying code updates the main ShoppingList instance
                            boolean allItemsExpired = true;
                            for (GenericValue sli : shoppingListItems) {
                                lastModified = sli.getTimestamp("lastUpdatedStamp");
                                cal = Calendar.getInstance();
                                cal.setTimeInMillis(lastModified.getTime());
                                cal.add(Calendar.DAY_OF_YEAR, maxDays);
                                expireDate = cal.getTime();
                                if (expireDate.equals(nowDate) || nowDate.after(expireDate)) {
                                    ; // good
                                } else {
                                    allItemsExpired = false;
                                }
                            }
                            if (!allItemsExpired) {
                                continue;
                            }
                            Debug.logInfo("deleteExpiredShoppingLists: Removing expired ShoppingList [" + sl.get("shoppingListId") + "]", module); // SCIPIO
                            for (GenericValue sli : shoppingListItems) {
                                Map<String, Object> servResult = dispatcher.runSync("removeShoppingListItem", UtilMisc.toMap("shoppingListId", sl.getString("shoppingListId"),
                                        "shoppingListItemSeqId", sli.getString("shoppingListItemSeqId"),
                                        "userLogin", userLogin));
                                if (ServiceUtil.isError(servResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(servResult));
                                }
                            }
                        }
                        Map<String, Object> servResult = dispatcher.runSync("removeShoppingList", UtilMisc.toMap("shoppingListId", sl.getString("shoppingListId"), "userLogin", userLogin));
                        if (ServiceUtil.isError(servResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(servResult));
                        }
                        deleted++;
                        transEntryCount++;
                    }
                    total++;
                }
                try {
                    Debug.logInfo("deleteExpiredShoppingLists: committing " + transEntryCount +
                            " ShoppingList removals (visited: " + total + "/" + shoppingList.size() + ")", module);
                    TransactionUtil.commit(beganTransaction);
                } finally {
                    beganTransaction = false;
                    transEntryCount = 0;
                }
            } catch(RuntimeException e) {
                if (beganTransaction) {
                    try {
                        TransactionUtil.rollback(beganTransaction, e.getMessage(), e);
                    } catch (GenericTransactionException e2) {
                        Debug.logError(e2, "deleteExpiredShoppingLists: Could not rollback nested transaction: " + e2, module);
                    }
                }
                throw e;
            } catch(GeneralException e) {
                if (beganTransaction) {
                    try {
                        TransactionUtil.rollback(beganTransaction, e.getMessage(), e);
                    } catch (GenericTransactionException e2) {
                        Debug.logError(e2, "deleteExpiredShoppingLists: Could not rollback nested transaction: " + e2, module);
                    }
                }
                Debug.logError("deleteExpiredShoppingLists: " + e, module);
                return ServiceUtil.returnError(e.toString());
            } finally {
                if (parentTransaction != null) {
                    try {
                        TransactionUtil.resume(parentTransaction);
                    } catch (GenericTransactionException e) {
                        Debug.logError("deleteExpiredShoppingLists: Could not resume parent transaction: " + e, module);
                        if (result == null) {
                            result = ServiceUtil.returnError("Could not resume parent transaction: " + e);
                        }
                    }
                }
            }
        }
        if (result == null) {
            result = ServiceUtil.returnSuccess();
        }
        result.put("skippedReferenced", skippedReferenced);
        result.put("deleted", deleted);
        return result;
    }

    public static Map<String,Object> convertAnonShoppingListToRegistered(DispatchContext dctx, Map<String, ? extends Object> context) { // SCIPIO
        String shoppingListId = (String) context.get("shoppingListId");
        String authToken = (String) context.get("shoppingListAuthToken");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        GenericValue targetUserLogin = (GenericValue) context.get("targetUserLogin");

        // TODO: REVIEW: permission pattern + localize
        boolean isAdmin = dctx.getSecurity().hasEntityPermission("PARTYMGR", "_UPDATE", userLogin);
        if (targetUserLogin == null) {
            targetUserLogin = userLogin;
        } else {
            // must be an admin
            if (!isAdmin) {
                return ServiceUtil.returnError("Must be admin to modify another user's lists");
            }
        }
        String targetPartyId = targetUserLogin.getString("partyId");
        if (targetPartyId == null) {
            return ServiceUtil.returnError("Target user has no partyId");
        }

        // authToken required if not admin
        if (!isAdmin && UtilValidate.isEmpty(authToken)) {
            return ServiceUtil.returnError("Missing shoppingListAuthToken (required for non-admin)");
        }

        GenericValue shoppingList = dctx.getDelegator().from("ShoppingList").where("shoppingListId", shoppingListId).queryOneSafe();
        if (shoppingList == null) {
            return ServiceUtil.returnError(UtilProperties.getMessage("OrderErrorUiLabels", "OrderNoShoppingListAvailable", (Locale) context.get("locale")));
        }

        String currentAuthToken = shoppingList.getString("shoppingListAuthToken");
        if (UtilValidate.isEmpty(currentAuthToken)) {
            if (!isAdmin) {
                // only admins can change lists without auth token
                return ServiceUtil.returnError("Non-admins cannot modify lists not created with an auth token");
            } else if (UtilValidate.isNotEmpty(authToken)) {
                // always match if passed
                return ServiceUtil.returnError("shoppingListAuthToken does not match");
            }
        } else if (!(isAdmin && UtilValidate.isEmpty(authToken))) {
            // compare the auth token
            if (!currentAuthToken.equals(authToken)) {
                return ServiceUtil.returnError("shoppingListAuthToken does not match");
            }
        }

        if (UtilValidate.isNotEmpty(shoppingList.getString("partyId"))) {
            return ServiceUtil.returnError("The shopping list is already registered");
        }

        // convert
        shoppingList.put("partyId", targetPartyId);
        shoppingList.put("shoppingListAuthToken", null);
        try {
            shoppingList.store();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.toString());
        }
        return ServiceUtil.returnSuccess();
    }
}
