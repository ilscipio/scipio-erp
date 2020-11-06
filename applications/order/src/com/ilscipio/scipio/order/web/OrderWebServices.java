package com.ilscipio.scipio.order.web;

import com.ilscipio.scipio.web.SocketSessionManager;
import de.bripkens.gravatar.DefaultImage;
import de.bripkens.gravatar.Gravatar;
import de.bripkens.gravatar.Rating;
import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.*;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.order.order.OrderReadHelper;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;
import org.springframework.core.annotation.OrderUtils;
import sun.net.www.content.text.Generic;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

public class OrderWebServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final int decimals = UtilNumber.getBigDecimalScale("invoice.decimals");
    public static final RoundingMode rounding = UtilNumber.getRoundingMode("invoice.rounding");
    public static final BigDecimal ZERO = BigDecimal.ZERO.setScale(decimals, rounding);

    public static Map<String, Object> sendOrderLiveData(DispatchContext dctx, Map<String, ? extends Object> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Delegator delegator = dctx.getDelegator();
        Map<String, Object> result = ServiceUtil.returnSuccess();
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        String orderId = (String) context.get("orderId");
        String channel = (String) context.get("channel");
        OrderReadHelper orh = new OrderReadHelper(delegator, orderId);

        String interval = ((String) context.get("interval")).toLowerCase();
        String webSiteId = ((String) context.get("webSiteId"));
        String productStoreId = ((String) context.get("productStoreId"));
        String terminalId = ((String) context.get("terminalId"));

        try {

                Map orderData = new HashMap();
                Timestamp orderDate = orh.getOrderHeader().getTimestamp("orderDate");
                LocalDateTime orderDateTime = orderDate.toLocalDateTime();
                Timestamp nowTimestamp = UtilDateTime.nowTimestamp();

                TimeZone timeZone = TimeZone.getDefault();
                Locale locale = Locale.getDefault();
                Timestamp begin = orderDate;

                switch (interval){
                    case "hour":
                        begin = UtilDateTime.getHourStart(nowTimestamp, 0, timeZone, locale);
                        break;

                    case "day":
                        begin = UtilDateTime.getDayStart(nowTimestamp, 0, timeZone, locale);
                        break;

                    case "week":
                        begin = UtilDateTime.getWeekStart(nowTimestamp, 0, timeZone, locale);
                        break;

                    case "month":
                        begin = UtilDateTime.getMonthStart(nowTimestamp, 0, timeZone, locale);
                        break;

                    case "year":
                        begin = UtilDateTime.getYearStart(nowTimestamp, 0, timeZone, locale);
                        break;

                    default:
                        begin = UtilDateTime.getDayStart(nowTimestamp, 0, timeZone, locale);
                }

                List<EntityCondition> curConditions = UtilMisc.toList(
                        EntityCondition.makeCondition("statusId", EntityOperator.NOT_EQUAL, "ORDER_CANCELLED"),
                        EntityCondition.makeCondition("orderTypeId", EntityOperator.EQUALS, "SALES_ORDER"),
                        EntityCondition.makeCondition("orderDate", EntityOperator.GREATER_THAN_EQUAL_TO, begin),
                        EntityCondition.makeCondition("orderDate", EntityOperator.LESS_THAN_EQUAL_TO, orderDate)
                );

                if(UtilValidate.isNotEmpty(productStoreId)){
                    curConditions.add(EntityCondition.makeCondition("productStoreId", EntityOperator.EQUALS, productStoreId));
                }

                if(UtilValidate.isNotEmpty(webSiteId)){
                    curConditions.add(EntityCondition.makeCondition("webSiteId", EntityOperator.EQUALS, webSiteId));
                }

                if(UtilValidate.isNotEmpty(terminalId)){
                    curConditions.add(EntityCondition.makeCondition("terminalId", EntityOperator.EQUALS, terminalId));
                }

                EntityCondition ecl = EntityCondition.makeCondition(curConditions, EntityOperator.AND);
                List<GenericValue> sales = delegator.findList("OrderStats",ecl,
                        UtilMisc.toSet("totalGrandAmount","totalOrders","totalSubRemainingAmount")
                        ,null
                        ,null,false);
                BigDecimal totalAmount =  ZERO;
                BigDecimal totalSubRemainingAmount = ZERO;
                Long totalOrders = Long.valueOf(0);
                for(GenericValue sale : sales){
                    BigDecimal amount = sale.getBigDecimal("totalGrandAmount");
                    totalAmount = totalAmount.add(amount).setScale(decimals, rounding);
                    BigDecimal subamount = sale.getBigDecimal("totalSubRemainingAmount");
                    totalSubRemainingAmount =totalSubRemainingAmount.add(subamount).setScale(decimals, rounding);
                    totalOrders+= sale.getLong("totalOrders");
                }

                Map orderGlobal = UtilMisc.toMap(
                        "dateTime",begin.toLocalDateTime(),
                        "dateTimeStr",begin.toLocalDateTime().toString(),
                        "totalAmount",totalAmount.setScale(decimals, rounding)
                        ,"totalSubRemainingAmount",totalSubRemainingAmount,
                        "totalOrders",totalOrders
                );

                Map orderMap =  UtilMisc.toMap(
                        "dateTime",orderDateTime,
                        "dateTimeStr",orderDateTime.toString(),
                        "totalAmount", orh.getOrderGrandTotal()
                        ,"totalAdjustments",orh.getOrderAdjustmentsTotal(),
                        "totalQuantity",orh.getTotalOrderItemsQuantity()
                );

                orderData.put("global",orderGlobal); //Adding global information to results
                orderData.put("order",orderMap);

                JSON obj = JSON.from(orderData);
                SocketSessionManager.broadcastToChannel(obj.toString(),channel);

        }catch(Exception e){
            Debug.logError("Error while sending order data to websocket",module);
            return ServiceUtil.returnError("Error while sending order data to websocket");
        }

        return result;
    }

    public static Map<String, Object> sendOrder(DispatchContext dctx, Map<String, ? extends Object> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Delegator delegator = dctx.getDelegator();
        Map<String, Object> result = ServiceUtil.returnSuccess();
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        String orderId = (String) context.get("orderId");
        String channel = (String) context.get("channel");
        OrderReadHelper orh = new OrderReadHelper(delegator, orderId);

        try {

            List<GenericValue> items = orh.getOrderItems();
            List orderItems = new ArrayList();
            for(GenericValue item : items){

                String changeByUserLoginId = item.getString("changeByUserLoginId");
                String gravatarImageURL = null;

                try{
                    if(UtilValidate.isNotEmpty(changeByUserLoginId)){
                        List<GenericValue> parties = delegator.findByAnd("PartyAndUserLogin",UtilMisc.toMap("userLoginId",changeByUserLoginId),null,true);
                        if(UtilValidate.isNotEmpty(parties)){
                            //Select first party from list
                            GenericValue party = parties.get(0);
                            Map<String,Object> userLoginEmail = dispatcher.runSync("getPartyEmail",UtilMisc.toMap("partyId",party.getString("partyId"),"userLogin",userLogin));
                            if(UtilValidate.isNotEmpty(userLoginEmail)){
                                String emailAddress = (String) userLoginEmail.get("emailAddress");
                                Map<String, Object> gravatar = dispatcher.runSync("getGravatarImage",UtilMisc.toMap("emailAddress",emailAddress,"size",200,"userLogin",userLogin));
                                if(UtilValidate.isNotEmpty(gravatar)){
                                    gravatarImageURL = (String) gravatar.get("gravatarImageUrl");
                                }
                            }
                        }

                    }
                }catch(Exception e){
                    Debug.logWarning("Exception while fetching gravatar image",module);
                }

                Map itemMap = UtilMisc.toMap(
                        "orderItemSeqId", item.getString("orderItemSeqId"),
                        "productId",item.getString("productId"),
                        "status",orh.getCurrentItemStatus(item),
                        "changeByUserLoginId", item.getString("changeByUserLoginId"),
                        "gravatarImageURL",gravatarImageURL,
                        "workEffort",orh.getCurrentOrderItemWorkEffort(item),
                        "pendingShipment", orh.getItemPendingShipmentQuantity(item),
                        "pickedQuantity", orh.getItemPickedQuantityBd(item),
                        "shipped",orh.getItemShippedQuantity(item),
                        "total",orh.getOrderItemTotal(item)
                );
                orderItems.add(itemMap);
            }


            Map orderMap =  UtilMisc.toMap(
                    "orderDate", orh.getOrderHeader().getString("orderDate"),
                    "customerPartyId",orh.getBillToPartyId(),
                    "orderId", orderId,
                    "status",orh.getCurrentStatusString(),
                    "statusId",orh.getOrderHeader().getString("statusId"),
                    "affiliateId",orh.getAffiliateId(),
                    "shippingLocations",orh.getShippingLocations(),
                    "totalAmount", orh.getOrderGrandTotal(),
                    "totalAdjustments",orh.getOrderAdjustmentsTotal(),
                    "totalQuantity",orh.getTotalOrderItemsQuantity(),
                    "orderItems",orderItems
            );


            JSON obj = JSON.from(orderMap);
            SocketSessionManager.broadcastToChannel(obj.toString(),channel);

        }catch(Exception e){
            Debug.logError("Error while sending order data to websocket",module);
            return ServiceUtil.returnError("Error while sending order data to websocket");
        }

        return result;
    }

    public static Map<String, Object> sendOrderItem(DispatchContext dctx, Map<String, ? extends Object> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Delegator delegator = dctx.getDelegator();
        Map<String, Object> result = ServiceUtil.returnSuccess();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Locale locale = (Locale) context.get("locale");

        String orderId = (String) context.get("orderId");
        String orderItemSeqId = (String) context.get("orderItemSeqId");
        String channel = (String) context.get("channel");

        try {
            GenericValue item = delegator.findOne("OrderItem",true,UtilMisc.toMap("orderId",orderId,"orderItemSeqId",orderItemSeqId));

            if(UtilValidate.isNotEmpty(item)){

                String changeByUserLoginId = item.getString("changeByUserLoginId");
                String gravatarImageURL = null;
                try{
                    if(UtilValidate.isNotEmpty(changeByUserLoginId)){
                        List<GenericValue> parties = delegator.findByAnd("PartyAndUserLogin",UtilMisc.toMap("userLoginId",changeByUserLoginId),null,true);
                        if(UtilValidate.isNotEmpty(parties)){
                            //Select first party from list
                            GenericValue party = parties.get(0);
                            Map<String,Object> userLoginEmail = dispatcher.runSync("getPartyEmail",UtilMisc.toMap("partyId",party.getString("partyId"),"userLogin",userLogin));
                            if(UtilValidate.isNotEmpty(userLoginEmail)){
                                String emailAddress = (String) userLoginEmail.get("emailAddress");
                                Map<String, Object> gravatar = dispatcher.runSync("getGravatarImage",UtilMisc.toMap("emailAddress",emailAddress,"size",200,"userLogin",userLogin));
                                if(UtilValidate.isNotEmpty(gravatar)){
                                    gravatarImageURL = (String) gravatar.get("gravatarImageUrl");
                                }
                            }
                        }

                    }
                }catch(Exception e){
                    Debug.logWarning("Exception while fetching gravatar image",module);
                }

                GenericValue statusItem = item.getRelatedOneCache("StatusItem");
                String statusStr = statusItem.getString("description",locale);

                Map itemMap = UtilMisc.toMap(
                        "orderId",orderId,
                        "orderItemSeqId", orderItemSeqId,
                        "productId",item.getString("productId"),
                        "status",statusStr,
                        "statusId",item.getString("statusId"),
                        "changeByUserLoginId", item.getString("changeByUserLoginId"),
                        "gravatarImageURL",gravatarImageURL
                );
                JSON obj = JSON.from(itemMap);
                SocketSessionManager.broadcastToChannel(obj.toString(),channel);
            }
        }catch(Exception e){
            Debug.logError("Error while sending order data to websocket",module);
            return ServiceUtil.returnError("Error while sending order data to websocket");
        }

        return result;
    }

}
