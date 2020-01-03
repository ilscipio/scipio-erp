package com.ilscipio.scipio.order.web;

import com.ilscipio.scipio.web.SocketSessionManager;
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
                    BigDecimal amount = new BigDecimal(sale.getString("totalGrandAmount"));
                    totalAmount.add(amount).setScale(decimals, rounding);
                    BigDecimal subamount = new BigDecimal(sale.getString("totalSubRemainingAmount"));
                    totalSubRemainingAmount.add(subamount).setScale(decimals, rounding);
                    totalOrders+= sale.getLong("totalOrders");
                }

                Map orderGlobal = UtilMisc.toMap(
                        "dateTime",begin,
                        "totalAmount",totalAmount.setScale(decimals, rounding)
                        ,"totalSubRemainingAmount",totalSubRemainingAmount,
                        "totalOrders",totalOrders
                );

                Map orderMap =  UtilMisc.toMap(
                        "dateTime",orderDate,
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
}
