/*
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
 */

import java.text.SimpleDateFormat

import javolution.util.FastList

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator
import org.ofbiz.product.product.ProductContentWrapper

int iCount = context.chartIntervalCount != null ? context.chartIntervalCount : 0;
String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "month"; //day|week|month|year

if (iCount <= 0)
    iCount = UtilDateTime.getIntervalDefaultCount(iScope);
fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);
dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);
//context.dateBeginText = UtilDateTime.toDateString(dateIntervals.getDateBegin());

exprList = [];
exprList.add(EntityCondition.makeCondition("orderTypeId", EntityOperator.EQUALS, "SALES_ORDER"));
exprList.add(EntityCondition.makeCondition("orderStatusId", EntityOperator.NOT_EQUAL, "ORDER_CANCELLED"));
exprList.add(EntityCondition.makeCondition("orderItemTypeId", "PRODUCT_ORDER_ITEM"));
exprList.add(EntityCondition.makeCondition("itemStatusId", EntityOperator.NOT_EQUAL, "ITEM_CANCELLED"));
//    exprList.add(EntityCondition.makeCondition("isPromo", "N"));

bestSellingProductsByDate = [:];
for (int i = 0; i < iCount; i++) {
    dateIntervals.setDateFormatter(new SimpleDateFormat("yyyy-MM-dd"));
    
    bestSellingProducts = [:];
    orderDateExprList = [];
    orderDateExprList.add(EntityCondition.makeCondition("orderDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals.getDateBegin()));
    orderDateExprList.add(EntityCondition.makeCondition("orderDate", EntityOperator.LESS_THAN_EQUAL_TO, dateIntervals.getDateEnd()));

    orderHeaderItemExpr = FastList.newInstance(exprList);
    orderHeaderItemExpr.addAll(orderDateExprList);    
    orderHeaderItemList = from("OrderHeaderAndItems").where(orderHeaderItemExpr).queryList();
   
    orderHeaderItemList.each { orderHeaderItem ->
        qtyOrdered = BigDecimal.ZERO;
        qtyOrdered += orderHeaderItem.quantity;
        if (orderHeaderItem.cancelQuantity) {
            qtyOrdered -= orderHeaderItem.cancelQuantity;
        }
        amount = BigDecimal.ZERO;;
        amount = qtyOrdered * orderHeaderItem.unitPrice;
        orderItemDetail = bestSellingProducts[orderHeaderItem.productId];
        if ((orderItemDetail != null) && (orderItemDetail.currencyUom).equals(orderHeaderItem.currencyUom)) {
            orderItemDetail.amount += amount;
            orderItemDetail.qtyOrdered += qtyOrdered;
        } else {
            orderItemDetail = [:];
            orderItemDetail.productId = orderHeaderItem.productId;
            product = from("Product").where("productId", orderHeaderItem.productId).queryOne()
            contentWrapper = new ProductContentWrapper(product, request);
            // Scipo: Do NOT HTML-escape this here
            orderItemDetail.productName = contentWrapper.get("PRODUCT_NAME");
            orderItemDetail.amount = amount;
            orderItemDetail.qtyOrdered = qtyOrdered;
            orderItemDetail.currencyUom = orderHeaderItem.currencyUom;
            bestSellingProducts.put(product.productId, orderItemDetail);
        }
    }    
    bestSellingProductsByDate.put(dateIntervals, bestSellingProducts);
    if (i + 1 < iCount)
        dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, dateIntervals.getDateEnd(), context.locale, context.timeZone);
}
//context.dateEndText = UtilDateTime.toDateString(dateIntervals.getDateEnd());

// Sorting List
topSellingProductsMap = [:];

for (key in bestSellingProductsByDate.keySet()) {    
    topSellingProducts = [];
    bestSellingProductsByDate[key].each { bestSellingProducts ->
        productId =  bestSellingProducts.getKey();
        bestSellingProduct =  bestSellingProducts.getValue(); 
        if (topSellingProducts && topSellingProducts.size() == 5) {
            for (int idx = 0; idx < topSellingProducts.size(); idx++) {
                if ((bestSellingProduct.qtyOrdered > topSellingProducts[idx].qtyOrdered) || 
                    (bestSellingProduct.qtyOrdered == topSellingProducts[idx].qtyOrdered && bestSellingProduct.amount > topSellingProducts[idx].amount)) {
                    topSellingProducts[idx] = bestSellingProduct; 
                    break;
                }
            }
        } else {
            topSellingProducts.add(bestSellingProduct);
        }
    }
    topSellingProductsMap.put(key, topSellingProducts);
}


context.bestSellingProducts = topSellingProductsMap;
context.now = UtilDateTime.toDateString(UtilDateTime.nowDate());
