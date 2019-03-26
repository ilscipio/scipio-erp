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
package org.ofbiz.order.order;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.io.IOUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.content.data.DataResourceWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Order Events
 */
public class OrderEvents {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static String downloadDigitalProduct(HttpServletRequest request, HttpServletResponse response) {
        HttpSession session = request.getSession();
        ServletContext application = session.getServletContext();
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        GenericValue userLogin = (GenericValue) session.getAttribute("userLogin");
        String dataResourceId = request.getParameter("dataResourceId");

        try {
            // has the userLogin.partyId ordered a product with DIGITAL_DOWNLOAD content associated for the given dataResourceId?
            GenericValue orderRoleAndProductContentInfo = EntityQuery.use(delegator).from("OrderRoleAndProductContentInfo")
                    .where("partyId", userLogin.get("partyId"),
                            "dataResourceId", dataResourceId,
                            "productContentTypeId", "DIGITAL_DOWNLOAD",
                            "statusId", "ITEM_COMPLETED")
                    .queryFirst();

            if (orderRoleAndProductContentInfo == null) {
                request.setAttribute("_ERROR_MESSAGE_", "No record of purchase for digital download found (dataResourceId=[" + dataResourceId + "]).");
                return "error";
            }


            // TODO: check validity based on ProductContent fields: useCountLimit, useTime/useTimeUomId

            if (orderRoleAndProductContentInfo.getString("mimeTypeId") != null) {
                response.setContentType(orderRoleAndProductContentInfo.getString("mimeTypeId"));
            }
            OutputStream os = response.getOutputStream();
            GenericValue dataResource = EntityQuery.use(delegator).from("DataResource").where("dataResourceId", dataResourceId).cache().queryOne();
            Map<String, Object> resourceData = DataResourceWorker.getDataResourceStream(dataResource, "", application.getInitParameter("webSiteId"), UtilHttp.getLocale(request), application.getRealPath("/"), false);
            os.write(IOUtils.toByteArray((InputStream) resourceData.get("stream")));
            os.flush();
        } catch (GeneralException | IOException e) {
            String errMsg = "Error downloading digital product content: " + e.toString();
            Debug.logError(e, errMsg, module);
            request.setAttribute("_ERROR_MESSAGE_", errMsg);
            return "error";
        }

        return "success";
    }

    public static String cancelSelectedOrderItems(HttpServletRequest request, HttpServletResponse response) {
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        HttpSession session = request.getSession();
        GenericValue userLogin = (GenericValue) session.getAttribute("userLogin");
        Locale locale = UtilHttp.getLocale(request);

        Map<String, Object> resultMap;
        String  orderId = request.getParameter("orderId");
        String[] selectedItems = request.getParameterValues("selectedItem");

        // SCIPIO: 2018-11: support alternative way to specify selectedItem
        // WARN: security: selectedItemParamPrefix can only be used to extract parameters, NOT to extract request attribute values (future)
        String selectedItemParamPrefix = request.getParameter("selectedItemParamPrefix");
        if (UtilValidate.isNotEmpty(selectedItemParamPrefix)) {
            List<String> selectedItemsList = UtilHttp.getParameterNamesWithValue(request, "Y", selectedItemParamPrefix);
            selectedItems = selectedItemsList.toArray(new String[selectedItemsList.size()]);
        }

        if (UtilValidate.isNotEmpty(selectedItems)) { // SCIPIO: isNotEmpty instead of null check
            for (String selectedItem : selectedItems) {
                String [] orderItemSeqIdAndOrderItemShipGrpId = selectedItem.split(":");
                String orderItemSeqId = orderItemSeqIdAndOrderItemShipGrpId[0];
                // SCIPIO: make shipGroupSeqId optional
                //String shipGroupSeqId = orderItemSeqIdAndOrderItemShipGrpId[1];
                String shipGroupSeqId = (orderItemSeqIdAndOrderItemShipGrpId.length >= 2) ? orderItemSeqIdAndOrderItemShipGrpId[1] : null;

                // SCIPIO: shipGroupSeqId and iqm_ parameter may be omitted for us, so this gave an NPE
                //BigDecimal cancelQuantity = new BigDecimal(request.getParameter("iqm_"+orderItemSeqId+":"+shipGroupSeqId));
                BigDecimal cancelQuantity = null;
                if (UtilValidate.isNotEmpty(shipGroupSeqId)) {
                    String iqmValue = request.getParameter("iqm_"+orderItemSeqId+":"+shipGroupSeqId);
                    if (UtilValidate.isNotEmpty(iqmValue)) {
                        cancelQuantity = new BigDecimal(iqmValue);
                    }
                }

                Map<String, Object> contextMap = new HashMap<>();
                contextMap.put("orderId", orderId);
                contextMap.put("orderItemSeqId", orderItemSeqId);
                contextMap.put("shipGroupSeqId", shipGroupSeqId);
                contextMap.put("cancelQuantity", cancelQuantity);
                contextMap.put("userLogin", userLogin);
                contextMap.put("locale", locale);
                try {
                    resultMap = dispatcher.runSync("cancelOrderItem", contextMap);
                    if (ServiceUtil.isError(resultMap)) {
                        String errorMessage = ServiceUtil.getErrorMessage(resultMap);
                        request.setAttribute("_ERROR_MESSAGE_", errorMessage);
                        Debug.logError(errorMessage, module);
                        return "error";
                    }
                } catch (GenericServiceException e) {
                    Debug.logError(e, module);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            return "success";
        }
        request.setAttribute("_ERROR_MESSAGE_", "No order item selected. Please select an order item to cancel");
        return "error";
    }
}
