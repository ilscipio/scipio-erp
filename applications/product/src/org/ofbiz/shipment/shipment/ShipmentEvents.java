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
package org.ofbiz.shipment.shipment;

import java.io.IOException;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.ce.webapp.control.util.AccessTokenProvider;

/**
 * ShipmentEvents - Events used for processing shipping fees
 */
public class ShipmentEvents {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * SCIPIO: Provides safe (usually session-scoped) access to the {@link #viewShipmentPackageRouteSegLabelImageUnsafe} event
     * using a token.
     * Added 2018-10-17.
     */
    private static final AccessTokenProvider<Object> viewShipmentAccessProvider = AccessTokenProvider
            .newWeakAccessTokenProvider(AccessTokenProvider.NoopEventHandler.getInstance());

    public static String viewShipmentPackageRouteSegLabelImage(HttpServletRequest request, HttpServletResponse response) {
        return viewShipmentPackageRouteSegLabelImageImpl(request, response); // SCIPIO: 2018-10-17: now delegated
    }

    private static String viewShipmentPackageRouteSegLabelImageImpl(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");

        String shipmentId = request.getParameter("shipmentId");
        String shipmentRouteSegmentId = request.getParameter("shipmentRouteSegmentId");
        String shipmentPackageSeqId = request.getParameter("shipmentPackageSeqId");

        GenericValue shipmentPackageRouteSeg = null;
        try {
            shipmentPackageRouteSeg = EntityQuery.use(delegator).from("ShipmentPackageRouteSeg").where("shipmentId", shipmentId, "shipmentRouteSegmentId", shipmentRouteSegmentId, "shipmentPackageSeqId", shipmentPackageSeqId).queryOne();
        } catch (GenericEntityException e) {
            String errorMsg = "Error looking up ShipmentPackageRouteSeg: " + e.toString();
            Debug.logError(e, errorMsg, module);
            request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            return "error";
        }

        if (shipmentPackageRouteSeg == null) {
            request.setAttribute("_ERROR_MESSAGE_", "Could not find ShipmentPackageRouteSeg where shipmentId=[" + shipmentId + "], shipmentRouteSegmentId=[" + shipmentRouteSegmentId + "], shipmentPackageSeqId=[" + shipmentPackageSeqId + "]");
            return "error";
        }

        byte[] bytes = shipmentPackageRouteSeg.getBytes("labelImage");
        if (bytes == null || bytes.length == 0) {
            request.setAttribute("_ERROR_MESSAGE_", "The ShipmentPackageRouteSeg was found where shipmentId=[" + shipmentId + "], shipmentRouteSegmentId=[" + shipmentRouteSegmentId + "], shipmentPackageSeqId=[" + shipmentPackageSeqId + "], but there was no labelImage on the value.");
            return "error";
        }

        // TODO: record the image format somehow to make this block nicer.  Right now we're just trying GIF first as a default, then if it doesn't work, trying PNG.
        // It would be nice to store the actual type of the image alongside the image data.
        try {
            UtilHttp.streamContentToBrowser(response, bytes, "image/gif");
        } catch (IOException e1) {
            try {
                UtilHttp.streamContentToBrowser(response, bytes, "image/png");
            } catch (IOException e2) {
                String errorMsg = "Error writing labelImage to OutputStream: " + e2.toString();
                Debug.logError(e2, errorMsg, module);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                return "error";
            }
        }

        return "success";
    }

    public static String viewShipmentPackageRouteSegLabelImageUnsafe(HttpServletRequest request, HttpServletResponse response) {
        // SCIPIO: 2018-10-17: security: require access token for this request
        String accessToken = request.getParameter("accessToken");
        if (accessToken == null || viewShipmentAccessProvider.get(accessToken) == null) {
            request.setAttribute("_ERROR_MESSAGE_", "viewShipmentPackageRouteSegLabelImage was accessed without an accessToken; access denied");
            return "error";
        }
        return viewShipmentPackageRouteSegLabelImageImpl(request, response);
    }
    
    /**
     * SCIPIO: Returns an access token that can be passed as accessToken parameter to {@link #viewShipmentPackageRouteSegLabelImageUnsafe}.
     */
    public static String getShipmentViewRequestAccessTokenString(HttpServletRequest request) {
        return viewShipmentAccessProvider.getRequestToken(request, "_SCP_REQSHIPVIEWTOKEN_", new Object()).toString();
    }

    public static String checkForceShipmentReceived(HttpServletRequest request, HttpServletResponse response) {
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        GenericValue userLogin = (GenericValue)request.getSession().getAttribute("userLogin");

        String shipmentId = request.getParameter("shipmentIdReceived");
        String forceShipmentReceived = request.getParameter("forceShipmentReceived");
        if (UtilValidate.isNotEmpty(shipmentId) && "Y".equals(forceShipmentReceived)) {
            try {
                Map<String, Object> inputMap = UtilMisc.<String, Object>toMap("shipmentId", shipmentId, "statusId", "PURCH_SHIP_RECEIVED");
                inputMap.put("userLogin", userLogin);
                Map<String, Object> resultMap = dispatcher.runSync("updateShipment", inputMap);
                if (ServiceUtil.isError(resultMap)) {
                    String errorMessage = ServiceUtil.getErrorMessage(resultMap);
                    request.setAttribute("_ERROR_MESSAGE_", errorMessage);
                    Debug.logError(errorMessage, module);
                    return "error";
                }
            } catch (GenericServiceException gse) {
                String errMsg = "Error updating shipment [" + shipmentId + "]: " + gse.toString();
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        }
        return "success";
    }
}

