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
package org.ofbiz.common.qrcode;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Locale;
import java.util.Map;

import javax.imageio.ImageIO;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Events for QRCode.
 */
public class QRCodeEvents {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /** Streams QR Code to the output. */
    public static String serveQRCodeImage(HttpServletRequest request, HttpServletResponse response) {
        HttpSession session = request.getSession();
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Map<String, Object> parameters = UtilHttp.getParameterMap(request);
        String message = (String) parameters.get("message");
        GenericValue userLogin = (GenericValue) request.getAttribute("userLogin");
        if (userLogin == null) {
            userLogin = (GenericValue) session.getAttribute("userLogin");
        }
        if (userLogin == null) {
            userLogin = (GenericValue) session.getAttribute("autoUserLogin");
        }
        Locale locale = UtilHttp.getLocale(request);

        if (UtilValidate.isEmpty(message)) {
            message = "Error get message parameter.";
        }
        String format = (String) parameters.get("format");
        if (UtilValidate.isEmpty(format)) {
            // SCIPIO: 2018-08-22: respect qrcode.properties default here
            //format = "jpg";
            format = QRCodeServices.QRCODE_DEFAULT_FORMAT;
        }
        String mimeType = "image/" + format;
        String width = (String) parameters.get("width");
        String height = (String) parameters.get("height");
        String encoding = (String) parameters.get("encoding");
        Boolean verifyOutput = Boolean.valueOf((String) parameters.get("verifyOutput"));
        String logoImageMaxWidth = (String) parameters.get("logoImageMaxWidth");
        String logoImageMaxHeight = (String) parameters.get("logoImageMaxHeight");

        // SCIPIO: 2018-08-22: new parameters
        String logoArg = (String) parameters.get("logo");
        //String logo = null;
        Boolean useLogo = null; // NOTE: stock ofbiz generateQRCodeImage service default is true
        if ("true".equals(logoArg)) {
            useLogo = true;
        } else if ("false".equals(logoArg)) {
            useLogo = false;
        } else if (UtilValidate.isNotEmpty(logoArg)) {
            useLogo = true;
            // FIXME: security risk, cannot be specified over request - needs strict mapping system
            //logo = logoArg;
        }
        String ecLevel = request.getParameter("ecLevel");
        String logoImageSize = request.getParameter("logoSize");
        String logoImageMaxSize = request.getParameter("logoMaxSize"); // takes priority over logoImageMaxWidth/Height

        try {
            response.setContentType(mimeType);
            OutputStream os = response.getOutputStream();
            Map<String, Object> context = UtilMisc.<String, Object>toMap("message", message, "format", format, "userLogin", userLogin, "locale", locale);
            if (UtilValidate.isNotEmpty(width)) {
                try {
                    context.put("width", Integer.parseInt(width));
                } catch (NumberFormatException e) {
                    // do nothing
                }
                if (UtilValidate.isEmpty(height)) {
                    try {
                        context.put("height", Integer.parseInt(width));
                    } catch (NumberFormatException e) {
                        // do nothing
                    }
                }
            }
            if (UtilValidate.isNotEmpty(height)) {
                try {
                    context.put("height", Integer.parseInt(height));
                } catch (NumberFormatException e) {
                    // do nothing
                }
                if (UtilValidate.isEmpty(width)) {
                    try {
                        context.put("width", Integer.parseInt(height));
                    } catch (NumberFormatException e) {
                        // do nothing
                    }
                }
            }
            if (UtilValidate.isNotEmpty(encoding)) {
                context.put("encoding", encoding);
            }
            if (UtilValidate.isNotEmpty(verifyOutput) && verifyOutput) {
                context.put("verifyOutput", verifyOutput);
            }
            if (UtilValidate.isNotEmpty(logoImageMaxWidth)) {
                try {
                    context.put("logoImageMaxWidth", Integer.parseInt(logoImageMaxWidth));
                } catch (NumberFormatException e) {
                    // do nothing
                }
            }
            if (UtilValidate.isNotEmpty(logoImageMaxHeight)) {
                try {
                    context.put("logoImageMaxHeight", Integer.parseInt(logoImageMaxHeight));
                } catch (NumberFormatException e) {
                    // do nothing
                }
            }
            context.put("useLogo", useLogo); // SCIPIO: 2018-08-22
            // SCIPIO: TODO?: future
            //if (logo != null) {
            //    context.put("logoImage", ...);
            //}
            context.put("ecLevel", ecLevel); // SCIPIO: 2018-08-22
            context.put("logoImageSize", logoImageSize); // SCIPIO: 2018-08-23
            context.put("logoImageMaxSize", logoImageMaxSize); // SCIPIO: 2018-08-23
            Map<String, Object> results = dispatcher.runSync("generateQRCodeImage", context);
            if (ServiceUtil.isSuccess(results)) {
                BufferedImage bufferedImage = (BufferedImage) results.get("bufferedImage");
                if (!ImageIO.write(bufferedImage, format, os)) {
                    String errMsg = UtilProperties.getMessage("QRCodeUiLabels", "ErrorWriteFormatToFile", new Object[] { format }, locale);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                os.flush();
            } else {
                String errMsg = ServiceUtil.getErrorMessage(results);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (IOException | GenericServiceException e) {
            String errMsg = UtilProperties.getMessage("QRCodeUiLabels", "ErrorGenerateQRCode", new Object[] { e.getMessage() }, locale);
            request.setAttribute("_ERROR_MESSAGE_", errMsg);
            return "error";
        }
        return "success";
    }
}
