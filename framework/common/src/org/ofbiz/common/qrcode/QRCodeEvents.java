/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
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

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.image.storer.ImageStorers;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;


/**
 * Events for QRCode.
 */
public class QRCodeEvents {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /** Streams QR Code to the output. */
    public static String serveQRCodeImage(HttpServletRequest request, HttpServletResponse response) {
        HttpSession session = request.getSession();
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Delegator delegator = (Delegator) request.getAttribute("delegator");
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
        String logo = null;
        Boolean useLogo = null; // NOTE: stock ofbiz generateQRCodeImage service default is true
        if ("true".equals(logoArg)) {
            useLogo = true;
        } else if ("false".equals(logoArg)) {
            useLogo = false;
        } else if (UtilValidate.isNotEmpty(logoArg)) {
            useLogo = true;
            logo = QRCodeLogoRegistry.getLocationForId(delegator, logoArg);
            if (logo == null) {
                Debug.logWarning("QRCode: Could not find logo for ID '" + logoArg 
                        + "'; it is usually setup automatically by the @qrcode macro"
                        + "; if accessing a different way, you can pre-whitelist using qrcode[-custom].properties or QRCodeLogoRegistry", module);
                useLogo = false;
            }
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
            if (logo != null) {
                context.put("logoImage", logo);
            }
            context.put("ecLevel", ecLevel); // SCIPIO: 2018-08-22
            context.put("logoImageSize", logoImageSize); // SCIPIO: 2018-08-23
            context.put("logoImageMaxSize", logoImageMaxSize); // SCIPIO: 2018-08-23
            Map<String, Object> results = dispatcher.runSync("generateQRCodeImage", context);
            if (ServiceUtil.isSuccess(results)) {
                BufferedImage bufferedImage = (BufferedImage) results.get("bufferedImage");
                if (!ImageStorers.write(bufferedImage, format, os, "IMAGE_QRCODE", null, delegator)) { // SCIPIO: ImageIO->ImageStorers
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
