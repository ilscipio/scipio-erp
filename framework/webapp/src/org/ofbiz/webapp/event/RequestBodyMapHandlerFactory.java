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
package org.ofbiz.webapp.event;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletRequest;

/**
 * Factory class that provides the proper <code>RequestBodyMapHandler</code> based on the content type of the <code>ServletRequest</code>.
 * <p>SCIPIO: NOTE: 2020-10: This no longer runs on ContextFilter; rather integrated into service handlers and screen "parameters"
 * map, while other exceptions must be managed by controller.</p>
 */
public class RequestBodyMapHandlerFactory {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private final static Map<String, RequestBodyMapHandler> requestBodyMapHandlers = new HashMap<String, RequestBodyMapHandler>();
    static {
        requestBodyMapHandlers.put("application/json", new JSONRequestBodyMapHandler());
    }

    /**
     * Returns the request body map, checking request attribute requestBodyMap to see if already parsed (SCIPIO).
     */
    public static Map<String, Object> getRequestBodyMap(ServletRequest request) {
        Map<String, Object> requestBodyMap = UtilGenerics.cast(request.getAttribute("requestBodyMap"));
        if (requestBodyMap == null) {
            try {
                requestBodyMap = RequestBodyMapHandlerFactory.extractMapFromRequestBody(request);
            } catch (IOException ioe) {
                Debug.logWarning(ioe, module);
            }
            if (requestBodyMap == null) {
                requestBodyMap = Collections.emptyMap();
            }
            request.setAttribute("requestBodyMap", requestBodyMap);
        }
        return requestBodyMap;
    }

    public static RequestBodyMapHandler getRequestBodyMapHandler(ServletRequest request) {
        String contentType = request.getContentType();
        if (contentType != null && contentType.indexOf(";") != -1) {
            contentType = contentType.substring(0, contentType.indexOf(";"));
        }
        return requestBodyMapHandlers.get(contentType);
    }

    public static Map<String, Object> extractMapFromRequestBody(ServletRequest request) throws IOException {
        Map<String, Object> outputMap = null;
        RequestBodyMapHandler handler = getRequestBodyMapHandler(request);
        if (handler != null) {
            outputMap = handler.extractMapFromRequestBody(request);
        }
        return outputMap;
    }
}
