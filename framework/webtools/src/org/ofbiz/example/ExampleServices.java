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
package org.ofbiz.example;

import java.io.IOException;
import java.util.Map;
import java.util.Set;

import javax.websocket.Session;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilRandom;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ModelParam;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;

/**
 * ExampleServices.
 */
public class ExampleServices {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static Map<String, Object> sendExamplePushNotifications(DispatchContext dctx, Map<String, ? extends Object> context) {
        String exampleId = (String) context.get("exampleId");
        String message = (String) context.get("message");
        @SuppressWarnings("deprecation") // FIXME
        Set<Session> clients = ExampleWebSockets.getClients();
        try {
            synchronized (clients) {
                // SCIPIO: Give log info to show this is doing something
                String fullMessage = message + ": " + exampleId;
                Debug.logInfo("Sending example text message to " + clients.size() 
                    + " clients: \"" + fullMessage + "\"", module);
                for (Session client : clients) {
                    client.getBasicRemote().sendText(fullMessage);
                }
            }
        } catch (IOException e) {
            Debug.logError(e.getMessage(), module);
        }
        return ServiceUtil.returnSuccess();
    }

    public static Map<String, Object> testAdminService(ServiceContext ctx) {
        for(String attrName : ctx.getModelService().getInParamNames()) {
            if (ctx.containsKey(attrName)) {
                Debug.logInfo("testAdminService: " + attrName + "=" + ctx.attr(attrName), module);
            }
        }
        Map<String, Object> result = ServiceUtil.returnSuccess();
        for(ModelParam param : ctx.getModelService().getOutModelParamList()) {
            if ("String".equals(param.getType()) && !param.isInternal()) {
                result.put(param.getName(), UtilRandom.generateAlphaNumericString(20));
            }
        }
        return result;
    }
}
