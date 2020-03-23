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
package org.ofbiz.example;

import java.io.IOException;
import java.util.Map;
import java.util.Set;

import javax.websocket.Session;

import org.ofbiz.base.util.Debug;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

/** ExampleServices. NOTE: 2020-03: This example is now obsolete. See GenericWebSocket and SocketSessionManager instead. */
public class ExampleServices {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static Map<String, Object> sendExamplePushNotifications(DispatchContext dctx, Map<String, ? extends Object> context) {
        String exampleId = (String) context.get("exampleId");
        String message = (String) context.get("message");
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
}
