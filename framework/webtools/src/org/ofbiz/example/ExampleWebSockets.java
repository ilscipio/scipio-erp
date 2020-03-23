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
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.servlet.http.HttpSession;
import javax.websocket.EndpointConfig;
import javax.websocket.OnClose;
import javax.websocket.OnMessage;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import javax.websocket.server.ServerEndpoint;

import org.ofbiz.base.util.Debug;
import org.ofbiz.security.Security;
import org.ofbiz.webapp.WebAppUtil;

/**
 * ExampleWebSockets. NOTE: 2020-03: This example is now obsolete. See GenericWebSocket and SocketSessionManager instead.
 * <p>
 * SCIPIO: 2018-10-03: Modified for custom configuration to get access to HttpSession.
 */
@ServerEndpoint(value = "/ws/pushNotifications", configurator = ExampleSessionConfigurator.class)
public class ExampleWebSockets {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    /**
     * SCIPIO: Max client count to prevent any kind of memory overload from repeated accesses
     * in case things go bad.
     * <p>
     * Added 2018-10-03.
     */
    private static final int MAX_CLIENTS = 25;

    private static final Set<Session> clients = Collections.synchronizedSet(new HashSet<Session>());

    @OnMessage
    public void onMessage(Session session, String msg, boolean last) {
        try {
            if (session.isOpen()) {
                synchronized (clients) {
                    for(Session client : clients){
                        if (!client.equals(session)){
                            client.getBasicRemote().sendText(msg);
                        }
                    }
                }
            }
        } catch (IOException e) {
            try {
                session.close();
            } catch (IOException ioe) {
                Debug.logError(ioe.getMessage(), module);
            }
        }
    }

    @OnOpen
    public void onOpen(Session session, EndpointConfig config) { // SCIPIO: 2018-10-03: Added EndpointConfig
        if (!checkClientAuthorization(session, config, "; denying client registration")) { // SCIPIO: 2018-10-03
            return;
        }
        synchronized(clients) { // SCIPIO: added block
            if (clients.size() >= MAX_CLIENTS) { // SCIPIO: 2018-10-03
                Debug.logWarning("Max websockets clients reached for example (" 
                        + MAX_CLIENTS + ")! Denying client registration websockets session " 
                        + getLogIdStr(session), module);
                return;
            }
            // Add session to the connected sessions clients set
            clients.add(session);
        }
    }

    @OnClose
    public void onClose(Session session) {
        // Remove session from the connected sessions clients set
        clients.remove(session);
    }

    public static Set<Session> getClients() {
        return clients;
    }

    /**
     * SCIPIO: An added security check to make sure the client is an admin and allowed
     * to receive messages from this example. This prevents leaking information
     * to unauthorized parties.
     * <p>
     * Added 2018-10-03.
     */
    private static boolean checkClientAuthorization(Session session, EndpointConfig config, String errorSuffix) {
        HttpSession httpSession = (HttpSession) config.getUserProperties().get(HttpSession.class.getName());
        if (httpSession == null) {
            Debug.logError("Could not get HttpSession for websockets session " 
                    + getLogIdStr(session) + " for client authorization" + errorSuffix, module);
            return false;
        }
        Security security = WebAppUtil.getSecurity(httpSession);
        if (security == null) {
            Debug.logError("Could not get Security object from HttpSession"
                    + " for websockets session " + getLogIdStr(session) + " for client authorization"
                    + errorSuffix, module);
            return false;
        }
        if (!security.hasEntityPermission("OFBTOOLS", "_VIEW", httpSession)) {
            Debug.logError("Client not authorized for OFBTOOLS_VIEW permission for websockets session " 
                    + getLogIdStr(session) +  errorSuffix, module);
            return false;
        }
        return true;
    }
    
    /**
     * SCIPIO: getLogIdStr.
     * <p>
     * WARN: TODO: REVIEW: Unclear if truly good idea security-wise to print these IDs in log, 
     * but for this example currently do not see a risk. For real applications, you may
     * want to honor the <code>requestHandler.properties#show-sessionId-in-log</code> setting.
     */
    private static String getLogIdStr(Session session) {
        return "'" + session.getId() + "'";
    }
}

