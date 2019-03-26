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
package org.ofbiz.webapp.control;

import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionActivationListener;
import javax.servlet.http.HttpSessionEvent;

import org.ofbiz.base.util.Debug;

/**
 * HttpSessionListener that gathers and tracks various information and statistics
 */
public class ControlActivationEventListener implements HttpSessionActivationListener {
    // Debug module name
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    // SCIPIO: New static var for sessionId show in log

    public ControlActivationEventListener() {}

    public void sessionWillPassivate(HttpSessionEvent event) {
        ControlEventListener.countPassivateSession();
        if (Debug.infoOn()) {
            Debug.logInfo("Passivating session:" + showSessionId(event.getSession()), module);
        }
    }

    public void sessionDidActivate(HttpSessionEvent event) {
        ControlEventListener.countActivateSession();
        if (Debug.infoOn()) {
            Debug.logInfo("Activating session:" + showSessionId(event.getSession()), module);
        }
    }

    public static String showSessionId(HttpSession session) {
        // SCIPIO: simplified
        return " sessionId=" + getSessionIdForLog(session);
    }

    /**
     * SCIPIO: Returns the session ID itself, for log display, without space and prefix; 
     * if no session, returns "[none]"; if hidden, returns "[hidden]".
     */
    public static String getSessionIdForLog(HttpSession session) {
        return RequestHandler.getSessionIdForLog(session);
    }
}
