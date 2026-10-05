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
package com.ilscipio.scipio.webtools.web;

import com.ilscipio.scipio.web.BrokerSessionConfigurator;
import com.ilscipio.scipio.web.GenericWebSocket;
import com.ilscipio.scipio.web.SocketPermissionVerifier;
import com.ilscipio.scipio.web.hotwire.TurboWebSocket;
import org.ofbiz.base.util.Debug;

import javax.websocket.Session;
import javax.websocket.server.ServerEndpoint;

/**
 * Implements Turbo.js (https://hotwire.dev) web socket for hotwire for admin webapp.
 */
@ServerEndpoint(value = "/ws-turbo/{channel}/{action}", configurator = BrokerSessionConfigurator.class)
public class AdminTurboWebSocket extends TurboWebSocket {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @Override
    protected SocketPermissionVerifier getPermissionVerifier(Session session) { return GenericWebSocket.ADMIN_PERM; }

}
