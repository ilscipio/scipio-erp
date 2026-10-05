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
package com.ilscipio.scipio.web.hotwire;

import com.ilscipio.scipio.web.SocketSessionManager;
import org.ofbiz.base.util.Debug;

import javax.websocket.Session;

/**
 * Implements Turbo.js (https://hotwire.dev) web socket session manager for hotwire for backend services.
 */
public class TurboSocketSessionManager extends SocketSessionManager {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final TurboSocketSessionManager DEFAULT = new TurboSocketSessionManager();

    /** Returns the default SocketSessionManager, typically for backend use. */
    public static TurboSocketSessionManager getDefault() {
        return DEFAULT;
    }

    @Override
    protected ChannelInfo makeChannelInfo(String channelName, Session session, Object args) {
        return new TurboChannelInfo(channelName, session, args);
    }

    public class TurboChannelInfo extends ChannelInfo {
        public TurboChannelInfo(String name, Session session, Object args) {
            super(name, session, args);
        }

        @Override
        protected ClientInfo getClientInfo(Session session) {
            return super.getClientInfo(session);
        }
    }

}
