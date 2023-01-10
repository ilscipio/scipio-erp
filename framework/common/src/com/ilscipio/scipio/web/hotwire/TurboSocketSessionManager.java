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
