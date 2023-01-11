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
