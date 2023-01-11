package com.ilscipio.scipio.webtools.web;

import javax.websocket.*;
import javax.websocket.server.ServerEndpoint;

import com.ilscipio.scipio.web.BrokerSessionConfigurator;
import com.ilscipio.scipio.web.CommonWebSocket;
import com.ilscipio.scipio.web.GenericWebSocket;
import com.ilscipio.scipio.web.SocketPermissionVerifier;
import org.ofbiz.base.util.Debug;


/**
 * Admin WebSocket.
 */
@ServerEndpoint(value = "/ws/{channel}/{action}", configurator = BrokerSessionConfigurator.class)
public class AdminWebSocket extends CommonWebSocket {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @Override
    protected SocketPermissionVerifier getPermissionVerifier(Session session) { return GenericWebSocket.ADMIN_PERM; }

}

