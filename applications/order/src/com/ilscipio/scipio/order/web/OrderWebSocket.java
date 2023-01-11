package com.ilscipio.scipio.order.web;

import javax.websocket.*;
import javax.websocket.server.ServerEndpoint;

import com.ilscipio.scipio.web.BrokerSessionConfigurator;
import com.ilscipio.scipio.web.CommonWebSocket;
import com.ilscipio.scipio.web.SocketPermissionVerifier;
import org.ofbiz.base.util.Debug;

/**
 * Order WebSocket.
 */
@ServerEndpoint(value = "/ws/{channel}/{action}", configurator = BrokerSessionConfigurator.class)
public class OrderWebSocket extends CommonWebSocket {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final SocketPermissionVerifier ORDER_PERM = new SocketPermissionVerifier.EntityViewOr("ORDERMGR");

    @Override
    protected SocketPermissionVerifier getPermissionVerifier(Session session) { return ORDER_PERM; }

}

