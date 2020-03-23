package com.ilscipio.scipio.webtools.web;

import java.util.*;

import javax.websocket.*;
import javax.websocket.server.ServerEndpoint;

import com.ilscipio.scipio.web.BrokerSessionConfigurator;
import com.ilscipio.scipio.web.GenericWebSocket;
import com.ilscipio.scipio.web.SocketSessionManager;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;


/**
 * Order WebSocket.
 */
@ServerEndpoint(value = "/ws/{channel}/{type}", configurator = BrokerSessionConfigurator.class)
public class AdminWebSocket extends GenericWebSocket {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected String getRequiredPermission() { return "OFBTOOLS"; }

    @OnOpen
    public void onOpen(Session session, EndpointConfig config) {
        super.onOpen(session, config);
    }

    @OnMessage
    public void onJsonMessage(String message, Session session) {
        super.onJsonMessage(message, session);
    }
}

