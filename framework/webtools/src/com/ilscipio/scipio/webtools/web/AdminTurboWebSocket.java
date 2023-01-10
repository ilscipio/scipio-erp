package com.ilscipio.scipio.webtools.web;

import com.ilscipio.scipio.web.BrokerSessionConfigurator;
import com.ilscipio.scipio.web.hotwire.TurboWebSocket;
import org.ofbiz.base.util.Debug;

import javax.websocket.EndpointConfig;
import javax.websocket.OnClose;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import javax.websocket.server.ServerEndpoint;

/**
 * Implements Turbo.js (https://hotwire.dev) web socket for hotwire for backend services.
 */
@ServerEndpoint(value = "/ws-turbo/{channel}/{action}", configurator = BrokerSessionConfigurator.class)
public class AdminTurboWebSocket extends TurboWebSocket {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @Override
    protected String getRequiredPermission(Session session) { return "OFBTOOLS"; }

    @OnOpen
    @Override
    public void onOpen(Session session, EndpointConfig config) {
        super.onOpen(session, config);
    }

    @OnClose
    @Override
    public void onClose(Session session) {
        super.onClose(session);
    }

}
