package com.ilscipio.scipio.web.hotwire;

import com.ilscipio.scipio.web.BrokerSessionConfigurator;
import com.ilscipio.scipio.web.GenericWebSocket;
import com.ilscipio.scipio.web.WebChannelHandler;
import org.ofbiz.base.util.Debug;

import javax.websocket.EndpointConfig;
import javax.websocket.OnClose;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import javax.websocket.server.ServerEndpoint;
import java.util.Collection;

/**
 * Implements Turbo.js (https://hotwire.dev) web socket for hotwire for backend services.
 */
//@ServerEndpoint(value = "/ws-turbo/{channel}/{action}", configurator = BrokerSessionConfigurator.class)
public class TurboWebSocket extends GenericWebSocket {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @Override
    protected String getRequiredPermission(Session session) { return "OFBTOOLS"; }

    @Override
    protected TurboSocketSessionManager getSocketSessionManager(Session session) {
        return TurboSocketSessionManager.getDefault();
    }

    @Override
    protected Collection<WebChannelHandler> getChannelHandlers(WebChannelHandler.Args args) {
        return super.getChannelHandlers(args);
    }

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

    /* Currently not needed for Turbo so don't expose
    @OnMessage
    @Override
    public void onJsonMessage(String message, Session session) {
        super.onJsonMessage(message, session);
    }
     */
}
