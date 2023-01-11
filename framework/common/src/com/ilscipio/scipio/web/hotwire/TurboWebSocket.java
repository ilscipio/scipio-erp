package com.ilscipio.scipio.web.hotwire;

import com.ilscipio.scipio.web.GenericWebSocket;
import com.ilscipio.scipio.web.WebChannelHandler;
import org.ofbiz.base.util.Debug;

import javax.websocket.EndpointConfig;
import javax.websocket.OnClose;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import java.util.Collection;

/**
 * Implements Turbo.js (https://hotwire.dev) web socket for hotwire for backend services.
 * <p>See <code>com.ilscipio.scipio.webtools.web.AdminTurboWebSocket</code> for concrete example.</p>
 * <p>Usage: Must be extended per-application, within the java source files for the application (/src/), in order to
 * define the appropriate mount-point using <code>@ServerEndpoint</code> annotation, which can be defined below.
 * If no other functionality is needed, only the {@link #getPermissionVerifier} method should usually be
 * overridden to provide adequate rights, which defaults to the backend <code>OFBTOOLS</code>.</p>
 * <p>When <code>@ServerEndpoint</code> annotation is defined on a concrete implementation, its mount-point is
 * automatically detected and whitelisted by <code>ContextFilter</code> as part of <code>allowedPaths</code>, so
 * editing web.xml should not be necessary.</p>
 * <p>SCIPIO: 3.0.0: Added for Hotwire/Turbo support.</p>
 */
//@ServerEndpoint(value = "/ws-turbo/{channel}/{action}", configurator = BrokerSessionConfigurator.class)
public abstract class TurboWebSocket extends GenericWebSocket {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

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

    /* Currently not needed for Turbo, so prevent @OnMessage functionality
    @OnMessage
    @Override
    public void onJsonMessage(String message, Session session) {
        super.onJsonMessage(message, session);
    }
     */

}
