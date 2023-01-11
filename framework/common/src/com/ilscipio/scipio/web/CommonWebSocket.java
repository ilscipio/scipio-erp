package com.ilscipio.scipio.web;

import javax.websocket.*;

/**
 * Common WebSocket with JSON message support.
 * <p>See <code>com.ilscipio.scipio.webtools.web.AdminWebSocket</code> for concrete example.</p>
 * <p>Usage: Must be extended per-application, within the java source files for the application (/src/), in order to
 * define the appropriate mount-point using <code>@ServerEndpoint</code> annotation, which can be defined below.
 * If no other functionality is needed, only the {@link #getPermissionVerifier} method should usually be
 * overridden to provide adequate rights, which defaults to the backend <code>OFBTOOLS</code>.</p>
 * <p>When <code>@ServerEndpoint</code> annotation is defined on a concrete implementation, its mount-point is
 * automatically detected and whitelisted by <code>ContextFilter</code> as part of <code>allowedPaths</code>, so
 * editing web.xml should not be necessary.</p>
 * <p>NOTE: The <code>{action}</code> parameter was previously named <code>{type}</code> (deprecated but not removed yet).</p>
 */
//@ServerEndpoint(value = "/ws/{channel}/{action}", configurator = BrokerSessionConfigurator.class)
public abstract class CommonWebSocket extends GenericWebSocket {

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

    @OnMessage
    @Override
    public void onJsonMessage(String message, Session session) {
        super.onJsonMessage(message, session);
    }

}
