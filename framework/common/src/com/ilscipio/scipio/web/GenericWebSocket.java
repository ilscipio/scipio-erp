package com.ilscipio.scipio.web;

import org.ofbiz.base.util.Debug;

import javax.websocket.*;
import java.util.Collection;

/**
 * Generic WebSocket.
 * <p>Contains one or more {@link WebChannelHandler}, by default a single one named "default" (last) that handles all
 * channels. The default may be overridden and custom ones may be added which are checked first.</p>
 * <p>The default permission requirement is <code>OFBTOOLS</code> for backend use and default security,
 * so {@link #getPermissionVerifier} should be overridden for application-specific permissions.</p>
 * <p>Annotations: This class does not include annotations such as <code>@OnOpen</code>, <code>@OnClose</code>
 * and <code>@onJsonMessage</code>, which can be automatically inherited by extending {@link CommonWebSocket} instead.</p>
 * <p>The endpoint annotation's mount-point is (as of 3.0.0) automatically detected and whitelisted by
 * <code>ContextFilter</code> as part of <code>allowedPaths</code>, so editing web.xml should no longer be necessary.</p>
 * <p>FIXME: The <code>{action}</code> was previously named <code>{type}</code>; the old name is still supported for now for client code.</p>
 * <p>SCIPIO: 3.0.0: Now mostly delegates to WebChannelHandler, which helps gets around
 * construction and missing per-channel handling, overriding for client code and other issues.</p>
 */
//@ServerEndpoint(value = "/ws/{channel}/{action}", configurator = BrokerSessionConfigurator.class)
public abstract class GenericWebSocket {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final SocketPermissionVerifier ADMIN_PERM = new SocketPermissionVerifier.EntityViewOr("OFBTOOLS");

    protected volatile EndpointConfig config;

    public void init(Session session, EndpointConfig config) { // TODO: REVIEW: correctness
        setConfig(config);
    }

    protected EndpointConfig getConfig() { return config; }

    protected void setConfig(EndpointConfig config) {
        this.config = config;
    }

    protected SocketSessionManager getSocketSessionManager(Session session) {
        return SocketSessionManager.getDefault();
    }

    /** Defines base permission to allow client subscription and broadcasts, default OFBTOOLS for backend/security. */
    protected SocketPermissionVerifier getPermissionVerifier(Session session) { return ADMIN_PERM; }

    /*
     * Parameter and arguments extraction and passing
     */

    protected WebChannelHandler.Args makeCommonEventArgs(Session session) {
        return new WebChannelHandler.Args();
    }

    protected WebChannelHandler.Args setCommonEventArgs(WebChannelHandler.Args args, Session session, String eventName, String message) {
        args.setEndpointConfig(getConfig());
        args.setSocketSessionManager(getSocketSessionManager(session));
        args.setPermissionVerifier(getPermissionVerifier(session));
        args.setSession(session);
        args.setEventName(eventName);
        String channelName = WebSocketUtil.getParameter(session, "channel");
        args.setChannelName(channelName);
        String actionType = WebSocketUtil.getParameter(session, "action");
        if (actionType == null) {
            actionType = WebSocketUtil.getParameter(session, "type"); // TODO: REMOVE: Deprecated
        }
        args.setActionType(actionType);
        args.setMessage(message);
        return args;
    }

    protected WebChannelHandler.Args getOpenEventArgs(Session session) {
        return setCommonEventArgs(makeCommonEventArgs(session), session, "open", null);
    }

    protected WebChannelHandler.Args getCloseEventArgs(Session session) {
        return setCommonEventArgs(makeCommonEventArgs(session), session, "close", null);
    }

    protected WebChannelHandler.Args getMessageEventArgs(Session session, String message) {
        return setCommonEventArgs(makeCommonEventArgs(session), session, "message", message);
    }


    /*
     * Event handler factory methods
     */

    /**
     * Returns an appropriate event handler, typically based on the channel name or other session parameter, or default handler.
     */
    protected Collection<WebChannelHandler> getChannelHandlers(WebChannelHandler.Args args) {
        return WebChannelHandler.getDefaultAsList();
    }


    /*
     * Events
     * NOTE: Annotations are included in subclasses (abstract or concrete)
     */

    public void onOpen(Session session, EndpointConfig config) {
        init(session, config);
        WebChannelHandler.Args args = getOpenEventArgs(session);
        Collection<WebChannelHandler> channelHandlers = getChannelHandlers(args);
        if (channelHandlers != null) {
            for(WebChannelHandler channelHandler : channelHandlers) {
                if (WebChannelHandler.isDebug()) {
                    Debug.logInfo("Calling open event for session [" + args.getSession().getId() +
                            "] to channel [" + args.getChannelName() + "] action [" + args.getActionType() + "]" +
                            " handler [" + channelHandler.getName() + "]", module);
                }
                channelHandler.onOpen(args);
            }
        }
    }

    public void onClose(Session session) {
        WebChannelHandler.Args args = getCloseEventArgs(session);
        Collection<WebChannelHandler> channelHandlers = getChannelHandlers(args);
        if (channelHandlers != null) {
            for(WebChannelHandler channelHandler : channelHandlers) {
                if (WebChannelHandler.isDebug()) {
                    Debug.logInfo("Calling close event for session [" + args.getSession().getId() +
                            "] to channel [" + args.getChannelName() + "] action [" + args.getActionType() + "]" +
                            " handler [" + channelHandler.getName() + "]", module);
                }
                channelHandler.onClose(args);
            }
        }
    }

    /**
     * Fetches information from client
     * <p>Requires json Object: {"action": "subscribe"|"message"|"unsubscribe", "data":{}} ("type" supported instead of "action")</p>
     */
    public void onJsonMessage(String message, Session session) {
        try {
            WebChannelHandler.Args args = getMessageEventArgs(session, message);
            if (args.hasChannel() && args.hasAction()) {
                Collection<WebChannelHandler> channelHandlers = getChannelHandlers(args);
                if (channelHandlers != null) {
                    for(WebChannelHandler channelHandler : channelHandlers) {
                        if (WebChannelHandler.isDebug()) {
                            Debug.logInfo("Calling message event for session [" + args.getSession().getId() +
                                    "] to channel [" + args.getChannelName() + "] action [" + args.getActionType() + "]" +
                                    " handler [" + channelHandler.getName() + "]", module);
                        }
                        channelHandler.onMessage(args);
                    }
                }
            }
        } catch (Exception e) {
            Debug.logError(Debug.verboseOn() ? e : null, "Error reading message for session [" + session.getId() + "]: " + e, module);
        }
    }

}