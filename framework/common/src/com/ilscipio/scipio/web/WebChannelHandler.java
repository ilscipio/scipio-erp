package com.ilscipio.scipio.web;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;

import javax.websocket.EndpointConfig;
import javax.websocket.Session;
import java.util.List;

/**
 * Web event handler for {@link GenericWebSocket}.
 * <p>Each *WebSocket can provide a different handler for events based on channel or other parameter, so that behavior
 * can be tweaked per-channel.</p>
 * <p>NOTE: Implementations can override the provided SocketSessionManager and requiredPermission from {@link Args}.</p>
 */
public class WebChannelHandler {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final WebChannelHandler DEFAULT = new WebChannelHandler("default");
    protected static final List<WebChannelHandler> DEFAULT_LIST = UtilMisc.unmodifiableArrayList(new WebChannelHandler("default"));
    private static final boolean DEBUG = true;

    protected final String name;

    public static WebChannelHandler getDefault() {
        return DEFAULT;
    }

    public static List<WebChannelHandler> getDefaultAsList() {
        return DEFAULT_LIST;
    }

    public WebChannelHandler(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    /*
     * Events
     */

    public Object onOpen(Args args) {
        args = checkOpenArgs(args);
        return subscribe(args);
    }

    public Object onClose(Args args) {
        args = checkCloseArgs(args);
        return unsubscribe(args);
    }

    public Object onMessage(Args args) {
        args = checkMessageArgs(args);
        if ("subscribe".equals(args.getActionType())) {
            return subscribe(args);
        } else if ("unsubscribe".equals(args.getActionType())) {
            return unsubscribe(args);
        } else if ("broadcast".equals(args.getActionType()) || "message".equals(args.getActionType())) {
            return broadcast(args);
        } else {
            return false;
        }
    }

    /*
     * Actions
     */

    public Object subscribe(Args args) {
        if (!args.hasChannel()) {
            return false;
        }
        if (isDebug()) {
            Debug.logInfo("Subscribing session [" + args.getSession().getId() +
                    "] to channel [" + args.getChannelName() + "]", module);
        }
        // TODO: REVIEW: I currently see no reason not to re-validate this, in case this could be
        //  manipulated - as long as the HttpSession is valid
        //getSocketSessionManager(args).addSessionInsecure(channelName, session);
        getSocketSessionManager(args).addSession(getRequiredPermission(args), args.getChannelName(), args.getSession(),
                args.getEndpointConfig());
        return true;
    }

    public Object unsubscribe(Args args) {
        if (args.hasChannel()) {
            if (isDebug()) {
                Debug.logInfo("Unsubscribing session [" + args.getSession().getId() +
                        "] from channel [" + args.getChannelName() + "]", module);
            }
            getSocketSessionManager(args).removeSession(args.getChannelName(), args.getSession());
            return true;
        } else {
            if (isDebug()) {
                Debug.logInfo("Unsubscribing session [" + args.getSession().getId() +
                        "] from all channels", module);
            }
            getSocketSessionManager(args).removeSession(args.getSession());
            return true;
        }
    }

    public Object broadcast(Args args) {
        if (UtilValidate.isNotEmpty(args.getMessage())) { // TODO: REVIEW: should be (args.getMessage() != null) to allow empty?
            if (args.hasChannel()) {
                if (isDebug()) {
                    Debug.logInfo("Broadcasting message [" + StringUtil.limitLength(args.getMessage(), 50) +
                            "] to channel [" + args.getChannelName() + "]", module);
                }
                getSocketSessionManager(args).broadcastToChannel(args.getMessage(), args.getChannelName());
                return true;
            } else {
                if (isDebug()) {
                    Debug.logInfo("Skipping broadcast for message [" + StringUtil.limitLength(args.getMessage(), 50) +
                            "] to all channels; not implemented", module);
                }
                // TODO: REVIEW: too broad and dangerous for use in practice
                //getSocketSessionManager(args).broadcastToAll(args.getMessage());
            }
        }
        return false;
    }

    /*
     * Helpers
     */

    /** May be overridden to check or transform the args. */
    protected Args checkOpenArgs(Args args) {
        return args;
    }

    /** May be overridden to check or transform the args. */
    protected Args checkCloseArgs(Args args) {
        return args;
    }

    /** May be overridden to check or transform the args. */
    protected Args checkMessageArgs(Args args) {
        return args;
    }

    /** May be overridden to use a WebChannelHandler-specific value. */
    protected SocketSessionManager getSocketSessionManager(Args args) {
        return args.getSocketSessionManager();
    }

    /** May be overridden to use a WebChannelHandler-specific value. */
    protected String getRequiredPermission(Args args) {
        return args.getRequiredPermission();
    }


    /**
     * Generalized arguments class used to pass args to event and action methods above so they can be easily supplemented.
     * <p>Client code can extend and override in classes extending {@link GenericWebSocket} if needed, but they are
     * not per-WebChannelHandler and methods above must handle as needed.</p>
     * <p>NOTE: This is simplified to be common to all events and actions above, as unneeded fields can simply be set null or ignored.</p>
     * <p>Not thread-safe.</p>
     */
    public static class Args {
        protected EndpointConfig endpointConfig;
        protected SocketSessionManager socketSessionManager;
        protected String requiredPermission;
        protected Session session;
        protected String eventName;
        protected String channelName;
        protected String actionType;
        protected String message;

        public Args() {
        }

        public Args(Args other) {
            this.endpointConfig = other.endpointConfig;
            this.socketSessionManager = other.socketSessionManager;
            this.requiredPermission = other.requiredPermission;
            this.eventName = other.eventName;
            this.session = other.session;
            this.channelName = other.channelName;
            this.actionType = other.actionType;
            this.message = other.message;
        }

        public EndpointConfig getEndpointConfig() {
            return endpointConfig;
        }

        public Args setEndpointConfig(EndpointConfig endpointConfig) {
            this.endpointConfig = endpointConfig;
            return this;
        }

        public SocketSessionManager getSocketSessionManager() {
            return socketSessionManager;
        }

        public Args setSocketSessionManager(SocketSessionManager socketSessionManager) {
            this.socketSessionManager = socketSessionManager;
            return this;
        }

        public String getRequiredPermission() {
            return requiredPermission;
        }

        public Args setRequiredPermission(String requiredPermission) {
            this.requiredPermission = requiredPermission;
            return this;
        }

        public String getEventName() {
            return eventName;
        }

        public Args setEventName(String eventName) {
            this.eventName = eventName;
            return this;
        }

        public Session getSession() {
            return session;
        }

        public Args setSession(Session session) {
            this.session = session;
            return this;
        }

        public String getChannelName() {
            return channelName;
        }

        public Args setChannelName(String channelName) {
            this.channelName = UtilValidate.nullIfEmpty(channelName);
            return this;
        }

        public boolean hasChannel() {
            return (getChannelName() != null);
        }

        public String getActionType() {
            return actionType;
        }

        public Args setActionType(String actionType) {
            this.actionType = UtilValidate.nullIfEmpty(actionType);
            return this;
        }

        public boolean hasAction() {
            return (getActionType() != null);
        }

        public String getMessage() {
            return message;
        }

        public Args setMessage(String message) {
            this.message = message;
            return this;
        }

        public boolean hasMessage() {
            return UtilValidate.isNotEmpty(getMessage());
        }
    }

    protected static boolean isDebug() {
        return DEBUG;
    }
}
