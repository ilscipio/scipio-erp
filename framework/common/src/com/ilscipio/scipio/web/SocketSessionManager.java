package com.ilscipio.scipio.web;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;
import org.ofbiz.webapp.WebAppUtil;

import javax.servlet.http.HttpSession;
import javax.websocket.EndpointConfig;
import javax.websocket.Session;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * SocketSessionManager.
 * <p>
 * This was rewritten using ConcurrentHashMaps to remove almost all the synchronized blocks (most unnecessary), which
 * were poor/contention in the original ofbiz example. The global client list was removed to avoid needless synchronization and as it was not even used anymore;
 * if channel name null/empty it goes to {@link #DEFAULT_CHANNEL}.
 * <p>
 * Security: To simplify and remove need for synchronization (not trivial), currently empty channels are never removed; this is fine for backend
 * since channel names should be a predefined fixed number; if ever used from frontend, it means channel names gotten over parameters must be
 * restricted to valid ones, or this could overload the server.
 * <p>
 * TODO: REVIEW: There could be issues with Session identity (hashCode/equals) depending on the implementation, and it *might*
 *  be possible that have to use Session.getId as String keys instead of Session. But may be moot...
 * <p>SCIPIO: 2.1.0: Converted static class to reusable/overridable instance.</p>
 */
public class SocketSessionManager {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final boolean DEBUG = UtilProperties.getPropertyAsBoolean("catalina", "webSocket.debug", false);
    /** For clients previously not registered to any specific channel; this distinguishes them. */
    public static final String DEFAULT_CHANNEL = "default";
    private static final SocketSessionManager DEFAULT = new SocketSessionManager();

    /**
     * Channel info map, without synchronization.
     * <p>TODO: REVIEW: Should this have had weak keys? Not possible yet here.
     * Currently fully relies on @OnClose in GenericWebSocket to clear sessions.</p>
     */
    private final Map<String, ChannelInfo> channelMap = new ConcurrentHashMap<>();

    protected final ThreadLocal allowLogging = new ThreadLocal<>();

    /** Returns the default SocketSessionManager, typically for backend use. */
    public static SocketSessionManager getDefault() {
        return DEFAULT;
    }

    /**
     * Adds Websocket Session to Session Manager.
     * */
    public void addSession(SocketPermissionVerifier permVerifier, String channel, Session session, EndpointConfig config) {
        if (permVerifier != null && !checkClientAuthorization(permVerifier, session, config, "; denying client registration")) { // SCIPIO: 2018-10-03
            if (isDebug()) {
                GenericValue userLogin = WebSocketUtil.getUserLogin(session, config);
                Debug.logInfo("Websocket: Permission denied for client with userLoginId '" + (userLogin != null ? userLogin.get("userLoginId") : "(none)" )
                        + "' for channel '" + (UtilValidate.isNotEmpty(channel) ? channel : DEFAULT_CHANNEL) + "', sessionId '" + session.getId() + "'", module);
            }
            return;
        }
        if (isDebug()) {
            GenericValue userLogin = WebSocketUtil.getUserLogin(session, config);
            Debug.logInfo("Websocket: Registering client with userLoginId '" + (userLogin != null ? userLogin.get("userLoginId") : "(none)" )
                    + "' to channel '" + (UtilValidate.isNotEmpty(channel) ? channel : DEFAULT_CHANNEL) + "', sessionId '" + session.getId() + "'", module);
        }
        // Add session to the connected sessions clients set
        addSessionInsecure(channel, session);
    }

    /**
     * Adds client session to channel - no security checks - only use if already verified!
     */
    public void addSessionInsecure(String channel, Session session) {
        if (UtilValidate.isEmpty(channel)) {
            channel = DEFAULT_CHANNEL;
        }
        ChannelInfo channelInfo = channelMap.get(channel);
        if (channelInfo == null) {
            channelInfo = makeChannelInfo(channel, session, null);
            ChannelInfo previousChannelInfo = channelMap.putIfAbsent(channel, channelInfo);
            if (previousChannelInfo != null) {
                channelInfo = previousChannelInfo;
            }
        }
        channelInfo.addSession(session);
    }

    /**
     * Removes session from channel - no security checks - only use if already verified!
     */
    public void removeSession(String channel, Session session) {
        if (UtilValidate.isEmpty(channel)) {
            channel = DEFAULT_CHANNEL;
        }
        ChannelInfo channelInfo = channelMap.get(channel);
        if (channelInfo == null) {
            return;
        }
        if (isDebug()) {
            Debug.logInfo("Websocket: Removing client session from channel '" + channel + "', sessiondId '" + session.getId() + "'", module);
        }
        channelInfo.removeSession(session);
    }

    /**
     * Removes session from all channels - no security checks - only use if already verified!
     */
    public void removeSession(Session session) {
        for(Map.Entry<String, ChannelInfo> entry : channelMap.entrySet()) {
            if (isDebug() && entry.getValue().hasSession(session)) {
                Debug.logInfo("Websocket: Removing client session from channel '" + entry.getKey() + "', sessiondId '" + session.getId() + "'", module);
            }
            entry.getValue().removeSession(session);
        }
    }

    /**
     * Removes sessions from all channels - no security checks - only use if already verified!
     */
    public void removeSessions(Collection<Session> sessions) {
        for(Map.Entry<String, ChannelInfo> entry : channelMap.entrySet()) {
            for(Session session : sessions) {
                entry.getValue().removeSession(session);
            }
        }
    }

//    public static void removeChannel(String channel) {
//        channelMap.remove(channel);
//    }

    // Not easily possible to do without adding expensive synchronization that doesn't appear needed anyway
//    /** Removes any empty channels from the manager (synchronized - slow). Does NOT check for expired sessions. */
//    public static void removeEmptyChannels() {
//        // DEV NOTE: we only partial have to synchronize this with addSessionInsecure because after channelInfo.addSession it
//        // puts the ChannelInfo back into channelInfoMap, so that even if removeEmptyChannels detects an empty ChannelInfo
//        // and removes it, addSessionInsecure just puts it back
//        Set<String> emptyChannels = new HashSet<>();
//        for (Map.Entry<String, ChannelInfo> entry : channelInfoMap.entrySet()) {
//            if (entry.getValue().isEmpty()) {
//                emptyChannels.add(entry.getKey());
//            }
//        }
//        for (String channel : emptyChannels) {
//            channelInfoMap.remove(channel);
//        }
//    }

    // No longer synchronized and these turn out to be unneeded operations, so better to just avoid until something needs these (previously used in code above)
//    public static Set<String> getSessionChannelNames(Session session) {
//        Set<String> channels = new HashSet<>();
//        for(ChannelInfo channelInfo : channelInfoMap.values()) {
//            if (channelInfo.hasSession(session)) {
//                channels.add(channelInfo.getName());
//            }
//        }
//        return channels;
//    }
//
//    public static List<ChannelInfo> getSessionChannels(Session session) {
//        List<ChannelInfo> channels = new ArrayList<>();
//        for(Map.Entry<String, ChannelInfo> channelInfoEntry : channelInfoMap.entrySet()) {
//            if (channelInfoEntry.getValue().hasSession(session)) {
//                channels.add(channelInfoEntry.getValue());
//            }
//        }
//        return channels;
//    }
//
//    /** Returns the first channel the session is in. */
//    public static ChannelInfo getSessionChannel(Session session) {
//        for(Map.Entry<String, ChannelInfo> channelInfoEntry : channelInfoMap.entrySet()) {
//            if (channelInfoEntry.getValue().hasSession(session)) {
//                return channelInfoEntry.getValue();
//            }
//        }
//        return null;
//    }

    public Set<Session> getAllSessions() {
        Set<Session> sessions = new HashSet<>();
        for(Map.Entry<String, ChannelInfo> channelEntry : channelMap.entrySet()) {
            sessions.addAll(channelEntry.getValue().getSessions());
        }
        return sessions;
    }

    /** For use with broadcasting methods; default returned by {@link #getMessageSender()}. */
    public interface MessageSender {
        /**
         * Sends message to destination.
         */
        void send(String message, Session session) throws IOException;
    }

    public static class BasicRemoteMessageSender implements MessageSender {
        public static final BasicRemoteMessageSender INSTANCE = new BasicRemoteMessageSender();
        @Override
        public void send(String message, Session session) throws IOException {
            session.getBasicRemote().sendText(message);
        }
    }

    public static class AsyncRemoteMessageSender implements MessageSender {
        public static final AsyncRemoteMessageSender INSTANCE = new AsyncRemoteMessageSender();
        @Override
        public void send(String message, Session session) throws IOException {
            session.getAsyncRemote().sendText(message);
        }
    }

    /** Returns the default MessageSender for broadcasting methods; by default BasicRemoteMessageSender. */
    protected MessageSender getMessageSender() {
        // TODO: REVIEW: According to docs this is blocking - wanted by default?
        return BasicRemoteMessageSender.INSTANCE;
        //return AsyncRemoteMessageSender.INSTANCE;
    }

    /**
     * Broadcasts to all sessions.
     * */
    public void broadcastToAll(String message) {
        broadcastToAll(message, getMessageSender());
    }

    /**
     * Broadcasts to all sessions.
     * */
    public void broadcastToAll(String message, MessageSender sender) {
        Set<Session> invalidSessions = null;
        int totalCount = 0;
        int successCount = 0;
        for(Session session : getAllSessions()) {
            try {
                if (session.isOpen()) {
                    sender.send(message, session);
                    successCount++;
                } else {
                    if (invalidSessions == null) {
                        invalidSessions = new HashSet<>();
                    }
                    invalidSessions.add(session);
                }
            } catch (IOException e) {
                if (invalidSessions == null) {
                    invalidSessions = new HashSet<>();
                }
                invalidSessions.add(session);
                try {
                    session.close();
                } catch (IOException ioe) {
                    if (isLog()) {
                        Debug.logError("Could not close websocket session: " + ioe, module);
                    }
                }
            }
            totalCount++;
        }
        if (invalidSessions != null) {
            removeSessions(invalidSessions);
        }
        if (isDebug()) {
            Debug.logInfo("Websocket: broadcasted message to " + successCount + "/" + totalCount + " sessions in all channels", module);
        }
    }

    /**
     * Broadcasts to a single client.
     * @return true only if message appears to have been sent
     */
    public boolean broadcastToClient(String message, String clientId) {
        return broadcastToClient(message, clientId, getMessageSender());
    }

    /**
     * Broadcasts to a single client.
     * @return true only if message appears to have been sent
     */
    public boolean broadcastToClient(String message, String clientId, MessageSender sender) {
        Set<Session> invalidSessions = null;
        try {
            for (Map.Entry<String, ChannelInfo> channelEntry : channelMap.entrySet()) {
                for (Map.Entry<Session, ClientInfo> clientEntry : channelEntry.getValue().getClientMap().entrySet()) {
                    Session session = clientEntry.getKey();
                    if (clientId.equals(session.getId())) {
                        try {
                            if (session.isOpen()) {
                                sender.send(message, session);
                                if (isDebug()) {
                                    Debug.logInfo("Websocket: broadcasted message to client '" + clientId + "'", module);
                                }
                                return true;
                            } else {
                                if (invalidSessions == null) {
                                    invalidSessions = new HashSet<>();
                                }
                                invalidSessions.add(session);
                            }
                        } catch (IOException e) {
                            if (invalidSessions == null) {
                                invalidSessions = new HashSet<>();
                            }
                            invalidSessions.add(session);
                            try {
                                session.close();
                            } catch (IOException ioe) {
                                if (isLog()) {
                                    Debug.logError("Could not close websocket session: " + ioe, module);
                                }
                            }
                        }
                        return false;
                    }
                }
            }
            return false;
        } finally {
            if (invalidSessions != null) {
                removeSessions(invalidSessions);
            }
        }
    }

    /**
     * Broadcasts to sessions on topic.
     * NOTE: Logging must be disable-able is for ScipioSocketAppender otherwise this creates endless logging loops.
     */
    public void broadcastToChannel(String message, String channel) {
        broadcastToChannel(message, channel, getMessageSender());
    }

    /**
     * Broadcasts to sessions on topic.
     * NOTE: Logging must be disable-able is for ScipioSocketAppender otherwise this creates endless logging loops.
     */
    public void broadcastToChannel(String message, String channel, MessageSender sender) {
        ChannelInfo channelInfo = channelMap.get(channel);
        if (channelInfo == null) {
            return;
        }
        Set<Session> invalidSessions = null;
        int totalCount = 0;
        int successCount = 0;
        for (Map.Entry<Session, ClientInfo> clientEntry : channelInfo.getClientMap().entrySet()) {
            Session session = clientEntry.getKey();
            try {
                if (session.isOpen()) {
                    sender.send(message, session);
                    successCount++;
                } else {
                    if (invalidSessions == null) {
                        invalidSessions = new HashSet<>();
                    }
                    invalidSessions.add(session);
                }
            } catch (IOException e) {
                if (invalidSessions == null) {
                    invalidSessions = new HashSet<>();
                }
                invalidSessions.add(session);
                try {
                    session.close();
                } catch (IOException ioe) {
                    if (isLog()) {
                        Debug.logError("Could not close websocket session: " + ioe, module);
                    }
                }
            }
            totalCount++;
        }
        if (invalidSessions != null) {
            removeSessions(invalidSessions);
        }
        if (isDebug()) {
            Debug.logInfo("Websocket: broadcasted message to " + successCount + "/" + totalCount + " sessions in channel '" + channel + "'", module);
        }
    }

    protected boolean checkClientAuthorization(SocketPermissionVerifier permVerifier, Session session, EndpointConfig config, String errorSuffix) {
        if (config == null) {
            if (isLog()) {
                Debug.logError("null EndpointConfig for websockets session '"
                        + session.getId() + "' for client authorization" + errorSuffix, module);
            }
            return false;
        }
        HttpSession httpSession = WebSocketUtil.getHttpSession(session, config);
        if (httpSession == null) {
            if (isLog()) {
                Debug.logError("Could not get HttpSession for websockets session '"
                        + session.getId() + "' for client authorization" + errorSuffix, module);
            }
            return false;
        }
        Security security = WebAppUtil.getSecurity(httpSession);
        if (security == null) {
            if (isLog()) {
                Debug.logError("Could not get Security object from HttpSession"
                        + " for websockets session '" + session.getId() + "' for client authorization"
                        + errorSuffix, module);
            }
            return false;
        }
        GenericValue userLogin = UtilHttp.getUserLogin(httpSession);
        if (!permVerifier.hasPermission(security, userLogin, httpSession, session)) {
            if (isLog()) {
                Debug.logWarning("Client with userLoginId '" + (userLogin != null ? userLogin.get("userLoginId") : "(none)")
                        + "' not authorized by permission for websockets session '"
                        + session.getId() + "'" + errorSuffix, module);
            }
            return false;
        }
        return true;
    }

    protected ChannelInfo makeChannelInfo(String channelName, Session session, Object args) {
        return new ChannelInfo(channelName, session, args);
    }

    public class ChannelInfo {
        private final String name;
        private final Map<Session, ClientInfo> clientMap = new ConcurrentHashMap<>(); // Better than synchronizing on Set

        protected ChannelInfo(String name, Session session, Object args) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        protected Map<Session, ClientInfo> getClientMap() {
            return clientMap;
        }

        public Set<Session> getSessions() {
            return Collections.unmodifiableSet(clientMap.keySet());
        }

        public boolean isEmpty() {
            return clientMap.isEmpty();
        }

        public boolean hasSession(Session session) {
            return clientMap.containsKey(session);
        }

        protected void addSession(Session session) { // TODO: REVIEW: does not yet require synchronized, but may in future for ClientInfo
            clientMap.putIfAbsent(session, getClientInfo(session)); // new ClientInfo
        }

        protected ClientInfo getClientInfo(Session session) {
            return ClientInfo.INSTANCE;
        }

        protected void removeSession(Session session) { // TODO: REVIEW: does not yet require synchronized, but may in future for ClientInfo
            clientMap.remove(session);
        }
    }

    public static class ClientInfo {
        // TODO?: Any required per-channel client info here (NOTE: synchronization or mutable pattern may be needed); for now we can share a single instance
        private static final ClientInfo INSTANCE = new ClientInfo();
    }

    public boolean isLog() { return !Boolean.FALSE.equals(allowLogging.get()); }

    public boolean isDebug() { return isLog() && (DEBUG || Debug.verboseOn()); }
}
