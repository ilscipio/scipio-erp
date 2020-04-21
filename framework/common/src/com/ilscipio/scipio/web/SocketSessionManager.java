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
 */
public class SocketSessionManager {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final boolean DEBUG = UtilProperties.getPropertyAsBoolean("catalina", "webSocket.debug", false);

    /** For clients previously not registered to any specific channel; this distinguishes them. */
    public static final String DEFAULT_CHANNEL = "default";

    /** Channel info map
     * NOTE: global client list has been removed because it forces global synchronization for no reason and complexity; DEFAULT_CHANNEL now exists instead. */
    private static final Map<String, ChannelInfo> channelMap = new ConcurrentHashMap<>();

    static final ThreadLocal allowLogging = new ThreadLocal<>();

    /**
     * Adds Websocket Session to Session Manager.
     * */
    public static void addSession(String permission, String channel, Session session, EndpointConfig config) {
        if (permission != null && !checkClientAuthorization(permission, session, config, "; denying client registration")) { // SCIPIO: 2018-10-03
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
    public static void addSessionInsecure(String channel, Session session) {
        if (UtilValidate.isEmpty(channel)) {
            channel = DEFAULT_CHANNEL;
        }
        ChannelInfo channelInfo = channelMap.get(channel);
        if (channelInfo == null) {
            channelInfo = new ChannelInfo(channel);
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
    public static void removeSession(String channel, Session session) {
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
    public static void removeSession(Session session) {
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
    public static void removeSessions(Collection<Session> sessions) {
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

    public static Set<Session> getAllSessions() {
        Set<Session> sessions = new HashSet<>();
        for(Map.Entry<String, ChannelInfo> channelEntry : channelMap.entrySet()) {
            sessions.addAll(channelEntry.getValue().getSessions());
        }
        return sessions;
    }

    /**
     * Broadcasts to all sessions.
     * */
    public static void broadcastToAll(String message) {
        Set<Session> invalidSessions = null;
        int totalCount = 0;
        int successCount = 0;
        for(Session session : getAllSessions()) {
            try {
                if (session.isOpen()) {
                    session.getBasicRemote().sendText(message);
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
                        Debug.logError("Could not close websocket session: " + ioe.toString(), module);
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
    public static boolean broadcastToClient(String message, String clientId) {
        Set<Session> invalidSessions = null;
        try {
            for (Map.Entry<String, ChannelInfo> channelEntry : channelMap.entrySet()) {
                for (Map.Entry<Session, ChannelInfo.ClientInfo> clientEntry : channelEntry.getValue().getClientMap().entrySet()) {
                    Session session = clientEntry.getKey();
                    if (clientId.equals(session.getId())) {
                        try {
                            if (session.isOpen()) {
                                session.getBasicRemote().sendText(message);
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
                                    Debug.logError("Could not close websocket session: " + ioe.toString(), module);
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
    public static void broadcastToChannel(String message, String channel) {
        ChannelInfo channelInfo = channelMap.get(channel);
        if (channelInfo == null) {
            return;
        }
        Set<Session> invalidSessions = null;
        int totalCount = 0;
        int successCount = 0;
        for (Map.Entry<Session, ChannelInfo.ClientInfo> clientEntry : channelInfo.getClientMap().entrySet()) {
            Session session = clientEntry.getKey();
            try {
                if (session.isOpen()) {
                    session.getBasicRemote().sendText(message);
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
                        Debug.logError("Could not close websocket session: " + ioe.toString(), module);
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

    private static boolean checkClientAuthorization(String permission, Session session, EndpointConfig config, String errorSuffix) {
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
        if (!security.hasEntityPermission(permission, "_VIEW", userLogin)) {
            if (isLog()) {
                Debug.logWarning("Client with userLoginId '" + (userLogin != null ? userLogin.get("userLoginId") : "(none)") + "' not authorized for " + permission + "_VIEW permission for websockets session '"
                        + session.getId() + "'" + errorSuffix, module);
            }
            return false;
        }
        return true;
    }

    public static class ChannelInfo {
        private final String name;
        private final Map<Session, ClientInfo> clientMap = new ConcurrentHashMap<>(); // Better than synchronizing on Set

        protected ChannelInfo(String name) {
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
            clientMap.putIfAbsent(session, ClientInfo.INSTANCE); // new ClientInfo
        }

        protected void removeSession(Session session) { // TODO: REVIEW: does not yet require synchronized, but may in future for ClientInfo
            clientMap.remove(session);
        }

        private static class ClientInfo {
            // TODO?: Any required per-channel client info here (NOTE: synchronization or mutable pattern may be needed); for now we can share a single instance
            private static final ClientInfo INSTANCE = new ClientInfo();
        }
    }

    public static boolean isLog() { return !Boolean.FALSE.equals(allowLogging.get()); }

    public static boolean isDebug() { return isLog() && (DEBUG || Debug.verboseOn()); }
}
