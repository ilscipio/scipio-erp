package com.ilscipio.scipio.web;

import org.ofbiz.base.util.Debug;
import org.ofbiz.security.Security;
import org.ofbiz.webapp.WebAppUtil;

import javax.servlet.http.HttpSession;
import javax.websocket.EndpointConfig;
import javax.websocket.Session;
import java.io.IOException;
import java.util.*;

public class SocketSessionManager {
    private static final Set<Session> clients = Collections.synchronizedSet(new HashSet<Session>());
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final int MAX_CLIENTS = 100;
    private static Map<String,List> clientData = Collections.synchronizedMap(new HashMap<String,List>());
    public static final String DATA_KEY_CHANNEL = "channel_";

    public static void addSession(Session session, EndpointConfig config) {
        addSession("OFBTOOLS", session, config);
    }

    /**
     * Adds Websocket Session to Session Manager (defaults to true)
     * */
    public static void addSession(String permission, Session session, EndpointConfig config) {
        addSession(Boolean.TRUE, permission, session, config);
    }

    public static void addUnsecureSession(Session session, EndpointConfig config) {
        addSession(Boolean.FALSE, null, session, config);
    }

    public static void addSession(Boolean requiresPermission, String permission, Session session, EndpointConfig config) {
        if (requiresPermission && !checkClientAuthorization(permission, session, config, "; denying client registration")) { // SCIPIO: 2018-10-03
            return;
        }
        synchronized(clients) { // SCIPIO: added block
            if (clients.size() >= MAX_CLIENTS) { // SCIPIO: 2018-10-03
                Debug.logWarning("Max websockets clients reached for example ("
                        + MAX_CLIENTS + ")! Denying client registration websockets session "
                        + getLogIdStr(session), module);
                return;
            }
            // Add session to the connected sessions clients set
            clients.add(session);
        }
    }

    /**
     * Remove Websocket Session
     * */
    public static void removeSession(Session session){
        clients.remove(session);
    }


    /**
     * Broadcasts to all sessions
     * */
    public static void broadcast(String message){
        synchronized(clients) {
            for (Session session : clients) {
                try {
                    if (session.isOpen()) {
                        synchronized (clients) {
                            for (Session client : clients) {
                                client.getBasicRemote().sendText(message);
                            }
                        }
                    }
                } catch (IOException e) {
                    try {
                        session.close();
                    } catch (IOException ioe) {
                        Debug.logError(ioe.getMessage(), module);
                    }
                }
            }
        }
    }

    /**
     * Broadcasts to a single client
     * */
    public static void broadcastToClient(String message,String clientId){
        synchronized(clients) {
            for (Session session : clients) {
                if(clientId.equals(session.getId())){
                    try {
                        if (session.isOpen()) {
                            session.getBasicRemote().sendText(message);
                        }
                    } catch (IOException e) {
                        try {
                            clients.remove(session);
                            session.close();
                        } catch (IOException ioe) {
                            Debug.logError(ioe.getMessage(), module);
                        }
                    }

                }

            }
        }
    }

    /**
     * Broadcasts to all sessions on topic
     * */
    public static void broadcastToChannel(String message,String channelName){
        String channel = DATA_KEY_CHANNEL+channelName;
        synchronized(clients) {
            List<Session> channelInfo = clientData.get(channel);
            if(channelInfo != null){
                for (Session session : channelInfo) {
                    try {
                        if (session.isOpen()) {
                            session.getBasicRemote().sendText(message);
                        }
                    } catch (IOException e) {
                        try {
                            channelInfo.remove(session);
                            session.close();
                        } catch (IOException ioe) {
                            Debug.logError(ioe.getMessage(), module);
                        }
                    }
                }
            }
        }
    }

    /**
     * Adds clientInfo
     * */
    public static void addToClientData(String key, Object data){
        synchronized(clientData) {
            if (clientData.containsKey(key)) {
                clientData.get(key).add(data);
            } else {
                List e = new ArrayList();
                e.add(data);
                clientData.put(key, e);
            }
        }
    }

    /**
     * Removes clientInfo - seldom used as clients will most likely just drop out of session.
     * */
    public static void removeClientData(String key, Object data){
        synchronized(clientData) {
            if (clientData.containsKey(key)) {
                clientData.remove(key);
            }
        }
    }

    private static boolean checkClientAuthorization(String permission, Session session, EndpointConfig config, String errorSuffix) {
        HttpSession httpSession = (HttpSession) config.getUserProperties().get(HttpSession.class.getName());
        if (httpSession == null) {
            Debug.logError("Could not get HttpSession for websockets session "
                    + getLogIdStr(session) + " for client authorization" + errorSuffix, module);
            return false;
        }
        Security security = WebAppUtil.getSecurity(httpSession);
        if (security == null) {
            Debug.logError("Could not get Security object from HttpSession"
                    + " for websockets session " + getLogIdStr(session) + " for client authorization"
                    + errorSuffix, module);
            return false;
        }
        if (!security.hasEntityPermission(permission, "_VIEW", httpSession)) {
            Debug.logError("Client not authorized for ORDERMGR_VIEW permission for websockets session "
                    + getLogIdStr(session) +  errorSuffix, module);
            return false;
        }
        return true;
    }
    private static String getLogIdStr(Session session) {
        return "'" + session.getId() + "'";
    }
}
