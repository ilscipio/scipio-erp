package com.ilscipio.scipio.order.web;

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
public class OrderWebSocket extends GenericWebSocket {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected String getRequiredPermission() { return "ORDERMGR"; }

    @OnOpen
    public void onOpen(Session session, EndpointConfig config) {
        Map params = session.getRequestParameterMap();
        if(params.get("channel") != null){
            String channelName = (String) ((List) params.get("channel")).get(0);
            String type = (String) ((List) params.get("type")).get(0);

            if("subscribe".equals(type)){
                SocketSessionManager.addSession(getRequiredPermission(), channelName, session,config);
            }
        }
    }

    /**
     * Fetches information from client
     * Requires json Object: {"type": "subscribe"|"message"|"unsubscribe", "data":{}}
     * */
    @OnMessage
    public void onJsonMessage(String message, Session session) {
        try{
            Map<String, List<String>> params = session.getRequestParameterMap();
            if(params != null){
                if(params.get("channel") != null && params.get("type") != null) {
                    String channelName = (String) params.get("channel").get(0);
                    String type = (String) params.get("type").get(0);

                    if("subscribe".equals(type)){
                        SocketSessionManager.addSessionInsecure(channelName,session);
                    }

                    if("unsubscribe".equals(type)){
                        SocketSessionManager.removeSession(channelName,session);
                    }

                    if("message".equals(type)){
                        if(UtilValidate.isNotEmpty(message)){
                            SocketSessionManager.broadcastToChannel(message,channelName);
                        }
                    }
                }


            }
        }catch (Exception e){
            Debug.logError(e, module);
        }

    }
}

