package com.ilscipio.scipio.order.web;

import java.io.IOException;
import java.util.*;

import javax.servlet.http.HttpSession;
import javax.websocket.*;
import javax.websocket.server.ServerEndpoint;

import com.ilscipio.scipio.web.BrokerSessionConfigurator;
import com.ilscipio.scipio.web.GenericWebSocket;
import com.ilscipio.scipio.web.SocketSessionManager;
import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;


/**
 * Order WebSocket.
 */
@ServerEndpoint(value = "/ws/{channel}/{type}", configurator = BrokerSessionConfigurator.class)
public class OrderWebSocket extends GenericWebSocket {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @OnOpen
    public void onOpen(Session session, EndpointConfig config) {
        Map params = session.getRequestParameterMap();
        if(params.get("channel") != null){
            String channelName = SocketSessionManager.DATA_KEY_CHANNEL+ ((List) params.get("channel")).get(0);
            String type = (String) ((List) params.get("type")).get(0);

            if("subscribe".equals(type)){
                SocketSessionManager.addSession("ORDERMGR", session,config);
                SocketSessionManager.addToClientData(channelName,session);
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
            Map params = session.getRequestParameterMap();
            if(params != null){
                if(params.get("channel") != null && params.get("type") != null) {
                    String channelName = SocketSessionManager.DATA_KEY_CHANNEL+params.get("channel");
                    String type = params.get("type").toString();

                    if("subscribe".equals(type)){
                        SocketSessionManager.addToClientData(channelName,session);
                    }

                    if("unsubscribe".equals(type)){
                        SocketSessionManager.removeClientData(channelName,session);
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

