package com.ilscipio.scipio.webtools.web;

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


/**
 * Order WebSocket.
 */
@ServerEndpoint(value = "/ws/{channel}/{type}", configurator = BrokerSessionConfigurator.class)
public class AdminWebSocket extends GenericWebSocket {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @OnOpen
    public void onOpen(Session session, EndpointConfig config) {
        Map params = session.getRequestParameterMap();
        if(params.get("channel") != null){
            String channelName = SocketSessionManager.DATA_KEY_CHANNEL+ ((List) params.get("channel")).get(0);
            String type = (String) ((List) params.get("type")).get(0);

            if("subscribe".equals(type)){
                SocketSessionManager.addToClientData(channelName,session);
            }
        }
        SocketSessionManager.addSession("OFBTOOLS", session,config);
    }
}

