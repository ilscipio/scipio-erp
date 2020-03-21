package com.ilscipio.scipio.webtools.web;

import java.util.*;

import javax.websocket.*;
import javax.websocket.server.ServerEndpoint;

import com.ilscipio.scipio.web.BrokerSessionConfigurator;
import com.ilscipio.scipio.web.GenericWebSocket;
import com.ilscipio.scipio.web.SocketSessionManager;
import org.ofbiz.base.util.Debug;


/**
 * Order WebSocket.
 */
@ServerEndpoint(value = "/ws/{channel}/{type}", configurator = BrokerSessionConfigurator.class)
public class AdminWebSocket extends GenericWebSocket {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected String getRequiredPermission() { return "OFBTOOLS"; }

    @OnOpen
    public void onOpen(Session session, EndpointConfig config) {
        Map<String, List<String>> params = session.getRequestParameterMap();
        String channel = null;
        if(params.get("channel") != null){
            String channelName = (String) ((List) params.get("channel")).get(0);
            String type = (String) ((List) params.get("type")).get(0);
            if("subscribe".equals(type)){
                channel = channelName;
            }
        }
        SocketSessionManager.addSession(getRequiredPermission(), channel, session, config);
    }
}

