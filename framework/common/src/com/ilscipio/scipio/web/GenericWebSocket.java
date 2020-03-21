package com.ilscipio.scipio.web;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilObject;
import org.ofbiz.base.util.UtilValidate;

import javax.websocket.*;
import java.util.List;
import java.util.Map;

/**
 * Order WebSocket.
 */
//@ServerEndpoint(value = "/ws/channel/{channel}/{type}", configurator = BrokerSessionConfigurator.class)
public class GenericWebSocket {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected volatile EndpointConfig config;

    protected EndpointConfig getConfig() { return config; }

    protected String getRequiredPermission() { return "OFBTOOLS"; }

    @OnOpen
    public void onOpen(Session session, EndpointConfig config) {
        this.config = config;
        Map<String, List<String>> params = session.getRequestParameterMap();
        if(params.get("channel") != null){
            String channelName = (String) ((List) params.get("channel")).get(0);
            String type = (String) ((List) params.get("type")).get(0);
            if("subscribe".equals(type)){
                SocketSessionManager.addSession(getRequiredPermission(), channelName, session, config);
            }
        }
    }

    @OnClose
    public void onClose(Session session) {
        SocketSessionManager.removeSession(session);
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

                    // NOTE: In this code the channel is not validated, meaning everything rides on the single OFBTOOLS check above
                    // and also accepts arbitrary channel names from browser -
                    // never reuse this code as-is in frontend, and in frontend the channel name would have to be restricted as well.

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