package org.ofbiz.example;

import javax.servlet.http.HttpSession;
import javax.websocket.HandshakeResponse;
import javax.websocket.server.HandshakeRequest;
import javax.websocket.server.ServerEndpointConfig;

/**
 * SCIPIO: A configurator used to make the HttpSession available
 * from {@link ExampleWebSockets#onOpen}.
 * <p>
 * Added 2018-10-03.
 */
public class ExampleSessionConfigurator extends ServerEndpointConfig.Configurator {

    @Override
    public void modifyHandshake(ServerEndpointConfig config, HandshakeRequest request, HandshakeResponse response) {
        config.getUserProperties().put(HttpSession.class.getName(), request.getHttpSession());
    }

}
