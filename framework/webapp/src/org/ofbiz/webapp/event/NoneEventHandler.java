package org.ofbiz.webapp.event;

import org.ofbiz.webapp.control.ConfigXMLReader;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * No-op event handler (SCIPIO).
 */
public class NoneEventHandler implements EventHandler {
    public static final NoneEventHandler DEFAULT = new NoneEventHandler();

    @Override
    public void init(ServletContext context) throws EventHandlerException {
    }

    @Override
    public String invoke(ConfigXMLReader.Event event, ConfigXMLReader.RequestMap requestMap, HttpServletRequest request, HttpServletResponse response) throws EventHandlerException {
        return "success";
    }
}
