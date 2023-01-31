package org.ofbiz.webapp.event;

import java.util.Iterator;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.ilscipio.scipio.ce.webapp.control.def.ControlResponse;
import org.ofbiz.webapp.control.ConfigXMLReader.Event;
import org.ofbiz.webapp.control.ConfigXMLReader.RequestMap;

/**
 * SCIPIO: Super event handler, for nested handling.
 */
public interface EventHandlerWrapper {

    /**
     * Initializes the handler. Since handlers use the singleton pattern this method should only be called
     * the first time the handler is used.
     *
     * @param context ServletContext This may be needed by the handler in order to lookup properties or XML
     * definition files for rendering pages or handler options.
     * @throws EventHandlerException
     */
    void init(ServletContext context) throws EventHandlerException;

    /**
     * Invoke the web event
     * @param handlers The (next) event handlers being invoked; call next() and then invoke()
     * @param event Contains information about what to execute
     * @param requestMap Contains information about the request-map the event was called from
     * @param request The servlet request object
     * @param response The servlet response object
     *
     * @return Object Result code, normally String or {@link ControlResponse}
     * @throws EventHandlerException
     */
    Object invoke(Iterator<EventHandlerWrapper> handlers, Event event, RequestMap requestMap, HttpServletRequest request, HttpServletResponse response) throws EventHandlerException;

}
