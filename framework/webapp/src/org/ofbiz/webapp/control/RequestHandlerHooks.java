package org.ofbiz.webapp.control;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * SCIPIO: INTERNAL helper class for developer use - client code should not use at this time! Subject to change frequently
 * or may be removed at later date.
 */
public abstract class RequestHandlerHooks {

    private static volatile List<HookHandler> hookHandlers = Collections.emptyList();

    public static void subscribe(HookHandler hookHandler) {
        synchronized(RequestHandlerHooks.class) {
            List<HookHandler> hookHandlers = new ArrayList<>(RequestHandlerHooks.hookHandlers);
            hookHandlers.add(hookHandler);
            RequestHandlerHooks.hookHandlers = Collections.unmodifiableList(hookHandlers);
        }
    }

    static List<HookHandler> getHookHandlers() {
        return hookHandlers;
    }

    public interface HookHandler {
        default void beginAllDoRequest(HttpServletRequest request, HttpServletResponse response, RequestHandler requestHandler) {};
        default void postPreprocessorEvents(HttpServletRequest request, HttpServletResponse response, RequestHandler requestHandler) {};
        default void postEvents(HttpServletRequest request, HttpServletResponse response, RequestHandler requestHandler) {};
        default void endAllDoRequest(HttpServletRequest request, HttpServletResponse response, RequestHandler requestHandler) {};
    }
}
