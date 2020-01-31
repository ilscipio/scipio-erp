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
        if (hookHandlers.contains(hookHandler)) {
            return;
        }
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
        default void beginDoRequest(HttpServletRequest request, HttpServletResponse response, RequestHandler requestHandler, RequestHandler.RequestState requestState) {};
        default void postPreprocessorEvents(HttpServletRequest request, HttpServletResponse response, RequestHandler requestHandler, RequestHandler.RequestState requestState) {};
        default void postEvents(HttpServletRequest request, HttpServletResponse response, RequestHandler requestHandler, RequestHandler.RequestState requestState) {};
        default void endDoRequest(HttpServletRequest request, HttpServletResponse response, RequestHandler requestHandler, RequestHandler.RequestState requestState) {};
    }
}
