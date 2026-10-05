/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
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
