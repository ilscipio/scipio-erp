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
package com.ilscipio.scipio.ce.webapp.control.def;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a controller event or view handler, analogous to handler element in site-conf.xsd.
 *
 * <p>Handlers define Java classes which handle specific named types (either request or view).
 * This allows different logics for processing input from requests.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}Handler(name = "custom-event", type = "request")
 * public class CustomEventHandler implements EventHandler {
 *     // ...
 * }
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for controller annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface Handler {

    /**
     * Handler name, used as the event or view type in request-map/view-map definitions.
     */
    String name();

    /**
     * Handler type: "request", "view", or "request-handler-wrapper".
     * <ul>
     * <li>request - for action/event handlers</li>
     * <li>view - for rendering handlers</li>
     * <li>request-handler-wrapper - super handler that wraps all event types</li>
     * </ul>
     * Default: "request"
     */
    String type() default "request";

    /**
     * Controller location as URL with controller:// protocol or as webapp name.
     * Default: applies to default controller.
     */
    String controller() default "";

    /**
     * For request-handler-wrapper only: restricts which event triggers this handler applies to.
     * Comma-separated list. Values: all, firstvisit, preprocessor, security-auth, request, postprocessor, after-login, before-logout
     * Default: "all"
     */
    String triggers() default "all";
}
