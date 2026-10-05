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
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a static response for a request-map, analogous to the response element in site-conf.xsd.
 *
 * <p>This annotation can be used on @Request annotated classes/methods to define static response
 * mappings. Multiple @Response annotations can be used to define different responses for
 * different event return values (success, error, etc.).</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}Request(uri = "myRequest")
 * {@literal @}Response(name = "success", type = "view", value = "main")
 * {@literal @}Response(name = "error", type = "view", value = "error")
 * public class MyRequestHandler {
 *     {@literal @}Event
 *     public String handleRequest(HttpServletRequest request, HttpServletResponse response) {
 *         // Return "success" or "error" to trigger corresponding response
 *         return "success";
 *     }
 * }
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for controller annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Repeatable(Responses.class)
public @interface Response {

    /**
     * The name of the response, which matches the string returned by the event.
     * Common values: "success", "error", "threshold-exceeded".
     */
    String name();

    /**
     * Response type. One of:
     * none, view, view-last, view-last-noparam, view-home,
     * request, request-redirect, request-redirect-noparam, request-redirect-last,
     * url, cross-redirect
     */
    String type();

    /**
     * Depending on the type, either the view name, request URI, or URL.
     */
    String value() default "";

    /**
     * If set to false, prevents saving this view to session for view-last responses.
     * Default: true
     */
    String allowViewSave() default "";

    /**
     * Saves the last (previous) request's view for future use with view-last responses.
     * Default: false
     */
    String saveLastView() default "false";

    /**
     * Saves the current request's view for future use with view-last responses.
     * Default: false
     */
    String saveCurrentView() default "false";

    /**
     * Saves the current request's view for future use with view-home responses.
     * Default: false
     */
    String saveHomeView() default "false";

    /**
     * HTTP status code for redirects. Common values: 301, 302, 303, 307.
     */
    String statusCode() default "";

    /**
     * For redirects: whether to save request attributes to pass to next request.
     * Values: "all", "messages", "none"
     */
    String saveRequest() default "";

    /**
     * For redirects: connection state. Set to "close" to force Connection: close header.
     */
    String connectionState() default "";

    /**
     * For redirects: if true, allows browser to cache 301 redirects.
     * Default: false
     */
    String allowCacheRedirect() default "";

    /**
     * Redirect parameters to include. Each string should be in format "name" or "name=from" or "name:value".
     */
    RedirectParameter[] redirectParameters() default {};
}
