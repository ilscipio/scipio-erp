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

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Configures view-as-json rendering, analogous to view-as-json element in site-conf.xsd.
 *
 * <p>This allows any rendered view to be returned as a JSON data object entry
 * along with potentially other entries, for any request that receives the
 * request parameter "scpViewAsJson=true".</p>
 *
 * <p>SCIPIO: 4.0.0: Added for controller annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface ViewAsJson {

    /**
     * If set to "true", view-as-json will run for any request that receives
     * the request parameter "scpViewAsJson=true".
     * If set to "false", view-as-json will never execute for this webapp.
     * Empty string means not specified (use default).
     */
    String enabled();

    /**
     * The request URI to use to generate the JSON response.
     * It should be a secure one, such as the provided "jsonExplicit" in common controllers.
     */
    String jsonRequestUri() default "";

    /**
     * If "true", view-as-json renders will still update session such as view-last logic.
     * If "false", this will try to prevent session data updates.
     * Default: "false"
     */
    String updateSession() default "";

    /**
     * If "true", the regular login view will be used when not logged in.
     * If "false", the special ajaxLogin view will be used when not logged in.
     * Default: "false"
     */
    String regularLogin() default "";
}
