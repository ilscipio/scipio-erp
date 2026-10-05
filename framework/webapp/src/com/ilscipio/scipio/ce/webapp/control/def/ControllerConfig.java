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
 * Defines controller-level configuration, analogous to the site-conf root element in site-conf.xsd.
 *
 * <p>This annotation is placed on a class that serves as the controller configuration definition.
 * It provides controller-level settings like error page, status code, default request, etc.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}ControllerConfig(
 *     controller = "shop",
 *     description = "Shop controller configuration",
 *     errorpage = "/error/error.jsp",
 *     defaultRequest = "main",
 *     includes = {
 *         {@literal @}Include(location = "component://common/webcommon/WEB-INF/common-controller.xml")
 *     }
 * )
 * public class ShopControllerConfig {}
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for controller annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface ControllerConfig {

    /**
     * Controller location as URL with controller:// protocol or as webapp name.
     */
    String controller() default "";

    /**
     * Controller description.
     */
    String description() default "";

    /**
     * Owner of this controller.
     */
    String owner() default "";

    /**
     * URI to forward when error occurs. Supports flexible expressions (${}).
     */
    String errorpage() default "";

    /**
     * Default HTTP status code for redirects. Common values: 301, 302, 303, 307.
     */
    String statusCode() default "";

    /**
     * Default request URI when a request cannot be found.
     */
    String defaultRequest() default "";

    /**
     * View used for protection mechanism (tarpitting).
     */
    String protectView() default "";

    /**
     * Controller includes.
     */
    Include[] includes() default {};

    /**
     * View-as-JSON configuration.
     */
    ViewAsJson viewAsJson() default @ViewAsJson(enabled = "");

    /**
     * Common settings for requests and views.
     */
    CommonSettings commonSettings() default @CommonSettings;

    /**
     * Input/output filters configuration.
     */
    InputOutputFilters inputOutputFilters() default @InputOutputFilters;
}
