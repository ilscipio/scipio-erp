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
package com.ilscipio.scipio.widget.def.screen;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines an include-portal-page widget, equivalent to widget-screen.xsd include-portal-page element.
 *
 * <p>NOTE: DEPRECATED - This feature is unmaintained and not used in Scipio since 2019-01-29.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}IncludePortalPage(id = "PORTAL_PAGE_ID")
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 *
 * @deprecated Unmaintained and not used in Scipio
 */
@Deprecated
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
@Repeatable(IncludePortalPageList.class)
public @interface IncludePortalPage {

    /**
     * The portal page ID.
     */
    String id() default "";

    /**
     * Show the portal in configuration mode; defaults to false.
     */
    String confMode() default "false";

    /**
     * If a derived private PortalPage exists for the actual UserLogin,
     * show the private PortalPage instead of the original; defaults to true.
     */
    String usePrivate() default "true";
}
