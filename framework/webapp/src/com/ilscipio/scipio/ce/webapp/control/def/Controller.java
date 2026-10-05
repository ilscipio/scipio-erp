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
 * Convenience annotation for parent class that allows to specify a controller for all {@link Request} class
 * and method annotations contained.
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Controller {

    /**
     * Controller location as URL with controller:// protocol or as webapp name, with default being the default
     * controller of the default webapp, which can also be specified explicitly as "default".
     *
     * <ul>Examples:
     * <li>default</li>
     * <li>shop</li>
     * <li>controller://shop/webapp/shop/WEB-INF/controller.xml</li>
     * </ul>
     */
    String controller();

}
