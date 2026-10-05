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
 * Defines a service action for screen widgets, equivalent to widget-common.xsd service element.
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}ServiceAction(serviceName = "getPartyContactMechList", resultMapName = "contactMechList")
 * {@literal @}ServiceAction(serviceName = "findOrders", autoFieldMap = true)
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Repeatable(ServiceActionList.class)
public @interface ServiceAction {

    /**
     * Service name to invoke; required.
     */
    String serviceName();

    /**
     * Field to store service result map; optional.
     *
     * <p>If not specified, results are placed directly into context.</p>
     */
    String resultMapName() default "";

    /**
     * Field name to store list results from service; optional.
     *
     * <p>Used primarily in form list actions to specify where the list results are stored.
     * If not specified, defaults to the form's list-name.</p>
     */
    String resultMapList() default "";

    /**
     * Whether to auto-map context fields to service parameters; optional.
     *
     * <p>Default is true.</p>
     */
    boolean autoFieldMap() default true;

    /**
     * Field containing the input map; optional.
     */
    String resultMapField() default "";

    /**
     * Field assignments for service input.
     */
    FieldMap[] fieldMaps() default {};
}
