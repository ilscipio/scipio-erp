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
package com.ilscipio.scipio.widget.def.form;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines auto-fields from a service definition, equivalent to widget-form.xsd auto-fields-service element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Repeatable(AutoFieldsServiceList.class)
public @interface AutoFieldsService {

    /**
     * Service name to get fields from; required.
     */
    String serviceName();

    /**
     * Map name to get/put values.
     */
    String mapName() default "";

    /**
     * Default field type for generated fields.
     */
    DefaultFieldType defaultFieldType() default DefaultFieldType.EDIT;

    /**
     * Default position for generated fields.
     */
    int defaultPosition() default 1;

    /**
     * SCIPIO: Default position span for generated fields.
     */
    int defaultPositionSpan() default 0;
}
