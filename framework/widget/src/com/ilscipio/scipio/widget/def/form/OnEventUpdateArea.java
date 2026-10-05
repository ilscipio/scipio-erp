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

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Defines a form-level AJAX update area, equivalent to widget-form.xsd on-event-update-area element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface OnEventUpdateArea {

    /**
     * Event type: "submit", etc.
     */
    String eventType();

    /**
     * ID of the area to update.
     */
    String areaId();

    /**
     * Target URL for the AJAX update.
     */
    String areaTarget();

    /**
     * Parameters to pass to the update area target.
     */
    ParameterDef[] parameters() default {};

    /**
     * Auto-parameters from a service.
     */
    AutoParametersService autoParametersService() default @AutoParametersService(UNSET = true);

    /**
     * Auto-parameters from an entity.
     */
    AutoParametersEntity autoParametersEntity() default @AutoParametersEntity(UNSET = true);
}
