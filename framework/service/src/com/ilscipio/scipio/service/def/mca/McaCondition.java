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
package com.ilscipio.scipio.service.def.mca;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * One condition of an {@link Mca} rule.
 *
 * <p>Set exactly one of {@link #fieldName()}, {@link #headerName()} or {@link #serviceName()};
 * that choice selects the condition-field, condition-header or condition-service form.</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({}) // Used as nested annotation only
public @interface McaCondition {

    /**
     * Message field to test (condition-field), such as "to" or "subject".
     */
    String fieldName() default "";

    /**
     * Message header to test (condition-header).
     */
    String headerName() default "";

    /**
     * Service that decides the condition (condition-service).
     */
    String serviceName() default "";

    /**
     * Comparison operator, such as "equals" or "matches".
     */
    String operator() default "";

    /**
     * Value to compare against.
     */
    String value() default "";
}
