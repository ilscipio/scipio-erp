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
package com.ilscipio.scipio.service.def;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines required permissions permission service for a {@link Service} definition, equivalent to services.xsd service
 * require-permissions permission-service element.
 *
 * <p>NOTE: When used inline on a {@link Service} annotation, only one is supported on the top element;
 * use {@link Service#permissions()} or {@link Permissions} annotation instead, in order to specify an explicit
 * {@link Permissions#joinType()} which is otherwise dangerously ambiguous.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface PermissionService {

    /**
     * Service name.
     */
    String service();

    /**
     * This will be used in error messages, etc; defaults to service name.
     */
    String resourceDescription() default "";

    /**
     * Main action to pass to permission service; "ADMIN", "CREATE", "UPDATE", "DELETE", "VIEW".
     */
    String mainAction() default "";

}
