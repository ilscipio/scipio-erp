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
package com.ilscipio.scipio.service.def.eeca;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a Scipio entity ECA action, equivalent to entity-eca.xsd ECA action element.
 *
 * <p>By default, when the service name attribute is omitted, the annotated service's name is used, taken
 * from the attached {@link com.ilscipio.scipio.service.def.Service} definition.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface EecaAction {

    /**
     * Service name to invoke; defaults to name borrowed from attached {@link com.ilscipio.scipio.service.def.Service} definition.
     *
     * <p>NOTE: You can omit either this name or the one on {@link EecaAction}, but not both; it is generally more useful
     * to omit the one on the {@link EecaAction} definition so that the service implementation is being invoked, since setting
     * the one on {@link Eeca} to the same service as {@link com.ilscipio.scipio.service.def.Service}
     * means you are defining behavior directly on the service being intercepted, which may be considered an anti-pattern.</p>
     */
    String service() default "";

    /**
     * Service invocation mode; "sync" or "async", default "sync".
     */
    String mode() default "";

    /**
     * Whether to transfer service result to value if applicable; "true" or "false", default "true".
     */
    String resultToValue() default "";

    /**
     * Abort on error; "true" or "false", default "false".
     */
    String abortOnError() default "";

    /**
     * Rollback transaction on error; "true" or "false", default "false".
     */
    String rollbackOnError() default "";

    /**
     * Persist; "true" or "false", default "false".
     */
    String persist() default "";

    /**
     * Run as user; default "system" (for EECAs only - not SECAs).
     */
    String runAsUser() default "";

    /**
     * If value-attr is specified the generic value object corresponding to the entity in question will be passed to
     * the action service as the given attribute (field) name.
     */
    String valueAttr() default "";

    /**
     * The job priority, 0-100 (low-high), defaults to 50 (normal) (org.ofbiz.service.job.JobPriority#NORMAL).
     */
    String priority() default "";

    /**
     * If set and service is persist="false" (default), only runs the eca if the named pool is named as
     * run-from-pool="[name]" in serviceengine.xml; if persist="true", creates the job using the named pool.
     */
    String jobPool() default "";

    /**
     * For "store" operation only: instead of passing the original entity value to be stored, which may be incomplete,
     * it will be reloaded from the data source by its primary key before being passed in the attribute denoted by
     * value-attr; "true" or "false", default "false".
     */
    String reloadValue() default "";

    /**
     * Assignments, executed immediately before this action.
     */
    EecaSet[] assignments() default {};

    @EecaAction()
    class DefaultType {}

}
