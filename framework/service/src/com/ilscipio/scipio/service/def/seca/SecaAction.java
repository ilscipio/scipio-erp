package com.ilscipio.scipio.service.def.seca;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a Scipio service ECA action, equivalent to services-eca.xsd ECA action element.
 *
 * <p>By default, when the service name attribute is omitted, the annotated service's name is used, taken
 * from the attached {@link com.ilscipio.scipio.service.def.Service} definition.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface SecaAction {

    /**
     * Service name to invoke; defaults to name borrowed from attached {@link com.ilscipio.scipio.service.def.Service} definition.
     *
     * <p>NOTE: You can omit either this name or the one on {@link SecaAction}, but not both; it is generally more useful
     * to omit the one on the {@link SecaAction} definition so that the service implementation is being invoked, since setting
     * the one on {@link Seca} to the same service as {@link com.ilscipio.scipio.service.def.Service}
     * means you are defining behavior directly on the service being intercepted, which may be considered an anti-pattern.</p>
     */
    String service() default "";

    /**
     * Service invocation mode; "sync" or "async", default "sync".
     */
    String mode() default "";

    /**
     * User to run as; defaults to currently running (not system for SECAs).
     */
    String runAsUser() default "";

    String resultMapName() default "";

    /**
     * Whether to separate service invocation in a new transaction; "true" or "false", default "false".
     */
    String newTransaction() default "";

    /**
     * Whether to inject service invocation result into context of next service invocation; "true" or "false", default "true".
     */
    String resultToContext() default "";

    /**
     * If true, copies the action's result Map into the service's result Map; "true" or "false", default "false".
     */
    String resultToResult() default "";

    String ignoreFailure() default "";

    String ignoreError() default "";

    String persist() default "";

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
     * Assignments, executed immediately before this action.
     */
    SecaSet[] assignments() default {};

    @SecaAction()
    class DefaultType {}

}
