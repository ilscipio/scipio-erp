package com.ilscipio.scipio.service.def.seca;

import com.ilscipio.scipio.service.def.Service;
import com.ilscipio.scipio.service.def.eeca.EecaList;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a Scipio service ECA, equivalent to services-eca.xsd eca element.
 *
 * <p>By default, when the service name attribute is omitted, the annotated service's name is used, taken
 * from the attached {@link com.ilscipio.scipio.service.def.Service} definition.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Repeatable(SecaList.class)
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface Seca {

    /**
     * Source service name to attach to and run the action from; defaults to name borrowed from attached {@link com.ilscipio.scipio.service.def.Service} definition.
     *
     * <p>NOTE: You can omit either this name or the one on {@link SecaAction}, but not both; it is generally more useful
     * to omit the one on the {@link SecaAction} definition so that the service implementation is being invoked, since setting
     * the one on {@link Seca} to the same service as {@link com.ilscipio.scipio.service.def.Service}
     * means you are defining behavior directly on the service being intercepted, which may be considered an anti-pattern.</p>
     */
    String service() default "";

    /**
     * Source service event to attach to and run the action from.
     *
     * <ul>Values:
     * <li><code>global-commit</code> - Runs when the transaction the service is running in is successfully committed.</li>
     * <li><code>global-commit-post-run</code> - Like global-commit, but gets its context post/after the run of the service and all non-global ECA rules.</li>
     * <li><code>global-rollback</code> - Runs when the transaction the service is running in is rolled back.</li>
     * <li><code>auth</code> - Runs before the user is authenticated. Note that when the service to which the eca is associated is executed asynchronously this event occurs at least two times (at the time of the call and at the time of the execution). For this reason it is only safe to use this event to call services that don't change the status of any system.</li>
     * <li><code>in-validate</code> - Runs before the input parameters are validated. Note that when the service to which the eca is associated is executed asynchronously this event occurs at least two times (at the time of the call and at the time of the execution). For this reason it is only safe to use this event to call services that don't change the status of any system.</li>
     * <li><code>out-validate</code></li>
     * <li><code>invoke</code></li>
     * <li><code>commit</code></li>
     * <li><code>return</code></li>
     * </ul>
     */
    String event();

    /**
     * Whether to run the action if the source service invocation returns failure; "true" or "false", default "false".
     */
    String runOnFailure() default "";

    /**
     * Whether to run the action if the source service invocation returns error; "true" or "false", default "false".
     */
    String runOnError() default "";

    /**
     * Conveniently enables or disables the SECA; "true" or "false", default "true".
     */
    String enabled() default "";

    /**
     * Condition to execute actions, as flexible expression which must evaluate to "true" or "false" (as string or boolean).
     *
     * <ul>Available context variables:
     * <li><code></code></li>
     * <li><code></code></li>
     * <li><code></code></li>
     * <li><code></code></li>
     * </ul>
     */
    String condition() default "";

    /**
     * Assignments, executed before all actions.
     *
     * <p>NOTE: To order before a specific action, specify as {@link SecaAction#assignments()}.</p>
     */
    SecaSet[] assignments() default {};

    /**
     * Actions (service invocations).
     *
     * <p>If omitted, a default-options action is used that relies on the {@link Service} annotation being
     * present to infer the service name.</p>
     *
     * <p>NOTE: This is a separate annotation because Scipio ECAs support multiple actions.</p>
     */
    SecaAction[] actions() default {};

}
