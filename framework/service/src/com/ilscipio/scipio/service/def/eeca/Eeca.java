package com.ilscipio.scipio.service.def.eeca;

import com.ilscipio.scipio.service.def.Service;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a Scipio entity ECA, equivalent to entity-eca.xsd eca element.
 *
 * <p>By default, when the service name attribute is omitted, the annotated service's name is used, taken
 * from the attached {@link com.ilscipio.scipio.service.def.Service} definition.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Repeatable(EecaList.class)
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface Eeca {

    /**
     * Entity name.
     */
    String entity();

    /**
     * Entity event.
     *
     * <ul>Values:
     * <li><code>validate</code></li>
     * <li><code>run</code></li>
     * <li><code>return</code></li>
     * <li><code>cache-check</code></li>
     * <li><code>cache-put</code></li>
     * <li><code>cache-clear</code></li>
     * </ul>
     */
    String event();

    /**
     * Entity operation.
     *
     * <ul>Values:
     * <li><code>create</code></li>
     * <li><code>store</code></li>
     * <li><code>remove</code></li>
     * <li><code>find</code></li>
     * <li><code>create-store</code></li>
     * <li><code>create-remove</code></li>
     * <li><code>store-remove</code></li>
     * <li><code>create-store-remove</code></li>
     * <li><code>any</code></li>
     * </ul>
     */
    String operation();

    /**
     * Whether to run the action if the entity operation returns error; "true" or "false", default "false".
     */
    String runOnError() default "";

    /**
     * Conveniently enables or disables the ECA; "true" or "false", default "true".
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
     * <p>NOTE: To order before a specific action, specify as {@link EecaAction#assignments()}.</p>
     */
    EecaSet[] assignments() default {};

    /**
     * Actions (service invocations).
     *
     * <p>If omitted, a default-options action is used that relies on the {@link Service} annotation being
     * present to infer the service name.</p>
     *
     * <p>NOTE: This is a separate annotation because Scipio ECAs support multiple actions.</p>
     */
    EecaAction[] actions() default {};

}
