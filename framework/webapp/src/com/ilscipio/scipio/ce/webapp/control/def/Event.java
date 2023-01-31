package com.ilscipio.scipio.ce.webapp.control.def;

import com.ilscipio.scipio.ce.base.metrics.Metric;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines controller events, analogous to controller preprocess/postprocessor event, also used for explicit
 * {@link Request} method tagging and to specify custom options other than the event defaults.
 *
 * <p>Corresponds to <code>event</code> from controller definition site-conf.xsd.</p>
 *
 * <p>Boolean types are emulated using strings "true" and "false" to make up for lack of ternary boxed Boolean type,
 * for future-proofing and extension/merge support.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface Event {

    /**
     * Event name (no slashes), required for section other than "request".
     */
    String name() default "";

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
    String controller() default "";

    /**
     * One of: "request", "firstvisit", "preprocessor", "postprocessor", "after-login", "before-logout", "after-logout",
     * default "request".
     *
     * <p>NOTE: If there is no {@link Request}, this is required.</p>
     */
    String section() default "";

    /**
     * A sequence number in the form: 00, 10, 50, ... used to determine the order of preprocessor/postprocessor events.
     *
     * <p>NOTE: Without this number, you must rely on controller definitions to order events, and overrides of events
     * listed are done by even name.</p>
     */
    String sequence() default "";

    /**
     * Event type, usually one of: "java", "service", default "java".
     */
    String type() default "";

    /**
     * Event name, value or expression, usually a service name.
     *
     * <p>For java event methods this can be omitted.</p>
     */
    String invoke() default "";

    /**
     * Event transaction timeout for event method, in seconds; default: "60".
     */
    String transactionTimeout() default "";

    /**
     * Event error handling; if on-error-result and the event returned "error", abort running transaction;
     * "on-error-result", "on-exception", "on-any-error", default "".
     */
    String abortTransaction() default "";

    /**
     * For service-multi, defines if the event should be wrapped in a transaction; "true" or "false", default "true".
     *
     * <p>NOTE: Currently has no effect on single java events, may in future.</p>
     */
    String globalTransaction() default "";

    /**
     * General event transaction enable/disable; "true" or "false", default "true".
     *
     * <p>NOTE: Currently this has NO effect on stock behavior and is here for future use/compatibility.</p>
     */
    String transaction() default "";

    /**
     * Variable (flexible "${...}") expression designating a Java object to synchronize the event on, typically stored
     * in request, session or application attributes.
     *
     * <p>If the object is null, no synchronization occurs. Typically you can setup the object using preprocessor
     * events or filter init method. ${} brackets may be omitted; implied.</p>
     *
     * <ul>Supported expression types (similar to request responses):
     * <li>requestAttributes.xxx</li>
     * <li>sessionAttributes.xxx</li>
     * <li>applicationAttributes.xxx</li>
     * </ul>
     *
     * <p>e.g.: value="${sessionAttributes.shoppingCart}"</p>
     * In addition, it is possible to specify groovy in form: value="${groovy:xxx}"; however, this is slow and not
     * recommended (only "request" and "response" objects may be available).
     */
    String[] synchronizedValues() default {};

    /**
     * {@link Metric} configuration (single).
     * @return Metric
     */
    Metric[] metric() default {};

}
