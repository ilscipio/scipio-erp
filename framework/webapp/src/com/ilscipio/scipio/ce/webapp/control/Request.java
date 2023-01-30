package com.ilscipio.scipio.ce.webapp.control;

import com.ilscipio.scipio.ce.base.metrics.Metric;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines controller requests with implicit or explicit event definition, analogous to controller request-uri definitions.
 *
 * <p>Corresponds to <code>request-map</code> from controller definition site-conf.xsd.</p>
 *
 * <p>If @Request annotates a method, that method will be used as event method using default event parameters;
 * to specify non-default event options, add the {@link Event} annotation. If @Request annotates a class, then its event
 * method should be annotated with {@link Event}. If @Request annotates a class, it is possible to annotate a method
 * using {@link ResponseProcessor} to return an appropriate request reponse; this substitutes for the </p>
 *
 * <p>Boolean types are emulated using strings "true" and "false" to make up for lack of ternary boxed Boolean type,
 * for future-proofing and extension/merge support.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface Request {

    /**
     * Request URI name (no slashes), required.
     *
     * <p>NOTE: Do not use slashes - reserved for future use - lack of slashes implies a controller request-map URI;
     * also because a name may be separately required.</p>
     *
     * <p>TODO: webapp-relative path support</p>
     */
    String uri();

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
     * The HTTP method allowed for this request; "get", "post" or "all", default "all".
     */
    String method() default "all";

    /**
     * Whether or not to track first visit (related to firstvisit preprocessor); "true" or "false", default "true".
     */
    String trackVisit() default "";

    /**
     * Whether or not to track statistics; "true" or "false", default "true".
     */
    String trackServerHit() default "";

    /**
     * Whether to redirect to https protocol whenever an insecure request is made; "true" or "false", default "true".
     */
    String secure() default "true";

    /**
     * Whether to forward to the login page when the user is not logged in; "true" or "false", default "false".
     */
    String auth() default "false";

    /**
     * Name of an alternative request URI whose event to use for login check; default "checkLogin".
     */
    String authCheckEvent() default "";

    /**
     * Whether to check for HTTPS client (x.509) security and throw exception if not secured; "true" or "false", default "false".
     */
    String cert() default "false";

    /**
     * Whether to allow overriding the view through the URL using the uri/view switch; "true" or "false", default "true".
     */
    String externalView() default "true";

    /**
     * Whether to allow direct public access or only internal request chaining; "true" or "false", default "true".
     */
    String directRequest() default "true";

    /**
     * Controls how the request-map should override or inherit from any existing request-maps with the same uri;
     * "replace" or "merge", default "replace".
     */
    String overrideMode() default "replace";

    /**
     * Description for request.
     */
    String description() default "";

    /**
     * {@link Metric} configuration (single).
     * @return Metric
     */
    Metric[] metric() default {};

    /**
     * Designates a response processor method, which selects the appropriate response by returning controller response
     * directive.
     *
     * <p>NOTE: The event method may also return these same controller response directives, so this method is
     * optional and not needed for simple events.</p>
     *
     * <p>TODO: NOT IMPLEMENTED</p>
     */
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.METHOD})
    @interface ResponseMethod {
    }

}
