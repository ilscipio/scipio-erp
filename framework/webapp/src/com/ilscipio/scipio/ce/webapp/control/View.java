package com.ilscipio.scipio.ce.webapp.control;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Allows to register a view-definition for a webapp controller from java code.
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.FIELD})
public @interface View {

    /**
     * View name, as referenced in request responses.
     */
    String name();

    String description() default "";

    /**
     * Controller location as URL with controller:// protocol or as webapp name, with default being the default
     * controller of the default webapp.
     * <p>Examples</p>
     * <ul>
     * <li>controller://shop/webapp/shop/WEB-INF/controller.xml</li>
     * <li>shop</li>
     * </ul>
     */
    String controller() default "";

    /**
     * The name of the view handler that will render the output: "screen", "screenfop", etc., default "default".
     *
     * <p>A most comprehensive list can be found in the handlers-controller.</p>
     */
    String type() default "default";

    /**
     * The page mapped to this view.
     */
    String page() default "";


    /**
     * Extended information passed to the view handler.
     */
    String info() default "";

    /**
     * Content-type, in the HTML sense.
     */
    String contentType() default "";

    /**
     * Charset, in the HTML sense.
     *
     * <p>By default "text/html" is used. If the encoding is "none" then no charset will be used.</p>
     */
    String encoding() default "";

    /**
     * Send no-cache headers if set to true, default "false".
     */
    String noCache() default "false";

    /**
     * Provides clickjacking protection by instructing browsers that this page should not be placed within a frame, default "sameorigin".
     *
     * <ul>Possible values:
     * <li>deny - no rendering within a frame</li>
     * <li>sameorigin - no rendering if origin mismatch</li>
     * <li>allow-from: - allow rendering if framing page is within the specified URI domain</li>
     * </ul>
     *
     * <p>Allow from is supported by IE and Firefox, but not Chrome or Safari. It will also interfere with In Page
     * Google Analytics since it requires your page to be framed by Google.</p>
     */
    String xFrameOptions() default "sameorigin";

    /**
     * <p>HTTP Strict-Transport-Security (HSTS) enforces secure (HTTP over SSL/TLS) connections to the server.</p>
     *
     * <p>This reduces impact of bugs in web applications leaking session data through cookies and external links and
     * defends against Man-in-the-middle attacks. HSTS also disables the ability for users to ignore SSL negotiation
     * warnings. If the security of the connection cannot be ensured (e.g. the server's TLS certificate is not trusted),
     * it shows an error message and do not allow the user to access the web application. As recommended by OWASP, by
     * default "max-age=31536000; includeSubDomains" is used except if the server is localhost or 127.0.0.1. If the
     * strict-transport-security is "none" then it will not be used.</p>
     */
    String strictTransportSecurity() default "";

    /**
     * Visibility of the attribute toward public-facing services; "public" or "internal", default "public".
     *
     * <p>Whether to allow rendering this view using public uri/view switch or allow only direct predetermined and
     * explicitly-coded view responses.</p>
     *
     * <p>Default: Typically configured in component://, otherwise "public" (legacy default). When set to internal,
     * behaves like direct-request false in request URIs: views cannot be accessed by view switch, only by direct
     * invocation by requests. Warning: Default "public" but may be changed to "internal" in future or certain
     * controllers.
     */
    String access() default "";

}
