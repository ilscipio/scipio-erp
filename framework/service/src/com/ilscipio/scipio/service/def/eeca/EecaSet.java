package com.ilscipio.scipio.service.def.eeca;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a Scipio entity ECA (field) set, equivalent to services-eca.xsd ECA set element.
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface EecaSet {

    String fieldName();

    String envName() default "";

    String value() default "";

    /**
     * Special formatting operation.
     *
     * <ul>Values:
     * <li><code>append</code></li>
     * <li><code>to-upper</code></li>
     * <li><code>to-lower</code></li>
     * <li><code>hash-code</code></li>
     * <li><code>long</code></li>
     * <li><code>double</code></li>
     * <li><code>upper-first-char</code></li>
     * <li><code>lower-first-char</code></li>
     * <li><code>db-to-java</code></li>
     * <li><code>java-to-db</code></li>
     * </ul>
     */
    String format() default "";

}
