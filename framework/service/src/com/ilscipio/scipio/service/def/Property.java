package com.ilscipio.scipio.service.def;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Defines entity auto-attributes for a {@link Service} definition, equivalent to services.xsd service auto-attributes
 * element.
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface Property {

    String name();

    String description() default "";

    String type() default "";

    String value() default "";

}
