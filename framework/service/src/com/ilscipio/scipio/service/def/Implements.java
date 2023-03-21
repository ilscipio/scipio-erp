package com.ilscipio.scipio.service.def;

import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Defines implemented services for a {@link Service} definition, equivalent to services.xsd service implements element.
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(ImplementsList.class)
public @interface Implements {

    /**
     * Implemented service name.
     */
    String service() default "";

    /**
     * If set to true all attributes inherited will have be optional whether or not they were in the implemented service
     * definition; "true" or "false", default "false".
     */
    String optional() default "false";

}
