package com.ilscipio.scipio.service.def;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines entity auto-attributes for a {@link Service} definition, equivalent to services.xsd service auto-attributes
 * element.
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(PropertyList.class)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface Property {

    String name();

    String description() default "";

    String type() default "";

    String value() default "";

}
