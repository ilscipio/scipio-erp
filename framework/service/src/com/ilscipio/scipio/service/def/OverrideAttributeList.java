package com.ilscipio.scipio.service.def;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a container annotation for service override (attribute) for a {@link Service} definition, equivalent to services.xsd service override element.
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface OverrideAttributeList {

    OverrideAttribute[] value() default {};

}
