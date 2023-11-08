package com.ilscipio.scipio.service.def;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines type-validate for a {@link Service} attribute definition, equivalent to services.xsd service attribute
 * type-validate element.
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface TypeValidate {

    String method();

    String className() default "org.ofbiz.base.util.UtilValidate";

    String failMessage() default "";

    String failProperty() default "";

    String failResource() default "";

}
