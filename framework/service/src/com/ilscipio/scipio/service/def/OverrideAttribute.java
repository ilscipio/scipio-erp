package com.ilscipio.scipio.service.def;

import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Defines attribute overrides for a {@link Service} definition, equivalent to services.xsd service implements element.
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(OverrideAttributeList.class)
public @interface OverrideAttribute {

    String name();

    String type() default "";

    String entityName() default "";

    String fieldName() default "";

    /**
     * Attribute mode; "IN", "OUT", "INOUT".
     */
    String mode() default "";

    String optional() default "";

    /**
     * The value specified will be used for the attribute if no value is passed in.
     *
     * <p>This will only happen if it is okay to not pass a value in, so if this is set then optional will be set to
     * true. If optional=false and this is set then the value will be overridden and with a value in default-value is
     * will set optional=true anyway.</p>
     */
    String defaultValue() default "";

    String formLabel() default "";

    String formDisplay() default "";

    /**
     * Applies only to String fields.
     *
     * <p>Only checked for incoming parameters/attributes (could change in the future, but this is meant for validating
     * input from users, other systems, etc). Defaults to "none" meaning no HTML is allowed (will result in an error
     * message). If some HTML is desired then use "any". There was previously "safe" but it's deprecated.</p>
     */
    String allowHtml() default "";

    /**
     * If set to true, if the attribute in context is of different type, it is automatically converted to this
     * attribute's type, otherwise a validation error occurs; "true" or "false", default "false".
     *
     * <p>SCIPIO: 2019-01-04: Added.</p>
     */
    String typeConvert() default "";

    TypeValidate[] typeValidate() default {};

    /**
     * Visibility of the attribute toward public-facing services; "public" or "internal", default "public".
     *
     * <p>When set to "internal", the attribute cannot be assigned from request parameters from direct controller event
     * calls.</p>
     *
     * <p>SCIPIO: Added.</p>
     */
    String access() default "";

    String eventAccess() default "";

}
