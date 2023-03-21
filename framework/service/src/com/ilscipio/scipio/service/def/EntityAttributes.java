package com.ilscipio.scipio.service.def;

import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Defines entity auto-attributes for a {@link Service} definition, equivalent to services.xsd service auto-attributes
 * element.
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(EntityAttributesList.class)
public @interface EntityAttributes {

    /**
     * Entity name from which to import fields as service attributes.
     */
    String entityName() default "";

    /**
     * Field name prefix.
     */
    String prefix() default "";

    /**
     * Mode for attributes; "IN", "OUT" or "INOUT".
     */
    String mode();

    /**
     * Fields to include; "all", "pk", "nonpk", default "all".
     */
    String include() default "all";

    /**
     * Whether attributes are optional; "true" or "false", default "false".
     */
    String optional() default "false";

    /**
     * Whether to automatically include attributes in forms (widgets); "true" or "false", default "true".
     */
    String formDisplay() default "true";

    /**
     * Whether to allow HTML in String attributes; "none" or "any", default "none".
     *
     * <p>Applies only to String fields. Only checked for incoming parameters/attributes (could change in the future,
     * but this is meant for validating input from users, other systems, etc). Defaults to "none" meaning no HTML is
     * allowed (will result in an error message). If some HTML is desired then use "any". There was previously "safe"
     * but it's deprecated.</p>
     */
    String allowHtml() default "none";

    /**
     * If set to true, if the attribute in context is of different type, it is automatically converted to this
     * attribute's type, otherwise a validation error occurs; "true" or "false", default "false".
     *
     * <p>SCIPIO: 2019-01-04: Added.</p>
     */
    String typeConvert() default "";

    /**
     * Field names to exclude.
     */
    String[] excludeFields() default {};

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
