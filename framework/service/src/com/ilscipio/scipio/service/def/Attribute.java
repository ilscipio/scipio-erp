package com.ilscipio.scipio.service.def;

import org.ofbiz.service.ServiceContext;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a service attribute for a {@link Service} definition, equivalent to services.xsd service attribute element.
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(AttributeList.class)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.FIELD})
public @interface Attribute {

    String name();

    String description() default "";

    /**
     * Attribute type, as {@link String}.
     *
     * <p>NOTE: This is normally the same as {@link Class#getName()}, as may be specified as {@link #typeCls()} instead.</p>
     */
    String type() default "";

    /**
     * Attribute type, as {@link Class}.
     */
    Class<?> typeCls() default Object.class;

    /**
     * Attribute mode; "IN", "OUT", "INOUT".
     *
     * <p>NOTE: This should always be specified unless overriding using only {@link #name()} and {@link #inject()}.</p>
     */
    String mode() default "";

    String optional() default "false";

    /**
     * The value specified will be used for the attribute if no value is passed in.
     *
     * <p>This will only happen if it is okay to not pass a value in, so if this is set then optional will be set to
     * true. If optional=false and this is set then the value will be overridden and with a value in default-value is
     * will set optional=true anyway.</p>
     */
    String defaultValue() default "";

    String formLabel() default "";

    String entityName() default "";

    String fieldName() default "";

    String requestAttributeName() default "";

    String sessionAttributeName() default "";

    String stringMapPrefix() default "";

    String stringListSuffix() default "";

    String formDisplay() default "true";

    /**
     * Applies only to String fields.
     *
     * <p>Only checked for incoming parameters/attributes (could change in the future, but this is meant for validating
     * input from users, other systems, etc). Defaults to "none" meaning no HTML is allowed (will result in an error
     * message). If some HTML is desired then use "any". There was previously "safe" but it's deprecated.</p>
     */
    String allowHtml() default "none";

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

    /**
     * When used as annotation on a protected field of a {@link org.ofbiz.service.LocalService} implementation,
     * when true, the {@link org.ofbiz.service.LocalService#init(ServiceContext)} super method automatically attempts to
     * inject the (default) value with appropriate type on the protected field; "true" or "false", default "false".
     *
     * <p>NOTE: {@link #mode()} Can be left empty while this is set to mark which field to inject, separately from
     * the rest of the attribute definition.</p>
     */
    String inject() default "";


}
