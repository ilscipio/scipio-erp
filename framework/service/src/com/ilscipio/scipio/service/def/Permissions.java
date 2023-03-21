package com.ilscipio.scipio.service.def;

import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Defines required permissions group for a {@link Service} definition, equivalent to services.xsd service require-permissions
 * element.
 *
 * <p>This encapsulates {@link #joinType()} and is necessary to disambiguate multiple {@link Permission} and
 * {@link PermissionService} entries.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(PermissionsList.class)
public @interface Permissions {

    /**
     * Required permissions join type; "OR" or "AND", default "OR".
     */
    String joinType() default "";

    /**
     * Entity permissions, joined using {@link #joinType()}.
     */
    Permission[] permissions() default {};

    /**
     * Permission services, joined using {@link #joinType()}.
     */
    PermissionService[] services() default {};

}
