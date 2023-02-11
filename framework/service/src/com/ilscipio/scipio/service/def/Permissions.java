package com.ilscipio.scipio.service.def;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Defines required permissions for a {@link Service} definition, equivalent to services.xsd service require-permissions
 * element.
 *
 * <p>This encapsulates {@link #joinType()}.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface Permissions {

    /**
     * Required permissions join type; "OR" or "AND", default "OR".
     */
    String joinType();

    /**
     * Entity permissions, joined using {@link #joinType()}.
     */
    Permission[] permissions() default {};

    /**
     * Permission services, joined using {@link #joinType()}.
     */
    PermissionService[] services() default {};

}
