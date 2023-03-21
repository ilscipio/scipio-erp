package com.ilscipio.scipio.service.def;

import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Defines entity permissions for a {@link Service} definition, equivalent to services.xsd service require-permissions
 * check-permission element.
 *
 * <p>NOTE: When used inline on a {@link Service} annotation, only one is supported on the top element;
 * use {@link Service#permissions()} or {@link Permissions} annotation instead, in order to specify an explicit
 * {@link Permissions#joinType()} which is otherwise dangerously ambiguous.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface Permission {

    /**
     * Permission name.
     */
    String permission();

    /**
     * Permission action, if separate from permission.
     */
    String action() default "";

}
