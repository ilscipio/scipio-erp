package com.ilscipio.scipio.service.def;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines required permissions permission service for a {@link Service} definition, equivalent to services.xsd service
 * require-permissions permission-service element.
 *
 * <p>NOTE: When used inline on a {@link Service} annotation, only one is supported on the top element;
 * use {@link Service#permissions()} or {@link Permissions} annotation instead, in order to specify an explicit
 * {@link Permissions#joinType()} which is otherwise dangerously ambiguous.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface PermissionService {

    /**
     * Service name.
     */
    String service();

    /**
     * This will be used in error messages, etc; defaults to service name.
     */
    String resourceDescription() default "";

    /**
     * Main action to pass to permission service; "ADMIN", "CREATE", "UPDATE", "DELETE", "VIEW".
     */
    String mainAction() default "";

}
