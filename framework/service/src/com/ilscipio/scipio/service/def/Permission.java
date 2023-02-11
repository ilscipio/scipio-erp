package com.ilscipio.scipio.service.def;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Defines entity permissions for a {@link Service} definition, equivalent to services.xsd service require-permissions
 * check-permission element.
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
