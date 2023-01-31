package com.ilscipio.scipio.ce.webapp.control.def;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Convenience annotation for parent class that allows to specify a controller for all {@link Request} class
 * and method annotations contained.
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Controller {

    /**
     * Controller location as URL with controller:// protocol or as webapp name, with default being the default
     * controller of the default webapp, which can also be specified explicitly as "default".
     *
     * <ul>Examples:
     * <li>default</li>
     * <li>shop</li>
     * <li>controller://shop/webapp/shop/WEB-INF/controller.xml</li>
     * </ul>
     */
    String controller();

}
