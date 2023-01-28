package com.ilscipio.scipio.ce.webapp.control;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines controller request response processor for {@link Request}.
 *
 * <p>Corresponds to <code>response</code> from controller definition site-conf.xsd.</p>
 *
 * <p>The method takes either (Object, HttpServletRequest, HttpServletResponse) or (Object, ServiceContext) where the
 * first parameter is the event result.</p>
 *
 * <p>Boolean types are emulated using strings "true" and "false" to make up for lack of ternary boxed Boolean type,
 * for future-proofing and extension/merge support.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface ResponseProcessor {

}
