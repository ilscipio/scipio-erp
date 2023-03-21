package com.ilscipio.scipio.ce.base.metrics.def;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Metric configuration.
 * <p>Calculate and maintain a moving average response time for a Request or Event. Request Metrics can be used for
 * monitoring and reporting, Event Metrics only for reporting. Request Metrics can also be used to trigger an alternate
 * response if the optional threshold attribute is used.</p>
 * <p>The metric works by gathering statistics until a configurable maximum is reached (number of requests or elapsed
 * time), then the average is calculated. A smoothing factor is used to smooth differences between calculations.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface Metric {

    /**
     * Unique name - required.
     */
    String name();

    /**
     * Positive integer number of requests to include in the metrics calculation; default "100".
     */
    String estimationSize() default "";

    /**
     * Positive integer number of milliseconds to include in the metrics calculation, default "1000".
     */
    String estimationTime() default "";

    /**
     * Positive decimal smoothing factor - used to smooth the differences between calculations; "1" disables smoothing, defaults "0.7".
     */
    String smoothing() default "";

    /**
     * The metric threshold in milliseconds; "0.0" disables, default "0.0";
     * <p>If the average response time exceeds this setting, then a "threshold-exceeded" response code will be generated.
     * That response code can be used in a response element. The threshold check will ignore the first three requests -
     * to give the metric a chance to stablize after startup. A value of "0.0" disables the threshold.</p>
     */
    String threshold() default "";

}
