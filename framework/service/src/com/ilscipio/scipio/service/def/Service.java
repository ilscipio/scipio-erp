package com.ilscipio.scipio.service.def;

import com.ilscipio.scipio.ce.base.metrics.def.Metric;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a Scipio service, equivalent to services.xsd service element.
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface Service {

    /**
     * Service name; defaults to method name or class name (camelCase).
     *
     * <p>NOTE: The name is not namespaced and may need to be prefixed to be globally unique.</p>
     */
    String name() default "";

    /**
     * Service description.
     */
    String description() default "";

    /**
     * Deprecation message.
     */
    String deprecated() default "";

    /**
     * Deprecation date.
     */
    String deprecatedSince() default "";

    /**
     * Deprecation replacement service/object name.
     */
    String deprecatedBy() default "";

    /**
     * Implemented service interfaces.
     */
    Implements[] implemented() default {};

    /**
     * Service auto-attributes from entity fields.
     */
    EntityAttributes[] entityAttributes() default {};

    /**
     * Service attributes.
     */
    Attribute[] attributes() default {};

    /**
     * Override attributes.
     */
    OverrideAttribute[] overrideAttributes() default {};

    /**
     * Whether a user login is required as attribute ("userLogin") for the call to succeed; "true" or "false", default
     * "false".
     */
    String auth() default "false";

    /**
     * Single permissions service (legacy/convenience).
     *
     * <p>NOTE: If more than one passed, an exception will be thrown. For enhanced control, use {@link #permissions()}.</p>
     */
    PermissionService[] permissionService() default {};

    /**
     * Single permission (legacy/convenience).
     *
     * <p>NOTE: If more than one passed, an exception will be thrown. For enhanced control, use {@link #permissions()}.</p>
     */
    Permission[] permission() default {};

    /**
     * Required permissions, with operator support.
     */
    Permissions[] permissions() default {};

    /**
     * Whether the service should be automatically exported for remote/external access by webapp (SOAP/WSDL/other);
     * "true" or "false", default "false".
     */
    String export() default "false";

    /**
     * Whether attributes should be validated on input and output (except on service error/fail); "true" or "false",
     * default "true".
     */
    String validate() default "true";

    String defaultEntityName() default "";

    /**
     * Whether to wrap service call in a database transaction; "true" or "false", default "true".
     *
     * <p>If set to true and there is no transaction already in place the Service Engine will begin one.
     * If set to false or there is a transaction already in place the Service Engine will do nothing (this also means
     * that if set to false and a transaction is already in place it will do nothing).</p>
     */
    String useTransaction() default "true";

    /**
     * Whether a new transaction will be required and started even if one was already in place for the invoking thread;
     * "true" or "false", default "false".
     *
     * <p>If set to true and there is a transaction already in place the Service Engine will suspend that transaction,
     * begin a new one just for this service, commit or rollback the local transaction when the service is complete, and
     * will resume the original transaction.</p>
     * <p>If set to true and there is no transaction already in place it will just begin a transaction and manage it as
     * would be done for a normal user-transaction=true.</p>
     * <p>If use-transaction=false this setting is ignored.</p>
     * <p>Beware: using require-new-transaction=true in a service called (maybe not directly) by a pre-invoke or earlier
     * event (preprocessor, firstvisit and so on) is not yet supported.</p>
     */
    String requireNewTransaction() default "false";

    /**
     * If set to true the result will be hidden from possible exposition in LocalDispatcher.runSync(); "true" or
     * "false", default "false".
     */
    String hideResultInLog() default "false";

    /**
     * Defines the timeout for the transaction, in seconds; default "0".
     *
     * <p>When omitted or "0" (default), defaults to the value set in the TransactionFactory being used (typically 60
     * seconds). This value is only used if this service begins a transaction (either require-new-transaction=true, or
     * use-transaction=true and there is no other transaction already in place). If use-transaction=false this setting
     * is ignored.</p>
     */
    String transactionTimeout() default "0";

    /**
     * Max retries when invoked as a job (unless overridden by job parameters); default "-1" (no limit).
     */
    String maxRetry() default "-1";

    /**
     * Debug flag; "true" or "false", default "false".
     */
    String debug() default "";

    /**
     * Whether invocations of this service should be wrapped in a semaphore to prevent concurrent invocations; "none",
     * "fail" or "wait", default "none".
     */
    String semaphore() default "none";

    String semaphoreWaitSeconds() default "300";

    String semaphoreSleep() default "500";

    /**
     * Logging verbosity; "normal", "debug" or "quiet", default "normal".
     *
     * <p>If set to quiet, the service engine will avoid making info-/timing-level logs about this service; if set to
     * debug, will provide extra info (same as debug=true attribute). May be needed for small or low-level services that
     * get called constantly and burden the log. NOTE: If log is configured as verbose, "quiet" may not be respected.
     * </p>
     *
     * <p>SCIPIO: 2.x.x: Added attribute.</p>
     */
    String log() default "";

    /**
     * Logging verbosity when invoked from ECAs; "normal", "debug", "quiet", default "normal".
     *
     * <p>If set to quiet, the service engine will avoid making info-/timing-level logs about this service when called
     * from ECAs, by default (may be overridden elsewhere); if set to debug, will provide extra info (same as debug=true
     * attribute). May be needed for small or low-level services that get called constantly and burden the log.
     * NOTE: If log is configured as verbose, "quiet" may not be respected.</p>
     *
     * <p>SCIPIO: 2.x.x: Added attribute.</p>
     */
    String logEca() default "";

    /**
     * Trace logging control, as regular expression.
     *
     * <p>If set as a regular expression, the trace log message in particular "Sync service [" + localName + "/" +
     * modelService.name + "] finished in [" + timeToRun + "] milliseconds" is filtered out based on the localName which
     * is the dispatcher name (JMSDispatcher, entity-default, default, etc.). This uses Matcher.matches() meaning ^ and
     * $ are not necessary.</p>
     *
     * <ul>Examples:
     * <li>log-trace-exclude-dispatcher-regex="JMSDispatcher"</li>
     * <li>log-trace-exclude-dispatcher-regex="entity-.*"</li>
     * <li>log-trace-exclude-dispatcher-regex=".*"</li>
     * </ul>
     *
     * <p>SCIPIO: 2.1.0: Added attribute.</p>
     */
    String logTraceExcludeDispatcherRegex() default "";

    /**
     * Default job priority when this service is run either async (ECA) or scheduled as a job; default "50" (normal).
     *
     * <p>NOTE: This currently has no effect on services invoked in "sync" mode.</p>
     *
     * <p>SCIPIO: 2.1.0: Added attribute.</p>
     */
    String priority() default "";

    /**
     * Default job pool when service scheduled as persistent job, when not overridden by caller.
     *
     * <p>NOTE: This occurs during job scheduling; changing this has no effect for a JobSandbox record already
     * scheduled.</p>
     *
     * <p>SCIPIO: 2.1.0: Added attribute.</p>
     */
    String jobPoolPersist() default "";

    /**
     * Fixed service start delay in milliseconds (ms), default "0" (none).
     *
     * <p>NOTE: This currently has no effect on services invoked in "sync" mode.</p>
     *
     * <p>SCIPIO: 2.1.0: Added attribute.</p>
     */
    String startDelay() default "";

    /**
     * Location of a class containing an accessor/factory method pointed to by accessor-invoke; default to location
     * attribute thus same class as service class.
     *
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    String accessorLocation() default "";

    /**
     * Name of a method that works as an accessor/factory method and returns an instance of the service class pointed to
     * by "location".
     *
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    String accessorInvoke() default "";

    String namespace() default "";

    String namespacePrefix() default "";

    Metric[] metrics() default {};

    Property[] properties() default {};

}
