package org.ofbiz.service.job;

import org.ofbiz.entity.GenericValue;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * Basic job information, used to describe newly-started jobs.
 * Simplification of {@link Job} interface for returning from {@link org.ofbiz.service.LocalDispatcher} and general usage (SCIPIO).
 * SCIPIO: {@link Job} now implements this interface into which several methods have now been moved.
 */
public interface JobInfo {
    JobInfo NONE = new NullJobInfo();
    String LOG_ID_SEP = "/";

    /**
     * Returns false if the job was not accepted or started by a call to {@link org.ofbiz.service.LocalDispatcher#runAsync}
     * or {@link org.ofbiz.service.LocalDispatcher#schedule}, otherwise true.
     * This is only meaningful if the abort was not an error otherwise an exception would be thrown.
     */
    default boolean isScheduled() { return true; }

    /**
     * Returns non-null if the job was not accepted or started by a call to {@link org.ofbiz.service.LocalDispatcher#runAsync}
     * or {@link org.ofbiz.service.LocalDispatcher#schedule}.
     * This is only meaningful if the abort was not an error otherwise an exception would be thrown.
     */
    default String getMessage() { return null; }

    /**
     * Returns the ID of this Job.
     */
    String getJobId();

    /**
     * Returns the name of this Job.
     */
    String getJobName();

    /**
     * Returns the time this job is scheduled to start.
     */
    Date getStartTime();

    /**
     * Returns the service this job invokes or null if not applicable (SCIPIO).
     */
    String getServiceName();

    /**
     * Returns a keyword describing the job type (generic, persisted, purge) (SCIPIO).
     * NOTE: generic normally refers to non-persisted async.
     */
    String getJobType();

    /**
     * Returns true only if the job is persisted as GenericValue entity (SCIPIO).
     */
    boolean isPersisted();

    /**
     * Returns the JobSandbox value associated with the job or null if not applicable or not persisted (SCIPIO).
     */
    GenericValue getJobValue();

    /**
     * Returns the pool for the job or null if not applicable (SCIPIO).
     * NOTE: This is a logical JobSandbox pool and does not necessarily reflect a physical or JVM thread pool.
     */
    String getJobPool();

    /**
     * Returns a log representation (like toString), usually the job ID, name and service, without brackets/parenthesis (SCIPIO).
     * NOTE: By convention in logs and exceptions this is wrapped in brackets [] by caller.
     */
    default String toLogId() {
        // Use less verbose for now
        //return LogUtil.toLogPropsNonNull("id", getJobId(), "name", getJobName(), "service", getServiceName(), "type", getJobType()).toString();
        return toLogId(getJobId(), getJobName(), getServiceName(), getJobType());
    }

    /**
     * Returns a log representation (like toString) for the map or JobSandbox GenericValue, usually the job ID, name and service, without brackets/parenthesis (SCIPIO).
     * NOTE: By convention in logs and exceptions this is wrapped in brackets [] by caller.
     */
    static String toLogId(Map<String, Object> job, String type) {
        // Use less verbose for now
        //return LogUtil.toLogPropsNonNull("id", job.get("jobId"), "name", job.get("jobName"), "service", job.get("serviceName"), "type", type).toString();
        return toLogId((String) job.get("jobId"), (String) job.get("jobName"), (String) job.get("serviceName"), type);
    }

    /**
     * Returns a log representation (like toString) for the map or JobSandbox GenericValue, usually the job ID, name and service, without brackets/parenthesis (SCIPIO).
     * NOTE: By convention in logs and exceptions this is wrapped in brackets [] by caller.
     */
    static String toLogId(Map<String, Object> job) {
        // Use less verbose for now
        //return LogUtil.toLogPropsNonNull("id", job.get("jobId"), "name", job.get("jobName"), "service", job.get("serviceName")).toString();
        return toLogId(job, null);
    }

    /**
     * Returns a log representation (like toString) for the given fields, usually the job ID, name and service, without brackets/parenthesis (SCIPIO).
     * NOTE: By convention in logs and exceptions this is wrapped in brackets [] by caller.
     */
    static String toLogId(String jobId, String jobName, String serviceName, String type) {
        // Use less verbose for now
        //return LogUtil.toLogPropsNonNull("id", job.get("jobId"), "name", job.get("jobName"), "service", job.get("serviceName")).toString();
        return jobId + LOG_ID_SEP + jobName + LOG_ID_SEP + serviceName + LOG_ID_SEP + type;
    }

    class NullJobInfo implements JobInfo {
        @Override
        public boolean isScheduled() { return false; }
        @Override
        public String getJobId() { return null; }
        @Override
        public String getJobName() { return null; }
        @Override
        public Date getStartTime() { return null; }
        @Override
        public String getServiceName() { return null; }
        @Override
        public String getJobType() { return null; }
        @Override
        public boolean isPersisted() { return false; }
        @Override
        public GenericValue getJobValue() { return null; }
        @Override
        public String getJobPool() { return null; }
    }

    /**
     * Returned by {@link org.ofbiz.service.LocalDispatcher#runAsync} and {@link org.ofbiz.service.LocalDispatcher#schedule}
     * methods when the service is non-fatally skipped (not scheduled by JobManager or other), which currently occurs in some corner cases.
     * This can be distinguished using {@link JobInfo#isScheduled()}.
     * <p>NOTE: Implementations should override the methods to provide more information as available.</p>
     * <p>WARNING: Some places that currently return this might later throw exceptions due to possible design issues in
     * the original classes.</p>
     */
    class UnscheduledJobInfo extends NullJobInfo {
        protected final String serviceName;
        protected final String message;

        public UnscheduledJobInfo(String serviceName, String message) {
            this.serviceName = serviceName;
            this.message = message;
        }

        @Override
        public String getMessage() { return message; }
        @Override
        public String getServiceName() { return serviceName; }
    }
}
