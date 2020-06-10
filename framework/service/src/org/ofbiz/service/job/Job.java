/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.service.job;

import org.ofbiz.base.util.LogUtil;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * A scheduled job.
 * <p>A job starts out in the created state. When the job is queued for execution, it
 * transitions to the queued state. While the job is executing it is in the running state.
 * When the job execution ends, it transitions to the finished or failed state - depending
 * on the outcome of the task that was performed.</p>
 */
public interface Job extends Runnable {

    enum State {CREATED, QUEUED, RUNNING, FINISHED, FAILED};
    String LOG_ID_SEP = "/";

    /**
     * Returns the current state of this job.
     */
    State currentState();

    /**
     * Returns the ID of this Job.
     */
    String getJobId();

    /**
     * Returns the name of this Job.
     */
    String getJobName();

    /**
     *  Returns the job execution time in milliseconds.
     *  Returns zero if the job has not run.
     */
    long getRuntime();

    /**
     * Returns true if this job is ready to be queued.
     */
    boolean isValid();

    /**
     * Transitions this job to the pre-queued (created) state. The job manager
     * will call this method when there was a problem adding this job to the queue.
     */
    void deQueue() throws InvalidJobException;

    /**
     * Transitions this job to the queued state.
     */
    void queue() throws InvalidJobException;

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
     * Returns a log representation (like toString) for the given fields, usually the job ID, name and service, without brackets/parenthesis (SCIPIO).
     * NOTE: By convention in logs and exceptions this is wrapped in brackets [] by caller.
     */
    static String toLogId(String jobId, String jobName, String serviceName, String type) {
        // Use less verbose for now
        //return LogUtil.toLogPropsNonNull("id", job.get("jobId"), "name", job.get("jobName"), "service", job.get("serviceName")).toString();
        return jobId + LOG_ID_SEP + jobName + LOG_ID_SEP + serviceName + LOG_ID_SEP + type;
    }

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
     * Puts basic task info into the map (SCIPIO).
     * Moved here from {@link JobPoller#getPoolState()}.
     */
    default Map<String, Object> toTaskInfoMap(Map<String, Object> map) {
        map.put("id", getJobId());
        map.put("name", getJobName());
        map.put("serviceName", getServiceName()); // SCIPIO: Generalized
        map.put("time", getStartTime());
        map.put("runtime", getRuntime());
        return map;
    }

    /**
     * Get basic task info as map (SCIPIO).
     * Moved here from {@link JobPoller#getPoolState()}.
     */
    default Map<String, Object> toTaskInfoMap() {
        return toTaskInfoMap(new HashMap<>());
    }
}

