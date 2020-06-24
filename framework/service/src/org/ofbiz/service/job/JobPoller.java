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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.ofbiz.base.config.GenericConfigException;
import org.ofbiz.base.start.Start;
import org.ofbiz.base.util.Assert;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.service.config.ServiceConfigListener;
import org.ofbiz.service.config.ServiceConfigUtil;
import org.ofbiz.service.config.model.ServiceConfig;
import org.ofbiz.service.config.model.ThreadPool;

/**
 * Job poller. Queues and runs jobs.
 */
public final class JobPoller implements ServiceConfigListener {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final JobPoller INSTANCE = new JobPoller();

    // SCIPIO: Changed these and methods to instance members (bad statics)
    private final AtomicInteger created = new AtomicInteger();
    private final ConcurrentHashMap<String, JobManager> jobManagers = new ConcurrentHashMap<>();
    private final ThreadPoolExecutor executor = createThreadPoolExecutor();
    private final Thread jobManagerPollerThread;

    // SCIPIO: Debug logging controls
    private final long noCapacityWarnInterval = UtilProperties.getPropertyAsLong("service", "jobManager.debug.poll.noCapacityWarnInterval", -1);
    private final boolean noCapacityWarnIntervalVerbose = UtilProperties.getPropertyAsBoolean("service", "jobManager.debug.poll.noCapacityWarnInterval.verbose", false);
    private final long pollSleepWarnInterval = UtilProperties.getPropertyAsLong("service", "jobManager.debug.poll.pollSleepWarnInterval", -1);
    private final boolean pollSleepWarnIntervalVerbose = UtilProperties.getPropertyAsBoolean("service", "jobManager.debug.poll.pollSleepWarnInterval.verbose", false);
    private final long startupPollSleepWarnInterval = UtilProperties.getPropertyAsLong("service", "jobManager.debug.poll.startupPollSleepWarnInterval", -1);
    private final boolean startupPollSleepWarnIntervalVerbose = UtilProperties.getPropertyAsBoolean("service", "jobManager.debug.poll.startupPollSleepWarnInterval.verbose", false);
    private final int debugJobStatsTopServiceCount = UtilProperties.getPropertyAsInteger("service", "jobManager.debug.stats.topServiceCount", 10);

    /**
     * Returns the <code>JobPoller</code> instance.
     */
    public static JobPoller getInstance() {
        return INSTANCE;
    }

    private ThreadPoolExecutor createThreadPoolExecutor() {
        try {
            ThreadPool threadPool = getThreadPoolConfig();
            return new ThreadPoolExecutor(threadPool.getMinThreads(), threadPool.getMaxThreads(), threadPool.getTtl(),
                    TimeUnit.MILLISECONDS, new PriorityBlockingQueue<>(threadPool.getJobs(), createPriorityComparator()), new JobInvokerThreadFactory(), new ThreadPoolExecutor.AbortPolicy());
        } catch (GenericConfigException e) {
            Debug.logError(e, "Exception thrown while getting <thread-pool> model, using default <thread-pool> values: ", module);
            return new ThreadPoolExecutor(ThreadPool.MIN_THREADS, ThreadPool.MAX_THREADS, ThreadPool.THREAD_TTL,
                    TimeUnit.MILLISECONDS, new PriorityBlockingQueue<>(ThreadPool.QUEUE_SIZE, createPriorityComparator()), new JobInvokerThreadFactory(), new ThreadPoolExecutor.AbortPolicy());
        }
    }

    private static Comparator<Runnable> createPriorityComparator() {
        return new Comparator<Runnable>() {

            /**
             * Sorts jobs by priority then by start time
             */
            @Override
            public int compare(Runnable o1, Runnable o2) {
                Job j1 = (Job) o1;
                Job j2 = (Job) o2;
                // Descending priority (higher number returns -1)
                int priorityCompare = Long.compare(j2.getPriority(), j1.getPriority());
                if (priorityCompare != 0) {
                    return priorityCompare;
                }
                // Ascending start time (earlier time returns -1)
                return Long.compare(j1.getStartTime().getTime(), j2.getStartTime().getTime());
            }
        };
    }

    private int pollWaitTime() { // SCIPIO: Simplified
        try {
            return getThreadPoolConfig().getPollDbMillis();
        } catch (GenericConfigException e) {
            Debug.logError(e, "Exception thrown while getting <thread-pool> model, using default <thread-pool> value for poll wait time (" + ThreadPool.POLL_WAIT + ")", module);
            return ThreadPool.POLL_WAIT;
        }
    }

    int queueSize() { // SCIPIO: Now instance method and simplified
        try {
            return getThreadPoolConfig().getJobs();
        } catch (GenericConfigException e) {
            Debug.logError(e, "Exception thrown while getting <thread-pool> model, using default <thread-pool> values: ", module);
            return ThreadPool.QUEUE_SIZE;
        }
    }


    /**
     * Register a {@link JobManager} with the job poller.
     *
     * @param jm The <code>JobManager</code> to register.
     * @throws IllegalArgumentException if <code>jm</code> is null
     */
    public void registerJobManager(JobManager jm) {
        Assert.notNull("jm", jm);
        jobManagers.putIfAbsent(jm.getDelegator().getDelegatorName(), jm);
    }

    private JobPoller() {
        if (pollEnabled()) {
            jobManagerPollerThread = new Thread(new JobManagerPoller(), "Scipio-JobPoller");
            jobManagerPollerThread.setDaemon(false);
            jobManagerPollerThread.start();
        } else {
            jobManagerPollerThread = null;
        }
        ServiceConfigUtil.registerServiceConfigListener(this);
    }

    /**
     * Returns a <code>Map</code> containing <code>JobPoller</code> statistics without queue iteration (SCIPIO).
     */
    public Map<String, Object> getGeneralPoolState() {
        Map<String, Object> poolState = new LinkedHashMap<>(); //new HashMap<>(); // SCIPIO: Linked makes printout clearer
        poolState.put("keepAliveTimeInSeconds", executor.getKeepAliveTime(TimeUnit.SECONDS));
        poolState.put("numberOfCoreInvokerThreads", executor.getCorePoolSize());
        poolState.put("currentNumberOfInvokerThreads", executor.getPoolSize());
        poolState.put("numberOfActiveInvokerThreads", executor.getActiveCount());
        poolState.put("maxNumberOfInvokerThreads", executor.getMaximumPoolSize());
        poolState.put("greatestNumberOfInvokerThreads", executor.getLargestPoolSize());
        poolState.put("numberOfCompletedTasks", executor.getCompletedTaskCount());
        // SCIPIO
        poolState.put("remainingCapacity", executor.getQueue().remainingCapacity());
        poolState.put("queueSize", executor.getQueue().size());
        return poolState;
    }

    /**
     * Returns a <code>Map</code> containing <code>JobPoller</code> statistics including task list.
     */
    public Map<String, Object> getPoolState() {
        return getPoolState(true, true);
    }

    /**
     * Returns a <code>Map</code> containing <code>JobPoller</code> statistics with optional queue task list and queue stats (SCIPIO).
     */
    public Map<String, Object> getPoolState(boolean includeQueueTaskStats, boolean includeTaskList) {
        Map<String, Object> poolState = getGeneralPoolState(); // SCIPIO: Refactored
        if (!includeQueueTaskStats) {
            if (includeTaskList) {
                List<Map<String, Object>> taskList = new ArrayList<>();
                for (Runnable task : executor.getQueue()) {
                    taskList.add(((Job) task).toTaskInfoMap()); // SCIPIO: Refactored
                }
                poolState.put("taskList", taskList);
            }
        } else {
            List<Map<String, Object>> taskList = includeTaskList ? new ArrayList<>() : null;
            Map<String, PoolTaskStats> poolStatsMap = new HashMap<>();
            Map<String, ServiceTaskStats> serviceStatsMap = new HashMap<>();
            int index = 0;
            for (Runnable task : executor.getQueue()) {
                Job job = (Job) task;
                String serviceName = job.getServiceName();
                if (serviceName != null) {
                    ServiceTaskStats stats = serviceStatsMap.get(serviceName);
                    if (stats == null) {
                        stats = new ServiceTaskStats(serviceName, index);
                        serviceStatsMap.put(serviceName, stats);
                    } else {
                        stats.taskCount++;
                        stats.lastQueueIndex = index;
                    }
                }
                String jobPool = job.getJobPool();
                if (jobPool != null) {
                    PoolTaskStats stats = poolStatsMap.get(jobPool);
                    if (stats == null) {
                        stats = new PoolTaskStats(jobPool, index);
                        poolStatsMap.put(jobPool, stats);
                    } else {
                        stats.taskCount++;
                        stats.lastQueueIndex = index;
                    }
                }
                if (includeTaskList) {
                    taskList.add(job.toTaskInfoMap()); // SCIPIO: Refactored
                }
                index++;
            }
            // SCIPIO: NOTE: show smaller lists first or this is unreadable in sentry
            List<PoolTaskStats> poolStatsList = new ArrayList<>(poolStatsMap.values());
            Collections.sort(poolStatsList, Collections.reverseOrder());
            List<Map<String, Object>> jobPoolTaskStats = new ArrayList<>(poolStatsList.size());
            for(PoolTaskStats stats : poolStatsList) {
                jobPoolTaskStats.add(stats.toMap(new LinkedHashMap<>()));
            }
            poolState.put("jobPoolCount", poolStatsList.size());
            poolState.put("jobPoolTaskStats", jobPoolTaskStats);

            List<ServiceTaskStats> serviceStatsList = new ArrayList<>(serviceStatsMap.values());
            Collections.sort(serviceStatsList, Collections.reverseOrder());
            List<Map<String, Object>> serviceTaskStats = new ArrayList<>(debugJobStatsTopServiceCount);
            int i = 0;
            for(ServiceTaskStats stats : serviceStatsList) {
                serviceTaskStats.add(stats.toMap(new LinkedHashMap<>()));
                i++;
                if (i > debugJobStatsTopServiceCount) {
                    break;
                }
            }
            poolState.put("serviceTaskCount", serviceStatsList.size());
            poolState.put("topServiceTaskStats", serviceTaskStats);

            if (includeTaskList) {
                poolState.put("taskList", taskList);
            }
        }
        return poolState;
    }

    private static class TaskStats implements Comparable<TaskStats> {
        int taskCount = 1;
        int firstQueueIndex;
        int lastQueueIndex;
        TaskStats(int firstQueueIndex) {
            this.firstQueueIndex = firstQueueIndex;
            this.lastQueueIndex = firstQueueIndex;
        }

        @Override
        public int compareTo(TaskStats o) {
            int result = Integer.compare(this.taskCount, o.taskCount);
            return (result != 0) ? result : Integer.compare(o.firstQueueIndex, this.firstQueueIndex); // smaller index = greater priority
        }

        public Map<String, Object> toMap(Map<String, Object> map) {
            map.put("taskCount", taskCount);
            map.put("firstQueueIndex", firstQueueIndex);
            map.put("lastQueueIndex", lastQueueIndex);
            return map;
        }
    }

    private static class PoolTaskStats extends TaskStats {
        String poolName;
        PoolTaskStats(String poolName, int firstQueueIndex) {
            super(firstQueueIndex);
            this.poolName = poolName;
        }

        public Map<String, Object> toMap(Map<String, Object> map) {
            map.put("pool", poolName);
            super.toMap(map);
            return map;
        }
    }

    private static class ServiceTaskStats extends TaskStats {
        String serviceName;
        ServiceTaskStats(String serviceName, int firstQueueIndex) {
            super(firstQueueIndex);
            this.serviceName = serviceName;
        }

        public Map<String, Object> toMap(Map<String, Object> map) {
            map.put("service", serviceName);
            super.toMap(map);
            return map;
        }
    }

    public ThreadPool getThreadPoolConfig() throws GenericConfigException { // SCIPIO
        return ServiceConfigUtil.getServiceEngine().getThreadPool();
    }

    public Map<String, Object> getThreadPoolConfigMap() { // SCIPIO
        try {
            return getThreadPoolConfig().toMap();
        } catch (GenericConfigException e) {
            Debug.logError(e, module);
            return Collections.emptyMap();
        }
    }

    protected String toLogPoolConfigStr(boolean verbose) { // SCIPIO: Debug logging
        return "; config: " + getThreadPoolConfigMap() + "; state: " + getPoolState(true, verbose);
    }

    @Override
    public void onServiceConfigChange(ServiceConfig serviceConfig) {
        if (!executor.isShutdown()) {
            ThreadPool threadPool = serviceConfig.getServiceEngine(ServiceConfigUtil.getEngine()).getThreadPool();
            executor.setCorePoolSize(threadPool.getMinThreads());
            executor.setMaximumPoolSize(threadPool.getMaxThreads());
            executor.setKeepAliveTime(threadPool.getTtl(), TimeUnit.MILLISECONDS);
        }
    }

    private boolean pollEnabled() {
        try {
            return getThreadPoolConfig().getPollEnabled();
        } catch (GenericConfigException e) {
            Debug.logError(e, "Exception thrown while getting thread pool configuration for poll enabled", module); // SCIPIO: switched to error
            return false;
        }
    }

    /**
     * Adds a job to the job queue.
     * @throws InvalidJobException if the job is in an invalid state.
     * @throws RejectedExecutionException if the poller is stopped.
     */
    public void queueNow(Job job) throws InvalidJobException {
        job.queue();
        try {
            executor.execute(job);
        } catch (RejectedExecutionException e) { // SCIPIO: NOTE: This happens normally, as other comments indicate
            if (JobManager.isDebug()) {
                Debug.log(JobManager.getDebugProblemLevel(), "Job [" + job.toLogId() + "] execution rejected by thread pool, will be dequeued and rescheduled: " + e.toString()
                        + toLogPoolConfigStr(false), module);
            } else {
                Debug.logInfo("Job [" + job.toLogId() + "] execution rejected by thread pool, will be dequeued and rescheduled: " + e.toString() + toLogPoolConfigStr(false), module);
            }
            job.deQueue();
        } catch (Exception e) { // SCIPIO: Treat anything else as error
            Debug.logError("Job [" + job.toLogId() + "] execution failed, will be dequeued and rescheduled: " + e.toString() + toLogPoolConfigStr(false), module);
            job.deQueue();
        }
    }

    /**
     * Stops the <code>JobPoller</code>. This method is called when Scipio shuts down.
     * The <code>JobPoller</code> cannot be restarted.
     */
    public void stop() {
        Debug.logInfo("Shutting down JobPoller.", module);
        if (jobManagerPollerThread != null) {
            jobManagerPollerThread.interrupt();
        }
        List<Runnable> queuedJobs = executor.shutdownNow();
        for (Runnable task : queuedJobs) {
            Job queuedJob = (Job) task;
            try {
                queuedJob.deQueue();
            } catch (Exception e) {
                if (JobManager.isDebug()) { // SCIPIO: improved logging
                    Debug.log(JobManager.getDebugProblemLevel(), "Problem while dequeueing job [" + queuedJob.toLogId() + "]: " + e.toString(), module);
                } else {
                    Debug.logWarning("Problem while dequeueing job [" + queuedJob.toLogId() + "]: " + e.toString(), module);
                }
            }
        }
        Debug.logInfo("JobPoller shutdown completed.", module);
    }

    private class JobInvokerThreadFactory implements ThreadFactory {
        @Override
        public Thread newThread(Runnable runnable) {
            return new Thread(runnable, "Scipio-JobQueue-" + created.getAndIncrement());
        }
    }

    // Polls all registered JobManagers for jobs to queue.
    private class JobManagerPoller implements Runnable {
        private volatile long lastCapacityFoundTime = 0; // SCIPIO

        // Do not check for interrupts in this method. The design requires the
        // thread to complete the job manager poll uninterrupted.
        public void run() {
            Debug.logInfo("JobPoller thread started; pool config: " + getThreadPoolConfigMap(), module);
            try {
                if (JobManager.isDebug() && startupPollSleepWarnInterval > 0) { // SCIPIO
                    long sleepStart = System.currentTimeMillis();
                    while (Start.getInstance().getCurrentState() != Start.ServerState.RUNNING) {
                        Thread.sleep(1000);
                    }
                    long realSleepTime = System.currentTimeMillis() - sleepStart;
                    if (realSleepTime > startupPollSleepWarnInterval) {
                        Debug.log(JobManager.getDebugProblemLevel(), "Polling thread sleep exceeded expected time at startup, took "
                                + realSleepTime + "ms, limit " + startupPollSleepWarnInterval + "ms" + toLogPoolConfigStr(startupPollSleepWarnIntervalVerbose), module);
                    }
                } else {
                    while (Start.getInstance().getCurrentState() != Start.ServerState.RUNNING) {
                        Thread.sleep(1000);
                    }
                }
                while (!executor.isShutdown()) {
                    // SCIPIO: NOTE: The following line was changed for priority patch
                    //int remainingCapacity = executor.getQueue().remainingCapacity();
                    int remainingCapacity = queueSize() - executor.getQueue().size();
                    if (JobManager.isDebug() && noCapacityWarnInterval > 0) { // SCIPIO
                        if (remainingCapacity > 0 || lastCapacityFoundTime <= 0) {
                            lastCapacityFoundTime = System.currentTimeMillis();
                        } else {
                            long capacityFoundTimeElapsed = System.currentTimeMillis() - lastCapacityFoundTime;
                            if (capacityFoundTimeElapsed >= noCapacityWarnInterval) {
                                Debug.log(JobManager.getDebugProblemLevel(), "No available thread queue capacity for "
                                        + capacityFoundTimeElapsed + "ms, limit " + noCapacityWarnInterval  + "ms"
                                        + "; " + jobManagers.size() + " JobManagers" + toLogPoolConfigStr(noCapacityWarnIntervalVerbose), module);
                                lastCapacityFoundTime = System.currentTimeMillis();
                            }
                        }
                    }
                    if (remainingCapacity > 0) {
                        // Build "list of lists"
                        Collection<JobManager> jmCollection = jobManagers.values();
                        List<Iterator<Job>> pollResults = new ArrayList<>();
                        for (JobManager jm : jmCollection) {
                            if (!jm.isAvailable()) {
                                if (Debug.infoOn()) {
                                    Debug.logInfo("The job manager is locked.", module);
                                }
                                continue;
                            }
                            jm.reloadCrashedJobs();
                            pollResults.add(jm.poll(remainingCapacity).iterator());
                        }
                        // Create queue candidate list from "list of lists"
                        List<Job> queueCandidates = new ArrayList<>();
                        boolean addingJobs = true;
                        while (addingJobs) {
                            addingJobs = false;
                            for (Iterator<Job> jobIterator : pollResults) {
                                if (jobIterator.hasNext()) {
                                    queueCandidates.add(jobIterator.next());
                                    addingJobs = true;
                                }
                            }
                        }
                        // The candidate list might be larger than the queue remaining capacity,
                        // but that is okay - the excess jobs will be dequeued and rescheduled.
                        for (Job job : queueCandidates) {
                            try {
                                queueNow(job);
                            } catch (InvalidJobException e) {
                                Debug.logError(e, module);
                            }
                        }
                    }
                    if (JobManager.isDebug() && pollSleepWarnInterval > 0) { // SCIPIO
                        long sleepStart = System.currentTimeMillis();
                        Thread.sleep(pollWaitTime());
                        long realSleepTime = System.currentTimeMillis() - sleepStart;
                        if (realSleepTime > pollSleepWarnInterval) {
                            Debug.log(JobManager.getDebugProblemLevel(), "Polling thread sleep exceeded expected time, took "
                                    + realSleepTime + "ms, limit " + pollSleepWarnInterval + "ms" + toLogPoolConfigStr(pollSleepWarnIntervalVerbose), module);
                        }
                    } else {
                        Thread.sleep(pollWaitTime());
                    }
                }
            } catch (InterruptedException e) {
                // Happens when JobPoller shuts down - nothing to do.
                Thread.currentThread().interrupt();
            }
            Debug.logInfo("JobPoller thread stopped.", module);
        }
    }
}
