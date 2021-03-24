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

import java.io.Serializable;
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
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.LongAccumulator;
import java.util.stream.Collectors;

import org.ofbiz.base.config.GenericConfigException;
import org.ofbiz.base.start.Start;
import org.ofbiz.base.util.Assert;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;
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

    // SCIPIO: Global service stats, by service name
    private final Map<String, Map<String, GlobalServiceStats>> globalServiceStats = new ConcurrentHashMap<>();
    private final Object globalServiceStatsLock = new Object();
    private final Map<CurrentServiceStats, Boolean> currentServiceStats = new ConcurrentHashMap<>();

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
        return getPoolState(true, true, null);
    }

    /**
     * Returns a <code>Map</code> containing <code>JobPoller</code> statistics with optional queue task list and queue stats (SCIPIO).
     */
    public Map<String, Object> getPoolState(boolean includeQueueTaskStats, boolean includeTaskList) {
        return getPoolState(includeQueueTaskStats, includeTaskList, null);
    }

    /**
     * Returns a <code>Map</code> containing <code>JobPoller</code> statistics with optional queue task list and queue stats (SCIPIO).
     */
    public Map<String, Object> getPoolState(boolean includeQueueTaskStats, boolean includeTaskList, Integer maxTasks) {
        Map<String, Object> poolState = getGeneralPoolState(); // SCIPIO: Refactored
        if (!includeQueueTaskStats) {
            if (includeTaskList) {
                List<Map<String, Object>> taskList = new ArrayList<>();
                int index = 0;
                for (Runnable task : executor.getQueue()) {
                    taskList.add(((Job) task).toTaskInfoMap()); // SCIPIO: Refactored
                    index++;
                    if (maxTasks != null && index >= maxTasks) {
                        break;
                    }
                }
                poolState.put("taskList", taskList);
            }
        } else {
            List<Map<String, Object>> taskList = includeTaskList ? new ArrayList<>() : null;
            Map<String, PoolTaskStats> poolStatsMap = new LinkedHashMap<>();
            Map<String, ServiceTaskStats> serviceStatsMap = new LinkedHashMap<>();
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
                if (includeTaskList && (maxTasks == null || index < maxTasks)) {
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
            poolState.put("serviceTaskCount", serviceStatsList.size());

            List<Map<String, Object>> nextServiceTaskStats = new ArrayList<>(debugJobStatsTopServiceCount);
            int i = 0;
            for(ServiceTaskStats stats : serviceStatsList) {
                nextServiceTaskStats.add(stats.toMap(new LinkedHashMap<>()));
                i++;
                if (i > debugJobStatsTopServiceCount) {
                    break;
                }
            }
            poolState.put("nextServiceTaskStats", nextServiceTaskStats);

            serviceStatsList.sort(Collections.reverseOrder());
            List<Map<String, Object>> popServiceTaskStats = new ArrayList<>(debugJobStatsTopServiceCount);
            i = 0;
            for(ServiceTaskStats stats : serviceStatsList) {
                popServiceTaskStats.add(stats.toMap(new LinkedHashMap<>()));
                i++;
                if (i > debugJobStatsTopServiceCount) {
                    break;
                }
            }
            poolState.put("popServiceTaskStats", popServiceTaskStats);

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
        return "; config: " + getThreadPoolConfigMap() + "; state: " + getPoolState(true, verbose, null)
                + "; executing: " + getCurrentServiceStatsMapList();
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

    protected CurrentServiceStats registerCurrentServiceCall(String serviceName, AbstractJob job, long startTime) {
        CurrentServiceStats serviceStats = new CurrentServiceStats(serviceName, job, startTime);
        currentServiceStats.put(serviceStats, true);
        return serviceStats;
    }

    protected void deregisterCurrentServiceCall(CurrentServiceStats serviceStats) {
        if (serviceStats != null) {
            currentServiceStats.remove(serviceStats);
        }
    }

    public Collection<CurrentServiceStats> getCurrentServiceStats() {
        return currentServiceStats.keySet();
    }

    public List<Map<String, Object>> getCurrentServiceStatsMapList() {
        return currentServiceStats.keySet().stream().sorted(new Comparator<CurrentServiceStats>() {
            @Override
            public int compare(CurrentServiceStats o1, CurrentServiceStats o2) {
                return o1.getServiceName().compareTo(o2.getServiceName());
            }
        }).map(CurrentServiceStats::toMap).collect(Collectors.toList());
    }

    protected GlobalServiceStats registerGlobalServiceCall(String serviceName, AbstractJob job,
                                                           Map<String, Object> serviceResult, Throwable exception,
                                                           long startTs, long runTime) {
        Map<String, GlobalServiceStats> statsTypeMap = globalServiceStats.get(job.getJobType());
        if (statsTypeMap == null) {
            synchronized(globalServiceStatsLock) {
                statsTypeMap = globalServiceStats.get(job.getJobType());
                if (statsTypeMap == null) {
                    statsTypeMap = new ConcurrentHashMap<>();
                    globalServiceStats.put(job.getJobType(), statsTypeMap);
                }
            }
        }
        GlobalServiceStats serviceStats = statsTypeMap.get(serviceName);
        if (serviceStats == null) {
            synchronized (globalServiceStatsLock) {
                serviceStats = statsTypeMap.get(serviceName);
                if (serviceStats == null) {
                    serviceStats = new GlobalServiceStats(serviceName);
                    statsTypeMap.put(serviceName, serviceStats);
                }
            }
        }
        serviceStats.registerServiceCall(job, serviceResult, exception, startTs, runTime);
        return serviceStats;
    }

    /**
     * Returns global service stats for the given service and job type (persist, generic, purge).
     */
    public GlobalServiceStats getGlobalServiceStats(String serviceName, String jobType) {
        Map<String, GlobalServiceStats> statsTypeMap = globalServiceStats.get(jobType);
        if (statsTypeMap == null) {
            return GlobalServiceStats.NONE;
        }
        GlobalServiceStats serviceStats = statsTypeMap.get(serviceName);
        return (serviceStats != null) ? serviceStats : GlobalServiceStats.NONE;
    }

    public Map<String, GlobalServiceStats> getGlobalServiceStatsMap(String jobType) {
        Map<String, GlobalServiceStats> statsTypeMap = globalServiceStats.get(jobType);
        return (statsTypeMap != null) ? Collections.unmodifiableMap(statsTypeMap) : Collections.emptyMap();
    }

    public List<GlobalServiceStats> getGlobalServiceStatsListCopy(String jobType) {
        Map<String, GlobalServiceStats> statsTypeMap = globalServiceStats.get(jobType);
        return (statsTypeMap != null) ? new ArrayList<>(statsTypeMap.values()) : new ArrayList<>();
    }

    public List<Map<String, Object>> getGlobalServiceStatsMapList(String jobType) {
        Map<String, GlobalServiceStats> statsTypeMap = globalServiceStats.get(jobType);
        if (statsTypeMap == null) {
            return Collections.emptyList();
        }
        return statsTypeMap.values().stream().sorted(new Comparator<GlobalServiceStats>() {
            @Override
            public int compare(GlobalServiceStats o1, GlobalServiceStats o2) {
                return o1.getServiceName().compareTo(o2.getServiceName());
            }
        }).map(GlobalServiceStats::toMap).collect(Collectors.toList());
    }

    public void clearGlobalServiceStats(String jobType) {
        if (jobType != null) {
            Map<String, GlobalServiceStats> statsTypeMap = globalServiceStats.get(jobType);
            if (statsTypeMap != null) {
                statsTypeMap.clear();
            }
        } else {
            globalServiceStats.clear();
        }
    }

    /**
     * Global service stats, updated from {@link GenericServiceJob#exec}.
     * WARN: Implemented without synchronization between the fields, possible for some calls to get lost, best-effort.
     */
    public static class CurrentServiceStats implements Serializable {
        protected final String serviceName;
        protected final AbstractJob job;
        protected long startTime;

        protected CurrentServiceStats(String serviceName, AbstractJob job, long startTime) {
            this.serviceName = serviceName;
            this.job = job;
            this.startTime = startTime;
        }

        public boolean isDefined() {
            return (getServiceName() != null);
        }

        public String getServiceName() {
            return serviceName;
        }

        public String getJobType() {
            return getJob().getJobType();
        }

        public String getJobId() {
            return getJob().getJobId();
        }

        public String getJobName() {
            return getJob().getJobName();
        }

        public AbstractJob getJob() {
            return job;
        }

        public long getStartTime() {
            return startTime;
        }

        public long getRunTime(long toTime) { return toTime - getStartTime(); }

        public long getRunTime() { return getRunTime(System.currentTimeMillis()); }

        public Map<String, Object> toMap() {
            return toMap(new LinkedHashMap<>());
        }

        public <M extends Map<String, Object>> M toMap(M map) {
            map.put("serviceName", getServiceName());
            map.put("jobType", getJobType());
            map.put("startTime", UtilDateTime.getTimestamp(getStartTime()));
            map.put("runTime", getRunTime());
            String jobId = getJobId();
            if (jobId != null) {
                map.put("jobId", jobId);
            }
            String jobName = getJobName();
            if (jobName != null) {
                map.put("jobName", jobName);
            }
            return map;
        }
    }

    /**
     * Global service stats, updated from {@link GenericServiceJob#exec}.
     * WARN: Implemented without synchronization between the fields, possible for some calls to get lost, best-effort.
     */
    public static class GlobalServiceStats implements Serializable {
        public static final GlobalServiceStats NONE = new GlobalServiceStats(null);

        protected final String serviceName;
        protected AtomicLong totalCalls = new AtomicLong(0);
        protected AtomicLong totalRuntime = new AtomicLong(0);
        protected LongAccumulator minRuntime = new LongAccumulator(Long::min, 0);
        protected LongAccumulator maxRuntime = new LongAccumulator(Long::max, 0);
        protected AtomicLong successCount = new AtomicLong(0);
        protected AtomicLong failCount = new AtomicLong(0);
        protected AtomicLong errorCount = new AtomicLong(0);
        protected AtomicLong exceptionCount = new AtomicLong(0);

        protected GlobalServiceStats(String serviceName) {
            this.serviceName = serviceName;
        }

        protected void registerServiceCall(AbstractJob job, Map<String, Object> serviceResult, Throwable exception,
                                           long startTs, long runTime) {
            totalCalls.addAndGet(1);
            totalRuntime.addAndGet(runTime);
            minRuntime.accumulate(runTime);
            maxRuntime.accumulate(runTime);
            if (exception != null) {
                exceptionCount.addAndGet(1);
            } else {
                String response = ServiceUtil.getResponse(serviceResult);
                if (ModelService.RESPOND_ERROR.equals(response)) {
                    errorCount.addAndGet(1);
                } else if (ModelService.RESPOND_FAIL.equals(response)) {
                    failCount.addAndGet(1);
                } else {
                    successCount.addAndGet(1);
                }
            }
        }

        public boolean isDefined() {
            return (getServiceName() != null);
        }

        public String getServiceName() {
            return serviceName;
        }

        public long getTotalCalls() {
            return totalCalls.get();
        }

        public long getTotalRuntime() {
            return totalRuntime.get();
        }

        public long getMinRuntime() {
            return minRuntime.get();
        }

        public long getMaxRuntime() {
            return maxRuntime.get();
        }

        public long getAverageRuntime() {
            long totalCalls = getTotalCalls();
            return (totalCalls > 0) ? getTotalRuntime() / totalCalls : 0;
        }

        public long getSuccessCount() {
            return successCount.get();
        }

        public long getFailCount() {
            return failCount.get();
        }

        public long getErrorCount() {
            return errorCount.get();
        }

        public long getExceptionCount() {
            return exceptionCount.get();
        }

        public Map<String, Object> toMap() {
            return toMap(new LinkedHashMap<>());
        }

        public <M extends Map<String, Object>> M toMap(M map) {
            map.put("serviceName", getServiceName());
            map.put("totalCalls", getTotalCalls());
            map.put("totalRuntime", getTotalRuntime());
            map.put("minRuntime", getMinRuntime());
            map.put("maxRuntime", getMaxRuntime());
            map.put("averageRuntime", getAverageRuntime());
            map.put("successCount", getSuccessCount());
            map.put("failCount", getFailCount());
            map.put("errorCount", getErrorCount());
            map.put("exceptionCount", getExceptionCount());
            return map;
        }
    }
}
