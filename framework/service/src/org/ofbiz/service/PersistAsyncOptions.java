package org.ofbiz.service;

/**
 * Persisted service job options for persisted {@link LocalDispatcher#runAsync} and {@link LocalDispatcher#schedule} calls.
 * WARN: Use {@link AsyncOptions#persist()} to determine if {@link PersistAsyncOptions} because class hierarchy may change.
 * TODO: REVIEW: should inherit MemoryAsyncOptions like PersistedServiceJob inherits GenericServiceJob? WARN: may be changed (no impact on client code)
 *  There are parameters for non-persisted jobs that do not apply to persisted jobs (transactionTimeout/requireNewTransaction not currently supported)
 *  and vice-versa and it's possible GenericServiceJob needed another abstract class instead of being parent of PersistedServiceJob ...
 */
public class PersistAsyncOptions extends AsyncOptions {
    public static final PersistAsyncOptions DEFAULT = new PersistAsyncOptions() {
        @Override
        public PersistAsyncOptions jobPool(String jobPool) { throw new UnsupportedOperationException(); }
        @Override
        public PersistAsyncOptions priority(Long priority) { throw new UnsupportedOperationException(); }
        @Override
        public PersistAsyncOptions startTime(Long startTime) { throw new UnsupportedOperationException(); }
        @Override
        public PersistAsyncOptions frequency(Integer frequency) { throw new UnsupportedOperationException(); }
        @Override
        public PersistAsyncOptions interval(Integer interval) { throw new UnsupportedOperationException(); }
        @Override
        public PersistAsyncOptions count(Integer count) { throw new UnsupportedOperationException(); }
        @Override
        public PersistAsyncOptions endTime(Long endTime) { throw new UnsupportedOperationException(); }
        @Override
        public PersistAsyncOptions maxRetry(Integer maxRetry) { throw new UnsupportedOperationException(); }
        @Override
        public PersistAsyncOptions eventId(String eventId) { throw new UnsupportedOperationException(); }
        @Override
        public PersistAsyncOptions schedule(Long startTime, Integer frequency, Integer interval, Integer count, Long endTime, Integer maxRetry, String eventId) {
            throw new UnsupportedOperationException();
        }
    };

    protected Long startTime;
    protected Integer frequency;
    protected Integer interval;
    protected Integer count;
    protected Long endTime;
    protected Integer maxRetry;
    protected String eventId;

    /** Default/main constructor, not recommended for client code - use {@link ServiceOptions#async(boolean)} and setters. */
    protected PersistAsyncOptions() {
    }

    /** Legacy parameter constructor, not recommended for client code - use {@link ServiceOptions#async(boolean)} and setters. */
    protected PersistAsyncOptions(String jobPool, Long priority, Long startTime, Integer frequency, Integer interval, Integer count, Long endTime, Integer maxRetry, String eventId) {
        super(jobPool, priority);
        schedule(startTime, frequency, interval, count, endTime, maxRetry, eventId);
    }

    @Override
    public boolean persist() {
        return true;
    }

    @Override
    public PersistAsyncOptions jobPool(String jobPool) {
        return (PersistAsyncOptions) super.jobPool(jobPool);
    }

    @Override
    public PersistAsyncOptions priority(Long priority) {
        return (PersistAsyncOptions) super.priority(priority);
    }

    /** The time in milliseconds the service should run. */
    public Long startTime() {
        return startTime;
    }

    /** The time in milliseconds the service should run. */
    public PersistAsyncOptions startTime(Long startTime) {
        this.startTime = startTime;
        return this;
    }

    /** The frequency of the recurrence (HOURLY, DAILY, MONTHLY, etc). */
    public Integer frequency() {
        return frequency;
    }

    /** The frequency of the recurrence (HOURLY, DAILY, MONTHLY, etc). */
    public PersistAsyncOptions frequency(Integer frequency) {
        this.frequency = frequency;
        return this;
    }

    /** The interval of the frequency recurrence. */
    public Integer interval() {
        return interval;
    }

    /** The interval of the frequency recurrence. */
    public PersistAsyncOptions interval(Integer interval) {
        this.interval = interval;
        return this;
    }

    /** The number of times to repeat. */
    public Integer count() {
        return count;
    }

    /** The number of times to repeat. */
    public PersistAsyncOptions count(Integer count) {
        this.count = count;
        return this;
    }

    /** The time in milliseconds the service should expire. */
    public Long endTime() {
        return endTime;
    }

    /** The time in milliseconds the service should expire. */
    public PersistAsyncOptions endTime(Long endTime) {
        this.endTime = endTime;
        return this;
    }

    /** The max number of retries on failure (-1 for no max). */
    public Integer maxRetry() {
        return maxRetry;
    }

    /** The max number of retries on failure (-1 for no max). */
    public PersistAsyncOptions maxRetry(Integer maxRetry) {
        this.maxRetry = maxRetry;
        return this;
    }

    /** Event ID the job should run, such as SCH_EVENT_STARTUP. */
    public String eventId() {
        return eventId;
    }

    /** Event ID the job should run, such as SCH_EVENT_STARTUP. */
    public PersistAsyncOptions eventId(String eventId) {
        this.eventId = eventId;
        return this;
    }

    /** Scheduling options, typically set together in LocalDispatcher and JobManager API. */
    public PersistAsyncOptions schedule(Long startTime, Integer frequency, Integer interval, Integer count, Long endTime, Integer maxRetry, String eventId) {
        this.startTime = startTime;
        this.frequency = frequency;
        this.interval = interval;
        this.count = count;
        this.endTime = endTime;
        this.maxRetry = maxRetry;
        this.eventId = eventId;
        return this;
    }

    @Override
    public String toString() {
        return "{" +
                "jobPool='" + jobPool + '\'' +
                ", priority=" + priority +
                ", startTime=" + startTime +
                ", frequency=" + frequency +
                ", interval=" + interval +
                ", count=" + count +
                ", endTime=" + endTime +
                ", maxRetry=" + maxRetry +
                ", eventId='" + eventId + '\'' +
                '}';
    }
}
