package org.ofbiz.service;

/**
 * Common job options for LocalDispatcher {@link LocalDispatcher#runAsync} and {@link LocalDispatcher#schedule} calls.
 * For persisted/schedule, see {@link PersistAsyncOptions}; for non-persisted, use {@link MemoryAsyncOptions},
 * through {@link ServiceOptions#async(boolean)} and {@link ServiceOptions#asyncDefault(boolean)}.
 * WARN: Use {@link AsyncOptions#persist()} to determine if {@link PersistAsyncOptions} because class hierarchy could change.
 * WARN: Members here are likely to change as a SyncServiceOptions class may be created in the future.
 * All members are null (generally meaning use default) and implementations determine the default and the defaults should be documented in their interfaces.
 * NOTE: Although "Service" omitted from name for brevity, generally this means service.
 */
public abstract class AsyncOptions extends ServiceOptions {
    // DEV NOTE: use SafeOptional<T> if null significant, otherwise assumed to mean use default (empty string and special values can be used for primitives)
    protected String jobPool;
    protected Long priority;

    protected AsyncOptions() {
    }

    /** Legacy parameter constructor, not recommended for client code - prefer default constructor and setters. */
    protected AsyncOptions(String jobPool, Long priority) {
        this.jobPool = jobPool;
        this.priority = priority;
    }

    /** If true, generally will be instance of {@link PersistAsyncOptions}, otherwise {@link MemoryAsyncOptions} (mainly used by engine). */
    public abstract boolean persist();

    /** The job pool, as configured in serviceengine.xml run-from-pool. */
    public String jobPool() {
        return jobPool;
    }

    /** The job pool, as configured in serviceengine.xml run-from-pool. */
    public AsyncOptions jobPool(String jobPool) {
        this.jobPool = jobPool;
        return this;
    }

    /** The job priority, 0-100, defaults to {@link org.ofbiz.service.job.JobPriority#NORMAL}. */
    public Long priority() {
        return priority;
    }

    /** The job priority, 0-100, defaults to {@link org.ofbiz.service.job.JobPriority#NORMAL}. */
    public AsyncOptions priority(Long priority) {
        this.priority = priority;
        return this;
    }

    @Override
    public String toString() {
        return "{" +
                "jobPool='" + jobPool + '\'' +
                ", priority=" + priority +
                '}';
    }
}
