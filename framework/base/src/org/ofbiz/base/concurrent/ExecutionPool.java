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
package org.ofbiz.base.concurrent;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.DelayQueue;
import java.util.concurrent.Delayed;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinWorkerThread;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.ofbiz.base.lang.SourceMonitored;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.cache.CacheLine;
import org.ofbiz.base.util.cache.UtilCache;

@SourceMonitored
public final class ExecutionPool {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final ExecutorService GLOBAL_BATCH = new ThreadPoolExecutor(0, Integer.MAX_VALUE, 5, TimeUnit.SECONDS, new SynchronousQueue<Runnable>(), new ExecutionPoolThreadFactory(null, "Scipio-batch"));
    // SCIPIO: Use ForkJoinWorkerThreadFactory, essential for jdk9
    //public static final ForkJoinPool GLOBAL_FORK_JOIN = new ForkJoinPool();
    private static final int MAX_CAP = 0x7fff; // FIXME: inaccessible as ForkJoinPool.MAX_CAP
    public static final ForkJoinPool GLOBAL_FORK_JOIN = new ForkJoinPool(Math.min(MAX_CAP, Runtime.getRuntime().availableProcessors()),
            ExecutionForkJoinWorkerThreadFactory.getDefault(), null, false);
    private static final ExecutorService pulseExecutionPool = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors(), new ExecutionPoolThreadFactory(null, "Scipio-ExecutionPoolPulseWorker"));
    private static final Boolean logQueueSize= UtilProperties.getPropertyAsBoolean("cache", "cache.delayqeue.log.enable", false);

    protected static class ExecutionPoolThreadFactory implements ThreadFactory {
        private final ThreadGroup group;
        private final String namePrefix;
        private volatile int count = 1;

        protected ExecutionPoolThreadFactory(ThreadGroup group, String namePrefix) {
            this.group = group;
            this.namePrefix = namePrefix;
        }

        public Thread newThread(Runnable r) {
            Thread t = new Thread(group, r);
            t.setDaemon(true);
            t.setPriority(Thread.NORM_PRIORITY);
            t.setName(namePrefix + "-" + count++);
            return t;
        }
    }

    public static ScheduledExecutorService getScheduledExecutor(ThreadGroup group, String namePrefix, int threadCount, long keepAliveSeconds, boolean preStart) {
        ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(threadCount, new ExecutionPoolThreadFactory(group, namePrefix));
        if (keepAliveSeconds > 0) {
            executor.setKeepAliveTime(keepAliveSeconds, TimeUnit.SECONDS);
            executor.allowCoreThreadTimeOut(true);
        }
        if (preStart) {
            executor.prestartAllCoreThreads();
        }
        return executor;
    }

    public static <F> List<F> getAllFutures(Collection<Future<F>> futureList) {
        List<F> result = new LinkedList<>();
        for (Future<F> future: futureList) {
            try {
                result.add(future.get());
            } catch (ExecutionException | InterruptedException e) {
                Debug.logError(e, module);
            }
        }
        return result;
    }

    /**
     * Adds a pulse to the global/static execution pool delay queue.
     */
    public static void addPulse(Pulse pulse) {
        if(logQueueSize && delayQueue.size() == 10000){
            Debug.logError("DelayQueue size hit 10.000 entries. Slow down possible.",module);
        }
        delayQueue.put(pulse);
    }

    /**
     * Removes a pulse to the global/static execution pool delay queue.
     * <p>SCIPIO: 2.1.0: UtilCache instances now avoid calling this method due to the immense size of the queue, and
     * this is relatively cost-free optimization as long as <code>putIfAbsent</code> operations are preferred over
     * <code>put</code> operations.</p>
     */
    public static void removePulse(Pulse pulse) {
        delayQueue.remove(pulse);
    }

    static {
        int numberOfExecutionPoolPulseWorkers = Runtime.getRuntime().availableProcessors();
        for (int i = 0; i < numberOfExecutionPoolPulseWorkers; i++) {
            pulseExecutionPool.execute(new ExecutionPoolPulseWorker());
        }
    }

    private static final DelayQueue<Pulse> delayQueue = new DelayQueue<>();

    public static class ExecutionPoolPulseWorker implements Runnable {
        @Override
        public void run() {
            try {
                while (true) {
                    delayQueue.take().run();
                }
            } catch (InterruptedException e) {
                Debug.logError(e, module);
            }
        }
    }

    /**
     * A delayed event handler object implementing a {@link #run()} callback method designed to be run at a certain
     * delay from now in the future as a single pulse.
     * <p>Expressed in nanosecond timestamps and nanosecond delay intervals in the constructors; the pulse expires
     * and/or triggers when {@link System#nanoTime()} is equal or greater than {@link #expireTimeNanos}.</p>
     * <p>Used to implement key expiry in {@link UtilCache}, where {@link org.ofbiz.base.util.cache.CacheLine#run()}
     * triggers a backend key removal.</p>
     * <p>SCIPIO: 2.1.0: {@link #compareTo(Delayed)} rewritten to be independent of {@link #equals(Object)} and
     * friendlier, allows #equals(Object) to simply implement instance identity to allow use in collections notably
     * for {@link UtilCache} and {@link CacheLine}.</p>
     * <p>SCIPIO: 2.1.0: Removed final modifiers from methods.</p>
     */
    public static abstract class Pulse implements Delayed, Runnable {

        /**
         * Error code returned by {@link #compareTo(Delayed)} whenever the other object in is an incomparable type.
         * <p>This is returned for friendliness rather than an exception, as the comparison interfaces are generally
         * used for ordering rather than identity like the {@link #equals(Object)} and {@link #hashCode()} operations.</p>
         * <p>SCIPIO: 2.1.0: Added to simplify {@link #compareTo(Delayed)}.</p>
         */
        public static final int CMP_INVALID = 10001;

        protected final long loadTimeNanos;
        protected final long expireTimeNanos;

        protected Pulse(long expireDelayNanos) {
            this(System.nanoTime(), expireDelayNanos);
        }

        protected Pulse(long loadTimeNanos, long expireDelayNanos) {
            this.loadTimeNanos = loadTimeNanos;
            this.expireTimeNanos = loadTimeNanos + expireDelayNanos;
        }

        public long getLoadTimeNanos() {
            return loadTimeNanos;
        }

        public long getExpireTimeNanos() {
            return expireTimeNanos;
        }

        /**
         * Returns delay before expiry in nanoseconds.
         * <p>SCIPIO: 2.1.0: Added.</p>
         */
        public long getExpireDelayNanos() { return getExpireTimeNanos() - getLoadTimeNanos(); }

        @Override
        public long getDelay(TimeUnit unit) {
            return unit.convert(expireTimeNanos - System.nanoTime(), TimeUnit.NANOSECONDS);
        }

        /**
         * Compares pulses based exclusively on natural ordering of {@link #expireTimeNanos}.
         * <p>SCIPIO: TODO: Comparator instead?</p>
         * <p>SCIPIO: 2.1.0: Rewrote to remove dependency on {@link #equals(Object)} and make robust.</p>
         * @see Comparable#compareTo(Object)
         */
        @Override
        public int compareTo(Delayed other) {
            if (!(other instanceof Pulse)) {
                // SCIPIO: 2.1.0: For friendliness I see no reason to throw exception, it doesn't really happen, so
                // instead return flag value
                //throw new IllegalArgumentException("Cannot compare this pulse to non-Pulse type [" +
                //        ((other != null) ? other.getClass().getName() : "null") + "]");
                return CMP_INVALID;
            }
            long r = timeDiff((Pulse) other);
            return (r == 0 ? 0 : (r > 0 ? 1 : -1));
        }

        /**
         * Returns true if and only if the other object is the same as this, by object identity, explicitly and as defined by {@link Object#equals(Object)}.
         * <p>SCIPIO: 2.1.0: Now explicitly uses {@link Object#equals(Object)} for system identity for speed and improved collections support,
         * now possible since re-implementation of {@link #compareTo(Delayed)} which is treated separately for ordering based on expiry time.</p>
         * @see #hashCode()
         * @see Object#equals(Object)
         */
        @Override
        public final boolean equals(Object other) {
            return super.equals(other);
            // SCIPIO: Pre-2.1.0: Old implementation, based entirely on expireTimeNanos:
            //if (other instanceof Pulse) {
            //    return timeDiff((Pulse) other) == 0;
            //}
            //return false;
        }

        /**
         * Returns the identity hash code for this instance, explicitly and as defined by {@link Object#hashCode()}.
         * <p>SCIPIO: 2.1.0: Now explicitly uses {@link Object#hashCode()} for system identity for speed and improved collections support,
         * now possible since re-implementation of {@link #compareTo(Delayed)} which is treated separately for ordering based on expiry time.</p>
         * @see #equals(Object)
         * @see Object#hashCode() 
         * @see System#identityHashCode(Object) 
         */
        @Override
        public final int hashCode() {
            return super.hashCode();
        }

        /**
         * Calculates the difference between this.expireTimeNanos and other.expireTimeNanos.
         * @param other used to calculate the difference
         * @return the time difference of the two instance's expireTimeNanos
         */
        public long timeDiff(Pulse other) {
            return expireTimeNanos - other.expireTimeNanos;
        }
    }

    /**
     * Transfers the current thread context class loader to the ForkJoinPool thread, required since jdk9.
     * May be set using -Djava.util.concurrent.ForkJoinPool.common.threadFactory=org.ofbiz.base.concurrent.ExecutionPool$ExecutionForkJoinWorkerThreadFactory
     */
    public static class ExecutionForkJoinWorkerThreadFactory implements ForkJoinPool.ForkJoinWorkerThreadFactory {
        private static final ForkJoinPool.ForkJoinWorkerThreadFactory DEFAULT = new ExecutionForkJoinWorkerThreadFactory();

        public static ForkJoinPool.ForkJoinWorkerThreadFactory getDefault() {
            return DEFAULT;
        }

        @Override
        public final ForkJoinWorkerThread newThread(ForkJoinPool pool) {
            return new ExecutionForkJoinWorkerThread(pool);
        }

        protected static class ExecutionForkJoinWorkerThread extends ForkJoinWorkerThread {
            protected ExecutionForkJoinWorkerThread(final ForkJoinPool pool) {
                super(pool);
                // set the correct classloader here
                setContextClassLoader(Thread.currentThread().getContextClassLoader());
            }
        }
    }
}
