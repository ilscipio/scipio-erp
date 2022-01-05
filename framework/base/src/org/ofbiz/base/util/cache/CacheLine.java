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
package org.ofbiz.base.util.cache;

import org.ofbiz.base.concurrent.ExecutionPool;

/**
 * Wraps a single element in a cache line instance in a UtilCache, thin single object wrapper.
 * <p>SCIPIO: NOTE: Cache line instances are created once per UtilCache.put() call (and once per putIfAbsent() where no
 * value existed), so they are never added more than once to the cache and so should be removed only once from all
 * internal data structures. This property combines with Scipio's modification of
 * {@link ExecutionPool.Pulse#equals(Object)} implementation giving cache line object identity (simply calling
 * {@link Object#equals(Object)}) to maintain the unique state of every successful put*() request.</p>
 * <p>SCIPIO: 2.1.0: Modified for UtilCache.Index support.</p>
 */
public abstract class CacheLine<V> extends ExecutionPool.Pulse {

    protected CacheLine(long loadTimeNanos, long expireDelayNanos) {
        super(loadTimeNanos, expireDelayNanos);
    }

    /**
     * Returns the cache line's main key in {@link UtilCache#memoryTable}.
     * <p>SCIPIO: 2.1.0: Added for UtilCache.Index support.</p>
     */
    public abstract <R> R getKey();

    /**
     * Returns the cache line's value, the raw instance.
     */
    public abstract V getValue();

    /**
     * changeLine callback.
     * <p>SCIPIO: 2.1.0: Deprecation: This is considered deprecated as seldom-used and requires unwanted overhead to implement.</p>
     */
    protected abstract CacheLine<V> changeLine(boolean useSoftReference, long expireDelaysNanos);

    protected abstract void remove();

    protected boolean differentExpireTime(long delayNanos) {
        return (this.expireTimeNanos - loadTimeNanos - delayNanos) != 0;
    }

    protected void cancel() {}

    @Override
    public void run() {
        remove();
    }
}

