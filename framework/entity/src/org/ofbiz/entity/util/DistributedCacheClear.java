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
package org.ofbiz.entity.util;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericPK;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;

import java.util.HashMap;
import java.util.Map;

/**
 * Distributed Cache Clear interface definition
 *
 * <p>SCIPIO: 3.0.0: Instances may now support thread-friendly constructor using same arguments as {@link #setDelegator(Delegator, String)}.</p>
 */
public interface DistributedCacheClear {

    /**
     * Initialization function.
     *
     * <p>SCIPIO: 3.0.0: Usage is now discouraged/optional; constructors now support this same interface instead, for thread safety.</p>
     */
    default void setDelegator(Delegator delegator, String userLoginId) {
    }

    public void distributedClearCacheLine(GenericValue value);

    public void distributedClearCacheLineFlexible(GenericEntity dummyPK);

    public void distributedClearCacheLineByCondition(String entityName, EntityCondition condition);

    public void distributedClearCacheLine(GenericPK primaryKey);

    public void clearAllCaches();

    /** Clears all util caches, automatically includes entity caches (SCIPIO). */
    default void clearAllUtilCaches() {
        clearAllUtilCaches(null);
    }

    /** Clears all util caches, automatically includes entity caches (SCIPIO). */
    public void clearAllUtilCaches(Map<String, Object> context);

    /**
     * Runs an arbitrary distributed (cache clear) service (SCIPIO).
     * <p>NOTE: Context may be modified (userLogin, other) and must be modifiable. userLogin is added only if null in
     * context.</p>
     */
    public void runDistributedService(String serviceName, Map<String, Object> context); // SCIPIO

    /** Runs an arbitrary distributed (cache clear) service (SCIPIO). */
    default void runDistributedService(String serviceName) {
        runDistributedService(serviceName, new HashMap<>());
    }
}
