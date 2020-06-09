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
package org.ofbiz.entity.cache;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentMap;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.util.EntityUtil;

public class EntityListCache extends AbstractEntityConditionCache<Object, List<GenericValue>> {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public EntityListCache(String delegatorName) {
        super(delegatorName, "entity-list");
    }

    public List<GenericValue> get(String entityName, EntityCondition condition) {
        return this.get(entityName, condition, null);
    }

    public List<GenericValue> get(String entityName, EntityCondition condition, List<String> orderBy) {
        ConcurrentMap<Object, List<GenericValue>> conditionCache = getConditionCache(entityName, condition);
        if (conditionCache == null) {
            return null;
        }
        Object orderByKey = getOrderByKey(orderBy);
        List<GenericValue> valueList = conditionCache.get(orderByKey);
        if (valueList == null) {
            // the valueList was not found for the given ordering, so grab the first one and order it in memory
            Iterator<List<GenericValue>> it = conditionCache.values().iterator();
            if (it.hasNext()) {
                valueList = it.next();
            }

            if (valueList != null) {
                // Does not need to be synchronized; if 2 threads do the same ordering,
                // the result will be exactly the same, and won't actually cause any
                // incorrect results.
                valueList = EntityUtil.orderBy(valueList, orderBy);
                // SCIPIO: Do not allow callers to modify the entity cache lists in place. This should have been here from the start.
                valueList = Collections.unmodifiableList(valueList);
                conditionCache.put(orderByKey, valueList);
            }
        }
        return valueList;
    }

    public void put(String entityName, EntityCondition condition, List<GenericValue> entities) {
        this.put(entityName, condition, null, entities);
    }

    public List<GenericValue> put(String entityName, EntityCondition condition, List<String> orderBy, List<GenericValue> entities) {
        ModelEntity entity = this.getDelegator().getModelEntity(entityName);
        if (entity.getNeverCache()) {
            if (Debug.verboseOn()) { // SCIPIO: only log if verbose on (but still log as warning!)
                Debug.logWarning("Tried to put a value of the " + entityName + " entity in the cache but this entity has never-cache set to true, not caching.", module);
            }
            return null;
        }
        for (GenericValue memberValue : entities) {
            memberValue.setImmutable();
        }
        // SCIPIO: Do not allow callers to modify the entity cache lists in place. This should have been here from the start.
        // TODO: REVIEW: unmodifiableList is also done in duplicate in GenericDelegator#findList because this class will be responsible anyway
        //  if we decide to make an array copy/trim to minimize memory usage and correctness (cpu vs memory), but we'll omit copy in the common case for now
        //  since we know GenericDelegator#findList is nice and these can be large copies (even if arraycopy).
        //entities = Collections.unmodifiableList(new ArrayList<>(entities));
        if (entities instanceof ArrayList) {
            entities = Collections.unmodifiableList(entities);
        } else {
            entities = Collections.unmodifiableList(new ArrayList<>(entities));
        }
        Map<Object, List<GenericValue>> conditionCache = getOrCreateConditionCache(entityName, getFrozenConditionKey(condition));
        return conditionCache.put(getOrderByKey(orderBy), entities);
    }

    public List<GenericValue> remove(String entityName, EntityCondition condition, List<String> orderBy) {
        return super.remove(entityName, condition, getOrderByKey(orderBy));
    }

    public static final Object getOrderByKey(List<String> orderBy) {
        return orderBy != null ? (Object) orderBy : "{null}";
    }
}
