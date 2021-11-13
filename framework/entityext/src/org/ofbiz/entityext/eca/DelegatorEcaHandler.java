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
package org.ofbiz.entityext.eca;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.atomic.AtomicReference;

import org.ofbiz.base.concurrent.ExecutionPool;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.eca.EntityEcaHandler;
import org.ofbiz.entityext.EntityServiceFactory;
import org.ofbiz.service.DispatchContext;

/**
 * EntityEcaUtil
 */
public class DelegatorEcaHandler implements EntityEcaHandler<EntityEcaRule> {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected Delegator delegator = null;
    protected String delegatorName = null;
    protected String entityEcaReaderName = null;
    protected AtomicReference<Future<DispatchContext>> dctx = new AtomicReference<Future<DispatchContext>>();

    public DelegatorEcaHandler() { }

    public void setDelegator(Delegator delegator) {
        this.delegator = delegator;
        this.delegatorName = delegator.getDelegatorName();
        this.entityEcaReaderName = EntityEcaUtil.getEntityEcaReaderName(delegator.getDelegatorBaseName());

        Callable<DispatchContext> creator = new Callable<DispatchContext>() {
            public DispatchContext call() {
                return EntityServiceFactory.getDispatchContext(DelegatorEcaHandler.this.delegator);
            }
        };
        FutureTask<DispatchContext> futureTask = new FutureTask<DispatchContext>(creator);
        if (this.dctx.compareAndSet(null, futureTask)) {
            ExecutionPool.GLOBAL_BATCH.submit(futureTask);
        }

        //preload the cache
        EntityEcaUtil.getEntityEcaCache(this.entityEcaReaderName);
    }

    protected DispatchContext getDispatchContext() throws GenericEntityException {
        Future<DispatchContext> future = this.dctx.get();
        try {
            return future != null ? future.get() : null;
        } catch (ExecutionException e) {
            throw (GenericEntityException) new GenericEntityException(e.getMessage()).initCause(e);
        } catch (InterruptedException e) {
            throw (GenericEntityException) new GenericEntityException(e.getMessage()).initCause(e);
        }
    }

    public Map<String, List<EntityEcaRule>> getEntityEventMap(String entityName) {
        Map<String, Map<String, List<EntityEcaRule>>> ecaCache = EntityEcaUtil.getEntityEcaCache(this.entityEcaReaderName);
        if (ecaCache == null) return null;
        return ecaCache.get(entityName);
    }

    public void evalRules(String currentOperation, Map<String, List<EntityEcaRule>> eventMap, String event, GenericEntity value, boolean isError) throws GenericEntityException {
        // if the eventMap is passed we save a HashMap lookup, but if not that's okay we'll just look it up now
        if (eventMap == null) eventMap = this.getEntityEventMap(value.getEntityName());
        if (UtilValidate.isEmpty(eventMap)) {
            //Debug.logInfo("Handler.evalRules for entity " + value.getEntityName() + ", event " + event + ", no eventMap for this entity", module);
            return;
        }

        List<EntityEcaRule> rules = eventMap.get(event);
        //Debug.logInfo("Handler.evalRules for entity " + value.getEntityName() + ", event " + event + ", num rules=" + (rules == null ? 0 : rules.size()), module);

        if (UtilValidate.isEmpty(rules)) {
            return;
        }

        if (!rules.isEmpty() && Debug.verboseOn()) Debug.logVerbose("Running ECA (" + event + ").", module);
        Set<String> actionsRun = new TreeSet<String>();
        GenericEntity reloadedValue = null; // SCIPIO: 2.1.0: reload-value support for store operation
        boolean checkReload = EntityEcaHandler.OP_STORE.equals(currentOperation);
        for (EntityEcaRule eca: rules) {
            GenericEntity effectiveValue = value;
            if (checkReload && eca.isReloadValue()) {
                if (reloadedValue == null) {
                    if (!value.containsPrimaryKey()) {
                        // This shouldn't happen for store operation, so throw exception because the service relies
                        // on us to provide a complete value.
                        throw new GenericEntityException("ECA for entity [" + eca.getEntityName() + "] for store" +
                                " operation invoked with missing or incomplete primary key; cannot honor reload-value");
                    }
                    if (value instanceof GenericValue) {
                        value.getDelegator().clearCacheLine((GenericValue) value);
                    }
                    reloadedValue = value.getDelegator().findOne(value.getEntityName(), value.getPrimaryKey(), false);
                    if (reloadedValue == null) {
                        throw new GenericEntityException("ECA for entity [" + eca.getEntityName() + "] for store" +
                                " operation cannot be invoked due to missing record in data source for primary key [" +
                                value.getPrimaryKey() + "; cannot honor reload-value");
                    }
                    // If called before the actual store operation (not recommended), merge with incoming value
                    if (EntityEcaHandler.EV_VALIDATE.equals(event) || EntityEcaHandler.EV_RUN.equals(event)) {
                        reloadedValue.putAll(value);
                    }
                }
                effectiveValue = reloadedValue;
            }
            eca.eval(currentOperation, this.getDispatchContext(), effectiveValue, isError, actionsRun);
        }
    }
}
