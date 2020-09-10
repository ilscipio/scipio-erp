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
package org.ofbiz.service.engine;

import java.util.Map;

import org.ofbiz.service.AsyncOptions;
import org.ofbiz.service.MemoryAsyncOptions;
import org.ofbiz.service.GenericRequester;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.JobInfo;
import org.ofbiz.service.PersistAsyncOptions;
import org.ofbiz.service.ServiceOptions;

/**
 * Generic Engine Interface.
 * SCIPIO: Modified for new overloads.
 */
public interface GenericEngine {

    int ASYNC_MODE = 22;
    int SYNC_MODE = 21;

    /**
     * Run the service synchronously and return the result.
     *
     * @param localName Name of the LocalDispatcher.
     * @param modelService Service model object.
     * @param context Map of name, value pairs composing the context.
     * @return Map of name, value pairs composing the result.
     * @throws GenericServiceException
     */
    public Map<String, Object> runSync(String localName, ModelService modelService, Map<String, Object> context) throws GenericServiceException;

    /**
     * Run the service synchronously and IGNORE the result.
     * SCIPIO: Added default implementation (redundant).
     *
     * @param localName Name of the LocalDispatcher.
     * @param modelService Service model object.
     * @param context Map of name, value pairs composing the context.
     * @throws GenericServiceException
     */
    default void runSyncIgnore(String localName, ModelService modelService, Map<String, Object> context) throws GenericServiceException {
        runSync(localName, modelService, context);
    }

    /**
     * Run the service asynchronously, passing an instance of GenericRequester that will receive the result.
     * <p>SCIPIO: NOTE: Implementation must override to implement job options.</p>
     *
     * @param localName Name of the LocalDispatcher.
     * @param modelService Service model object.
     * @param context Map of name, value pairs composing the context.
     * @param requester Object implementing GenericRequester interface which will receive the result.
     * @param serviceOptions The service options, either {@link PersistAsyncOptions} for persisted job or {@link MemoryAsyncOptions} for non-persisted async service (SCIPIO);
     *                       for read-only defaults use {@link ServiceOptions#asyncDefault(boolean)}, otherwise {@link ServiceOptions#async(boolean)}.
     * @return The new job information or UnscheduledJobInfo if not scheduled (SCIPIO)
     * @throws GenericServiceException
     */
    default JobInfo runAsync(String localName, ModelService modelService, Map<String, Object> context, GenericRequester requester, AsyncOptions serviceOptions)
            throws GenericServiceException {
        return runAsync(localName, modelService, context, requester, serviceOptions.persist());
    }

    /**
     * Run the service asynchronously and IGNORE the result.
     * <p>SCIPIO: NOTE: Implementation must override to implement job options.</p>
     *
     * @param localName Name of the LocalDispatcher.
     * @param modelService Service model object.
     * @param context Map of name, value pairs composing the context.
     * @param serviceOptions The service options, either {@link PersistAsyncOptions} for persisted job or {@link MemoryAsyncOptions} for non-persisted async service (SCIPIO);
     *                       for read-only defaults use {@link ServiceOptions#asyncDefault(boolean)}, otherwise {@link ServiceOptions#async(boolean)}.
     * @return The new job information or UnscheduledJobInfo if not scheduled (SCIPIO)
     * @throws GenericServiceException
     */
    default JobInfo runAsync(String localName, ModelService modelService, Map<String, Object> context, AsyncOptions serviceOptions) throws GenericServiceException {
        return runAsync(localName, modelService, context, serviceOptions.persist());
    }

    /**
     * Run the service asynchronously, passing an instance of GenericRequester that will receive the result.
     *
     * @param localName Name of the LocalDispatcher.
     * @param modelService Service model object.
     * @param context Map of name, value pairs composing the context.
     * @param requester Object implementing GenericRequester interface which will receive the result.
     * @param persist True for store/run; False for run.
     * @return The new job information or UnscheduledJobInfo if not scheduled (SCIPIO)
     * @throws GenericServiceException
     */
    JobInfo runAsync(String localName, ModelService modelService, Map<String, Object> context, GenericRequester requester, boolean persist)
            throws GenericServiceException;

    /**
     * Run the service asynchronously and IGNORE the result.
     *
     * @param localName Name of the LocalDispatcher.
     * @param modelService Service model object.
     * @param context Map of name, value pairs composing the context.
     * @param persist True for store/run; False for run.
     * @return The new job information or UnscheduledJobInfo if not scheduled (SCIPIO)
     * @throws GenericServiceException
     */
    JobInfo runAsync(String localName, ModelService modelService, Map<String, Object> context, boolean persist) throws GenericServiceException;

    /**
     * Send the service callbacks
     * @param modelService Service model object
     * @param context Map of name, value pairs composing the context
     * @param mode Service mode (sync or async)
     * @throws GenericServiceException
     */
    public void sendCallbacks(ModelService modelService, Map<String, Object> context, int mode) throws GenericServiceException;
    public void sendCallbacks(ModelService modelService, Map<String, Object> context, Map<String, Object> result, int mode) throws GenericServiceException;
    public void sendCallbacks(ModelService modelService, Map<String, Object> context, Throwable t, int mode) throws GenericServiceException;
}

