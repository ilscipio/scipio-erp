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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.tomcat.util.buf.StringUtils;
import org.ofbiz.base.config.GenericConfigException;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.serialize.XmlSerializer;
import org.ofbiz.service.AsyncOptions;
import org.ofbiz.service.MemoryAsyncOptions;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericRequester;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.PersistAsyncOptions;
import org.ofbiz.service.ServiceDispatcher;
import org.ofbiz.service.ServiceOptions;
import org.ofbiz.service.config.ServiceConfigUtil;
import org.ofbiz.service.job.GenericServiceJob;
import org.ofbiz.service.job.Job;
import org.ofbiz.service.JobInfo;
import org.ofbiz.service.job.JobManager;
import org.ofbiz.service.job.JobManagerException;
import org.ofbiz.service.job.JobPriority;
import org.ofbiz.service.job.PersistedServiceJob;

/**
 * Generic Asynchronous Engine.
 * SCIPIO: Modified for new overloads.
 */
public abstract class GenericAsyncEngine extends AbstractEngine {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected GenericAsyncEngine(ServiceDispatcher dispatcher) {
        super(dispatcher);
    }

    @Override
    public abstract Map<String, Object> runSync(String localName, ModelService modelService, Map<String, Object> context) throws GenericServiceException;

    @Override
    public abstract void runSyncIgnore(String localName, ModelService modelService, Map<String, Object> context) throws GenericServiceException;

    @Override
    public JobInfo runAsync(String localName, ModelService modelService, Map<String, Object> context, boolean persist) throws GenericServiceException {
        return runAsync(localName, modelService, context, null, ServiceOptions.asyncDefault(persist));
    }

    @Override
    public JobInfo runAsync(String localName, ModelService modelService, Map<String, Object> context, GenericRequester requester, boolean persist) throws GenericServiceException {
        return runAsync(localName, modelService, context, requester, ServiceOptions.asyncDefault(persist));
    }

    @Override
    public JobInfo runAsync(String localName, ModelService modelService, Map<String, Object> context, AsyncOptions serviceOptions) throws GenericServiceException {
        return runAsync(localName, modelService, context, null, serviceOptions);
    }

    @Override
    public JobInfo runAsync(String localName, ModelService modelService, Map<String, Object> context, GenericRequester requester, AsyncOptions serviceOptions) throws GenericServiceException {
        DispatchContext dctx = dispatcher.getLocalContext(localName);
        JobInfo job;

        if (serviceOptions.persist()) {
            // SCIPIO: TODO: some options of PersistAsyncOptions not yet implemented here as well as transactionTimeout/requireNewTransaction (latter only
            PersistAsyncOptions pao = (PersistAsyncOptions) serviceOptions;
            // check for a delegator
            if (dispatcher.getDelegator() == null) {
                throw new GenericServiceException("No reference to delegator; cannot run persisted services.");
            }

            GenericValue jobV = null;
            // Build the value object(s).
            try {
                // Create the runtime data
                String dataId = dispatcher.getDelegator().getNextSeqId("RuntimeData");

                GenericValue runtimeData = dispatcher.getDelegator().makeValue("RuntimeData", "runtimeDataId", dataId);

                // SCIPIO: 2019-03-08: Do not throw exceptions needlessly; instead, simply skip any non-serializable values.
                //runtimeData.set("runtimeInfo", XmlSerializer.serialize(context));
                List<String> errorMessageList = new ArrayList<>();
                runtimeData.set("runtimeInfo", XmlSerializer.serializeOrNull(context, errorMessageList));
                runtimeData.create();

                // Get the userLoginId out of the context
                String authUserLoginId = null;
                if (context.get("userLogin") != null) {
                    GenericValue userLogin = (GenericValue) context.get("userLogin");
                    authUserLoginId = userLogin.getString("userLoginId");
                }

                // Create the job info
                String jobId = dispatcher.getDelegator().getNextSeqId("JobSandbox");
                String jobName = Long.toString(System.currentTimeMillis());

                if (errorMessageList.size() > 0) { // SCIPIO
                    Debug.logError("Persisted job [name=" + jobName + ", service=" + modelService.name + "] error: " + errorMessageList.size()
                        + " error(s) while serializing runtimeInfo (context):\n" + StringUtils.join(errorMessageList, '\n'), module);
                }

                Map<String, Object> jFields = UtilMisc.toMap("jobId", jobId, "jobName", jobName, "runTime", UtilDateTime.nowTimestamp());
                String poolId = pao.jobPool();
                if (poolId == null) {
                    poolId = modelService.getJobPoolPersist();
                    if (poolId == null) {
                        poolId = ServiceConfigUtil.getServiceEngine().getThreadPool().getSendToPool();
                    }
                }
                jFields.put("poolId", poolId); // SCIPIO: added jobPool
                jFields.put("statusId", "SERVICE_PENDING");
                jFields.put("serviceName", modelService.name);
                jFields.put("loaderName", localName);
                jFields.put("maxRetry", (pao.maxRetry() != null) ? (long) pao.maxRetry() : (long) modelService.maxRetry);
                jFields.put("runtimeDataId", dataId);
                // SCIPIO: NOTE: we leave priority null unless explicit so it picks up default from ModelService/JobPriority
                jFields.put("priority", (pao.priority() != null) ? pao.priority() : null);
                if (UtilValidate.isNotEmpty(authUserLoginId)) {
                    jFields.put("authUserLoginId", authUserLoginId);
                }

                jobV = dispatcher.getDelegator().makeValue("JobSandbox", jFields);
                jobV.create();
                job = PersistedServiceJob.makeResultJob(dctx, jobV, pao); // SCIPIO
            } catch (GenericEntityException e) {
                throw new GenericServiceException("Unable to create persisted job", e);
            // SCIPIO: 2019-03-08: Try to absorb and log these errors as much as possible, but without crashing
            //} catch (SerializeException e) {
            //    throw new GenericServiceException("Problem serializing service attributes", e);
            //} catch (FileNotFoundException e) {
            //    throw new GenericServiceException("Problem serializing service attributes", e);
            //} catch (IOException e) {
            //    throw new GenericServiceException("Problem serializing service attributes", e);
            } catch (GenericConfigException e) {
                throw new GenericServiceException("Problem serializing service attributes", e);
            }

            Debug.logInfo("Persisted job queued : " + jobV.getString("jobName"), module);
        } else {
            MemoryAsyncOptions mao = (MemoryAsyncOptions) serviceOptions;
            JobManager jMgr = dispatcher.getJobManager();
            if (jMgr != null) {
                if (serviceOptions.jobPool() == null || isJobPoolApplicable(serviceOptions.jobPool())) { // SCIPIO: jobPool
                    String name = Long.toString(System.currentTimeMillis());
                    String jobId = modelService.name + "." + name;
                    job = new GenericServiceJob(dctx, jobId, name, modelService, serviceOptions, context, requester); // SCIPIO: jobPool
                    try {
                        dispatcher.getJobManager().runJob((Job) job);
                    } catch (JobManagerException jse) {
                        throw new GenericServiceException("Cannot run job.", jse);
                    }
                } else {
                    // SCIPIO: TODO: REVIEW: Maybe this case should be handled by callers instead?
                    String msg = "Not running async service '" + modelService.name + "' because job pool '" + serviceOptions.jobPool() + "' does not match instance run-from-pool configuration (serviceengine.xml)";
                    if (Debug.verboseOn()) {
                        Debug.logVerbose(msg, module);
                    }
                    job = new JobInfo.UnscheduledJobInfo(modelService.name, msg);
                }
            } else {
                throw new GenericServiceException("Cannot get JobManager instance to invoke the job");
            }
        }
        return job;
    }

    @Override
    protected boolean allowCallbacks(ModelService model, Map<String, Object> context, int mode) throws GenericServiceException {
        return mode == GenericEngine.SYNC_MODE;
    }

    protected static boolean isJobPoolApplicable(String jobPool) throws GenericServiceException {
        try {
            return ServiceConfigUtil.getServiceEngine().getThreadPool().getRunFromPoolNames().contains(jobPool);
        } catch (GenericConfigException e) {
            throw new GenericServiceException(e);
        }
    }
}
