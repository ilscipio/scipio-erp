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
package org.ofbiz.service;

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;

/**
 * A default {@link LocalDispatcherFactory} implementation.
 * SCIPIO: Modified for new overloads.
 */
public class GenericDispatcherFactory implements LocalDispatcherFactory {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static boolean ecasDisabled = false;

    @Override
    public LocalDispatcher createLocalDispatcher(String name, Delegator delegator) {
        if (UtilValidate.isEmpty(name)) {
            throw new IllegalArgumentException("The name of a LocalDispatcher cannot be a null or empty String");
        }
        // attempts to retrieve an already registered DispatchContext with the name "name"
        LocalDispatcher dispatcher = ServiceDispatcher.getLocalDispatcher(name, delegator);
        // if not found then create a new GenericDispatcher object; the constructor will also register a new DispatchContext in the ServiceDispatcher with name "dispatcherName"
        if (dispatcher == null) {
            dispatcher = new GenericDispatcher(name, delegator);
        }
        return dispatcher;
    }

    // The default LocalDispatcher implementation.
    private static class GenericDispatcher extends GenericAbstractDispatcher {

        private GenericDispatcher(String name, Delegator delegator) {
            ClassLoader loader;
            try {
                loader = Thread.currentThread().getContextClassLoader();
            } catch (SecurityException e) {
                loader = this.getClass().getClassLoader();
            }
            this.name = name;
            this.dispatcher = ServiceDispatcher.getInstance(delegator);
            /*
             * FIXME: "this" reference escape. DispatchContext constructor uses
             * this object before it is fully constructed.
             */
            DispatchContext ctx = new DispatchContext(name, loader, this);
            this.dispatcher.register(ctx);
            this.ctx = ctx;
            if (Debug.verboseOn()) Debug.logVerbose("[GenericDispatcher] : Created Dispatcher for: " + name, module);
        }

        @Override
        public void disableEcas() {
            ecasDisabled = true;
        }

        @Override
        public void enableEcas() {
            ecasDisabled = false;
        }

        @Override
        public boolean isEcasDisabled() {
            return ecasDisabled;
        }

        @Override
        public ServiceResult runSync(String serviceName, Map<String, ? extends Object> context) throws ServiceValidationException, GenericServiceException {
            ModelService service = ctx.getModelService(serviceName);
            return dispatcher.runSync(this.name, service, context);
        }

        @Override
        public ServiceResult runSync(String serviceName, Map<String, ? extends Object> context, int transactionTimeout, boolean requireNewTransaction) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            ModelService service = ctx.getModelService(serviceName);
            // clone the model service for updates
            ModelService cloned = new ModelService(service);
            cloned.requireNewTransaction = requireNewTransaction;
            if (requireNewTransaction) {
                cloned.useTransaction = true;
            }
            if (transactionTimeout != -1) {
                cloned.transactionTimeout = transactionTimeout;
            }
            return dispatcher.runSync(this.name, cloned, context);
        }

        @Override
        public ServiceResult runSync(String serviceName, int transactionTimeout, boolean requireNewTransaction, Object... context) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            return runSync(serviceName, ServiceUtil.makeContext(context), transactionTimeout, requireNewTransaction);
        }

        @Override
        public void runSyncIgnore(String serviceName, Map<String, ? extends Object> context) throws GenericServiceException {
            ModelService service = ctx.getModelService(serviceName);
            dispatcher.runSyncIgnore(this.name, service, context);
        }

        @Override
        public void runSyncIgnore(String serviceName, Map<String, ? extends Object> context, int transactionTimeout, boolean requireNewTransaction) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            ModelService service = ctx.getModelService(serviceName);
            // clone the model service for updates
            ModelService cloned = new ModelService(service);
            cloned.requireNewTransaction = requireNewTransaction;
            if (requireNewTransaction) {
                cloned.useTransaction = true;
            }
            if (transactionTimeout != -1) {
                cloned.transactionTimeout = transactionTimeout;
            }
            dispatcher.runSyncIgnore(this.name, cloned, context);
        }

        @Override
        public void runSyncIgnore(String serviceName, int transactionTimeout, boolean requireNewTransaction, Object... context) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            runSyncIgnore(serviceName, ServiceUtil.makeContext(context), transactionTimeout, requireNewTransaction);
        }

        @Override
        public JobInfo runAsync(String serviceName, Map<String, ? extends Object> context, GenericRequester requester, boolean persist, int transactionTimeout, boolean requireNewTransaction) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            // SCIPIO: NOTE: Duplicated in better overload below
            ModelService service = ctx.getModelService(serviceName);
            // clone the model service for updates
            ModelService cloned = new ModelService(service);
            cloned.requireNewTransaction = requireNewTransaction;
            if (requireNewTransaction) {
                cloned.useTransaction = true;
            }
            if (transactionTimeout != -1) {
                cloned.transactionTimeout = transactionTimeout;
            }
            return dispatcher.runAsync(this.name, cloned, context, requester, persist);
        }

        @Override
        public JobInfo runAsync(String serviceName, GenericRequester requester, boolean persist, int transactionTimeout, boolean requireNewTransaction, Object... context) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            return runAsync(serviceName, ServiceUtil.makeContext(context), requester, persist, transactionTimeout, requireNewTransaction);
        }

        @Override
        public JobInfo runAsync(String serviceName, Map<String, ? extends Object> context, GenericRequester requester, boolean persist) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            ModelService service = ctx.getModelService(serviceName);
            return dispatcher.runAsync(this.name, service, context, requester, persist);
        }

        @Override
        public JobInfo runAsync(String serviceName, GenericRequester requester, boolean persist, Object... context) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            return runAsync(serviceName, ServiceUtil.makeContext(context), requester, persist);
        }

        @Override
        public JobInfo runAsync(String serviceName, Map<String, ? extends Object> context, GenericRequester requester) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            return runAsync(serviceName, context, requester, true);
        }

        @Override
        public JobInfo runAsync(String serviceName, GenericRequester requester, Object... context) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            return runAsync(serviceName, ServiceUtil.makeContext(context), requester);
        }

        @Override
        public JobInfo runAsync(String serviceName, Map<String, ? extends Object> context, boolean persist) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            ModelService service = ctx.getModelService(serviceName);
            return dispatcher.runAsync(this.name, service, context, persist);
        }

        @Override
        public JobInfo runAsync(String serviceName, boolean persist, Object... context) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            return runAsync(serviceName, ServiceUtil.makeContext(context), persist);
        }

        /**
         * SCIPIO: Gets modified model service based on service options, based on overload above, for new overloads below.
         * <p>TODO: REVIEW: useTransaction/requireNewTransaction/transactionTimeout are used currently ONLY by:
         * {@link ServiceDispatcher#runAsync(java.lang.String, org.ofbiz.service.ModelService, java.util.Map, org.ofbiz.service.GenericRequester, AsyncOptions)}
         * so for persisted services, they do NOT apply to the actual service execution, so that method's transaction
         * is only used for the storing of the JobSandbox value; so to properly implement
         * requireNewTransaction/transactionTimeout for PersistAsyncOptions, new fields on JobSandbox are needed
         * and it's important here to use the {@link AsyncOptions#persist()} check instead of class hierarchy otherwise
         * the parameters could get applied to the wrong code block transaction later...</p>
         */
        private ModelService getModelService(String serviceName, AsyncOptions serviceOptions) throws GenericServiceException {
            ModelService service = ctx.getModelService(serviceName);
            if (serviceOptions instanceof MemoryAsyncOptions && !serviceOptions.persist()) {
                MemoryAsyncOptions asyncSrvOpts = (MemoryAsyncOptions) serviceOptions;
                Boolean requireNewTransaction = asyncSrvOpts.requireNewTransaction();
                int transactionTimeout = (asyncSrvOpts.transactionTimeout() != null) ? asyncSrvOpts.transactionTimeout() : -1;
                if (requireNewTransaction != null || transactionTimeout != -1) {
                    service = new ModelService(service); // clone the model service for updates
                    service.requireNewTransaction = requireNewTransaction;
                    if (Boolean.TRUE.equals(requireNewTransaction)) {
                        service.useTransaction = true;
                    }
                    if (transactionTimeout != -1) {
                        service.transactionTimeout = transactionTimeout;
                    }
                }
            }
            return service;
        }

        @Override
        public JobInfo runAsync(String serviceName, Map<String, ? extends Object> context, AsyncOptions serviceOptions) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            return dispatcher.runAsync(this.name, getModelService(serviceName, serviceOptions), context, serviceOptions);
        }

        @Override
        public JobInfo runAsync(String serviceName, Map<String, ?> context, GenericRequester requester, AsyncOptions serviceOptions) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            return dispatcher.runAsync(this.name, getModelService(serviceName, serviceOptions), context, requester, serviceOptions);
        }

        @Override
        public JobInfo runAsync(String serviceName, GenericRequester requester, AsyncOptions serviceOptions, Object... context) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            return dispatcher.runAsync(this.name, getModelService(serviceName, serviceOptions), ServiceUtil.makeContext(context), requester, serviceOptions);
        }

        @Override
        public JobInfo runAsync(String serviceName, Map<String, ? extends Object> context) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            return runAsync(serviceName, context, true);
        }

        @Override
        public GenericResultWaiter runAsyncWait(String serviceName, Map<String, ?> context, AsyncOptions serviceOptions) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            GenericResultWaiter waiter = new GenericResultWaiter();
            this.runAsync(serviceName, context, waiter, serviceOptions);
            return waiter;
        }

        @Override
        public GenericResultWaiter runAsyncWait(String serviceName, AsyncOptions serviceOptions, Object... context) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            return runAsyncWait(serviceName, ServiceUtil.makeContext(context), serviceOptions);
        }

        @Override
        public GenericResultWaiter runAsyncWait(String serviceName, Map<String, ? extends Object> context, boolean persist) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            GenericResultWaiter waiter = new GenericResultWaiter();
            this.runAsync(serviceName, context, waiter, persist);
            return waiter;
        }

        @Override
        public GenericResultWaiter runAsyncWait(String serviceName, boolean persist, Object... context) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            return runAsyncWait(serviceName, ServiceUtil.makeContext(context), persist);
        }

        @Override
        public GenericResultWaiter runAsyncWait(String serviceName, Map<String, ? extends Object> context) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
            return runAsyncWait(serviceName, context, true);
        }
    }

}
