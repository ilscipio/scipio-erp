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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import javax.transaction.RollbackException;
import javax.transaction.Status;
import javax.transaction.Synchronization;
import javax.transaction.SystemException;
import javax.transaction.Transaction;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.transaction.GenericTransactionException;
import org.ofbiz.entity.transaction.TransactionFactoryLoader;
import org.ofbiz.entity.transaction.TransactionUtil;

/**
 * This class is used to execute services when a transaction is either
 * committed or rolled back.  It should generally be accessed via
 * LocalDispatcher's addCommitService and addRollbackService methods
 * or by using the service ECA event attribute values global-commit,
 * global-rollback or global-commit-post-run
 * <p>
 * SCIPIO: 2017-12-20: This now implements ServiceSyncRegistrations so it can return info
 * about the registered services.
 */
public class ServiceSynchronization implements Synchronization, ServiceSyncRegistrations { // SCIPIO: added ServiceSyncRegistrations

    public static final String MODULE = ServiceSynchronization.class.getName();

    private static Map<Transaction, ServiceSynchronization> syncingleton = new WeakHashMap<>();
    private List<ServiceExecution> services = new ArrayList<>();

    public static void registerCommitService(DispatchContext dctx, String serviceName, String runAsUser, Map<String, ? extends Object> context, boolean async, boolean persist) throws GenericServiceException {
        ServiceSynchronization sync = ServiceSynchronization.getInstance();
        sync.services.add(new ServiceExecution(dctx, serviceName, runAsUser, context, async, persist, false));
    }

    public static void registerRollbackService(DispatchContext dctx, String serviceName, String runAsUser, Map<String, ? extends Object> context, boolean async, boolean persist) throws GenericServiceException {
        ServiceSynchronization sync = ServiceSynchronization.getInstance();
        sync.services.add(new ServiceExecution(dctx, serviceName, runAsUser, context, async, persist, true));
    }

    protected static ServiceSynchronization getInstance() throws GenericServiceException {
        ServiceSynchronization sync = null;
        try {
            Transaction transaction = TransactionFactoryLoader.getInstance().getTransactionManager().getTransaction();
            synchronized (transaction) {
                sync = syncingleton.get(transaction);
                if (sync == null) {
                    sync = new ServiceSynchronization();
                    transaction.registerSynchronization(sync);
                    syncingleton.put(transaction, sync);
                }
            }
        } catch (SystemException | IllegalStateException | RollbackException e) {
            throw new GenericServiceException(e.getMessage(), e);
        }
        return sync;
    }

    @Override
    public void afterCompletion(int status) {
        for (ServiceExecution serviceExec : this.services) {
            serviceExec.runService(status);
        }
    }

    @Override
    public void beforeCompletion() {

    }

    static class ServiceExecution implements ServiceSyncRegistration { // SCIPIO: added ServiceRegistration
        protected DispatchContext dctx = null;
        protected String serviceName;
        protected String runAsUser = null;
        protected Map<String, ? extends Object> context = null;
        protected boolean rollback = false;
        protected boolean persist = true;
        protected boolean async = false;

        ServiceExecution(DispatchContext dctx, String serviceName, String runAsUser, Map<String, ? extends Object> context, boolean async, boolean persist, boolean rollback) {
            this.dctx = dctx;
            this.serviceName = serviceName;
            this.runAsUser = runAsUser;
            this.context = context;
            this.async = async;
            this.persist = persist;
            this.rollback = rollback;
        }

        protected void runService(int status) {
            if ((status == Status.STATUS_COMMITTED && !rollback) || (status == Status.STATUS_ROLLEDBACK && rollback)) {
                Thread thread = new Thread() {
                    @Override
                    public void run() {
                        String msgPrefix = null;
                        if (rollback) {
                            msgPrefix = "[Rollback] ";
                        } else {
                            msgPrefix = "[Commit] ";
                        }

                        boolean beganTx;
                        try {
                            // begin the new tx
                            beganTx = TransactionUtil.begin();
                            // configure and run the service
                            try {
                                // obtain the model and get the valid context
                                ModelService model = dctx.getModelService(serviceName);
                                Map<String, Object> thisContext;
                                if (model.validate) {
                                    thisContext = model.makeValid(context, ModelService.IN_PARAM);
                                } else {
                                    thisContext = new HashMap<>();
                                    thisContext.putAll(context);
                                }

                                // set the userLogin object
                                thisContext.put("userLogin", ServiceUtil.getUserLogin(dctx, thisContext, runAsUser));
                                if (async) {
                                    Debug.logInfo(msgPrefix + "Invoking [" + serviceName + "] via runAsync", MODULE);
                                    dctx.getDispatcher().runAsync(serviceName, thisContext, persist);
                                } else {
                                    Debug.logInfo(msgPrefix + "Invoking [" + serviceName + "] via runSyncIgnore", MODULE);
                                    dctx.getDispatcher().runSyncIgnore(serviceName, thisContext);
                                }
                            } catch (Throwable t) {
                                Debug.logError(t, "Problem calling " + msgPrefix + "service : " + serviceName + " / " + context, MODULE);
                                try {
                                    TransactionUtil.rollback(beganTx, t.getMessage(), t);
                                } catch (GenericTransactionException e) {
                                    Debug.logError(e, MODULE);
                                }

                            } finally {
                                // commit the transaction
                                try {
                                    TransactionUtil.commit(beganTx);
                                } catch (GenericTransactionException e) {
                                    Debug.logError(e, MODULE);
                                }
                            }
                        } catch (GenericTransactionException e) {
                            Debug.logError(e, MODULE);
                        }

                    }
                };
                thread.start();
            }
        }

        @Override
        public String getServiceName() { // SCIPIO
            return serviceName;
        }

        @Override
        public boolean isCommit() { // SCIPIO
            return !rollback;
        }

        @Override
        public boolean isRollback() { // SCIPIO
            return rollback;
        }

        @Override
        public Map<String, ?> getContext() { // SCIPIO
            return context;
        }

        @Override
        public boolean isAsync() { // SCIPIO
            return async;
        }

        @Override
        public boolean isPersist() { // SCIPIO
            return persist;
        }
    }

    @Override
    public Collection<ServiceSyncRegistration> getAllRegistrations() { // SCIPIO
        return Collections.<ServiceSyncRegistration>unmodifiableList(services);
    }

    @Override
    public Collection<ServiceSyncRegistration> getRegistrationsForService(String serviceName) { // SCIPIO
        List<ServiceSyncRegistration> regs = new ArrayList<>();
        for(ServiceSyncRegistration reg : services) {
            if (serviceName.equals(reg.getServiceName())) {
                regs.add(reg);
            }
        }
        return regs;
    }

    @Override
    public Collection<ServiceSyncRegistration> getCommitRegistrationsForService(String serviceName) { // SCIPIO
        List<ServiceSyncRegistration> regs = new ArrayList<>();
        for(ServiceSyncRegistration reg : services) {
            if (reg.isCommit() && serviceName.equals(reg.getServiceName())) {
                regs.add(reg);
            }
        }
        return regs;
    }

    @Override
    public Collection<ServiceSyncRegistration> getRollbackRegistrationsForService(String serviceName) { // SCIPIO
        List<ServiceSyncRegistration> regs = new ArrayList<>();
        for(ServiceSyncRegistration reg : services) {
            if (reg.isRollback() && serviceName.equals(reg.getServiceName())) {
                regs.add(reg);
            }
        }
        return regs;
    }

    @Override
    public ServiceSyncRegistration getFirstRegistrationForService(String serviceName) { // SCIPIO
        for(ServiceSyncRegistration reg : services) {
            if (serviceName.equals(reg.getServiceName())) {
                return reg;
            }
        }
        return null;
    }

    @Override
    public ServiceSyncRegistration getFirstCommitRegistrationForService(String serviceName) { // SCIPIO
        for(ServiceSyncRegistration reg : services) {
            if (reg.isCommit() && serviceName.equals(reg.getServiceName())) {
                return reg;
            }
        }
        return null;
    }

    @Override
    public ServiceSyncRegistration getFirstRollbackRegistrationForService(String serviceName) { // SCIPIO
        for(ServiceSyncRegistration reg : services) {
            if (reg.isRollback() && serviceName.equals(reg.getServiceName())) {
                return reg;
            }
        }
        return null;
    }

    @Override
    public void addCommitService(DispatchContext dctx, String serviceName, String runAsUser,
            Map<String, ? extends Object> context, boolean async, boolean persist) throws GenericServiceException { // SCIPIO
        services.add(new ServiceExecution(dctx, serviceName, runAsUser, context, async, persist, false));
    }

    @Override
    public void addRollbackService(DispatchContext dctx, String serviceName, String runAsUser,
            Map<String, ? extends Object> context, boolean async, boolean persist) throws GenericServiceException { // SCIPIO
        services.add(new ServiceExecution(dctx, serviceName, runAsUser, context, async, persist, true));
    }

    @Override
    public boolean removeService(ServiceSyncRegistration serviceRegistration) throws GenericServiceException { // SCIPIO
        return services.remove(serviceRegistration);
    }

    @Override
    public int removeService(String serviceName) throws GenericServiceException { // SCIPIO
        Iterator<ServiceExecution> it = services.iterator();
        int removed = 0;
        while(it.hasNext()) {
            ServiceExecution reg = it.next();
            if (serviceName.equals(reg.getServiceName())) {
                it.remove();
                removed++;
            }
        }
        return removed;
    }

    @Override
    public int removeCommitService(String serviceName) throws GenericServiceException { // SCIPIO
        Iterator<ServiceExecution> it = services.iterator();
        int removed = 0;
        while(it.hasNext()) {
            ServiceExecution reg = it.next();
            if (reg.isCommit() && serviceName.equals(reg.getServiceName())) {
                it.remove();
                removed++;
            }
        }
        return removed;
    }

    @Override
    public int removeRollbackService(String serviceName) throws GenericServiceException { // SCIPIO
        Iterator<ServiceExecution> it = services.iterator();
        int removed = 0;
        while(it.hasNext()) {
            ServiceExecution reg = it.next();
            if (reg.isRollback() && serviceName.equals(reg.getServiceName())) {
                it.remove();
                removed++;
            }
        }
        return removed;
    }

}
