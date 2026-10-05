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
/*
 * Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
 * under the GNU Affero General Public License, version 3, or a commercial
 * license from Ilscipio GmbH (file LICENSE). The original code stays under
 * the Apache License, version 2.0, as stated above.
 */
package org.ofbiz.service.job;

import java.util.Date;

import org.ofbiz.base.util.Assert;
import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.transaction.GenericTransactionException;
import org.ofbiz.entity.transaction.TransactionUtil;

/**
 * Abstract Job.
 */
public abstract class AbstractJob implements Job {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private final String jobId;
    private final String jobName;
    protected State currentState = State.CREATED;
    private long elapsedTime = 0;
    private final Date startTime = new Date();

    protected AbstractJob(String jobId, String jobName) {
        Assert.notNull("jobId", jobId, "jobName", jobName);
        this.jobId = jobId;
        this.jobName = jobName;
    }

    @Override
    public State currentState() {
        return currentState;
    }

    @Override
    public String getJobId() {
        return this.jobId;
    }

    @Override
    public String getJobName() {
        return this.jobName;
    }

    @Override
    public void queue() throws InvalidJobException {
        if (currentState != State.CREATED) {
            throw new InvalidJobException("Illegal state change");
        }
        this.currentState = State.QUEUED;
    }

    @Override
    public void deQueue() throws InvalidJobException {
        if (currentState != State.QUEUED) {
            throw new InvalidJobException("Illegal state change");
        }
        this.currentState = State.CREATED;
    }

    /**
     *  Executes this Job. The {@link #run()} method calls this method.
     */
    public abstract void exec() throws InvalidJobException;

    /** SCIPIO: W1-01c: called once when the job ends or leaves the queue (the job count per store, G17). */
    private volatile Runnable doneCallback;

    void setDoneCallback(Runnable doneCallback) {
        this.doneCallback = doneCallback;
    }

    void runDoneCallback() {
        Runnable callback = doneCallback;
        doneCallback = null;
        if (callback != null) {
            callback.run();
        }
    }

    /**
     * SCIPIO: W1-01c: hands the job to the executor. When the executor does not take the job (any exception), the
     * job count of its store ends here; when it takes the job, {@link #run} ends the count (G17).
     */
    static void execute(java.util.concurrent.Executor executor, Job job) {
        boolean taken = false;
        try {
            executor.execute(job);
            taken = true;
        } finally {
            if (!taken) {
                endCount(job);
            }
        }
    }

    /** SCIPIO: W1-01c: ends the job count of a task that leaves the queue without a run (G17). */
    static void endCount(Object task) {
        if (task instanceof AbstractJob) {
            ((AbstractJob) task).runDoneCallback();
        }
    }

    @Override
    public void run() {
        try {
            runJob();
        } finally {
            runDoneCallback();
        }
    }

    private void runJob() {
        long startMillis = System.currentTimeMillis();
        try {
            exec();
        } catch (InvalidJobException e) {
            Debug.logWarning(e.getMessage(), module);
        }
        // sanity check; make sure we don't have any transactions in place
        try {
            // roll back current TX first
            if (TransactionUtil.isTransactionInPlace()) {
                Debug.logWarning("*** NOTICE: JobInvoker finished w/ a transaction in place! Rolling back.", module);
                TransactionUtil.rollback();
            }
            // now resume/rollback any suspended txs
            if (TransactionUtil.suspendedTransactionsHeld()) {
                int suspended = TransactionUtil.cleanSuspendedTransactions();
                Debug.logWarning("Resumed/Rolled Back [" + suspended + "] transactions.", module);
            }
        } catch (GenericTransactionException e) {
            Debug.logWarning(e, module);
        }
        elapsedTime = System.currentTimeMillis() - startMillis;
    }

    @Override
    public long getRuntime() {
        return elapsedTime;
    }

    @Override
    public Date getStartTime() {
        return (Date) startTime.clone();
    }

    /*
     * Returns JobPriority.NORMAL, the default setting
     */
    @Override
    public long getPriority() {
        return JobPriority.NORMAL;
    }
}
