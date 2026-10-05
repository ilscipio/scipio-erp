/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package org.ofbiz.service.job;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.Test;
import org.ofbiz.entity.GenericValue;

/**
 * SCIPIO: 4.0.0: W1-01c, finding 4: the job count of a store ends once on every path of a queued job (G17).
 */
public class JobCountTest {

    private static final class CountedJob extends AbstractJob {
        private final AtomicInteger count;

        CountedJob(AtomicInteger count, boolean fails) {
            super("job", "job");
            this.count = count;
            count.incrementAndGet();
            setDoneCallback(count::decrementAndGet);
            this.fails = fails;
        }

        private final boolean fails;

        @Override
        public void exec() throws InvalidJobException {
            if (fails) {
                throw new IllegalStateException("job fails");
            }
        }

        @Override public boolean isValid() { return true; }
        @Override public String getServiceName() { return "testService"; }
        @Override public String getJobType() { return "test"; }
        @Override public boolean isPersist() { return false; }
        @Override public GenericValue getJobValue() { return null; }
        @Override public String getJobPool() { return "pool"; }
    }

    @Test
    public void rejectedJobEndsItsCount() {
        AtomicInteger count = new AtomicInteger();
        CountedJob job = new CountedJob(count, false);
        assertThrows(RejectedExecutionException.class, () -> AbstractJob.execute(task -> {
            throw new RejectedExecutionException("full");
        }, job));
        assertEquals(0, count.get());
    }

    @Test
    public void anyExecutorErrorEndsTheCount() {
        AtomicInteger count = new AtomicInteger();
        CountedJob job = new CountedJob(count, false);
        assertThrows(IllegalStateException.class, () -> AbstractJob.execute(task -> {
            throw new IllegalStateException("executor error");
        }, job));
        assertEquals(0, count.get(), "the generic error path ends the count");
        job.runDoneCallback();
        assertEquals(0, count.get(), "the count ends once");
    }

    @Test
    public void runAndDrainEndTheCountOnce() {
        AtomicInteger count = new AtomicInteger();
        List<Runnable> queue = new ArrayList<>();
        CountedJob ran = new CountedJob(count, false);
        CountedJob failed = new CountedJob(count, true);
        CountedJob drained = new CountedJob(count, false);
        AbstractJob.execute(queue::add, ran);
        AbstractJob.execute(queue::add, failed);
        AbstractJob.execute(queue::add, drained);
        assertEquals(3, count.get(), "a job in the executor queue keeps its count");
        ran.run();
        assertThrows(IllegalStateException.class, failed::run);
        assertEquals(1, count.get(), "a job that ends, also with an error, ends its count");
        // JobPoller.stop(): the executor gives back the queued jobs
        AbstractJob.endCount(drained);
        AbstractJob.endCount(ran);
        assertEquals(0, count.get());
    }
}
