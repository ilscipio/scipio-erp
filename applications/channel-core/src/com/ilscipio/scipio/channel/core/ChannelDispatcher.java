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
package com.ilscipio.scipio.channel.core;

import java.util.List;

/**
 * Pushes due stock tasks to a {@link StockGateway} in the same JVM. In the pooled runtime the hub does this work
 * from the control plane through the MCP topic "channel" (claim, push, report); this class does the same loop for the
 * cell mode and for the tests.
 *
 * <p>One call of {@link #runDue} takes the due tasks once. The caller runs it at a fixed interval (5 s). With the retry
 * rule of {@link SyncTask} a stock change reaches the channel within 60 s, also when the channel fails four times in a row.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public final class ChannelDispatcher {
    private final SyncQueue queue;
    private final StockGateway gateway;

    public ChannelDispatcher(SyncQueue queue, StockGateway gateway) {
        this.queue = queue;
        this.gateway = gateway;
    }

    /** Pushes the due tasks. Returns the number of tasks that the channel took. */
    public int runDue(int limit) {
        List<SyncTask> tasks = queue.claim(limit, SyncQueue.DEFAULT_LEASE);
        int ok = 0;
        for (SyncTask t : tasks) {
            try {
                gateway.updateStock(t.channelId, t.externalId, t.quantity);
                queue.succeeded(t.taskId);
                ok++;
            } catch (StockGateway.GatewayException e) {
                queue.failed(t.taskId, e.isRetryable(), e.getRetryAfter(), e.getMessage(), e.getFixHint());
            } catch (RuntimeException e) {
                queue.failed(t.taskId, true, null, e.getClass().getSimpleName() + ": " + e.getMessage(), null);
            }
        }
        return ok;
    }
}
