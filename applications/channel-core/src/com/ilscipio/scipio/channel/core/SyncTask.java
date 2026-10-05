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

import java.time.Duration;
import java.time.Instant;

/**
 * One stock push to one channel: the row of ChannelSyncTask. It holds the state machine (pending, claimed, done, failed)
 * and the retry rule, so that the store code and the tests share one logic.
 *
 * <p>Retry rule: attempt n waits 2^n seconds (2, 4, 8, 16), then 30 seconds, up to {@link #MAX_ATTEMPTS} attempts.
 * Four failures in a row take 2+4+8+16 = 30 s of waiting, so the fifth attempt is at 30 s or a little later (the caller's
 * poll interval). A wait time that the channel gives (rate limit) wins when it is longer.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public final class SyncTask {
    public enum State { PENDING, CLAIMED, DONE, FAILED }

    public static final int MAX_ATTEMPTS = 8;
    public static final Duration MAX_BACKOFF = Duration.ofSeconds(30);

    public final String taskId;
    public final String channelId;
    public final String productId;
    public final Instant createdDate;
    public String externalId;
    public int quantity;
    public State state = State.PENDING;
    public int attempts;
    public Instant dueDate;
    public Instant leaseUntil;
    public String lastError;
    public Instant doneDate;

    public SyncTask(String taskId, String channelId, String productId, String externalId, int quantity, Instant now) {
        this.taskId = Listing.req(taskId, "taskId");
        this.channelId = Listing.req(channelId, "channelId");
        this.productId = Listing.req(productId, "productId");
        this.externalId = externalId;
        this.quantity = quantity;
        this.createdDate = now;
        this.dueDate = now;
    }

    public boolean isOpen() {
        return state == State.PENDING || state == State.CLAIMED;
    }

    /** True when {@link #claim} takes this task now: pending and due, or claimed with an expired lease. */
    public boolean isClaimable(Instant now) {
        if (state == State.PENDING) {
            return !dueDate.isAfter(now);
        }
        return state == State.CLAIMED && leaseUntil != null && !leaseUntil.isAfter(now);
    }

    public boolean isLeased(Instant now) {
        return state == State.CLAIMED && leaseUntil != null && leaseUntil.isAfter(now);
    }

    public void claim(Instant now, Duration lease) {
        if (!isClaimable(now)) {
            throw new IllegalStateException("Task " + taskId + " is not claimable: " + state);
        }
        state = State.CLAIMED;
        leaseUntil = now.plus(lease);
    }

    public void succeed(Instant now) {
        requireClaimed();
        state = State.DONE;
        doneDate = now;
        leaseUntil = null;
        lastError = null;
    }

    /**
     * Records a failed push.
     *
     * @param retryable false: the task fails now (a wrong request will not work on a repeat)
     * @param retryAfter the wait time that the channel asks for, or null
     */
    public void fail(Instant now, boolean retryable, Duration retryAfter, String error) {
        requireClaimed();
        attempts++;
        lastError = error;
        leaseUntil = null;
        if (!retryable || attempts >= MAX_ATTEMPTS) {
            state = State.FAILED;
            doneDate = now;
            return;
        }
        Duration wait = backoff(attempts);
        if (retryAfter != null && retryAfter.compareTo(wait) > 0) {
            wait = retryAfter;
        }
        state = State.PENDING;
        dueDate = now.plus(wait);
    }

    public static Duration backoff(int attempt) {
        long secs = 1L << Math.min(attempt, 10);
        Duration d = Duration.ofSeconds(secs);
        return d.compareTo(MAX_BACKOFF) > 0 ? MAX_BACKOFF : d;
    }

    /** A new stock value for a task that no worker holds: the push takes the value now. */
    public void retarget(int newQuantity, Instant now) {
        if (state != State.PENDING) {
            throw new IllegalStateException("Only a pending task takes a new quantity");
        }
        quantity = newQuantity;
        dueDate = now;
    }

    private void requireClaimed() {
        if (state != State.CLAIMED) {
            throw new IllegalStateException("Task " + taskId + " is not claimed: " + state);
        }
    }

    public SyncTask copy() {
        SyncTask t = new SyncTask(taskId, channelId, productId, externalId, quantity, createdDate);
        t.state = state;
        t.attempts = attempts;
        t.dueDate = dueDate;
        t.leaseUntil = leaseUntil;
        t.lastError = lastError;
        t.doneDate = doneDate;
        return t;
    }
}
