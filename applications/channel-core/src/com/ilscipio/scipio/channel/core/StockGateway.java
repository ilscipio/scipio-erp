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

/**
 * Port to a channel for stock pushes. The hub implements the call with a connector (ChannelConnector.updateStock);
 * channel-core does not know the connector. The tests use a fake channel.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public interface StockGateway {

    void updateStock(String channelId, String externalId, int quantity) throws GatewayException;

    /** The push failed. */
    class GatewayException extends Exception {
        private static final long serialVersionUID = 1L;
        private final boolean retryable;
        private final transient Duration retryAfter;
        private final String fixHint;

        public GatewayException(String message, boolean retryable, Duration retryAfter, String fixHint) {
            super(message);
            this.retryable = retryable;
            this.retryAfter = retryAfter;
            this.fixHint = fixHint;
        }

        public boolean isRetryable() {
            return retryable;
        }

        public Duration getRetryAfter() {
            return retryAfter;
        }

        public String getFixHint() {
            return fixHint;
        }
    }
}
