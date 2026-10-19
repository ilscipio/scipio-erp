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
package com.ilscipio.scipio.acp;

import com.ilscipio.scipio.entity.def.Entity;
import com.ilscipio.scipio.entity.def.Field;
import com.ilscipio.scipio.entity.def.Index;
import com.ilscipio.scipio.entity.def.IndexField;
import com.ilscipio.scipio.entity.def.PrimaryKey;

/**
 * SCIPIO: 4.0.0: Agentic Commerce Protocol entities: checkout sessions, idempotency keys and order webhook deliveries.
 */
public class AcpEntities {

    /**
     * One ACP checkout session. {@code stateJson} holds what the agent sent (items, buyer, fulfillment, selected
     * option, discount codes); prices, taxes and shipping are computed again on every call.
     */
    @Entity(
        name = "AcpCheckoutSession",
        packageName = "com.ilscipio.scipio.acp",
        title = "ACP Checkout Session",
        fields = {
            @Field(name = "checkoutSessionId", type = "id-long"),
            @Field(name = "productStoreId", type = "id"),
            @Field(name = "webSiteId", type = "id"),
            @Field(name = "webappName", type = "name"),
            @Field(name = "tokenId", type = "id-ne"),
            @Field(name = "userLoginId", type = "id-vlong"),
            @Field(name = "status", type = "short-varchar", description = "ACP status (not_ready_for_payment, ready_for_payment, completed, ...)"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "stateJson", type = "very-long"),
            @Field(name = "shipContactMechId", type = "id"),
            @Field(name = "orderId", type = "id"),
            @Field(name = "paymentRef", type = "id-vlong", description = "Stripe PaymentIntent id"),
            @Field(name = "totalAmount", type = "currency-amount"),
            @Field(name = "expiresDate", type = "date-time"),
            @Field(name = "createdDate", type = "date-time")
        },
        primaryKeys = { @PrimaryKey(field = "checkoutSessionId") },
        indexes = {
            @Index(name = "ACP_CS_ORDER_IDX", fields = { @IndexField(name = "orderId") }),
            @Index(name = "ACP_CS_EXPIRES_IDX", fields = { @IndexField(name = "expiresDate") })
        }
    )
    public interface AcpCheckoutSessionEntity {}

    /**
     * The answer to one Idempotency-Key of one token. A repeat with the same body gets the stored answer; a repeat
     * with another body gets 422; a repeat while the first call runs gets 409.
     */
    @Entity(
        name = "AcpIdempotencyKey",
        packageName = "com.ilscipio.scipio.acp",
        title = "ACP Idempotency Key",
        fields = {
            @Field(name = "tokenId", type = "id-ne"),
            @Field(name = "idempotencyKey", type = "id-vlong"),
            @Field(name = "requestSha", type = "long-varchar"),
            @Field(name = "state", type = "short-varchar", description = "RUNNING or DONE"),
            @Field(name = "statusCode", type = "numeric"),
            @Field(name = "responseBody", type = "very-long"),
            @Field(name = "createdDate", type = "date-time")
        },
        primaryKeys = { @PrimaryKey(field = "tokenId"), @PrimaryKey(field = "idempotencyKey") },
        indexes = {
            @Index(name = "ACP_IDEM_CREATED_IDX", fields = { @IndexField(name = "createdDate") })
        }
    )
    public interface AcpIdempotencyKeyEntity {}

    /**
     * One signed order event for the agent (order_create, order_update). The job acpDeliverWebhooks sends it and
     * retries with backoff.
     */
    @Entity(
        name = "AcpWebhookDelivery",
        packageName = "com.ilscipio.scipio.acp",
        title = "ACP Webhook Delivery",
        fields = {
            @Field(name = "deliveryId", type = "id-ne"),
            @Field(name = "checkoutSessionId", type = "id-long"),
            @Field(name = "orderId", type = "id"),
            @Field(name = "productStoreId", type = "id"),
            @Field(name = "eventType", type = "short-varchar"),
            @Field(name = "payload", type = "very-long"),
            @Field(name = "status", type = "short-varchar", description = "PENDING, SENT or PARKED"),
            @Field(name = "attempts", type = "numeric"),
            @Field(name = "nextAttemptDate", type = "date-time"),
            @Field(name = "lastError", type = "long-varchar"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "sentDate", type = "date-time")
        },
        primaryKeys = { @PrimaryKey(field = "deliveryId") },
        indexes = {
            @Index(name = "ACP_WH_STATUS_IDX", fields = { @IndexField(name = "status"), @IndexField(name = "nextAttemptDate") })
        }
    )
    public interface AcpWebhookDeliveryEntity {}
}
