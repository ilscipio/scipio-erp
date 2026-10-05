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
package com.ilscipio.scipio.channel.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.seca.*;

/**
 * Service definitions of channel-core. The hub (control plane) calls them through the MCP topic "channel"
 * (see ChannelMcp); every write has a permission check (CHANNELCORE_UPDATE) and an MCP audit row (blueprint 3, rule 4).
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public class ChannelServices {
    private static final String IMPL = "com.ilscipio.scipio.channel.service.ChannelServiceImpl";

    @Service(
        name = "channelSaveSetting",
        engine = "java",
        location = IMPL,
        invoke = "saveSetting",
        description = "Creates or changes the settings of one channel (one marketplace). Missing currency, tax rule and retention "
                + "come from the defaults of the marketplace. A product store belongs to one channel, and a hub account to one marketplace.",
        auth = "true",
        attributes = {
            @Attribute(name = "connectorId", type = "String", mode = "IN", optional = "false", description = "Hub connector id, for example ebay"),
            @Attribute(name = "marketplaceId", type = "String", mode = "IN", optional = "false", description = "Marketplace code, for example us or de"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "accountId", type = "String", mode = "IN", optional = "false", description = "Id of the ChannelAccount in the hub"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pricesIncludeTax", type = "String", mode = "IN", optional = "true", description = "Y or N"),
            @Attribute(name = "salesChannelEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "retentionDays", type = "Integer", mode = "IN", optional = "true", description = "Days to keep buyer data; 0 or empty with clearRetention=Y: no channel limit"),
            @Attribute(name = "clearRetention", type = "String", mode = "IN", optional = "true", description = "Y: no channel limit"),
            @Attribute(name = "retentionFrom", type = "String", mode = "IN", optional = "true", description = "CLOSED or PLACED"),
            @Attribute(name = "active", type = "String", mode = "IN", optional = "true", description = "Y or N"),
            @Attribute(name = "buffer", type = "Integer", mode = "IN", optional = "true", description = "Stock rule: quantity to hold back"),
            @Attribute(name = "maxQuantity", type = "Integer", mode = "IN", optional = "true", description = "Stock rule: cap"),
            @Attribute(name = "channelId", type = "String", mode = "OUT", optional = "false")
        }
    )
    public interface ChannelSaveSetting {}

    @Service(
        name = "channelSaveListing",
        engine = "java",
        location = IMPL,
        invoke = "saveListing",
        description = "Records the state of a listing that the hub made or changed: the id on the channel, the state, the issues and the fix hint.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "channelId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "externalId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "listingState", type = "String", mode = "IN", optional = "true", description = "DRAFT, PENDING, LIVE, REJECTED or ENDED"),
            @Attribute(name = "errorsJson", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fixHint", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "variationGroupId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "variationAxesJson", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ChannelSaveListing {}

    @Service(
        name = "channelListingPrice",
        engine = "java",
        location = IMPL,
        invoke = "listingPrice",
        description = "The price of a product on a channel: the DEFAULT_PRICE row of the channel store group in the currency of the channel "
                + "with the tax rule of the channel. It never converts a currency and never adds tax. Without a fitting row it gives a fix hint.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "channelId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "price", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "currencyUomId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "pricesIncludeTax", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "errorCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "fixHint", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface ChannelListingPrice {}

    @Service(
        name = "channelProductData",
        engine = "java",
        location = IMPL,
        invoke = "productData",
        description = "Read only. The channel data of a product for a listing (W1-12b): the channel category and attributes (ChannelProductData), "
                + "the quantity for the channel (available to promise with the stock rule of the channel), the SKU, GTIN and MPN "
                + "(GoodIdentification), and for a variant its group (the virtual product) and its axes (feature type: value). "
                + "No ChannelProductData row gives no categoryId.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "channelId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "categoryId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "attributesJson", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "guessedJson", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "quantity", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "sku", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "gtin", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "mpn", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "variationGroupId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "variationAxesJson", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface ChannelProductData {}

    @Service(
        name = "channelStockChanged",
        engine = "java",
        location = IMPL,
        invoke = "stockChanged",
        description = "The stock of a product changed: plans one stock push for each channel that lists it. Called by the ECA on inventory changes; "
                + "call it by hand to send the stock of a product again.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN", optional = "true", description = "Used when productId is empty"),
            @Attribute(name = "tasksPlanned", type = "Integer", mode = "OUT", optional = "true")
        }
    )
    public interface ChannelStockChanged {}

    @Service(
        name = "channelResyncStock",
        engine = "java",
        location = IMPL,
        invoke = "resyncStock",
        description = "Plans the stock push of a product to every channel that lists it (checked form of channelStockChanged; needs CHANNELCORE_UPDATE).",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "tasksPlanned", type = "Integer", mode = "OUT", optional = "true")
        }
    )
    public interface ChannelResyncStock {}

    /** Inventory changes plan the stock pushes after the outer transaction has committed (global-commit); an error here never fails the inventory change. */
    @Seca(
        service = "createInventoryItemDetail",
        event = "global-commit",
        actions = {
            @SecaAction(service = "channelStockChanged", mode = "sync", newTransaction = "true", ignoreError = "true", ignoreFailure = "true",
                    resultToContext = "false")
        }
    )
    public interface CreateInventoryItemDetailChannelStockSeca {}

    @Service(
        name = "channelClaimStockTasks",
        engine = "java",
        location = IMPL,
        invoke = "claimStockTasks",
        description = "The hub takes the due stock pushes (oldest first) and holds them for the lease time. A task for a listing is not given while "
                + "another task of the same listing is held.",
        auth = "true",
        attributes = {
            @Attribute(name = "limit", type = "Integer", mode = "IN", optional = "true", defaultValue = "50"),
            @Attribute(name = "leaseSeconds", type = "Integer", mode = "IN", optional = "true", defaultValue = "30"),
            @Attribute(name = "tasks", type = "List", mode = "OUT", optional = "true", description = "Each: taskId, channelId, productId, externalId, quantity, attempts")
        }
    )
    public interface ChannelClaimStockTasks {}

    @Service(
        name = "channelReportStockTask",
        engine = "java",
        location = IMPL,
        invoke = "reportStockTask",
        description = "The hub reports the result of a stock push. success=Y: the channel took the quantity. Else the task waits and repeats "
                + "(retryable=Y, with the wait time of the channel) or fails for good and writes a fix hint on the listing.",
        auth = "true",
        attributes = {
            @Attribute(name = "taskId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "success", type = "String", mode = "IN", optional = "false", description = "Y or N"),
            @Attribute(name = "retryable", type = "String", mode = "IN", optional = "true", description = "Y or N; default Y"),
            @Attribute(name = "retryAfterSeconds", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "error", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fixHint", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "taskState", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface ChannelReportStockTask {}

    @Service(
        name = "channelIntakeOrder",
        engine = "java",
        location = IMPL,
        invoke = "intakeOrder",
        description = "The hub hands a channel order to the store. The store makes one order with the external id, the stock falls and the "
                + "other channels get their new quantity. A repeat of the same order makes no second order.",
        auth = "true",
        attributes = {
            @Attribute(name = "channelId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "order", type = "Map", mode = "IN", optional = "false", description = "externalOrderId, status, placedAt, currency, total, lines[], buyer fields"),
            @Attribute(name = "intakeStatus", type = "String", mode = "OUT", optional = "true", description = "CREATED, DUPLICATE, WAITING, IGNORED, UNMAPPED or REJECTED"),
            @Attribute(name = "orderId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "message", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "unmappedSkus", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface ChannelIntakeOrder {}

    @Service(
        name = "channelCloseOrder",
        engine = "java",
        location = IMPL,
        invoke = "closeOrder",
        description = "A channel order is shipped or cancelled: the retention clock of the buyer data starts.",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "false")
        }
    )
    public interface ChannelCloseOrder {}

    @Service(
        name = "channelRunRetention",
        engine = "java",
        useTransaction = "false",
        location = IMPL,
        invoke = "runRetention",
        description = "Erases the buyer data of each channel order that is due (for example Amazon: 30 days after the order is closed). "
                + "Runs each day as a scheduled job.",
        auth = "true",
        attributes = {
            @Attribute(name = "erased", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "failedOrderIds", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface ChannelRunRetention {}

    @Service(
        name = "channelBuyerDeletion",
        engine = "java",
        useTransaction = "false",
        location = IMPL,
        invoke = "buyerDeletion",
        description = "The channel asks to erase a buyer (eBay account-deletion notice, event BUYER_DATA_DELETION): erases the buyer data "
                + "of all orders of that buyer on all channels of the connector, at once.",
        auth = "true",
        attributes = {
            @Attribute(name = "connectorId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "buyerExternalId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "erased", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "failedOrderIds", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface ChannelBuyerDeletion {}

    // ---- event outbox (W1-07) ----

    @Service(
        name = "channelOutboxOrderCreated",
        engine = "java",
        location = IMPL,
        invoke = "outboxOrderCreated",
        description = "Hook of the order transaction: writes the outbox event ORDER_CREATED for a sales order (any channel, or the own store), "
                + "and ORDER_NEEDS_SHIPPING when the order is approved already. Called by the ECA on storeOrder; no permission check.",
        auth = "false",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ChannelOutboxOrderCreated {}

    @Service(
        name = "channelOutboxOrderStatus",
        engine = "java",
        location = IMPL,
        invoke = "outboxOrderStatus",
        description = "Hook of the order status change: writes the outbox event ORDER_NEEDS_SHIPPING when a sales order with a physical item "
                + "is approved. Called by the ECA on changeOrderStatus; no permission check.",
        auth = "false",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ChannelOutboxOrderStatus {}

    @Service(
        name = "channelOutboxStockChanged",
        engine = "java",
        location = IMPL,
        invoke = "outboxStockChanged",
        description = "Hook of the stock change: writes the outbox event STOCK_CHANGED for one inventory detail row. "
                + "Called by the ECA on createInventoryItemDetail; no permission check.",
        auth = "false",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemDetailSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "availableToPromiseDiff", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "quantityOnHandDiff", type = "BigDecimal", mode = "IN", optional = "true")
        }
    )
    public interface ChannelOutboxStockChanged {}

    /**
     * The three hooks run in the transaction of their cause (event commit: before the commit, no new transaction), and an error fails the cause:
     * an order or a stock change without its event cannot commit.
     */
    @Seca(
        service = "storeOrder",
        event = "commit",
        actions = {
            @SecaAction(service = "channelOutboxOrderCreated", mode = "sync", newTransaction = "false", ignoreError = "false", ignoreFailure = "false",
                    resultToContext = "false")
        }
    )
    public interface StoreOrderOutboxSeca {}

    @Seca(
        service = "changeOrderStatus",
        event = "commit",
        actions = {
            @SecaAction(service = "channelOutboxOrderStatus", mode = "sync", newTransaction = "false", ignoreError = "false", ignoreFailure = "false",
                    resultToContext = "false")
        }
    )
    public interface ChangeOrderStatusOutboxSeca {}

    @Seca(
        service = "createInventoryItemDetail",
        event = "commit",
        actions = {
            @SecaAction(service = "channelOutboxStockChanged", mode = "sync", newTransaction = "false", ignoreError = "false", ignoreFailure = "false",
                    resultToContext = "false")
        }
    )
    public interface CreateInventoryItemDetailOutboxSeca {}

    @Service(
        name = "channelClaimOutboxEvents",
        engine = "java",
        location = IMPL,
        invoke = "claimOutboxEvents",
        description = "The desk takes a batch of outbox events (oldest first) and holds them for the lease time. Each claim counts one attempt. "
                + "An event that is not acknowledged or released before the lease ends is given again (at least once). Handle each event once by its eventId.",
        auth = "true",
        attributes = {
            @Attribute(name = "consumerId", type = "String", mode = "IN", optional = "false", description = "Name of the consumer, for example desk-1"),
            @Attribute(name = "limit", type = "Integer", mode = "IN", optional = "true", defaultValue = "50"),
            @Attribute(name = "leaseSeconds", type = "Integer", mode = "IN", optional = "true", defaultValue = "60"),
            @Attribute(name = "eventTypes", type = "List", mode = "IN", optional = "true", description = "Only these types; empty: all"),
            @Attribute(name = "events", type = "List", mode = "OUT", optional = "true", description = "Each: eventId, eventType, payloadJson, createdDate, attempts, leaseUntil")
        }
    )
    public interface ChannelClaimOutboxEvents {}

    @Service(
        name = "channelAckOutboxEvents",
        engine = "java",
        location = IMPL,
        invoke = "ackOutboxEvents",
        description = "The desk handled the events. An event that is done already counts as done (a repeat is safe). "
                + "An event that another consumer holds under a running lease goes to notOwner.",
        auth = "true",
        attributes = {
            @Attribute(name = "consumerId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "eventIds", type = "List", mode = "IN", optional = "false"),
            @Attribute(name = "done", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "unknown", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "notOwner", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface ChannelAckOutboxEvents {}

    @Service(
        name = "channelReleaseOutboxEvents",
        engine = "java",
        location = IMPL,
        invoke = "releaseOutboxEvents",
        description = "The desk could not handle the events. They go back to the list at once, or after retryAfterSeconds. The attempt count stays.",
        auth = "true",
        attributes = {
            @Attribute(name = "consumerId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "eventIds", type = "List", mode = "IN", optional = "false"),
            @Attribute(name = "error", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "retryAfterSeconds", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "done", type = "List", mode = "OUT", optional = "true", description = "Ids that went back to the list"),
            @Attribute(name = "unknown", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "notOwner", type = "List", mode = "OUT", optional = "true", description = "Ids that are done, or held by another consumer")
        }
    )
    public interface ChannelReleaseOutboxEvents {}

    @Service(
        name = "channelListParkedOutboxEvents",
        engine = "java",
        location = IMPL,
        invoke = "listParkedOutboxEvents",
        description = "Lists the parked outbox events: events whose attempts reached the property outbox.maxAttempts (default 10). "
                + "Claim skips them and purge keeps them. Acknowledge one to close it.",
        auth = "true",
        attributes = {
            @Attribute(name = "limit", type = "Integer", mode = "IN", optional = "true", defaultValue = "50"),
            @Attribute(name = "events", type = "List", mode = "OUT", optional = "true", description = "Each: eventId, eventType, payloadJson, createdDate, attempts, parkedDate, lastError")
        }
    )
    public interface ChannelListParkedOutboxEvents {}

    @Service(
        name = "channelPurgeOutbox",
        engine = "java",
        location = IMPL,
        invoke = "purgeOutbox",
        description = "Deletes the outbox events that are done for longer than the retention time (property outbox.retentionDays in "
                + "channel-core.properties, default 7 days). Open events stay. Runs each day as a scheduled job.",
        auth = "true",
        attributes = {
            @Attribute(name = "retentionDays", type = "Integer", mode = "IN", optional = "true", description = "Replaces the property for this call"),
            @Attribute(name = "purged", type = "Integer", mode = "OUT", optional = "true")
        }
    )
    public interface ChannelPurgeOutbox {}
}
