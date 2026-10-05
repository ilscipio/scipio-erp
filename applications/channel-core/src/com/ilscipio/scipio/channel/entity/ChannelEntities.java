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
package com.ilscipio.scipio.channel.entity;

import com.ilscipio.scipio.entity.def.*;

/**
 * Store entities of channel-core (blueprint 7.3). The master entities (ChannelAccount, ChannelCredential, ChannelWebhook,
 * ChannelSyncRun) belong to the hub in scipio-ai; this component holds a reference to the account only (accountId).
 *
 * <p>W1-08 additions to the entities of 7.3: ChannelSetting (currency, tax rule, retention, one row for each
 * marketplace), ChannelSyncTask (the work list of stock pushes), the columns variationGroupId, variationAxesJson and
 * lastPushedQuantity on ChannelListing, and the buyer-data columns on ChannelOrderRef.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public class ChannelEntities {

    @Entity(
        name = "ChannelSetting",
        packageName = "com.ilscipio.scipio.channel",
        title = "Channel Setting",
        description = "One sales channel of the store. One row for each marketplace (ebay-us and ebay-de are two rows): "
                + "own hub account, product store, currency, tax rule of the prices and buyer-data retention.",
        fields = {
            @Field(name = "channelId", type = "id-ne", description = "connectorId-marketplaceId, lower case, for example ebay-de"),
            @Field(name = "connectorId", type = "id-ne", description = "Hub connector id, for example ebay, amazon"),
            @Field(name = "marketplaceId", type = "id-ne", description = "Marketplace code, for example us, de"),
            @Field(name = "productStoreId", type = "id-ne", description = "The product store of this channel (blueprint 7.3)"),
            @Field(name = "accountId", type = "id-long", description = "Id of the ChannelAccount in the hub (reference only)"),
            @Field(name = "currencyUomId", type = "id-ne", description = "Currency of the channel"),
            @Field(name = "pricesIncludeTax", type = "indicator", description = "Y: the price that the buyer sees includes tax"),
            @Field(name = "salesChannelEnumId", type = "id", description = "OrderHeader.salesChannelEnumId of the orders of this channel"),
            @Field(name = "retentionDays", type = "numeric", description = "Days to keep buyer data; empty: no channel limit"),
            @Field(name = "retentionFrom", type = "id", description = "CLOSED (day the order is shipped or cancelled) or PLACED"),
            @Field(name = "active", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "channelId")
        },
        relations = {
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "ProductStore", keyMaps = {@KeyMap(fieldName = "productStoreId")}),
            @Relation(type = RelationType.ONE_NOFK, title = "Currency", relEntityName = "Uom", keyMaps = {@KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")})
        },
        indexes = {
            @Index(name = "CHNL_SETT_ACCOUNT", unique = true, fields = {@IndexField(name = "accountId")})
        }
    )
    public interface ChannelSettingEntity {}

    @Entity(
        name = "ChannelListing",
        packageName = "com.ilscipio.scipio.channel",
        title = "Channel Listing",
        description = "One sellable product (a variant SKU for a variation product) on one channel. "
                + "State: draft, pending, live, rejected, ended.",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "channelId", type = "id-ne"),
            @Field(name = "externalId", type = "id-long", description = "Listing id on the channel"),
            @Field(name = "listingState", type = "id", description = "DRAFT, PENDING, LIVE, REJECTED, ENDED"),
            @Field(name = "lastSyncDate", type = "date-time"),
            @Field(name = "errorsJson", type = "very-long", description = "Issues of the last sync"),
            @Field(name = "fixHint", type = "description", description = "One sentence: what the seller does"),
            @Field(name = "variationGroupId", type = "id", description = "Virtual product id of a variation product"),
            @Field(name = "variationAxesJson", type = "long-varchar", description = "Axes of the variant, for example {\"Size\":\"M\"}"),
            @Field(name = "lastPushedQuantity", type = "numeric", description = "Quantity that the channel last acknowledged")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "channelId")
        },
        relations = {
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "Product", keyMaps = {@KeyMap(fieldName = "productId")}),
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "ChannelSetting", keyMaps = {@KeyMap(fieldName = "channelId")})
        },
        indexes = {
            @Index(name = "CHNL_LIST_EXTERNAL", fields = {@IndexField(name = "channelId"), @IndexField(name = "externalId")}),
            @Index(name = "CHNL_LIST_GROUP", fields = {@IndexField(name = "variationGroupId")})
        }
    )
    public interface ChannelListingEntity {}

    @Entity(
        name = "ChannelProductData",
        packageName = "com.ilscipio.scipio.channel",
        title = "Channel Product Data",
        description = "Channel specific data of a product: category and attributes for the channel schema. guessedJson names the attributes that the AI guessed.",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "channelId", type = "id-ne"),
            @Field(name = "categoryId", type = "id-long"),
            @Field(name = "attributesJson", type = "very-long"),
            @Field(name = "guessedJson", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "channelId")
        },
        relations = {
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "Product", keyMaps = {@KeyMap(fieldName = "productId")}),
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "ChannelSetting", keyMaps = {@KeyMap(fieldName = "channelId")})
        }
    )
    public interface ChannelProductDataEntity {}

    @Entity(
        name = "ChannelStockRule",
        packageName = "com.ilscipio.scipio.channel",
        title = "Channel Stock Rule",
        description = "Quantity on a channel = available to promise - buffer, at most maxQuantity.",
        fields = {
            @Field(name = "channelId", type = "id-ne"),
            @Field(name = "buffer", type = "numeric"),
            @Field(name = "maxQuantity", type = "numeric", description = "Empty: no cap")
        },
        primaryKeys = {
            @PrimaryKey(field = "channelId")
        },
        relations = {
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "ChannelSetting", keyMaps = {@KeyMap(fieldName = "channelId")})
        }
    )
    public interface ChannelStockRuleEntity {}

    @Entity(
        name = "ChannelOrderRef",
        packageName = "com.ilscipio.scipio.channel",
        title = "Channel Order Reference",
        description = "Link between a store order and a channel order, with the fees, the payout and the buyer-data retention dates.",
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "channelId", type = "id-ne"),
            @Field(name = "externalOrderId", type = "id-long"),
            @Field(name = "payoutId", type = "id-long"),
            @Field(name = "feesAmount", type = "currency-amount"),
            @Field(name = "placedDate", type = "date-time"),
            @Field(name = "buyerExternalId", type = "id-long", description = "Buyer id at the channel; no name, no e-mail"),
            @Field(name = "closedDate", type = "date-time", description = "Day the order was shipped or cancelled"),
            @Field(name = "buyerDataDueDate", type = "date-time", description = "Day the buyer data goes; empty: no channel limit"),
            @Field(name = "buyerDataErasedDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId")
        },
        relations = {
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "OrderHeader", keyMaps = {@KeyMap(fieldName = "orderId")}),
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "ChannelSetting", keyMaps = {@KeyMap(fieldName = "channelId")})
        },
        indexes = {
            @Index(name = "CHNL_ORDREF_EXT", unique = true, fields = {@IndexField(name = "channelId"), @IndexField(name = "externalOrderId")}),
            @Index(name = "CHNL_ORDREF_DUE", fields = {@IndexField(name = "buyerDataDueDate")}),
            @Index(name = "CHNL_ORDREF_BUYER", fields = {@IndexField(name = "buyerExternalId")})
        }
    )
    public interface ChannelOrderRefEntity {}

    @Entity(
        name = "ChannelSyncTask",
        packageName = "com.ilscipio.scipio.channel",
        title = "Channel Sync Task",
        description = "One stock push to one channel. The hub claims due tasks, pushes them and reports the result. "
                + "A task that fails for good writes the error on the listing.",
        fields = {
            @Field(name = "taskId", type = "id-ne"),
            @Field(name = "channelId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "externalId", type = "id-long"),
            @Field(name = "quantity", type = "numeric"),
            @Field(name = "taskState", type = "id", description = "PENDING, CLAIMED, DONE, FAILED"),
            @Field(name = "attempts", type = "numeric"),
            @Field(name = "dueDate", type = "date-time"),
            @Field(name = "leaseUntil", type = "date-time"),
            @Field(name = "lastError", type = "description"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "doneDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "taskId")
        },
        relations = {
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "ChannelSetting", keyMaps = {@KeyMap(fieldName = "channelId")})
        },
        indexes = {
            @Index(name = "CHNL_SYNC_DUE", fields = {@IndexField(name = "taskState"), @IndexField(name = "dueDate")}),
            @Index(name = "CHNL_SYNC_LISTING", fields = {@IndexField(name = "channelId"), @IndexField(name = "productId")})
        }
    )
    public interface ChannelSyncTaskEntity {}

    @Entity(
        name = "ChannelOutboxEvent",
        packageName = "com.ilscipio.scipio.channel",
        title = "Channel Outbox Event",
        description = "The event outbox of the store (W1-07). The store writes a row in the transaction of its cause (new order, order that needs "
                + "shipping, stock change). The desk claims rows through the MCP topic channel, acknowledges them, and the store purges done rows after the retention time.",
        fields = {
            @Field(name = "eventId", type = "id-ne"),
            @Field(name = "eventType", type = "id", description = "ORDER_CREATED, ORDER_NEEDS_SHIPPING, STOCK_CHANGED"),
            @Field(name = "payloadJson", type = "very-long"),
            @Field(name = "dedupeKey", type = "id-long", description = "Unique: a second write with the same key is skipped; equals eventId when the cause has no key"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "claimedBy", type = "id-long", description = "Consumer that holds or held the event"),
            @Field(name = "claimedDate", type = "date-time"),
            @Field(name = "leaseUntil", type = "date-time", description = "While in the future the event is held, or (no claimer) waits for a retry"),
            @Field(name = "doneDate", type = "date-time"),
            @Field(name = "attempts", type = "numeric"),
            @Field(name = "lastError", type = "description"),
            @Field(name = "parkedDate", type = "date-time", description = "Set when attempts reached outbox.maxAttempts: claim skips the event, purge keeps it")
        },
        primaryKeys = {
            @PrimaryKey(field = "eventId")
        },
        indexes = {
            @Index(name = "CHNL_OUTBOX_KEY", unique = true, fields = {@IndexField(name = "dedupeKey")}),
            @Index(name = "CHNL_OUTBOX_OPEN", fields = {@IndexField(name = "doneDate"), @IndexField(name = "createdDate")})
        }
    )
    public interface ChannelOutboxEventEntity {}
}
