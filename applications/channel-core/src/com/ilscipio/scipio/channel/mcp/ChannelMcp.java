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
package com.ilscipio.scipio.channel.mcp;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;

/**
 * MCP server profile of channel-core: the topic {@code channel}. The hub in the control plane reaches the store only through
 * this topic (blueprint 3, rules 3 and 4): it claims stock pushes and reports the result, hands over orders, and saves the
 * state of listings. Each write goes through a Scipio service with a permission check and an audit row.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
@McpServer(name = "channel", title = "Sales channels", component = "channel-core",
        description = "Sales channels of the store: one channel for each marketplace, listings, stock pushes, channel orders, buyer-data retention.",
        entities = {"ChannelSetting", "ChannelListing", "ChannelProductData", "ChannelStockRule", "ChannelOrderRef", "ChannelSyncTask", "ChannelOutboxEvent"},
        topics = {
            @McpTopic(name = "channel", title = "Sales channels", order = 40, featured = true,
                    description = "Channels (eBay US, eBay DE, Amazon DE ...): settings, listings, stock pushes, order intake, buyer-data retention.")
        },
        serviceTools = {
            @McpServiceTool(service = "channelSaveSetting", topic = "channel", name = "save_setting",
                    description = "Create or change one channel (one marketplace): store, hub account, currency, tax rule, retention, stock rule.",
                    readOnly = false, requiresConfirmation = true, order = 20),
            @McpServiceTool(service = "channelSaveListing", topic = "channel", name = "save_listing",
                    description = "Record the state of a listing: id on the channel, state, issues, fix hint.",
                    readOnly = false, order = 40),
            @McpServiceTool(service = "channelListingPrice", topic = "channel", name = "price",
                    description = "The price of a product on a channel (currency and tax rule of the channel), or a fix hint.",
                    readOnly = true, order = 45),
            @McpServiceTool(service = "channelProductData", topic = "channel", name = "product_data",
                    description = "The channel data of a product for a listing: channel category and attributes, quantity for the channel, SKU, GTIN, MPN, variant group and axes.",
                    readOnly = true, order = 46),
            @McpServiceTool(service = "channelResyncStock", topic = "channel", name = "stock_resync",
                    description = "Plan the stock push of a product to every channel that lists it.",
                    readOnly = false, order = 50),
            @McpServiceTool(service = "channelClaimStockTasks", topic = "channel", name = "stock_claim",
                    description = "Hub: take the due stock pushes for a lease time.",
                    readOnly = false, order = 55),
            @McpServiceTool(service = "channelReportStockTask", topic = "channel", name = "stock_report",
                    description = "Hub: report the result of a stock push (success, or retryable with a wait time, or failed).",
                    readOnly = false, order = 56),
            @McpServiceTool(service = "channelIntakeOrder", topic = "channel", name = "order_intake",
                    description = "Hub: hand a channel order to the store. Makes the store order, lowers the stock, plans the pushes to the other channels.",
                    readOnly = false, order = 60),
            @McpServiceTool(service = "channelCloseOrder", topic = "channel", name = "order_close",
                    description = "Hub: a channel order is shipped or cancelled; the retention clock of its buyer data starts.",
                    readOnly = false, order = 65),
            @McpServiceTool(service = "channelRunRetention", topic = "channel", name = "retention_run",
                    description = "Erase the buyer data of each channel order that is due. A daily job runs this too.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 70),
            @McpServiceTool(service = "channelBuyerDeletion", topic = "channel", name = "buyer_deletion",
                    description = "Hub: the channel asks to erase a buyer (eBay account-deletion notice). Erases the buyer data at once.",
                    readOnly = false, destructive = "true", order = 75),
            @McpServiceTool(service = "channelClaimOutboxEvents", topic = "channel", name = "outbox_claim",
                    description = "Desk: take a batch of store events (new order, order to ship, stock change) for a lease time. At least once: handle each by eventId.",
                    readOnly = false, order = 80),
            @McpServiceTool(service = "channelAckOutboxEvents", topic = "channel", name = "outbox_ack",
                    description = "Desk: the events are handled. A repeat is safe.",
                    readOnly = false, order = 81),
            @McpServiceTool(service = "channelReleaseOutboxEvents", topic = "channel", name = "outbox_release",
                    description = "Desk: the events could not be handled; give them back at once or after a wait time.",
                    readOnly = false, order = 82),
            @McpServiceTool(service = "channelListParkedOutboxEvents", topic = "channel", name = "outbox_parked",
                    description = "Desk: list the parked events (failed outbox.maxAttempts times). Claim skips them; purge keeps them.",
                    readOnly = true, order = 82),
            @McpServiceTool(service = "channelPurgeOutbox", topic = "channel", name = "outbox_purge",
                    description = "Delete done events older than the retention time (default 7 days). A daily job runs this too. Open events stay.",
                    readOnly = false, destructive = "true", order = 83)
        })
public final class ChannelMcp {

    private ChannelMcp() {
    }

    @McpTool(topic = "channel", name = "settings", readOnly = true, order = 10,
            description = "List the channels of the store with currency, tax rule, retention and stock rule.")
    public static Object settings(McpCallContext ctx) throws McpToolException {
        try {
            List<Map<String, Object>> rows = new ArrayList<>();
            for (GenericValue s : EntityQuery.use(ctx.getDelegator()).from("ChannelSetting").orderBy("channelId").queryList()) {
                Map<String, Object> row = new LinkedHashMap<>(s.getAllFields());
                GenericValue rule = EntityQuery.use(ctx.getDelegator()).from("ChannelStockRule").where("channelId", s.getString("channelId")).queryOne();
                row.put("buffer", rule == null ? 0 : rule.get("buffer"));
                row.put("maxQuantity", rule == null ? null : rule.get("maxQuantity"));
                rows.add(row);
            }
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("channels", rows);
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Could not read the channels: " + e.getMessage());
        }
    }

    @McpTool(topic = "channel", name = "listings", readOnly = true, order = 30,
            description = "List channel listings with state, issues and fix hint. Filter by product, channel or state.")
    public static Object listings(McpCallContext ctx,
            @McpParam(name = "productId", description = "Product id", required = false) String productId,
            @McpParam(name = "channelId", description = "Channel id, for example ebay-de", required = false) String channelId,
            @McpParam(name = "listingState", description = "DRAFT, PENDING, LIVE, REJECTED or ENDED", required = false) String listingState,
            @McpParam(name = "limit", description = "Rows to return; default 50", required = false) Integer limit) throws McpToolException {
        Map<String, Object> where = new LinkedHashMap<>();
        if (UtilValidate.isNotEmpty(productId)) {
            where.put("productId", productId);
        }
        if (UtilValidate.isNotEmpty(channelId)) {
            where.put("channelId", channelId);
        }
        if (UtilValidate.isNotEmpty(listingState)) {
            where.put("listingState", listingState.trim().toUpperCase(java.util.Locale.ROOT));
        }
        try {
            List<GenericValue> found = EntityQuery.use(ctx.getDelegator()).from("ChannelListing").where(where)
                    .orderBy("channelId", "productId").maxRows(ctx.limit(limit)).queryList();
            List<Map<String, Object>> rows = new ArrayList<>();
            for (GenericValue v : found) {
                rows.add(new LinkedHashMap<>(v.getAllFields()));
            }
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("listings", rows);
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Could not read the listings: " + e.getMessage());
        }
    }

    @McpTool(topic = "channel", name = "stock_queue", readOnly = true, order = 58,
            description = "Open stock pushes (pending or held) with their attempts and last error.")
    public static Object stockQueue(McpCallContext ctx,
            @McpParam(name = "channelId", description = "Channel id", required = false) String channelId) throws McpToolException {
        try {
            Map<String, Object> where = new LinkedHashMap<>();
            if (UtilValidate.isNotEmpty(channelId)) {
                where.put("channelId", channelId);
            }
            List<Map<String, Object>> rows = new ArrayList<>();
            for (GenericValue t : EntityQuery.use(ctx.getDelegator()).from("ChannelSyncTask").where(where).orderBy("createdDate").queryList()) {
                String state = t.getString("taskState");
                if ("PENDING".equals(state) || "CLAIMED".equals(state) || "FAILED".equals(state)) {
                    rows.add(new LinkedHashMap<>(t.getAllFields()));
                }
            }
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("tasks", rows);
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Could not read the stock pushes: " + e.getMessage());
        }
    }
}
