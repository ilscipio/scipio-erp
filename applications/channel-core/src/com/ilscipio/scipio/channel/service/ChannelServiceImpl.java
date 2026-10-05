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

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.Callable;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.channel.core.ChannelSetting;
import com.ilscipio.scipio.channel.core.ChannelStore;
import com.ilscipio.scipio.channel.core.EventOutbox;
import com.ilscipio.scipio.channel.core.IncomingOrder;
import com.ilscipio.scipio.channel.core.Listing;
import com.ilscipio.scipio.channel.core.ListingState;
import com.ilscipio.scipio.channel.core.MarketplaceDefaults;
import com.ilscipio.scipio.channel.core.OrderIntake;
import com.ilscipio.scipio.channel.core.OutboxEvent;
import com.ilscipio.scipio.channel.core.OutboxEvents;
import com.ilscipio.scipio.channel.core.PricePolicy;
import com.ilscipio.scipio.channel.core.RetentionRun;
import com.ilscipio.scipio.channel.core.StockRule;
import com.ilscipio.scipio.channel.core.StockSource;
import com.ilscipio.scipio.channel.core.StockSync;
import com.ilscipio.scipio.channel.core.SyncQueue;
import com.ilscipio.scipio.channel.core.SyncTask;
import com.ilscipio.scipio.channel.store.EntityBuyerDataEraser;
import com.ilscipio.scipio.channel.store.EntityChannelStore;
import com.ilscipio.scipio.channel.store.EntityOrderCreator;
import com.ilscipio.scipio.channel.store.EntityOutboxStore;

/**
 * Service implementations of channel-core: thin adapters between the service engine and the classes of the
 * package {@code core}, which hold the rules and the unit tests.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public final class ChannelServiceImpl {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private ChannelServiceImpl() {
    }

    // ---- helpers ----

    private static Map<String, Object> denied(DispatchContext dctx, Map<String, ?> context) {
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (dctx.getSecurity().hasPermission("CHANNELCORE_UPDATE", userLogin) || dctx.getSecurity().hasPermission("CHANNELCORE_ADMIN", userLogin)) {
            return null;
        }
        return ServiceUtil.returnError("Permission CHANNELCORE_UPDATE is required.");
    }

    /** ATP of a product in the inventory facility of the channel store (ProductStore.inventoryFacilityId); all facilities when the store has none. */
    private static StockSource stockSource(final DispatchContext dctx, final Map<String, ?> context) {
        return (productId, productStoreId) -> {
            try {
                String facilityId = null;
                GenericValue ps = EntityQuery.use(dctx.getDelegator()).from("ProductStore").where("productStoreId", productStoreId).cache().queryOne();
                if (ps != null) {
                    facilityId = ps.getString("inventoryFacilityId");
                }
                Map<String, Object> in = UtilMisc.<String, Object>toMap("productId", productId, "userLogin", context.get("userLogin"));
                if (UtilValidate.isNotEmpty(facilityId)) {
                    in.put("facilityId", facilityId);
                }
                Map<String, Object> res = dctx.getDispatcher().runSync(
                        UtilValidate.isNotEmpty(facilityId) ? "getInventoryAvailableByFacility" : "getProductInventoryAvailable", in);
                BigDecimal atp = ServiceUtil.isError(res) ? null : (BigDecimal) res.get("availableToPromiseTotal");
                return atp == null ? BigDecimal.ZERO : atp;
            } catch (GenericServiceException | GenericEntityException e) {
                throw new IllegalStateException("Could not read the stock of " + productId + ": " + e.getMessage(), e);
            }
        };
    }

    private static Map<String, Object> deniedView(DispatchContext dctx, Map<String, ?> context) {
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        for (String p : new String[] {"CHANNELCORE_VIEW", "CHANNELCORE_UPDATE", "CHANNELCORE_ADMIN"}) {
            if (dctx.getSecurity().hasPermission(p, userLogin)) {
                return null;
            }
        }
        return ServiceUtil.returnError("Permission CHANNELCORE_VIEW is required.");
    }

    private static RetentionRun.Transactional ownTransaction() {
        return new RetentionRun.Transactional() {
            @Override
            public <T> T run(Callable<T> work) throws Exception {
                return TransactionUtil.inTransaction(work, "Channel buyer-data erase", 120, true).call();
            }
        };
    }

    private static String yn(Object v, String def) {
        return UtilValidate.isEmpty((String) v) ? def : ((String) v).trim().toUpperCase(java.util.Locale.ROOT);
    }

    // ---- settings, listings, price ----

    public static Map<String, Object> saveSetting(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> err = denied(dctx, context);
        if (err != null) {
            return err;
        }
        ChannelStore store = new EntityChannelStore(dctx.getDelegator());
        try {
            String connectorId = (String) context.get("connectorId");
            String marketplaceId = (String) context.get("marketplaceId");
            String channelId = ChannelSetting.channelId(connectorId, marketplaceId);
            MarketplaceDefaults def = null;
            try {
                def = MarketplaceDefaults.of(connectorId, marketplaceId);
            } catch (IllegalArgumentException e) {
                if (UtilValidate.isEmpty((String) context.get("currencyUomId")) || UtilValidate.isEmpty((String) context.get("pricesIncludeTax"))) {
                    return ServiceUtil.returnError(e.getMessage() + ". Give currencyUomId and pricesIncludeTax.");
                }
            }
            Optional<ChannelSetting> old = store.setting(channelId);
            String currency = UtilValidate.isNotEmpty((String) context.get("currencyUomId")) ? (String) context.get("currencyUomId")
                    : (old.isPresent() ? old.get().currencyUomId : def.currencyUomId);
            boolean incl = UtilValidate.isNotEmpty((String) context.get("pricesIncludeTax")) ? "Y".equals(yn(context.get("pricesIncludeTax"), "N"))
                    : (old.isPresent() ? old.get().pricesIncludeTax : def.pricesIncludeTax);
            Integer days = old.isPresent() ? old.get().retentionDays : (def == null ? null : def.retentionDays);
            if (context.get("retentionDays") != null) {
                days = (Integer) context.get("retentionDays");
            }
            if ("Y".equals(yn(context.get("clearRetention"), "N"))) {
                days = null;
            }
            String fromStr = (String) context.get("retentionFrom");
            ChannelSetting.RetentionFrom from = UtilValidate.isNotEmpty(fromStr) ? ChannelSetting.RetentionFrom.valueOf(fromStr.trim().toUpperCase(java.util.Locale.ROOT))
                    : (old.isPresent() ? old.get().retentionFrom : (def == null ? null : def.retentionFrom));
            String salesChannel = UtilValidate.isNotEmpty((String) context.get("salesChannelEnumId")) ? (String) context.get("salesChannelEnumId")
                    : (old.isPresent() ? old.get().salesChannelEnumId : null);
            boolean active = UtilValidate.isNotEmpty((String) context.get("active")) ? "Y".equals(yn(context.get("active"), "Y"))
                    : (!old.isPresent() || old.get().active);
            ChannelSetting s = new ChannelSetting(connectorId, marketplaceId, (String) context.get("productStoreId"),
                    (String) context.get("accountId"), currency, incl, salesChannel, days, from, active);

            // one product store for each channel, one hub account for each marketplace
            for (GenericValue other : EntityQuery.use(dctx.getDelegator()).from("ChannelSetting").queryList()) {
                if (channelId.equals(other.getString("channelId"))) {
                    continue;
                }
                if (s.productStoreId.equals(other.getString("productStoreId"))) {
                    return ServiceUtil.returnError("The product store " + s.productStoreId + " belongs to the channel "
                            + other.getString("channelId") + ". Each channel has its own product store.");
                }
                if (s.accountId.equals(other.getString("accountId"))) {
                    return ServiceUtil.returnError("The account " + s.accountId + " belongs to the channel "
                            + other.getString("channelId") + ". Each marketplace has its own channel account.");
                }
            }
            store.saveSetting(s);
            if (context.get("buffer") != null || context.get("maxQuantity") != null) {
                StockRule oldRule = store.stockRule(channelId);
                Integer buffer = context.get("buffer") != null ? (Integer) context.get("buffer") : Integer.valueOf(oldRule.buffer);
                Integer max = context.get("maxQuantity") != null ? (Integer) context.get("maxQuantity") : oldRule.maxQuantity;
                store.saveStockRule(channelId, new StockRule(buffer, max));
            }
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("channelId", channelId);
            return result;
        } catch (IllegalArgumentException | GenericEntityException e) {
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    public static Map<String, Object> saveListing(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> err = denied(dctx, context);
        if (err != null) {
            return err;
        }
        ChannelStore store = new EntityChannelStore(dctx.getDelegator());
        String productId = (String) context.get("productId");
        String channelId = (String) context.get("channelId");
        if (!store.setting(channelId).isPresent()) {
            return ServiceUtil.returnError("Unknown channel " + channelId);
        }
        try {
            Listing l = store.listing(productId, channelId).orElseGet(() -> new Listing(productId, channelId));
            if (context.get("externalId") != null) {
                l.changeExternalId((String) context.get("externalId")); // a new id resets the last acknowledged quantity
            }
            if (UtilValidate.isNotEmpty((String) context.get("listingState"))) {
                l.state = ListingState.parse((String) context.get("listingState"));
            }
            // a field that the caller does not send stays; an empty string clears it
            if (context.get("errorsJson") != null) {
                l.errorsJson = ((String) context.get("errorsJson")).isEmpty() ? null : (String) context.get("errorsJson");
            }
            if (context.get("fixHint") != null) {
                l.fixHint = ((String) context.get("fixHint")).isEmpty() ? null : (String) context.get("fixHint");
            }
            if (context.get("variationGroupId") != null) {
                l.variationGroupId = (String) context.get("variationGroupId");
                l.variationAxesJson = (String) context.get("variationAxesJson");
            }
            l.lastSyncDate = Clock.systemUTC().instant();
            store.saveListing(l);
        } catch (IllegalArgumentException e) {
            return ServiceUtil.returnError(e.getMessage());
        }
        return ServiceUtil.returnSuccess();
    }

    public static Map<String, Object> listingPrice(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> denied = deniedView(dctx, context);
        if (denied != null) {
            return denied;
        }
        Delegator delegator = dctx.getDelegator();
        ChannelStore store = new EntityChannelStore(delegator);
        String productId = (String) context.get("productId");
        Optional<ChannelSetting> opt = store.setting((String) context.get("channelId"));
        if (!opt.isPresent()) {
            return ServiceUtil.returnError("Unknown channel " + context.get("channelId"));
        }
        ChannelSetting s = opt.get();
        try {
            List<String> groupIds = new ArrayList<>();
            for (GenericValue m : EntityQuery.use(delegator).from("ProductStoreGroupMember").where("productStoreId", s.productStoreId)
                    .filterByDate().queryList()) {
                groupIds.add(m.getString("productStoreGroupId"));
            }
            // the rows of the store groups win; the rows of "_NA_" are a fallback only
            List<PricePolicy.PriceRow> groupRows = new ArrayList<>();
            List<PricePolicy.PriceRow> naRows = new ArrayList<>();
            for (GenericValue p : EntityQuery.use(delegator).from("ProductPrice").where("productId", productId,
                    "productPriceTypeId", "DEFAULT_PRICE", "productPricePurposeId", "PURCHASE").orderBy("-fromDate")
                    .filterByDate().queryList()) {
                PricePolicy.PriceRow row = new PricePolicy.PriceRow(p.getString("currencyUomId"), p.getBigDecimal("price"),
                        "Y".equals(p.getString("taxInPrice")));
                if (groupIds.contains(p.getString("productStoreGroupId"))) {
                    groupRows.add(row);
                } else if ("_NA_".equals(p.getString("productStoreGroupId"))) {
                    naRows.add(row);
                }
            }
            PricePolicy.Result r = PricePolicy.resolve(s, groupRows);
            if (!r.isOk()) {
                PricePolicy.Result fallback = PricePolicy.resolve(s, naRows);
                if (fallback.isOk() || groupRows.isEmpty()) {
                    r = fallback;
                }
            }
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("price", r.price);
            result.put("currencyUomId", r.currencyUomId);
            result.put("pricesIncludeTax", s.pricesIncludeTax ? "Y" : "N");
            result.put("errorCode", r.errorCode);
            result.put("fixHint", r.fixHint);
            return result;
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** The GoodIdentification types that hold a GTIN, in the order of preference. */
    private static final List<String> GTIN_TYPES = UtilMisc.toList("EAN", "UPCA", "UPCE", "ISBN");

    public static Map<String, Object> productData(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> denied = deniedView(dctx, context);
        if (denied != null) {
            return denied;
        }
        Delegator delegator = dctx.getDelegator();
        ChannelStore store = new EntityChannelStore(delegator);
        String productId = (String) context.get("productId");
        String channelId = (String) context.get("channelId");
        Optional<ChannelSetting> opt = store.setting(channelId);
        if (!opt.isPresent()) {
            return ServiceUtil.returnError("Unknown channel " + channelId);
        }
        if (UtilValidate.isEmpty(opt.get().productStoreId)) {
            return ServiceUtil.returnError("The channel " + channelId + " has no productStoreId");
        }
        try {
            GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", productId).queryOne();
            if (product == null) {
                return ServiceUtil.returnError("Unknown product " + productId);
            }
            Map<String, Object> result = ServiceUtil.returnSuccess();
            GenericValue data = EntityQuery.use(delegator).from("ChannelProductData").where("productId", productId, "channelId", channelId).queryOne();
            if (data != null) {
                result.put("categoryId", data.getString("categoryId"));
                result.put("attributesJson", data.getString("attributesJson"));
                result.put("guessedJson", data.getString("guessedJson"));
            }
            result.put("quantity", store.stockRule(channelId).apply(stockSource(dctx, context).availableToPromise(productId, opt.get().productStoreId)));
            String sku = identification(delegator, productId, UtilMisc.toList("SKU"));
            result.put("sku", sku != null ? sku : productId);
            result.put("gtin", identification(delegator, productId, GTIN_TYPES));
            result.put("mpn", identification(delegator, productId, UtilMisc.toList("MANUFACTURER_ID_NO")));
            if ("Y".equals(product.getString("isVariant"))) {
                GenericValue assoc = EntityQuery.use(delegator).from("ProductAssoc")
                        .where("productIdTo", productId, "productAssocTypeId", "PRODUCT_VARIANT").filterByDate().queryFirst();
                Map<String, String> axes = assoc == null ? new LinkedHashMap<>() : variationAxes(delegator, assoc.getString("productId"), productId);
                if (!axes.isEmpty()) {
                    // a variant without an axis is listed as a single product (the hub needs a group and an axis together)
                    result.put("variationGroupId", assoc.getString("productId"));
                    result.put("variationAxesJson", com.ilscipio.scipio.channel.core.FlatJson.write(axes));
                }
            }
            return result;
        } catch (GenericEntityException | RuntimeException e) {
            return ServiceUtil.returnError(String.valueOf(e.getMessage()));
        }
    }

    private static String identification(Delegator delegator, String productId, List<String> types) throws GenericEntityException {
        for (String type : types) {
            GenericValue id = EntityQuery.use(delegator).from("GoodIdentification")
                    .where("productId", productId, "goodIdentificationTypeId", type).queryFirst();
            if (id != null && UtilValidate.isNotEmpty(id.getString("idValue"))) {
                return id.getString("idValue");
            }
        }
        return null;
    }

    /** The axes of a variant: each feature of the variant whose type is a selectable feature type of the virtual product. */
    private static Map<String, String> variationAxes(Delegator delegator, String virtualId, String variantId) throws GenericEntityException {
        Set<String> axisTypes = new HashSet<>();
        for (GenericValue appl : EntityQuery.use(delegator).from("ProductFeatureAndAppl")
                .where("productId", virtualId, "productFeatureApplTypeId", "SELECTABLE_FEATURE").filterByDate().queryList()) {
            axisTypes.add(appl.getString("productFeatureTypeId"));
        }
        Map<String, String> axes = new LinkedHashMap<>();
        for (GenericValue appl : EntityQuery.use(delegator).from("ProductFeatureAndAppl").where("productId", variantId)
                .orderBy("sequenceNum", "productFeatureTypeId").filterByDate().queryList()) {
            String type = appl.getString("productFeatureTypeId");
            String applType = appl.getString("productFeatureApplTypeId");
            if (!axisTypes.contains(type) || UtilValidate.isEmpty(appl.getString("description"))
                    || !("STANDARD_FEATURE".equals(applType) || "DISTINGUISHING_FEAT".equals(applType))) {
                continue;
            }
            GenericValue featureType = EntityQuery.use(delegator).from("ProductFeatureType").where("productFeatureTypeId", type).cache().queryOne();
            String name = featureType != null && UtilValidate.isNotEmpty(featureType.getString("description")) ? featureType.getString("description") : type;
            axes.putIfAbsent(name, appl.getString("description"));
        }
        return axes;
    }

    // ---- stock ----

    public static Map<String, Object> stockChanged(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        String productId = (String) context.get("productId");
        try {
            if (UtilValidate.isEmpty(productId) && UtilValidate.isNotEmpty((String) context.get("inventoryItemId"))) {
                GenericValue item = EntityQuery.use(delegator).from("InventoryItem").where("inventoryItemId", context.get("inventoryItemId")).queryOne();
                productId = item == null ? null : item.getString("productId");
            }
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(productId)) {
            return ServiceUtil.returnSuccess(); // an inventory item without a product: nothing to send
        }
        ChannelStore store = new EntityChannelStore(delegator);
        StockSync sync = new StockSync(store, stockSource(dctx, context), Clock.systemUTC());
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("tasksPlanned", sync.stockChanged(productId).size());
        return result;
    }

    /** The checked form of stockChanged for the MCP action stock_resync. The ECA keeps the unchecked one. */
    public static Map<String, Object> resyncStock(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> err = denied(dctx, context);
        if (err != null) {
            return err;
        }
        return stockChanged(dctx, context);
    }

    public static Map<String, Object> claimStockTasks(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> err = denied(dctx, context);
        if (err != null) {
            return err;
        }
        ChannelStore store = new EntityChannelStore(dctx.getDelegator());
        int limit = context.get("limit") == null ? 50 : Math.max(1, Math.min(500, (Integer) context.get("limit")));
        int lease = context.get("leaseSeconds") == null ? 30 : Math.max(5, Math.min(600, (Integer) context.get("leaseSeconds")));
        List<Map<String, Object>> out = new ArrayList<>();
        for (SyncTask t : new SyncQueue(store, Clock.systemUTC(), stockSource(dctx, context)).claim(limit, Duration.ofSeconds(lease))) {
            Map<String, Object> row = new LinkedHashMap<>();
            row.put("taskId", t.taskId);
            row.put("channelId", t.channelId);
            row.put("productId", t.productId);
            row.put("externalId", t.externalId);
            row.put("quantity", t.quantity);
            row.put("attempts", t.attempts);
            out.add(row);
        }
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("tasks", out);
        return result;
    }

    public static Map<String, Object> reportStockTask(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> err = denied(dctx, context);
        if (err != null) {
            return err;
        }
        ChannelStore store = new EntityChannelStore(dctx.getDelegator());
        SyncQueue queue = new SyncQueue(store, Clock.systemUTC());
        String taskId = (String) context.get("taskId");
        try {
            if ("Y".equals(yn(context.get("success"), "N"))) {
                queue.succeeded(taskId);
            } else {
                Integer after = (Integer) context.get("retryAfterSeconds");
                queue.failed(taskId, !"N".equals(yn(context.get("retryable"), "Y")), after == null ? null : Duration.ofSeconds(after),
                        (String) context.get("error"), (String) context.get("fixHint"));
            }
        } catch (IllegalArgumentException | IllegalStateException e) {
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("taskState", store.task(taskId).map(t -> t.state.name()).orElse(null));
        return result;
    }

    // ---- orders and retention ----

    public static Map<String, Object> intakeOrder(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> err = denied(dctx, context);
        if (err != null) {
            return err;
        }
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        ChannelStore store = new EntityChannelStore(delegator);
        IncomingOrder order;
        try {
            @SuppressWarnings("unchecked")
            Map<String, Object> raw = (Map<String, Object>) context.get("order");
            order = IncomingOrder.fromMap(raw);
        } catch (RuntimeException e) {
            return ServiceUtil.returnError("The order is not valid: " + e.getMessage());
        }
        StockSync sync = new StockSync(store, stockSource(dctx, context), Clock.systemUTC());
        OrderIntake intake = new OrderIntake(store, new EntityOrderCreator(delegator, dispatcher, userLogin), sync);
        OrderIntake.Result r = intake.intake((String) context.get("channelId"), order);
        if (r.status == OrderIntake.Status.REJECTED) {
            return ServiceUtil.returnError(r.message); // the transaction rolls back: nothing of the order stays
        }
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("intakeStatus", r.status.name());
        result.put("orderId", r.orderId);
        result.put("message", r.message);
        result.put("unmappedSkus", r.unmappedSkus);
        return result;
    }

    private static RetentionRun retention(DispatchContext dctx, Map<String, ?> context) {
        return new RetentionRun(new EntityChannelStore(dctx.getDelegator()),
                new EntityBuyerDataEraser(dctx.getDelegator(), dctx.getDispatcher(), (GenericValue) context.get("userLogin")),
                Clock.systemUTC(), ownTransaction());
    }

    public static Map<String, Object> closeOrder(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> err = denied(dctx, context);
        if (err != null) {
            return err;
        }
        try {
            retention(dctx, context).orderClosed((String) context.get("orderId"));
        } catch (IllegalArgumentException | IllegalStateException e) {
            return ServiceUtil.returnError(e.getMessage());
        }
        return ServiceUtil.returnSuccess();
    }

    public static Map<String, Object> runRetention(DispatchContext dctx, Map<String, ? extends Object> context) {
        GenericValue caller = (GenericValue) context.get("userLogin");
        if (caller == null || !"system".equals(caller.getString("userLoginId"))) {
            Map<String, Object> err = denied(dctx, context); // the scheduled job runs as the system user
            if (err != null) {
                return err;
            }
        }
        RetentionRun.Result r = retention(dctx, context).run();
        if (!r.failedOrderIds.isEmpty()) {
            Debug.logWarning("Channel retention: the buyer data of " + r.failedOrderIds.size() + " orders could not be erased: "
                    + r.failedOrderIds, module);
        }
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("erased", r.erased);
        result.put("failedOrderIds", r.failedOrderIds);
        return result;
    }

    public static Map<String, Object> buyerDeletion(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> err = denied(dctx, context);
        if (err != null) {
            return err;
        }
        try {
            RetentionRun.Result r = retention(dctx, context).buyerDeletion((String) context.get("connectorId"),
                    (String) context.get("buyerExternalId"));
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("erased", r.erased);
            result.put("failedOrderIds", r.failedOrderIds);
            return result;
        } catch (IllegalArgumentException e) {
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    // ---- event outbox (W1-07) ----

    private static EventOutbox outbox(DispatchContext dctx) {
        return new EventOutbox(new EntityOutboxStore(dctx.getDelegator()), Clock.systemUTC(), outboxMaxAttempts());
    }

    /** Reads what an outbox event needs to know of an order, in the transaction of the caller. Null when the order does not exist. */
    private static OutboxEvents.OrderFacts orderFacts(Delegator delegator, String orderId) throws GenericEntityException {
        // Lock the order row first: two transactions on one order must write their events one after the other (see lockCauseRow).
        EntityOutboxStore.lockCauseRow(delegator, "OrderHeader", "orderId", orderId);
        GenericValue h = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();
        if (h == null) {
            return null;
        }
        OutboxEvents.OrderFacts f = new OutboxEvents.OrderFacts();
        f.orderId = orderId;
        f.orderTypeId = h.getString("orderTypeId");
        f.statusId = h.getString("statusId");
        f.productStoreId = h.getString("productStoreId");
        f.salesChannelEnumId = h.getString("salesChannelEnumId");
        f.grandTotal = h.getBigDecimal("grandTotal");
        f.currencyUom = h.getString("currencyUom");
        if (UtilValidate.isNotEmpty(f.productStoreId)) {
            GenericValue setting = EntityQuery.use(delegator).from("ChannelSetting").where("productStoreId", f.productStoreId).queryFirst();
            f.channelId = setting == null ? null : setting.getString("channelId");
        }
        for (GenericValue item : EntityQuery.use(delegator).from("OrderItem").where("orderId", orderId).queryList()) {
            String itemStatus = item.getString("statusId");
            if ("ITEM_CANCELLED".equals(itemStatus) || "ITEM_REJECTED".equals(itemStatus)) {
                continue;
            }
            f.itemCount++;
            String productId = item.getString("productId");
            if (!f.needsShipping && UtilValidate.isNotEmpty(productId)) {
                GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", productId).cache().queryOne();
                GenericValue type = product == null ? null
                        : EntityQuery.use(delegator).from("ProductType").where("productTypeId", product.getString("productTypeId")).cache().queryOne();
                f.needsShipping = type != null && "Y".equals(type.getString("isPhysical"));
            }
        }
        return f;
    }

    /**
     * Hook (service ECA on storeOrder, event commit): writes ORDER_CREATED in the order transaction. An error here fails the
     * order transaction, so an order without its event cannot commit.
     */
    public static Map<String, Object> outboxOrderCreated(DispatchContext dctx, Map<String, ? extends Object> context) {
        String orderId = (String) context.get("orderId");
        if (UtilValidate.isEmpty(orderId)) {
            return ServiceUtil.returnSuccess();
        }
        try {
            OutboxEvents.orderCreated(outbox(dctx), orderFacts(dctx.getDelegator(), orderId));
        } catch (GenericEntityException | IllegalStateException e) {
            return ServiceUtil.returnError("Could not write the outbox event of order " + orderId + ": " + e.getMessage());
        }
        return ServiceUtil.returnSuccess();
    }

    /** Hook (service ECA on changeOrderStatus, event commit): writes ORDER_NEEDS_SHIPPING when the order is approved. */
    public static Map<String, Object> outboxOrderStatus(DispatchContext dctx, Map<String, ? extends Object> context) {
        String orderId = (String) context.get("orderId");
        if (UtilValidate.isEmpty(orderId) || !OutboxEvents.ORDER_APPROVED.equals(context.get("statusId"))) {
            return ServiceUtil.returnSuccess();
        }
        try {
            OutboxEvents.orderApproved(outbox(dctx), orderFacts(dctx.getDelegator(), orderId));
        } catch (GenericEntityException | IllegalStateException e) {
            return ServiceUtil.returnError("Could not write the outbox event of order " + orderId + ": " + e.getMessage());
        }
        return ServiceUtil.returnSuccess();
    }

    /** Hook (service ECA on createInventoryItemDetail, event commit): writes STOCK_CHANGED in the transaction of the stock change. */
    public static Map<String, Object> outboxStockChanged(DispatchContext dctx, Map<String, ? extends Object> context) {
        String inventoryItemId = (String) context.get("inventoryItemId");
        if (UtilValidate.isEmpty(inventoryItemId)) {
            return ServiceUtil.returnSuccess();
        }
        try {
            GenericValue item = EntityQuery.use(dctx.getDelegator()).from("InventoryItem").where("inventoryItemId", inventoryItemId).queryOne();
            if (item == null) {
                return ServiceUtil.returnSuccess();
            }
            OutboxEvents.StockFacts s = new OutboxEvents.StockFacts();
            s.productId = item.getString("productId");
            s.inventoryItemId = inventoryItemId;
            s.inventoryItemDetailSeqId = (String) context.get("inventoryItemDetailSeqId");
            s.facilityId = item.getString("facilityId");
            s.orderId = (String) context.get("orderId");
            s.availableToPromiseDiff = (BigDecimal) context.get("availableToPromiseDiff");
            s.quantityOnHandDiff = (BigDecimal) context.get("quantityOnHandDiff");
            OutboxEvents.stockChanged(outbox(dctx), s);
        } catch (GenericEntityException | IllegalStateException e) {
            return ServiceUtil.returnError("Could not write the outbox event of the stock change: " + e.getMessage());
        }
        return ServiceUtil.returnSuccess();
    }

    @SuppressWarnings("unchecked")
    private static List<String> idList(Object v) {
        List<String> out = new ArrayList<>();
        if (v instanceof Collection) {
            for (Object o : (Collection<Object>) v) {
                if (o != null && UtilValidate.isNotEmpty(o.toString())) {
                    out.add(o.toString());
                }
            }
        } else if (v instanceof String && UtilValidate.isNotEmpty((String) v)) {
            for (String part : ((String) v).split("[,\\s]+")) {
                if (!part.isEmpty()) {
                    out.add(part);
                }
            }
        }
        return out;
    }

    public static Map<String, Object> claimOutboxEvents(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> err = denied(dctx, context);
        if (err != null) {
            return err;
        }
        String consumerId = (String) context.get("consumerId");
        int limit = context.get("limit") == null ? 50 : Math.max(1, Math.min(500, (Integer) context.get("limit")));
        int lease = context.get("leaseSeconds") == null ? 60 : Math.max(5, Math.min(3600, (Integer) context.get("leaseSeconds")));
        Set<String> types = new HashSet<>(idList(context.get("eventTypes")));
        List<Map<String, Object>> out = new ArrayList<>();
        try {
            for (OutboxEvent e : outbox(dctx).claim(consumerId, limit, Duration.ofSeconds(lease), types)) {
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("eventId", e.eventId);
                row.put("eventType", e.eventType);
                row.put("payloadJson", e.payloadJson);
                row.put("createdDate", e.createdDate.toString());
                row.put("attempts", e.attempts);
                row.put("leaseUntil", e.leaseUntil.toString());
                out.add(row);
            }
        } catch (IllegalArgumentException e) {
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("events", out);
        return result;
    }

    public static Map<String, Object> ackOutboxEvents(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> err = denied(dctx, context);
        if (err != null) {
            return err;
        }
        try {
            return outcome(outbox(dctx).ack((String) context.get("consumerId"), idList(context.get("eventIds"))));
        } catch (IllegalArgumentException e) {
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    public static Map<String, Object> releaseOutboxEvents(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> err = denied(dctx, context);
        if (err != null) {
            return err;
        }
        Integer after = (Integer) context.get("retryAfterSeconds");
        try {
            return outcome(outbox(dctx).release((String) context.get("consumerId"), idList(context.get("eventIds")),
                    (String) context.get("error"), after == null ? null : Duration.ofSeconds(after)));
        } catch (IllegalArgumentException e) {
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    private static Map<String, Object> outcome(EventOutbox.Outcome o) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("done", o.done);
        result.put("unknown", o.unknown);
        result.put("notOwner", o.notOwner);
        return result;
    }

    /** Attempts after which an event is parked: property outbox.maxAttempts of channel-core.properties, default 10. */
    static int outboxMaxAttempts() {
        int n = UtilProperties.getPropertyAsInteger("channel-core", "outbox.maxAttempts", EventOutbox.DEFAULT_MAX_ATTEMPTS);
        return n < 1 ? EventOutbox.DEFAULT_MAX_ATTEMPTS : n;
    }

    /** Desk: lists the parked events (attempts reached outbox.maxAttempts). */
    public static Map<String, Object> listParkedOutboxEvents(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> err = denied(dctx, context);
        if (err != null) {
            return err;
        }
        int limit = context.get("limit") == null ? 50 : Math.max(1, Math.min(500, (Integer) context.get("limit")));
        List<Map<String, Object>> out = new ArrayList<>();
        for (OutboxEvent e : outbox(dctx).parked(limit)) {
            Map<String, Object> row = new LinkedHashMap<>();
            row.put("eventId", e.eventId);
            row.put("eventType", e.eventType);
            row.put("payloadJson", e.payloadJson);
            row.put("createdDate", e.createdDate.toString());
            row.put("attempts", e.attempts);
            row.put("parkedDate", e.parkedDate.toString());
            row.put("lastError", e.lastError);
            out.add(row);
        }
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("events", out);
        return result;
    }

    /** Retention of done events in days: property outbox.retentionDays of channel-core.properties, default 7. */
    static int outboxRetentionDays() {
        int days = UtilProperties.getPropertyAsInteger("channel-core", "outbox.retentionDays", EventOutbox.DEFAULT_RETENTION_DAYS);
        return days < 0 ? EventOutbox.DEFAULT_RETENTION_DAYS : days;
    }

    /** Deletes the done events older than the retention. Runs each day as a scheduled job. Open events stay. */
    public static Map<String, Object> purgeOutbox(DispatchContext dctx, Map<String, ? extends Object> context) {
        GenericValue caller = (GenericValue) context.get("userLogin");
        if (caller == null || !"system".equals(caller.getString("userLoginId"))) {
            Map<String, Object> err = denied(dctx, context); // the scheduled job CHNL_OUTBOX_PURGE runs as the system user
            if (err != null) {
                return err;
            }
        }
        Integer override = (Integer) context.get("retentionDays");
        int days = override != null && override >= 0 ? override : outboxRetentionDays();
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("purged", outbox(dctx).purge(Duration.ofDays(days)));
        return result;
    }
}
