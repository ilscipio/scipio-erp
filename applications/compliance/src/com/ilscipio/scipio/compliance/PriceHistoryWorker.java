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
package com.ilscipio.scipio.compliance;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityQuery;

/**
 * Price history for the EU prior-price rule (Price Indication Directive 98/6/EC Art. 6a): a price reduction
 * must show the lowest price that applied in the 30 days before the reduction.
 *
 * <p>The shop records the shown price of a product in {@code ProductPriceSnapshot} whenever it changes (render
 * time, so rule and promotion prices count too). Without enough history the shop shows no crossed-out price,
 * which is the safe choice.</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public final class PriceHistoryWorker {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final int PRIOR_DAYS = 30;

    /** Last recorded price per product/store/currency, to write only on change. */
    private static final UtilCache<String, BigDecimal> lastPrice = UtilCache.createUtilCache("compliance.lastShownPrice", 20000, 0, 3600000, false);

    private PriceHistoryWorker() {}

    /**
     * Records the shown price if it differs from the last snapshot. Safe to call on every product view: the write runs
     * in its own transaction, so a failed insert never marks the caller's (screen render) transaction rollback-only.
     */
    public static void recordShownPrice(Delegator delegator, String productId, String productStoreId, String currencyUomId, BigDecimal price) {
        if (UtilValidate.isEmpty(productId) || UtilValidate.isEmpty(productStoreId) || UtilValidate.isEmpty(currencyUomId) || price == null) {
            return;
        }
        // the price field is currency-amount (2 decimals): compare what the database keeps, e.g. 39.192 is stored as 39.19
        BigDecimal stored = price.setScale(2, RoundingMode.HALF_UP);
        String key = delegator.getDelegatorName() + "::" + productId + "::" + productStoreId + "::" + currencyUomId;
        BigDecimal cached = lastPrice.get(key);
        if (cached != null && cached.compareTo(stored) == 0) {
            return;
        }
        try {
            TransactionUtil.doNewTransaction(() -> {
                GenericValue last = EntityQuery.use(delegator).from("ProductPriceSnapshot")
                        .where("productId", productId, "productStoreId", productStoreId, "currencyUomId", currencyUomId)
                        .orderBy("-snapshotDate").queryFirst();
                Timestamp now = UtilDateTime.nowTimestamp();
                if ((last == null || last.getBigDecimal("price").compareTo(stored) != 0)
                        && (last == null || now.after(last.getTimestamp("snapshotDate")))) {
                    delegator.create("ProductPriceSnapshot", UtilMisc.toMap("productId", productId, "productStoreId", productStoreId,
                            "currencyUomId", currencyUomId, "snapshotDate", now, "price", stored));
                }
                return null;
            }, "Could not record the price snapshot of product " + productId, 0, false);
            lastPrice.put(key, stored);
        } catch (GenericEntityException e) {
            Debug.logWarning("Could not record the price snapshot of product " + productId + ": " + e.getMessage(), module);
        }
    }

    /**
     * The prior price of a reduction: the lowest price in the 30 days before the current price started.
     * Null when the current price is not a reduction or the history is too short.
     */
    public static BigDecimal getPriorLowestPrice(Delegator delegator, String productId, String productStoreId, String currencyUomId, BigDecimal currentPrice) {
        if (currentPrice == null) {
            return null;
        }
        try {
            List<GenericValue> snaps = EntityQuery.use(delegator).from("ProductPriceSnapshot")
                    .where("productId", productId, "productStoreId", productStoreId, "currencyUomId", currencyUomId)
                    .orderBy("-snapshotDate").queryList();
            if (snaps.isEmpty()) {
                return null;
            }
            // start of the current price: the oldest snapshot of the latest run with the current price
            int i = 0;
            Timestamp start = null;
            while (i < snaps.size() && snaps.get(i).getBigDecimal("price").compareTo(currentPrice) == 0) {
                start = snaps.get(i).getTimestamp("snapshotDate");
                i++;
            }
            if (start == null || i >= snaps.size()) {
                return null; // no earlier price known
            }
            Timestamp windowStart = UtilDateTime.adjustTimestamp(start, java.util.Calendar.DAY_OF_YEAR, -PRIOR_DAYS);
            BigDecimal lowest = null;
            for (int j = i; j < snaps.size(); j++) {
                GenericValue s = snaps.get(j);
                BigDecimal p = s.getBigDecimal("price");
                lowest = lowest == null || p.compareTo(lowest) < 0 ? p : lowest;
                if (s.getTimestamp("snapshotDate").before(windowStart)) {
                    break; // this price was in effect at the window start; older ones do not count
                }
            }
            return lowest != null && lowest.compareTo(currentPrice) > 0 ? lowest : null;
        } catch (GenericEntityException e) {
            Debug.logWarning("Could not read the price history of product " + productId + ": " + e.getMessage(), module);
            return null;
        }
    }

    /**
     * What the shop may show as the old price. EU stores: only the 30-day prior price (key rule "prior30").
     * Other stores: the list or default price as before (key rule "list"). Keys: oldPrice, percent, rule; empty map
     * when no old price may be shown.
     */
    public static Map<String, Object> getReferencePrice(Delegator delegator, String productId, String productStoreId, String currencyUomId,
                                                        Object currentPriceObj, Object listPriceObj, boolean recordCurrent) {
        BigDecimal currentPrice = toAmount(currentPriceObj);
        BigDecimal listPrice = toAmount(listPriceObj);
        Map<String, Object> out = new LinkedHashMap<>();
        if (currentPrice == null) {
            return out;
        }
        if (recordCurrent) {
            recordShownPrice(delegator, productId, productStoreId, currencyUomId, currentPrice);
        }
        GenericValue profile = LegalDocumentWorker.getProfile(delegator, productStoreId);
        boolean eu = profile == null || LegalDocumentWorker.hasJurisdiction(profile, "EU");
        BigDecimal old = eu ? getPriorLowestPrice(delegator, productId, productStoreId, currencyUomId, currentPrice)
                            : (listPrice != null && listPrice.compareTo(currentPrice) > 0 ? listPrice : null);
        if (old == null) {
            return out;
        }
        out.put("oldPrice", old);
        out.put("rule", eu ? "prior30" : "list");
        out.put("percent", old.subtract(currentPrice).multiply(BigDecimal.valueOf(100)).divide(old, 0, java.math.RoundingMode.DOWN).intValue());
        return out;
    }

    /** A positive amount from a number or text; null for anything else (FreeMarker cannot pass null). */
    static BigDecimal toAmount(Object o) {
        try {
            BigDecimal v = o instanceof BigDecimal ? (BigDecimal) o : (o instanceof Number ? new BigDecimal(o.toString())
                    : (o != null && !o.toString().trim().isEmpty() ? new BigDecimal(o.toString().trim()) : null));
            return v != null && v.signum() > 0 ? v : null;
        } catch (NumberFormatException e) {
            return null;
        }
    }

    /** Snapshots older than the window plus a margin are not needed (keep the last one before the cut). */
    public static int purgeOldSnapshots(Delegator delegator, int keepDays) throws GenericEntityException {
        Timestamp cut = UtilDateTime.adjustTimestamp(UtilDateTime.nowTimestamp(), java.util.Calendar.DAY_OF_YEAR, -keepDays);
        return delegator.removeByCondition("ProductPriceSnapshot", EntityCondition.makeCondition("snapshotDate", EntityOperator.LESS_THAN, cut));
    }
}
