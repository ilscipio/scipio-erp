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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/** The four decisions of the W1-08 row: variation listings, buyer-data retention, tax and currency, one account per marketplace. */
public class ChannelDecisionsTest {
    private Fixture f;

    @BeforeEach
    void setUp() {
        f = new Fixture();
    }

    // ---- decision 1: variation listings ----

    private static Map<String, String> axes(String size, String color) {
        Map<String, String> m = new LinkedHashMap<>();
        m.put("Size", size);
        m.put("Color", color);
        return m;
    }

    @Test
    void aVariationProductIsOneListingForEachVariantSku() {
        List<VariationPlanner.Variant> variants = Arrays.asList(
                new VariationPlanner.Variant("TEE-S-RED", "TEE-S-RED", axes("S", "Red")),
                new VariationPlanner.Variant("TEE-M-RED", "TEE-M-RED", axes("M", "Red")),
                new VariationPlanner.Variant("TEE-M-BLUE", "TEE-M-BLUE", axes("M", "Blue")));
        List<Listing> plan = VariationPlanner.plan("TEE", variants, "ebay-de", f.store);
        assertEquals(3, plan.size());
        for (Listing l : plan) {
            assertEquals("TEE", l.variationGroupId);
            assertEquals("ebay-de", l.channelId);
            assertNotEquals("TEE", l.productId, "the virtual parent has no listing");
        }
        Map<String, String> attrs = VariationPlanner.viewAttributes(plan.get(1));
        assertEquals("TEE", attrs.get("variation.group"));
        assertEquals("M", attrs.get("variation.axis.Size"));
        assertEquals("Red", attrs.get("variation.axis.Color"));
    }

    @Test
    void aVariantListingKeepsItsOwnStockAndState() {
        f.live("TEE-S", "ebay-de", "DE-S", 5);
        f.live("TEE-M", "ebay-de", "DE-M", 5);
        f.atp.put("TEE-S", 0);
        f.atp.put("TEE-M", 9);
        f.sync.stockChanged("TEE-S");
        f.sync.stockChanged("TEE-M");
        f.runUntil(Duration.ofSeconds(60));
        assertEquals(0, f.channel.quantityOf("ebay-de", "DE-S"));
        assertEquals(9, f.channel.quantityOf("ebay-de", "DE-M"));
    }

    @Test
    void aGroupWithMixedAxesOrTwinVariantsIsRefused() {
        assertThrows(IllegalArgumentException.class, () -> VariationPlanner.plan("TEE", Arrays.asList(
                new VariationPlanner.Variant("A", "A", axes("S", "Red")),
                new VariationPlanner.Variant("B", "B", Collections.singletonMap("Size", "M"))), "ebay-de", f.store));
        assertThrows(IllegalArgumentException.class, () -> VariationPlanner.plan("TEE", Arrays.asList(
                new VariationPlanner.Variant("A", "A", axes("S", "Red")),
                new VariationPlanner.Variant("B", "B", axes("S", "Red"))), "ebay-de", f.store));
        assertThrows(IllegalArgumentException.class, () -> VariationPlanner.plan("TEE", Arrays.asList(
                new VariationPlanner.Variant("A", "SAME", axes("S", "Red")),
                new VariationPlanner.Variant("B", "SAME", axes("M", "Red"))), "ebay-de", f.store));
    }

    @Test
    void flatJsonRoundTrip() {
        Map<String, String> m = new LinkedHashMap<>();
        m.put("Größe", "16 \"oz\"");
        m.put("a\\b", "x,y:z}");
        assertEquals(m, FlatJson.read(FlatJson.write(m)));
        assertTrue(FlatJson.read(null).isEmpty());
        assertTrue(FlatJson.read("{}").isEmpty());
    }

    // ---- decision 2: buyer-data retention ----

    private OrderIntake intake(final List<String> created) {
        f.store.productBySku.put("SKU-1", "P1");
        f.atp.put("P1", 40);
        f.live("P1", "amazon-de", "AZ-1", 35);
        f.live("P1", "ebay-de", "DE-1", 40);
        return new OrderIntake(f.store, (s, o, lines) -> {
            String id = "ORD-" + (created.size() + 1);
            created.add(id);
            return id;
        }, f.sync);
    }

    @Test
    void amazonBuyerDataGoesThirtyDaysAfterTheOrderIsClosed() {
        List<String> erased = new ArrayList<>();
        OrderIntake intake = intake(new ArrayList<String>());
        RetentionRun run = new RetentionRun(f.store, erased::add, f.clock);

        assertEquals(OrderIntake.Status.CREATED, intake.intake("amazon-de", SampleOrders.paid("AMZ-1", "EUR", "SKU-1", 1)).status);
        OrderRef ref = f.store.orderRef("amazon-de", "AMZ-1").get();
        assertNull(ref.buyerDataDueDate, "an open order has no due date");

        run.orderClosed(ref.orderId); // shipped at T0
        ref = f.store.orderRef("amazon-de", "AMZ-1").get();
        assertEquals(f.clock.instant().plus(Duration.ofDays(30)), ref.buyerDataDueDate);

        f.clock.advance(Duration.ofDays(29));
        assertEquals(0, run.run().erased);
        f.clock.advance(Duration.ofDays(1));
        assertEquals(1, run.run().erased);
        assertEquals(Collections.singletonList(ref.orderId), erased);
        assertEquals(0, run.run().erased, "the run erases an order once");
        assertTrue(f.store.orderRef("amazon-de", "AMZ-1").get().buyerDataErasedDate != null);
    }

    @Test
    void aShippedOrderThatComesInClosedStartsTheClockAtOnce() {
        OrderIntake intake = intake(new ArrayList<String>());
        Instant shipped = f.clock.instant();
        intake.intake("amazon-de", SampleOrders.order("AMZ-2", "SHIPPED", "EUR", "SKU-1", 1, "B"));
        assertEquals(shipped.plus(Duration.ofDays(30)), f.store.orderRef("amazon-de", "AMZ-2").get().buyerDataDueDate);
    }

    @Test
    void aChannelWithoutALimitKeepsTheBuyerDataUntilTheChannelSaysDelete() {
        List<String> erased = new ArrayList<>();
        OrderIntake intake = intake(new ArrayList<String>());
        RetentionRun run = new RetentionRun(f.store, erased::add, f.clock);
        intake.intake("ebay-de", SampleOrders.order("EB-1", "SHIPPED", "EUR", "SKU-1", 1, "ebay-user-7"));
        intake.intake("amazon-de", SampleOrders.order("AMZ-3", "PAID", "EUR", "SKU-1", 1, "ebay-user-7"));
        f.clock.advance(Duration.ofDays(3650));
        assertEquals(0, run.run().erased, "eBay has no day limit; the Amazon order is still open");

        // the eBay account-deletion notice: only the eBay order of that buyer goes
        RetentionRun.Result r = run.buyerDeletion("ebay", "ebay-user-7");
        assertEquals(1, r.erased);
        assertEquals(Collections.singletonList(f.store.orderRef("ebay-de", "EB-1").get().orderId), erased);
        assertNull(f.store.orderRef("ebay-de", "EB-1").get().buyerExternalId, "the buyer id goes with the data");
    }

    @Test
    void aFailedEraseStaysDueAndIsReported() {
        OrderIntake intake = intake(new ArrayList<String>());
        RetentionRun run = new RetentionRun(f.store, id -> {
            throw new IllegalStateException("boom");
        }, f.clock);
        intake.intake("amazon-de", SampleOrders.order("AMZ-4", "SHIPPED", "EUR", "SKU-1", 1, "B"));
        f.clock.advance(Duration.ofDays(31));
        RetentionRun.Result r = run.run();
        assertEquals(0, r.erased);
        assertEquals(1, r.failedOrderIds.size());
        assertNull(f.store.orderRef("amazon-de", "AMZ-4").get().buyerDataErasedDate);
    }

    @Test
    void marketplaceDefaults() {
        MarketplaceDefaults amazon = MarketplaceDefaults.of("amazon", "DE");
        assertEquals(Integer.valueOf(30), amazon.retentionDays);
        assertEquals(ChannelSetting.RetentionFrom.CLOSED, amazon.retentionFrom);
        assertNull(MarketplaceDefaults.of("ebay", "us").retentionDays);
        assertThrows(IllegalArgumentException.class, () -> MarketplaceDefaults.of("ebay", "zz"));
    }

    // ---- decision 3: tax-inclusive prices and a currency per channel ----

    @Test
    void currencyAndTaxRuleFollowTheMarketplace() {
        assertEquals("USD", MarketplaceDefaults.of("ebay", "us").currencyUomId);
        assertFalse(MarketplaceDefaults.of("ebay", "us").pricesIncludeTax);
        assertEquals("EUR", MarketplaceDefaults.of("ebay", "de").currencyUomId);
        assertTrue(MarketplaceDefaults.of("ebay", "de").pricesIncludeTax);
        assertEquals("GBP", MarketplaceDefaults.of("amazon", "gb").currencyUomId);
        assertEquals("AUD", MarketplaceDefaults.of("ebay", "au").currencyUomId);
        assertEquals("HKD", MarketplaceDefaults.of("ebay", "hk").currencyUomId);
    }

    @Test
    void thePriceMustMatchCurrencyAndTaxRuleAndIsNeverConverted() {
        ChannelSetting de = f.store.setting("ebay-de").get();
        ChannelSetting us = f.store.setting("ebay-us").get();
        List<PricePolicy.PriceRow> rows = Arrays.asList(
                new PricePolicy.PriceRow("USD", new BigDecimal("10.00"), false),
                new PricePolicy.PriceRow("EUR", new BigDecimal("11.90"), true));

        PricePolicy.Result r = PricePolicy.resolve(de, rows);
        assertTrue(r.isOk());
        assertEquals(new BigDecimal("11.90"), r.price);
        assertEquals("EUR", r.currencyUomId);
        assertEquals(new BigDecimal("10.00"), PricePolicy.resolve(us, rows).price);

        // a net price for a gross channel is an error with a fix hint, not a silent conversion
        PricePolicy.Result mismatch = PricePolicy.resolve(de, Collections.singletonList(
                new PricePolicy.PriceRow("EUR", new BigDecimal("10.00"), false)));
        assertFalse(mismatch.isOk());
        assertEquals("PRICE_TAX_MISMATCH", mismatch.errorCode);
        assertTrue(mismatch.fixHint.contains("with tax"));

        PricePolicy.Result none = PricePolicy.resolve(de, Collections.singletonList(
                new PricePolicy.PriceRow("USD", new BigDecimal("10.00"), false)));
        assertEquals("NO_PRICE", none.errorCode);
        assertTrue(none.fixHint.contains("EUR"));
    }

    @Test
    void anOrderInAnotherCurrencyIsRefused() {
        OrderIntake intake = intake(new ArrayList<String>());
        OrderIntake.Result r = intake.intake("ebay-de", SampleOrders.paid("EB-9", "USD", "SKU-1", 1));
        assertEquals(OrderIntake.Status.REJECTED, r.status);
        assertTrue(r.message.contains("EUR"));
    }

    // ---- decision 4: one channel account per marketplace ----

    @Test
    void eachMarketplaceIsOwnChannelWithOwnAccountStoreAndRules() {
        ChannelSetting us = f.store.setting("ebay-us").get();
        ChannelSetting de = f.store.setting("ebay-de").get();
        assertEquals("ebay", us.connectorId);
        assertEquals("ebay", de.connectorId);
        assertNotEquals(us.channelId, de.channelId);
        assertNotEquals(us.accountId, de.accountId);
        assertNotEquals(us.productStoreId, de.productStoreId);
        assertNotEquals(us.currencyUomId, de.currencyUomId);
        assertEquals("ebay-us", ChannelSetting.channelId("eBay", " US "));
    }

    @Test
    void aProductHasOneListingForEachMarketplace() {
        f.live("P1", "ebay-us", "US-1", null);
        f.live("P1", "ebay-de", "DE-1", null);
        assertEquals(2, f.store.listingsOfProduct("P1").size());
        f.atp.put("P1", 8);
        f.sync.stockChanged("P1");
        f.runUntil(Duration.ofSeconds(60));
        assertEquals(6, f.channel.quantityOf("ebay-us", "US-1"));
        assertEquals(8, f.channel.quantityOf("ebay-de", "DE-1"));
    }

    // ---- order intake ----

    @Test
    void intakeIsIdempotentAndHandlesPendingAndUnknownSkus() {
        List<String> created = new ArrayList<>();
        OrderIntake intake = intake(created);
        IncomingOrder o = SampleOrders.paid("EB-5", "EUR", "SKU-1", 1);
        assertEquals(OrderIntake.Status.CREATED, intake.intake("ebay-de", o).status);
        OrderIntake.Result again = intake.intake("ebay-de", o);
        assertEquals(OrderIntake.Status.DUPLICATE, again.status);
        assertEquals("ORD-1", again.orderId);
        assertEquals(1, created.size());

        assertEquals(OrderIntake.Status.WAITING, intake.intake("ebay-de",
                SampleOrders.order("EB-6", "PENDING_PAYMENT", "EUR", "SKU-1", 1, "B")).status);
        assertEquals(OrderIntake.Status.IGNORED, intake.intake("ebay-de",
                SampleOrders.order("EB-7", "CANCELLED", "EUR", "SKU-1", 1, "B")).status);
        OrderIntake.Result unknown = intake.intake("ebay-de", SampleOrders.paid("EB-8", "EUR", "NOPE", 1));
        assertEquals(OrderIntake.Status.UNMAPPED, unknown.status);
        assertEquals(Collections.singletonList("NOPE"), unknown.unmappedSkus);
        assertEquals(1, created.size(), "only the first order made a store order");
        assertEquals(OrderIntake.Status.REJECTED, intake.intake("nope-xx", o).status);
    }

    @Test
    void anOrderLineFindsItsProductByTheExternalListingId() {
        List<String> created = new ArrayList<>();
        OrderIntake intake = intake(created);
        IncomingOrder base = SampleOrders.paid("EB-10", "EUR", "OTHER-SKU", 1);
        IncomingOrder.Line line = new IncomingOrder.Line("L1", null, "DE-1", "A product", 1, new BigDecimal("10"), null);
        IncomingOrder o = new IncomingOrder("EB-10", "PAID", base.placedAt, null, "B", "N", null, null,
                Collections.singletonList(line), "EUR", null, null, new BigDecimal("10"), null, false, null);
        assertEquals(OrderIntake.Status.CREATED, intake.intake("ebay-de", o).status);
    }

    @Test
    void anIncomingOrderReadsFromTheLooseMapOfTheMcpCall() {
        Map<String, Object> line = new LinkedHashMap<>();
        line.put("externalLineId", "L1");
        line.put("sku", "SKU-1");
        line.put("title", "A product");
        line.put("quantity", 2);
        line.put("unitPrice", "10.00");
        line.put("tax", 1.6);
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("externalOrderId", "X-1");
        m.put("status", "PAID");
        m.put("placedAt", "2026-10-01T10:00:00Z");
        m.put("currency", "EUR");
        m.put("total", "24.90");
        m.put("taxCollectedByChannel", true);
        m.put("lines", Collections.singletonList(line));
        IncomingOrder o = IncomingOrder.fromMap(m);
        assertEquals("X-1", o.externalOrderId);
        assertEquals(2, o.lines.get(0).quantity);
        assertEquals(new BigDecimal("1.6"), o.lines.get(0).tax);
        assertTrue(o.taxCollectedByChannel);
        assertEquals(Instant.parse("2026-10-01T10:00:00Z"), o.updatedAt);
    }

    @Test
    void syncTaskBackoff() {
        assertEquals(Duration.ofSeconds(2), SyncTask.backoff(1));
        assertEquals(Duration.ofSeconds(16), SyncTask.backoff(4));
        assertEquals(Duration.ofSeconds(30), SyncTask.backoff(5));
        assertEquals(Duration.ofSeconds(30), SyncTask.backoff(50));
    }
}
