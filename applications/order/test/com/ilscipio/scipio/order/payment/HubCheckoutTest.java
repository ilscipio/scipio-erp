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
package com.ilscipio.scipio.order.payment;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.order.mcp.OrderMcp;
import com.ilscipio.scipio.service.def.Service;

/** The checkout token and the payment records of the card payment through the hub (W1-10d). */
public class HubCheckoutTest {
    private static final String KEY = "ck_test_0123456789abcdefghijklmnopqrstuv";

    private static Map<String, Object> claims(String amount) {
        return HubCheckout.claims("emberandwick", "WS10001", new BigDecimal(amount), "usd", "https://emberandwick.shop.scipioerp.com",
                "jane@example.com", 1_790_000_000L);
    }

    @Test
    void aSignedTokenGivesItsClaimsBack() {
        String token = HubCheckout.sign(claims("24.50"), KEY);
        assertTrue(token.startsWith("v1."), token);
        Map<String, Object> c = HubCheckout.verify(token, KEY);
        assertEquals("emberandwick", c.get("store"));
        assertEquals("WS10001", c.get("orderId"));
        assertEquals("24.5", c.get("amount"));
        assertEquals("USD", c.get("currency"));
        assertEquals("https://emberandwick.shop.scipioerp.com", c.get("origin"));
        assertEquals(1_790_000_000L + HubCheckout.TTL_SECONDS, ((Number) c.get("exp")).longValue());
    }

    @Test
    void aChangedAmountFailsTheSignature() {
        String token = HubCheckout.sign(claims("24.50"), KEY);
        String[] parts = token.split("\\.");
        String json = new String(Base64.getUrlDecoder().decode(parts[1]), StandardCharsets.UTF_8).replace("\"24.5\"", "\"0.5\"");
        String tampered = parts[0] + "." + Base64.getUrlEncoder().withoutPadding().encodeToString(json.getBytes(StandardCharsets.UTF_8))
                + "." + parts[2];
        IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> HubCheckout.verify(tampered, KEY));
        assertEquals("bad_signature", e.getMessage());
    }

    @Test
    void theKeyOfAnotherStoreFails() {
        String token = HubCheckout.sign(claims("24.50"), KEY);
        assertEquals("bad_signature", assertThrows(IllegalArgumentException.class,
                () -> HubCheckout.verify(token, "ck_test_other_store_key_000000000000")).getMessage());
        assertEquals("malformed", assertThrows(IllegalArgumentException.class, () -> HubCheckout.verify("v2.a.b", KEY)).getMessage());
        assertEquals("malformed", assertThrows(IllegalArgumentException.class, () -> HubCheckout.verify(null, KEY)).getMessage());
        assertThrows(IllegalArgumentException.class, () -> HubCheckout.sign(claims("1"), ""));
    }

    @Test
    void thePaymentMustPayTheOrderInItsCurrency() {
        assertNull(HubPaymentServices.checkPayment(new BigDecimal("24.50"), "USD", new BigDecimal("24.50"), "usd"));
        assertEquals(HubPaymentServices.ERR_SHORT, HubPaymentServices.checkPayment(new BigDecimal("24.50"), "USD", new BigDecimal("24.49"), "USD"));
        assertEquals(HubPaymentServices.ERR_CURRENCY, HubPaymentServices.checkPayment(new BigDecimal("24.50"), "USD", new BigDecimal("24.50"), "EUR"));
        assertNull(HubPaymentServices.checkRefund(new BigDecimal("24.50"), new BigDecimal("10"), new BigDecimal("14.50")));
        assertEquals(HubPaymentServices.ERR_REFUND_TOO_LARGE,
                HubPaymentServices.checkRefund(new BigDecimal("24.50"), new BigDecimal("10"), new BigDecimal("14.51")));
        assertEquals(HubPaymentServices.ERR_NOT_PAID, HubPaymentServices.checkRefund(BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ONE));
    }

    @Test
    void theThreeActionsAreWiredToTheirServices() {
        Map<String, String> want = new HashMap<>();
        want.put("payment_hub_record", "recordHubPayment");
        want.put("payment_hub_info", "getHubPaymentInfo");
        want.put("payment_hub_refund_record", "recordHubPaymentRefund");
        Set<String> found = new HashSet<>();
        for (McpServiceTool t : OrderMcp.class.getAnnotation(McpServer.class).serviceTools()) {
            if (want.containsKey(t.name())) {
                assertEquals("order", t.topic());
                assertEquals(want.get(t.name()), t.service());
                assertEquals("payment_hub_info".equals(t.name()), t.readOnly());
                found.add(t.name());
            }
        }
        assertEquals(want.keySet(), found);
        Set<String> defs = new HashSet<>();
        for (Class<?> c : HubPaymentServices.class.getDeclaredClasses()) {
            Service s = c.getAnnotation(Service.class);
            if (s != null) {
                defs.add(s.name());
                assertEquals("true", s.auth());
            }
        }
        assertTrue(defs.containsAll(want.values()), defs.toString());
    }

    @Test
    void aCallerWithoutThePermissionRecordsNothing() {
        DispatchContext dctx = mock(DispatchContext.class);
        Security security = mock(Security.class);
        when(dctx.getSecurity()).thenReturn(security);
        when(security.hasEntityPermission(anyString(), anyString(), any(GenericValue.class))).thenReturn(false);
        Map<String, Object> ctx = new HashMap<>();
        ctx.put("userLogin", mock(GenericValue.class));
        ctx.put("orderId", "WS10001");
        ctx.put("externalPaymentId", "pi_1");
        ctx.put("amount", "24.50");
        ctx.put("currencyUomId", "USD");
        ctx.put("paymentStatus", "succeeded");
        assertTrue(ServiceUtil.isError(HubPaymentServices.recordHubPayment(dctx, ctx)));
        assertTrue(ServiceUtil.isError(HubPaymentServices.recordHubPaymentRefund(dctx, ctx)));
        assertTrue(ServiceUtil.isError(HubPaymentServices.getHubPaymentInfo(dctx, ctx)));
        // the delegator of the mock is null: a service that passed the check would fail with an exception, not an error map
    }

    // ---------------------------------------------------------------- review fixes

    @AfterEach
    void restoreKeyLookup() {
        HubCheckout.keyLookup = d -> org.ofbiz.entity.util.EntityUtilProperties.getPropertyValue(HubCheckout.RESOURCE, "key", d);
    }

    static Map<String, Object> deskCall(boolean deskPermission) {
        DispatchContext dctx = mock(DispatchContext.class);
        Security security = mock(Security.class);
        when(dctx.getSecurity()).thenReturn(security);
        // the owner login has ORDERMGR_UPDATE; only the desk login has HUBPAY_RECORD
        when(security.hasEntityPermission(anyString(), anyString(), any(GenericValue.class))).thenReturn(true);
        when(security.hasPermission(eq(HubCheckout.PERMISSION_RECORD), any(GenericValue.class))).thenReturn(deskPermission);
        Map<String, Object> ctx = new HashMap<>();
        ctx.put("dctx", dctx);
        ctx.put("userLogin", mock(GenericValue.class));
        ctx.put("orderId", "WS10001");
        ctx.put("externalPaymentId", "pi_1");
        ctx.put("amount", "24.50");
        ctx.put("currencyUomId", "USD");
        ctx.put("paymentStatus", "succeeded");
        ctx.put("externalRefundId", "re_1");
        return ctx;
    }

    static Map<String, Object> run(boolean refund, Map<String, Object> ctx) {
        DispatchContext dctx = (DispatchContext) ctx.remove("dctx");
        return refund ? HubPaymentServices.recordHubPaymentRefund(dctx, ctx) : HubPaymentServices.recordHubPayment(dctx, ctx);
    }

    @Test
    void theRecordMacFitsOnlyItsValues() {
        String mac = HubCheckout.recordMac(KEY, HubCheckout.KIND_PAYMENT, "WS10001", "pi_1", "24.50", "usd", null);
        assertTrue(HubCheckout.macMatches(KEY, mac, HubCheckout.KIND_PAYMENT, "WS10001", "pi_1", "24.5", "USD", ""), "same values, other spelling");
        assertFalse(HubCheckout.macMatches(KEY, mac, HubCheckout.KIND_PAYMENT, "WS10001", "pi_1", "2450", "USD", null), "tampered amount");
        assertFalse(HubCheckout.macMatches(KEY, mac, HubCheckout.KIND_PAYMENT, "WS10001", "pi_1", "24.50", "EUR", null), "tampered currency");
        assertFalse(HubCheckout.macMatches(KEY, mac, HubCheckout.KIND_PAYMENT, "WS10002", "pi_1", "24.50", "USD", null), "other order");
        assertFalse(HubCheckout.macMatches(KEY, mac, HubCheckout.KIND_REFUND, "WS10001", "pi_1", "24.50", "USD", null), "other kind");
        assertFalse(HubCheckout.macMatches("ck_other", mac, HubCheckout.KIND_PAYMENT, "WS10001", "pi_1", "24.50", "USD", null), "other key");
        assertFalse(HubCheckout.macMatches(KEY, null, HubCheckout.KIND_PAYMENT, "WS10001", "pi_1", "24.50", "USD", null), "no MAC");
        // the fixed vector: the desk (StoreCheckoutTest) computes the same text
        assertEquals("0B31ErcZ7B-ykxNzeQH3xXMAgA7ZqWZXD-jyT7LetaA", mac);
    }

    @Test
    void aRecordWithATamperedAmountOrWithoutMacIsRefused() {
        HubCheckout.keyLookup = d -> KEY;
        String good = HubCheckout.recordMac(KEY, HubCheckout.KIND_PAYMENT, "WS10001", "pi_1", "24.50", "USD", null);
        Map<String, Object> ctx = deskCall(true);
        Map<String, Object> r = run(false, ctx);
        assertEquals(HubPaymentServices.ERR_MAC_MISSING, r.get("errorCode"), "missing MAC");
        ctx = deskCall(true);
        ctx.put("mac", good);
        ctx.put("amount", "0.50");
        assertEquals(HubPaymentServices.ERR_MAC_BAD, run(false, ctx).get("errorCode"), "tampered amount");
        ctx = deskCall(true);
        ctx.put("mac", HubCheckout.recordMac(KEY, HubCheckout.KIND_REFUND, "WS10001", "pi_1", "24.50", "USD", "re_1"));
        ctx.put("amount", "240.00");
        assertEquals(HubPaymentServices.ERR_MAC_BAD, run(true, ctx).get("errorCode"), "tampered refund amount");
        ctx = deskCall(true);
        assertEquals(HubPaymentServices.ERR_MAC_MISSING, run(true, ctx).get("errorCode"), "refund without MAC");
        // a valid MAC passes the check (the DB part needs a delegator)
        assertNull(HubPaymentServices.checkRecordCall((DispatchContext) deskCall(true).get("dctx"), mock(GenericValue.class), good,
                HubCheckout.KIND_PAYMENT, "WS10001", "pi_1", "24.50", "USD", null));
        HubCheckout.keyLookup = d -> null;
        ctx = deskCall(true);
        ctx.put("mac", good);
        assertEquals(HubPaymentServices.ERR_NO_KEY, run(false, ctx).get("errorCode"));
    }

    @Test
    void onlyTheDeskLoginMayRecordEvenWithOrderManagerRights() {
        HubCheckout.keyLookup = d -> KEY;
        Map<String, Object> ctx = deskCall(false);
        ctx.put("mac", HubCheckout.recordMac(KEY, HubCheckout.KIND_PAYMENT, "WS10001", "pi_1", "24.50", "USD", null));
        assertEquals(HubPaymentServices.ERR_PERMISSION, run(false, ctx).get("errorCode"));
        ctx = deskCall(false);
        assertEquals(HubPaymentServices.ERR_PERMISSION, run(true, ctx).get("errorCode"));
    }

    @Test
    void aPaymentThatTheOrderCannotTakeIsAnOrphan() {
        assertEquals(HubPaymentServices.Outcome.RECORD, HubPaymentServices.outcome("ORDER_CREATED", "PAYMENT_NOT_RECEIVED", null, false, "pi_2"));
        assertEquals(HubPaymentServices.Outcome.ALREADY_RECORDED, HubPaymentServices.outcome("ORDER_APPROVED", "PAYMENT_RECEIVED", "pi_2", false, "pi_2"));
        assertEquals(HubPaymentServices.Outcome.ALREADY_RECORDED, HubPaymentServices.outcome("ORDER_APPROVED", "PAYMENT_RECEIVED", null, true, "pi_2"));
        // paid by another PaymentIntent (a second tab), cancelled, declined (no open preference), no order
        assertEquals(HubPaymentServices.Outcome.ORPHAN, HubPaymentServices.outcome("ORDER_APPROVED", "PAYMENT_RECEIVED", "pi_1", false, "pi_2"));
        assertEquals(HubPaymentServices.Outcome.ORPHAN, HubPaymentServices.outcome("ORDER_CANCELLED", "PAYMENT_NOT_RECEIVED", null, false, "pi_2"));
        assertEquals(HubPaymentServices.Outcome.ORPHAN, HubPaymentServices.outcome("ORDER_CREATED", null, null, false, "pi_2"));
        assertEquals(HubPaymentServices.Outcome.ORPHAN, HubPaymentServices.outcome(null, null, null, false, "pi_2"));
        assertEquals("already_paid", HubPaymentServices.orphanReason("ORDER_APPROVED", "PAYMENT_RECEIVED"));
        assertEquals("order_cancelled", HubPaymentServices.orphanReason("ORDER_CANCELLED", "PAYMENT_NOT_RECEIVED"));
        assertEquals("no_open_card_payment", HubPaymentServices.orphanReason("ORDER_CREATED", null));
        assertEquals("no_order", HubPaymentServices.orphanReason(null, null));
    }

    @Test
    void thePayLinkIsSignedPerOrderAndTheJobCancelsOnlyOpenOrders() {
        String sig = HubCheckout.payLinkSignature(KEY, "WS10001");
        assertNotEquals(sig, HubCheckout.payLinkSignature(KEY, "WS10002"));
        assertNotEquals(sig, HubCheckout.payLinkSignature("ck_other", "WS10001"));
        HubCheckout.keyLookup = d -> KEY;
        javax.servlet.http.HttpServletRequest req = mock(javax.servlet.http.HttpServletRequest.class);
        when(req.getParameter(HubCheckout.PAY_LINK_PARAM)).thenReturn(sig);
        org.ofbiz.entity.Delegator delegator = mock(org.ofbiz.entity.Delegator.class);
        assertTrue(HubCheckout.payLinkValid(req, delegator, "WS10001"));
        assertFalse(HubCheckout.payLinkValid(req, delegator, "WS10002"), "the link of one order opens no other order");
        when(req.getParameter(HubCheckout.PAY_LINK_PARAM)).thenReturn(sig.substring(1));
        assertFalse(HubCheckout.payLinkValid(req, delegator, "WS10001"));
        assertTrue(HubPaymentServices.mayAutoCancel("ORDER_CREATED"));
        assertFalse(HubPaymentServices.mayAutoCancel("ORDER_APPROVED"));
        assertFalse(HubPaymentServices.mayAutoCancel("ORDER_COMPLETED"));
    }

    @Test
    void theCheckoutKeyRowsNeverLeaveThroughMcp() {
        java.util.List<String> p = java.util.List.of("hubcheckout");
        assertTrue(com.ilscipio.scipio.mcp.catalog.EntityCatalog.isProtectedRow("SystemProperty", "hubcheckout", p));
        assertFalse(com.ilscipio.scipio.mcp.catalog.EntityCatalog.isProtectedRow("SystemProperty", "general", p));
        assertFalse(com.ilscipio.scipio.mcp.catalog.EntityCatalog.isProtectedRow("Product", "hubcheckout", p));
    }
}
