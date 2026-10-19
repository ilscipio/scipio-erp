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
package com.ilscipio.scipio.acp.payment;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.UtilValidate;

import com.ilscipio.scipio.acp.AcpConfig;
import com.ilscipio.scipio.acp.checkout.AcpContext;
import com.ilscipio.scipio.order.payment.HubCheckout;

/**
 * SCIPIO: 4.0.0: Charges the Shared Payment Token of an ACP checkout. Two handlers: the Stripe account of the store
 * ({@link StripeSptHandler}) and the Scipio hub ({@link HubSptHandler}, EXT_STRIPE_HUB).
 */
public interface AcpPaymentHandler {

    /** The handler id of the checkout session (capabilities.payment.handlers[].id). */
    String HANDLER_ID = "card_tokenized";

    String name();

    /** The payment method type of the order payment preference. */
    String paymentMethodTypeId();

    /** True when this handler records the payment on the order itself; false when the hub records it later. */
    boolean recordsPayment();

    /** The config object of the handler for the agent. */
    Map<String, Object> config(AcpContext ctx);

    ChargeResult charge(AcpContext ctx, ChargeRequest request);

    /**
     * The outcome of an earlier charge of the order when the answer was lost: SUCCEEDED, DECLINED (no payment), or
     * null when it is not known yet. Default: the order payment preference shows a received payment.
     */
    default ChargeResult lookup(AcpContext ctx, String orderId) {
        try {
            org.ofbiz.entity.GenericValue pref = org.ofbiz.entity.util.EntityQuery.use(ctx.getDelegator()).from("OrderPaymentPreference")
                    .where("orderId", orderId, "paymentMethodTypeId", paymentMethodTypeId()).queryFirst();
            if (pref != null && ("PAYMENT_RECEIVED".equals(pref.getString("statusId")) || "PAYMENT_SETTLED".equals(pref.getString("statusId")))) {
                return ChargeResult.of(ChargeResult.Status.SUCCEEDED, pref.getString("manualRefNum"), "succeeded", null);
            }
        } catch (org.ofbiz.entity.GenericEntityException e) {
            return null;
        }
        return null;
    }

    final class ChargeRequest {
        public String checkoutSessionId;
        public String orderId;
        /** The same for every retry of one payment attempt (session, attempt, token hash). */
        public String idempotencyKey;
        public long amountMinor;
        public String currency;
        public String sharedPaymentToken;
        public String buyerEmail;
    }

    final class ChargeResult {
        public enum Status { SUCCEEDED, DECLINED, ACTION_REQUIRED, ERROR }

        public Status status;
        public String reference;
        public String code;
        public String message;

        public static ChargeResult of(Status status, String reference, String code, String message) {
            ChargeResult r = new ChargeResult();
            r.status = status;
            r.reference = reference;
            r.code = code;
            r.message = message;
            return r;
        }
    }

    /**
     * The handler of the store: payment.handler = stripe | hub | auto. Auto picks the hub when hubcheckout.properties
     * has storeId, url and key, else Stripe when a secret key is set. Null when nothing is configured.
     */
    static AcpPaymentHandler forStore(AcpContext ctx) {
        String mode = AcpConfig.get(ctx.getDelegator(), ctx.getProductStoreId(), "payment.handler", "auto").trim();
        boolean hub = HubSptHandler.isConfigured(ctx);
        boolean stripe = StripeSptHandler.isConfigured(ctx);
        if ("hub".equalsIgnoreCase(mode)) return hub ? new HubSptHandler() : null;
        if ("stripe".equalsIgnoreCase(mode)) return stripe ? new StripeSptHandler() : null;
        if (hub) return new HubSptHandler();
        if (stripe) return new StripeSptHandler();
        return null;
    }

    /** The handler entry of capabilities.payment.handlers (dev.acp.tokenized.card with psp stripe). */
    static Map<String, Object> describe(AcpContext ctx, AcpPaymentHandler handler) {
        Map<String, Object> h = new LinkedHashMap<>();
        h.put("id", HANDLER_ID);
        h.put("name", "dev.acp.tokenized.card");
        h.put("display_name", "Credit Card");
        h.put("version", "2026-01-22");
        h.put("spec", "https://acp.dev/handlers/tokenized.card");
        h.put("requires_delegate_payment", Boolean.TRUE);
        h.put("requires_pci_compliance", Boolean.FALSE);
        h.put("psp", "stripe");
        h.put("config_schema", "https://acp.dev/schemas/handlers/tokenized.card/config.json");
        h.put("instrument_schemas", List.of("https://acp.dev/schemas/handlers/tokenized.card/instrument.json"));
        Map<String, Object> config = new LinkedHashMap<>(handler.config(ctx));
        config.putIfAbsent("psp", "stripe");
        config.putIfAbsent("accepted_brands", List.of("visa", "mastercard", "amex", "discover"));
        config.putIfAbsent("accepted_funding_types", List.of("credit", "debit", "prepaid"));
        config.putIfAbsent("supports_3ds", Boolean.FALSE);
        h.put("config", config);
        h.put("display_order", 0);
        return h;
    }

    static String nz(String s) {
        return UtilValidate.isEmpty(s) ? "" : s;
    }

    /** The hub settings of hubcheckout.properties are complete. */
    static boolean hubSettingsPresent(AcpContext ctx) {
        return UtilValidate.isNotEmpty(HubCheckout.checkoutKey(ctx.getDelegator()))
                && UtilValidate.isNotEmpty(org.ofbiz.entity.util.EntityUtilProperties.getPropertyValue(HubCheckout.RESOURCE, "storeId", ctx.getDelegator()))
                && UtilValidate.isNotEmpty(org.ofbiz.entity.util.EntityUtilProperties.getPropertyValue(HubCheckout.RESOURCE, "url", ctx.getDelegator()));
    }
}
