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
package com.ilscipio.scipio.acp.service;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.acp.AcpJson;
import com.ilscipio.scipio.acp.checkout.AcpContext;
import com.ilscipio.scipio.acp.feed.AcpFeed;
import com.ilscipio.scipio.acp.web.AcpIdempotency;
import com.ilscipio.scipio.acp.webhook.AcpWebhooks;
import com.ilscipio.scipio.service.def.Attribute;
import com.ilscipio.scipio.service.def.Service;
import com.ilscipio.scipio.service.def.seca.Seca;
import com.ilscipio.scipio.service.def.seca.SecaAction;

/**
 * SCIPIO: 4.0.0: ACP services: the jobs (session expiry, webhook delivery, feed) and the order hooks.
 */
public final class AcpServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final String IMPL = "com.ilscipio.scipio.acp.service.AcpServices";

    private AcpServices() {}

    @Service(
        name = "acpExpireSessions",
        engine = "java",
        location = IMPL,
        invoke = "expireSessions",
        description = "Marks open ACP checkout sessions past expiresDate as expired and removes old idempotency keys (hourly job).",
        auth = "true",
        attributes = {
            @Attribute(name = "expired", type = "Integer", mode = "OUT", optional = "true")
        }
    )
    public interface AcpExpireSessions {}

    public static Map<String, Object> expireSessions(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        try {
            int n = com.ilscipio.scipio.acp.checkout.AcpCheckout.expireSessions(delegator, dctx.getDispatcher());
            AcpIdempotency.purge(delegator);
            Map<String, Object> out = ServiceUtil.returnSuccess();
            out.put("expired", n);
            return out;
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError("acpExpireSessions: " + e.getMessage());
        }
    }

    @Service(
        name = "acpDeliverWebhooks",
        engine = "java",
        location = IMPL,
        invoke = "deliverWebhooks",
        description = "Sends due ACP order webhooks (job every 5 minutes; also started after an order event).",
        auth = "true",
        attributes = {
            @Attribute(name = "sent", type = "Integer", mode = "OUT", optional = "true")
        }
    )
    public interface AcpDeliverWebhooks {}

    public static Map<String, Object> deliverWebhooks(DispatchContext dctx, Map<String, ? extends Object> context) {
        try {
            Map<String, Object> out = ServiceUtil.returnSuccess();
            out.put("sent", AcpWebhooks.deliver(dctx.getDelegator(), 200));
            return out;
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError("acpDeliverWebhooks: " + e.getMessage());
        }
    }

    @Service(
        name = "acpGenerateFeeds",
        engine = "java",
        location = IMPL,
        invoke = "generateFeeds",
        description = "Rebuilds the ACP product feed of every store whose feed an agent has read (daily job). "
                + "Pass productStoreId and baseUrl to build one store on demand.",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "baseUrl", type = "String", mode = "IN", optional = "true", description = "The secure address of the store webapp, e.g. https://shop.example.com/shop"),
            @Attribute(name = "products", type = "Integer", mode = "OUT", optional = "true")
        }
    )
    public interface AcpGenerateFeeds {}

    public static Map<String, Object> generateFeeds(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        String storeId = (String) context.get("productStoreId");
        int total = 0;
        try {
            if (UtilValidate.isNotEmpty(storeId)) {
                String baseUrl = (String) context.get("baseUrl");
                if (UtilValidate.isEmpty(baseUrl)) return ServiceUtil.returnError("baseUrl is required with productStoreId");
                total = AcpFeed.generate(new AcpContext(delegator, dctx.getDispatcher(), storeId, (String) context.get("webSiteId"), null,
                        stripSlash(baseUrl), null, null, Locale.US));
            } else {
                File root = AcpFeed.root(delegator); // only the feeds of this tenant
                File[] dirs = root.listFiles(File::isDirectory);
                for (File dir : dirs != null ? dirs : new File[0]) {
                    File meta = new File(dir, "metadata.json");
                    if (!meta.isFile()) continue;
                    Map<String, Object> m = AcpJson.parse(new String(Files.readAllBytes(meta.toPath()), StandardCharsets.UTF_8));
                    Map<String, Object> x = AcpJson.map(m.get("x_scipio"));
                    String id = AcpJson.str(m, "id");
                    if (x == null || id == null || !id.startsWith("feed_")) continue;
                    try {
                        total += AcpFeed.generate(new AcpContext(delegator, dctx.getDispatcher(), id.substring(5), AcpJson.str(x, "webSiteId"),
                                AcpJson.str(x, "webappName"), AcpJson.str(x, "baseUrl"), null, null, Locale.US));
                    } catch (IOException | GenericEntityException | RuntimeException e) {
                        Debug.logError(e, "ACP: feed of " + id + " failed: " + e.getMessage(), module);
                    }
                }
            }
        } catch (IOException | GenericEntityException e) {
            return ServiceUtil.returnError("acpGenerateFeeds: " + e.getMessage());
        }
        Map<String, Object> out = ServiceUtil.returnSuccess();
        out.put("products", total);
        return out;
    }

    // ---------------------------------------------------------------- order hooks

    @Service(
        name = "acpOrderChanged",
        engine = "java",
        location = IMPL,
        invoke = "orderChanged",
        description = "Hook of order and shipment changes: writes an ACP order_update event when the order came from an agent "
                + "checkout and starts the delivery. Called by the ECAs; no permission check.",
        auth = "false",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface AcpOrderChanged {}

    @Seca(
        service = "changeOrderStatus",
        event = "global-commit",
        actions = {
            @SecaAction(service = "acpOrderChanged", mode = "sync", newTransaction = "true", ignoreError = "true", ignoreFailure = "true",
                    resultToContext = "false")
        }
    )
    public interface ChangeOrderStatusAcpSeca {}

    @Seca(
        service = "updateShipment",
        event = "global-commit",
        actions = {
            @SecaAction(service = "acpOrderChanged", mode = "sync", newTransaction = "true", ignoreError = "true", ignoreFailure = "true",
                    resultToContext = "false")
        }
    )
    public interface UpdateShipmentAcpSeca {}

    @Seca(
        service = "createReturnItemResponse",
        event = "global-commit",
        actions = {
            @SecaAction(service = "acpOrderChanged", mode = "sync", newTransaction = "true", ignoreError = "true", ignoreFailure = "true",
                    resultToContext = "false")
        }
    )
    public interface ReturnResponseAcpSeca {}

    public static Map<String, Object> orderChanged(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        String orderId = (String) context.get("orderId");
        try {
            if (UtilValidate.isEmpty(orderId) && UtilValidate.isNotEmpty(context.get("shipmentId"))) {
                GenericValue sh = EntityQuery.use(delegator).from("Shipment").where("shipmentId", context.get("shipmentId")).queryOne();
                orderId = sh != null ? sh.getString("primaryOrderId") : null;
            }
            if (UtilValidate.isEmpty(orderId)) return ServiceUtil.returnSuccess();
            GenericValue session = EntityQuery.use(delegator).from("AcpCheckoutSession").where("orderId", orderId).queryFirst();
            if (session == null) return ServiceUtil.returnSuccess();
            AcpWebhooks.enqueueOrderEvent(delegator, session, AcpWebhooks.ORDER_UPDATE);
            dctx.getDispatcher().runAsync("acpDeliverWebhooks", UtilMisc.toMap("userLogin",
                    EntityQuery.use(delegator).from("UserLogin").where("userLoginId", "system").cache().queryOne()), false);
        } catch (Exception e) {
            Debug.logWarning("ACP: order " + orderId + " event not queued: " + e.getMessage(), module);
        }
        return ServiceUtil.returnSuccess();
    }

    private static String stripSlash(String s) {
        return s.endsWith("/") ? s.substring(0, s.length() - 1) : s;
    }
}
