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
package com.ilscipio.scipio.shop.misc;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.Timestamp;
import java.time.Instant;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.product.store.ProductStoreWorker;

import com.ilscipio.scipio.product.store.StorePauses;

/**
 * The storefront of a paused store (R4-15, J5-01). A request preprocessor: while the seller pauses the store, each page
 * answers 503 with the note and the back-on day, so the storefront takes no order. The text is the text of R4-10.
 *
 * <p>SCIPIO: 4.0.0: Added (R4-15).</p>
 */
public final class StorePauseEvents {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private StorePauseEvents() {
    }

    public static String checkPause(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        String productStoreId = ProductStoreWorker.getProductStoreId(request);
        GenericValue row;
        try {
            row = StorePauses.row(delegator, productStoreId);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Could not read the pause of the store " + productStoreId, module);
            return respondUnknown(request.getRequestURI(), response);
        }
        if (row == null) {
            return "success";
        }
        return respond(row.getTimestamp("pausedDate"), row.getTimestamp("pauseUntilDate"), row.getString("pauseNote"), () -> {
            GenericValue store = ProductStoreWorker.getProductStore(productStoreId, delegator);
            return store == null ? null : store.getString("storeName");
        }, Instant.now(), response);
    }

    /**
     * The rule of the preprocessor: a live store (no pause, or the back-on day passed) gives "success" and the request runs on.
     * A paused store gets 503, Retry-After and the pause page, and "none" stops the request: no page, no cart, no checkout.
     */
    static String respond(Timestamp pausedDate, Timestamp until, String note, java.util.function.Supplier<String> storeName, Instant now,
                          HttpServletResponse response) {
        if (!StorePauses.isPaused(pausedDate, until, now)) {
            return "success";
        }
        String name = storeName.get();
        try {
            response.setStatus(503);
            response.setHeader("Retry-After", "3600");
            response.setHeader("Cache-Control", "no-store");
            response.setContentType("text/html;charset=UTF-8");
            response.getOutputStream().write(page(name == null ? "The shop" : name, until, note).getBytes(StandardCharsets.UTF_8));
        } catch (IOException e) {
            Debug.logWarning("Could not write the pause page: " + e.getMessage(), module);
        }
        return "none";
    }

    /**
     * The state of the pause is unknown (a read error, review C7): the checkout fails closed (503 and a short page), the other
     * pages of the storefront stay open.
     */
    static String respondUnknown(String requestUri, HttpServletResponse response) {
        if (!isCheckout(requestUri)) {
            return "success";
        }
        try {
            response.setStatus(503);
            response.setHeader("Retry-After", "60");
            response.setHeader("Cache-Control", "no-store");
            response.setContentType("text/html;charset=UTF-8");
            response.getOutputStream().write(("<!doctype html><html lang=\"en\"><meta charset=\"utf-8\"><title>Please try again</title>"
                    + "<body style=\"font-family:system-ui,sans-serif;max-width:32rem;margin:20vh auto;padding:0 1rem\">"
                    + "<h1>Please try again</h1><p>The shop cannot take orders right now. Please try again in a minute.</p></body></html>")
                    .getBytes(StandardCharsets.UTF_8));
        } catch (IOException e) {
            Debug.logWarning("Could not write the page: " + e.getMessage(), module);
        }
        return "none";
    }

    /** A request of the checkout: the last part of the path names a checkout, an order or a payment step. */
    static boolean isCheckout(String requestUri) {
        if (requestUri == null) {
            return false;
        }
        String path = requestUri;
        int q = path.indexOf('?');
        if (q >= 0) {
            path = path.substring(0, q);
        }
        int semi = path.indexOf(';');
        if (semi >= 0) {
            path = path.substring(0, semi);
        }
        String last = path.substring(path.lastIndexOf('/') + 1).toLowerCase(java.util.Locale.ROOT);
        return last.contains("checkout") || last.contains("order") || last.contains("payment");
    }

    /** The page "<name> is taking a break" with the back-on day and the note (escaped). */
    static String page(String name, Timestamp until, String note) {
        UtilCodec.SimpleEncoder html = UtilCodec.getEncoder("html");
        String back = until == null ? "" : " Back on " + until.toInstant().toString().substring(0, 10) + ".";
        String noteHtml = note == null || note.isEmpty() ? "" : "<p>" + html.encode(note) + "</p>";
        return "<!doctype html><html lang=\"en\"><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">"
                + "<title>" + html.encode(name) + " is taking a break</title>"
                + "<body style=\"font-family:system-ui,sans-serif;max-width:32rem;margin:20vh auto;padding:0 1rem\">"
                + "<h1>" + html.encode(name) + " is taking a break</h1><p>The shop takes no orders for now." + html.encode(back) + "</p>"
                + noteHtml + "</body></html>";
    }
}
