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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayOutputStream;
import java.lang.reflect.Proxy;
import java.nio.charset.StandardCharsets;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletOutputStream;
import javax.servlet.WriteListener;
import javax.servlet.http.HttpServletResponse;

import org.junit.jupiter.api.Test;

/** R4-15 (J5-01): the shop preprocessor of a paused store. */
public class StorePauseEventsTest {
    static final Instant NOW = Instant.parse("2026-10-10T10:00:00Z");
    static final Timestamp SINCE = Timestamp.from(Instant.parse("2026-10-09T08:00:00Z"));

    /** A response that records the status, the headers and the body. */
    static final class Recorded {
        int status = 200;
        final Map<String, String> headers = new HashMap<>();
        final ByteArrayOutputStream body = new ByteArrayOutputStream();
        final HttpServletResponse response = (HttpServletResponse) Proxy.newProxyInstance(StorePauseEventsTest.class.getClassLoader(),
                new Class<?>[] {HttpServletResponse.class}, (proxy, method, args) -> {
                    switch (method.getName()) {
                        case "setStatus":
                            status = (Integer) args[0];
                            return null;
                        case "setHeader":
                            headers.put((String) args[0], (String) args[1]);
                            return null;
                        case "setContentType":
                            headers.put("Content-Type", (String) args[0]);
                            return null;
                        case "getOutputStream":
                            return new ServletOutputStream() {
                                @Override
                                public void write(int b) {
                                    body.write(b);
                                }

                                public boolean isReady() {
                                    return true;
                                }

                                public void setWriteListener(WriteListener listener) {
                                }
                            };
                        default:
                            return null;
                    }
                });

        String text() {
            return new String(body.toByteArray(), StandardCharsets.UTF_8);
        }
    }

    @Test
    void aPausedStoreAnswersEachPageWith503AndTheNote() {
        Recorded r = new Recorded();
        String answer = StorePauseEvents.respond(SINCE, Timestamp.from(Instant.parse("2026-10-12T23:59:59Z")), "On holiday <3",
                () -> "Candle & Co", NOW, r.response);
        assertEquals("none", answer, "the preprocessor stops the request: no page, no cart, no checkout");
        assertEquals(503, r.status);
        assertEquals("3600", r.headers.get("Retry-After"));
        assertEquals("no-store", r.headers.get("Cache-Control"));
        assertTrue(r.text().contains("Candle &amp; Co is taking a break"), r.text());
        assertTrue(r.text().contains("Back on 2026-10-12."), r.text());
        assertTrue(r.text().contains("On holiday &lt;3"), "the note is escaped: " + r.text());
    }

    @Test
    void aLiveStorePasses() {
        Recorded r = new Recorded();
        assertEquals("success", StorePauseEvents.respond(null, null, null, () -> "Candle & Co", NOW, r.response));
        assertEquals(200, r.status);
        assertEquals(0, r.body.size());
    }

    @Test
    void theBackOnDayOpensTheStorefront() {
        Recorded r = new Recorded();
        assertEquals("success", StorePauseEvents.respond(SINCE, Timestamp.from(Instant.parse("2026-10-10T09:59:59Z")), "On holiday",
                () -> "Candle & Co", NOW, r.response), "the back-on day passed: the job did not remove the row yet");
        assertEquals(200, r.status);
        assertNull(r.headers.get("Retry-After"));
    }

    @Test
    void anUnknownStateRefusesTheCheckoutAndKeepsTheOtherPagesOpen() {
        Recorded checkout = new Recorded();
        assertEquals("none", StorePauseEvents.respondUnknown("/shop/control/checkoutoptions;jsessionid=x?a=b", checkout.response),
                "review C7: the checkout fails closed when the pause cannot be read");
        assertEquals(503, checkout.status);
        assertTrue(checkout.text().contains("cannot take orders right now"), checkout.text());
        Recorded order = new Recorded();
        assertEquals("none", StorePauseEvents.respondUnknown("/shop/control/createOrder", order.response));
        Recorded page = new Recorded();
        assertEquals("success", StorePauseEvents.respondUnknown("/shop/control/product/candle", page.response), "a product page stays open");
        assertEquals(200, page.status);
    }

    @Test
    void aPauseWithoutABackOnDayNamesNoDay() {
        Recorded r = new Recorded();
        StorePauseEvents.respond(SINCE, null, null, () -> null, NOW, r.response);
        assertTrue(r.text().contains("The shop is taking a break"), r.text());
        assertFalse(r.text().contains("Back on"), r.text());
    }
}
