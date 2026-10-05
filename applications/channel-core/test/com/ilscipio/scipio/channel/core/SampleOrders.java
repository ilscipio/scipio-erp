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

import java.math.BigDecimal;
import java.time.Instant;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/** Order data for the tests. */
final class SampleOrders {
    private SampleOrders() {
    }

    static IncomingOrder paid(String externalId, String currency, String sku, int qty) {
        return order(externalId, "PAID", currency, sku, qty, "BUYER-1");
    }

    static IncomingOrder order(String externalId, String status, String currency, String sku, int qty, String buyer) {
        Map<String, String> ship = new LinkedHashMap<>();
        ship.put("name", "Erika Muster");
        ship.put("line1", "Hauptstrasse 1");
        ship.put("city", "Berlin");
        ship.put("postalCode", "10115");
        ship.put("countryCode", "DE");
        return new IncomingOrder(externalId, status, Fixture.T0, null, buyer, "Erika Muster", "erika@example.com", ship,
                Collections.singletonList(new IncomingOrder.Line("L1", sku, null, "A product", qty, new BigDecimal("10.00"),
                        new BigDecimal("1.60"))),
                currency, new BigDecimal("4.90"), new BigDecimal("1.60"), new BigDecimal("64.90"), new BigDecimal("6.50"), true,
                null);
    }

    @SuppressWarnings("unused")
    static Instant at(String iso) {
        return Instant.parse(iso);
    }
}
