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
package org.ofbiz.service.eca;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

public class EcaConditionExpressionTest {

    private static Map<String, Object> vars(Object... kv) {
        Map<String, Object> m = new HashMap<>();
        for (int i = 0; i < kv.length; i += 2) m.put((String) kv[i], kv[i + 1]);
        return m;
    }

    @Test
    public void blankIsTrue() {
        assertTrue(EcaConditionExpression.eval(null, vars(), "t"));
        assertTrue(EcaConditionExpression.eval("  ", vars(), "t"));
    }

    @Test
    public void emptyHelperAndMissingVariables() {
        assertTrue(EcaConditionExpression.eval("empty(quoteId)", vars(), "t"));
        assertTrue(EcaConditionExpression.eval("empty(quoteId)", vars("quoteId", ""), "t"));
        assertFalse(EcaConditionExpression.eval("!empty(quoteId)", vars(), "t"));
        assertTrue(EcaConditionExpression.eval("!empty(quoteId)", vars("quoteId", "Q1"), "t"));
        assertTrue(EcaConditionExpression.eval("!empty(expMonth) && !empty(expYear)", vars("expMonth", "01", "expYear", "2030"), "t"));
    }

    @Test
    public void comparisonsAndNestedAccess() {
        assertTrue(EcaConditionExpression.eval("statusId == 'ORDER_APPROVED'", vars("statusId", "ORDER_APPROVED"), "t"));
        assertFalse(EcaConditionExpression.eval("statusId == 'ORDER_APPROVED'", vars("statusId", "ORDER_CREATED"), "t"));
        assertFalse(EcaConditionExpression.eval("statusId == 'ORDER_APPROVED'", vars(), "t"));
        assertTrue(EcaConditionExpression.eval("quantity > 0", vars("quantity", new BigDecimal("2.5")), "t"));
        assertTrue(EcaConditionExpression.eval("autoCreateKeywords != 'N'", vars(), "t"));
        Map<String, Object> userLogin = vars("partyId", "P1");
        assertTrue(EcaConditionExpression.eval("empty(partyId) && !empty(userLogin) && !empty(userLogin.partyId)", vars("userLogin", userLogin), "t"));
        assertTrue(EcaConditionExpression.eval("xor(true, false)", vars(), "t"));
    }

    @Test
    public void brokenExpressionIsFalse() {
        assertFalse(EcaConditionExpression.eval("this is not groovy (", vars(), "t"));
    }
}
